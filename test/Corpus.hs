-- | Fixture recordings, and reading them back.
--
-- A fixture is three files with one name: @NAME.wav@, the audio;
-- @NAME.txt@, what it decodes to; and @NAME.call@, a plain-text spec
-- saying how to decode it and what is actually known about it.  The
-- loader walks the directory, so adding a recording to the suite is
-- adding three files and nothing else.
--
-- Two kinds of spec, told apart by which key names the modem:
--
--   * @standard:@ points a demodulator straight at the file.  That works
--     for a synthesised fixture, where the audio is nothing but the
--     modulation.
--
--   * @modes:@ runs the whole modem over it, which is the only faithful
--     way to read a recorded call back -- the handshake starts the data
--     receiver at the right instant, at the right rate, in the right
--     channel, and a receiver pointed at the top of the file does none
--     of that.  See "Modec.Replay".
--
-- Three assertions, and the difference between them matters.
-- @connect:@ names the standard and bit rate the call must reach.
-- @expect:@ lines are the hand-verified truth: text the far end really
-- sent, checked against the service's own documentation or read off the
-- board by eye.  Those must never break.  The @.txt@ reference is
-- weaker -- it is what this recording decoded to on the day it was
-- minted, junk included -- and @tolerance:@ says how far the decode may
-- drift from it in bytes of edit distance.  A deliberate improvement
-- that moves the reference is accepted by re-minting:
--
-- > cabal run modec -- replay --mint NAME --mode M --seconds N FILE.wav
--
-- The @source:@ and @seconds:@ keys are there so that command can be
-- reconstructed from the fixture.
module Corpus (fixtureDir, liveDir, fixtureTests, liveTests) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, replaceExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Modec.DSP (Signal)
import Modec.FSK
import Modec.Handshake (Role (..), Standard (..), hcV8)
import Modec.Metrics (editDistance)
import Modec.Mnp (MnpConfig (..), defaultMnpConfig)
import Modec.Modem
import Modec.Replay
import Modec.Standards
import Modec.Wav

fixtureDir :: FilePath
fixtureDir = "test/fixtures"

liveDir :: FilePath
liveDir = fixtureDir </> "live"

-- | A @.call@ file: comment lines, then @key: value@ lines.  Keys may
-- repeat, which is how a fixture asks for more than one @expect:@.
type Spec = [(String, String)]

parseSpec :: String -> Spec
parseSpec = mapMaybe entry . lines
  where
    entry l
      | (c : _) <- dropWhile isSpace l, c == '#' = Nothing
      | (k, ':' : v) <- break (== ':') l, not (null k) = Just (trim k, trim v)
      | otherwise = Nothing
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

key :: Spec -> String -> Maybe String
key sp k = lookup k sp

keys :: Spec -> String -> [String]
keys sp k = [ v | (k', v) <- sp, k' == k, not (null v) ]

flagOf :: Spec -> String -> Bool
flagOf sp k = key sp k `elem` [Just "yes", Just "true", Just "on"]

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])      -> [a]
  (a, _ : b)   -> a : splitOn c b

standardOf :: String -> Standard
standardOf m = case m of
  "bell103" -> Bell103; "v21" -> V21; "v23" -> V23; "bell212a" -> Bell212A
  "v22" -> V22; "v22bis" -> V22bis; "v32" -> V32; "v32bis" -> V32bis
  _ -> error ("unknown mode " ++ m)

-- | The synthesised fixtures: audio that is nothing but the modulation,
-- so a demodulator can be pointed at the head of the file.
fixtureTests :: IO TestTree
fixtureTests = corpusGroup "minimodem fixtures" fixtureDir fskCase

-- | The recorded calls.
liveTests :: IO TestTree
liveTests = corpusGroup "live calls" liveDir replayCase

corpusGroup :: String -> FilePath -> (FilePath -> Spec -> B.ByteString -> Wav -> Assertion)
            -> IO TestTree
corpusGroup label dir run = do
  files <- sort . filter (".wav" `isSuffixOf`) <$> listDirectory dir
  cases <- forM files $ \f -> do
    sp <- parseSpec <$> readFile (dir </> replaceExtension f "call")
    return $ testCase (dropExtension f) $ do
      w <- readWav (dir </> f)
      expected <- B.readFile (dir </> replaceExtension f "txt")
      run f sp expected w
  return (testGroup label cases)

fskCase :: FilePath -> Spec -> B.ByteString -> Wav -> Assertion
fskCase f sp expected w = do
  let spec = specOf (fromMaybe (error (f ++ ": no standard:")) (key sp "standard"))
      fs = fromIntegral (wavRate w)
      got = B.pack (demodulate fs spec framing8N1 defaultDemodParams (wavSamples w))
  assertEqual "decoded bytes" (BC.unpack expected) (BC.unpack got)
  where
    specOf n = case n of
      "bell103-originate" -> bell103Originate
      "bell103-answer"    -> bell103Answer
      "v21-ch1"           -> v21Channel1
      "v21-ch2"           -> v21Channel2
      _                   -> error (f ++ ": unknown standard " ++ n)

replayCase :: FilePath -> Spec -> B.ByteString -> Wav -> Assertion
replayCase f sp expected w = do
  let fs = fromIntegral (wavRate w) :: Double
      role = if key sp "role" == Just "answer" then Answer else Originate
      modes = map standardOf (splitOn ',' (fromMaybe (error (f ++ ": no modes:")) (key sp "modes")))
      cfg0 = defaultModemConfig fs role modes
      cfg = cfg0
        { mcHandshake = (mcHandshake cfg0) { hcV8 = flagOf sp "v8" }
        , mcMnp = fmap (\c -> (defaultMnpConfig 2400 (role == Originate)) { mnClass = read c })
                       (key sp "mnp")
        }
      r = replay (defaultReplayConfig cfg) (wavSamples w :: Signal)
      got = rrBytes r
      text = map (toEnum . fromIntegral) got :: String
      connected = [ show s ++ " " ++ show (round (linkBitRate l) :: Int)
                  | (_, EvConnected s l) <- rrEvents r ]
      want = fromMaybe "none" (key sp "connect")
      tol = maybe 0 read (key sp "tolerance") :: Int
      retrains = length [ () | (_, EvRetrain _) <- rrEvents r ]

  -- what the call reached, which is the first thing a live fixture is for
  case (want, connected) of
    ("none", []) -> return ()
    ("none", (c : _)) -> assertFailure
      ("connected " ++ c ++ ", and this recording has no far end that can: "
       ++ show (length got) ++ " bytes came back")
    (_, []) -> assertFailure ("never connected; wanted " ++ want ++ trace r)
    (_, (c : _)) -> assertEqual ("connect" ++ trace r) want c

  -- 5.5, and whether the link held.  Optional, because most fixtures
  -- are below V.32 and cannot retrain at all; where it is stated it is
  -- the only thing that separates a rate that connects from a rate that
  -- works.  A V.32bis call at 14400 connected and delivered nothing
  -- either side of the receiver fix that made it usable -- what changed
  -- was that it stopped asking for a retrain four seconds in.
  mapM_ (\n -> assertBool
           ("retrained " ++ show retrains ++ " times, over " ++ show n ++ trace r)
           (retrains <= n))
        (map read (keys sp "retrains") :: [Int])

  -- the hand-verified truth: what the far end really sent
  mapM_ (\e -> assertBool ("missing from the decode: " ++ show e ++ "\n" ++ show text)
                          (e `isInfixOf` text))
        (keys sp "expect")

  -- and the drift alarm against the day this was minted
  let d = editDistance (B.unpack expected) got
  assertBool ("decode is " ++ show d ++ " bytes from the reference, tolerance "
              ++ show tol ++ "\n  reference: " ++ show (BC.unpack expected)
              ++ "\n  got:       " ++ show text)
             (d <= tol)
  where
    trace r = "\n" ++ unlines [ "  " ++ show t ++ "  " ++ ph | (t, ph) <- rrPhases r ]
