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
module Corpus (fixtureDir, liveDir, fixtureTests, liveTests, specTests) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, replaceExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Modec.Link
import Modec.DSP (Signal)
import Modec.FSK
import Modec.Fixture
import Modec.Metrics (editDistance)
import Modec.Modem
import Modec.Replay
import Modec.Standards
import Modec.Wav

fixtureDir :: FilePath
fixtureDir = "test/fixtures"

liveDir :: FilePath
liveDir = fixtureDir </> "live"

-- | The synthesised fixtures: audio that is nothing but the modulation,
-- so a demodulator can be pointed at the head of the file.
fixtureTests :: IO TestTree
fixtureTests = corpusGroup "minimodem fixtures" fixtureDir fskCase

-- | The recorded calls.
liveTests :: IO TestTree
liveTests = corpusGroup "live calls" liveDir replayCase

-- | The @.call@ format itself, against every fixture checked in.
--
-- Until Modec.Fixture there was no such thing to assert: the minter and
-- this corpus each had their own idea of the format, and the only place
-- they met was a file on disk.  Rendering a parsed spec and parsing it
-- again has to give the same spec back, and every key a fixture actually
-- uses has to survive the trip -- which is what would have caught a
-- reader and a writer drifting apart.
specTests :: IO TestTree
specTests = do
  fx <- specsIn fixtureDir
  lv <- specsIn liveDir
  return $ testGroup "the .call format"
    [ testCase "every fixture parses" $
        forM_ (fx ++ lv) $ \(f, src) ->
          case parseCallSpec src of
            Left e -> assertFailure (f ++ ": " ++ e)
            Right _ -> return ()
    , testCase "rendering a spec and reading it back is a fixed point" $
        forM_ (fx ++ lv) $ \(f, src) -> do
          sp <- either (assertFailure . ((f ++ ": ") ++)) return (parseCallSpec src)
          sp' <- either (assertFailure . ((f ++ ", re-read: ") ++)) return
                        (parseCallSpec (renderCallSpec sp))
          assertEqual (f ++ ": survived a round trip") sp sp'
    , testCase "what a fixture asserts survives being written out" $
        forM_ (fx ++ lv) $ \(f, src) -> do
          sp <- either (assertFailure . ((f ++ ": ") ++)) return (parseCallSpec src)
          let back = renderCallSpec sp
              stated k = [ trim v | l <- lines src, (k', ':' : v) <- [break (== ':') l], k' == k ]
              rendered k = [ trim v | l <- lines back, (k', ':' : v) <- [break (== ':') l], k' == k ]
              trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
          -- Every key the corpus reads, so a key that stopped being
          -- rendered would fail here rather than silently stop asserting.
          forM_ ["role", "modes", "v8", "mnp", "standard", "connect", "retrains", "tolerance"] $ \k ->
            assertEqual (f ++ ": " ++ k) (filter (not . null) (stated k))
                                         (filter (not . null) (rendered k))
    ]
  where
    specsIn dir = do
      files <- sort . filter (".call" `isSuffixOf`) <$> listDirectory dir
      mapM (\f -> (,) f <$> readFile (dir </> f)) files

corpusGroup :: String -> FilePath -> (FilePath -> CallSpec -> B.ByteString -> Wav -> Assertion)
            -> IO TestTree
corpusGroup label dir run = do
  files <- sort . filter (".wav" `isSuffixOf`) <$> listDirectory dir
  cases <- forM files $ \f -> do
    src <- readFile (dir </> replaceExtension f "call")
    sp <- either (fail . ((f ++ ": ") ++)) return (parseCallSpec src)
    return $ testCase (dropExtension f) $ do
      w <- readWav (dir </> f)
      expected <- B.readFile (dir </> replaceExtension f "txt")
      run f sp expected w
  return (testGroup label cases)

fskCase :: FilePath -> CallSpec -> B.ByteString -> Wav -> Assertion
fskCase f sp expected w = do
  let spec = specOf (fromMaybe (error (f ++ ": no standard:")) (csStandard sp))
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

replayCase :: FilePath -> CallSpec -> B.ByteString -> Wav -> Assertion
replayCase _ sp expected w = do
  let fs = fromIntegral (wavRate w) :: Double
      cfg = callSpecConfig fs sp
      r = replay (defaultReplayConfig cfg) (wavSamples w :: Signal)
      got = rrBytes r
      text = map (toEnum . fromIntegral) got :: String
      connected = connectLine (rrEvents r)
      want = csConnect sp
      tol = csTolerance sp
      retrains = retrainCount (rrEvents r)

  -- what the call reached, which is the first thing a live fixture is for
  case (want, connected) of
    ("none", "none") -> return ()
    ("none", c) -> assertFailure
      ("connected " ++ c ++ ", and this recording has no far end that can: "
       ++ show (length got) ++ " bytes came back")
    (_, "none") -> assertFailure ("never connected; wanted " ++ want ++ trace r)
    (_, c) -> assertEqual ("connect" ++ trace r) want c

  -- 5.5, and whether the link held.  Optional, because most fixtures
  -- are below V.32 and cannot retrain at all; where it is stated it is
  -- the only thing that separates a rate that connects from a rate that
  -- works.  A V.32bis call at 14400 connected and delivered nothing
  -- either side of the receiver fix that made it usable -- what changed
  -- was that it stopped asking for a retrain four seconds in.
  mapM_ (\n -> assertBool
           ("retrained " ++ show retrains ++ " times, over " ++ show n ++ trace r)
           (retrains <= n))
        (csRetrains sp)

  -- the hand-verified truth: what the far end really sent
  mapM_ (\e -> assertBool ("missing from the decode: " ++ show e ++ "\n" ++ show text)
                          (e `isInfixOf` text))
        (csExpect sp)

  -- and the drift alarm against the day this was minted
  let d = editDistance (B.unpack expected) got
  assertBool ("decode is " ++ show d ++ " bytes from the reference, tolerance "
              ++ show tol ++ "\n  reference: " ++ show (BC.unpack expected)
              ++ "\n  got:       " ++ show text)
             (d <= tol)
  where
    trace r = "\n" ++ unlines [ "  " ++ show t ++ "  " ++ ph | (t, ph) <- rrPhases r ]
