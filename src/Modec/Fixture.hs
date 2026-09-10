-- | The @.call@ file: what a corpus fixture says about itself.
--
-- A fixture is three files sharing a stem -- @NAME.wav@, the recording;
-- @NAME.txt@, the decode this modem produced the day it was minted; and
-- @NAME.call@, a spec saying how to replay the first and what to make of
-- the result.
--
-- The format was written by @modec replay --mint@ and read by the test
-- corpus, in two components that cannot import each other: the
-- executable links @process@ and @network@, and the test suite is
-- forbidden them (see the note in modec.cabal).  So each had its own
-- copy, and a rename on either side would not have failed to compile --
-- it would have broken the corpus at test time, or worse, quietly
-- stopped asserting something.  Both sides read this module now.
--
-- Reading the file is the caller's business, as it is for
-- "Modec.Replay": what is here is pure.
module Modec.Fixture
  ( CallSpec (..)
  , emptyCallSpec
  , parseCallSpec
  , renderCallSpec
  , callSpecConfig
  , connectLine
  , retrainCount
  ) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)

import Modec.Handshake (HsConfig (..))
import Modec.Link (linkBitRate)
import Modec.Mnp (MnpConfig (..), defaultMnpConfig)
import Modec.Modem (ModemConfig (..), ModemEvent (..), defaultModemConfig)
import Modec.Standards

-- | Everything a @.call@ file can say.
--
-- The comment lines are carried rather than discarded: they are where a
-- fixture explains what it is for, and 'renderCallSpec' has to be able
-- to give back what it was given.
data CallSpec = CallSpec
  { csComment   :: [String]        -- ^ leading @#@ lines, without the @#@
  , csSource    :: Maybe FilePath  -- ^ where the recording came from
  , csSeconds   :: Maybe Double    -- ^ stop the replay here
  , csRole      :: Role            -- ^ which end this modem was
  , csModes     :: [Standard]      -- ^ modes to negotiate, best first
  , csV8        :: Bool            -- ^ V.8 was in use on the call
  , csMnp       :: Maybe Int       -- ^ MNP class offered, if any
  , csStandard  :: Maybe String    -- ^ the other kind of fixture: one modulation, no call
  , csConnect   :: String          -- ^ what the call must reach, or @"none"@
  , csRetrains  :: [Int]           -- ^ ceilings on 5.5 retrains
  , csExpect    :: [String]        -- ^ substrings a human verified the far end sent
  , csTolerance :: Int             -- ^ edit distance allowed from @NAME.txt@
  } deriving (Eq, Show)

emptyCallSpec :: CallSpec
emptyCallSpec = CallSpec
  { csComment = [], csSource = Nothing, csSeconds = Nothing
  , csRole = Originate, csModes = allStandards, csV8 = False, csMnp = Nothing
  , csStandard = Nothing, csConnect = "none", csRetrains = []
  , csExpect = [], csTolerance = 0 }

-- | Comment lines, then @key: value@ lines.  Keys may repeat, which is
-- how a fixture asks for more than one @expect:@.
--
-- Left on anything it cannot make sense of, so a typo in a mode name is
-- a failure with a reason rather than a fixture that silently asserts
-- less than it meant to.
parseCallSpec :: String -> Either String CallSpec
parseCallSpec src = do
    modes <- case key "modes" of
      Nothing -> Right (csModes emptyCallSpec)
      Just ms -> mapM named (splitOn ',' ms)
    return emptyCallSpec
      { csComment   = [ dropWhile isSpace (drop 1 (dropWhile isSpace l))
                      | l <- lines src, isComment l ]
      , csSource    = key "source"
      , csSeconds   = fmap read (key "seconds")
      , csRole      = if key "role" == Just "answer" then Answer else Originate
      , csModes     = modes
      , csV8        = flag "v8"
      , csMnp       = fmap read (key "mnp")
      , csStandard  = key "standard"
      , csConnect   = fromMaybe "none" (key "connect")
      , csRetrains  = map read (keys "retrains")
      , csExpect    = keys "expect"
      , csTolerance = maybe 0 read (key "tolerance")
      }
  where
    named m = maybe (Left ("unknown mode " ++ show m)) Right (standardNamed m)
    entries = mapMaybe entry (lines src)
    entry l
      | isComment l = Nothing
      | (k, ':' : v) <- break (== ':') l, not (null k) = Just (trim k, trim v)
      | otherwise = Nothing
    isComment l = case dropWhile isSpace l of { ('#' : _) -> True; _ -> False }
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    key k = lookup k entries
    keys k = [ v | (k', v) <- entries, k' == k, not (null v) ]
    flag k = key k `elem` [Just "yes", Just "true", Just "on"]

-- | The inverse.  What @--mint@ writes.
--
-- The @expect:@ line is emitted empty when there is nothing to say,
-- because what the far end really sent is not something a decode can
-- assert about itself; a human fills it in.
renderCallSpec :: CallSpec -> String
renderCallSpec cs
  -- A fixture that names a standard: is one modulation and no call.
  -- There is no end to be, nothing to negotiate, and nothing to be
  -- tolerant of -- fskCase asserts the bytes exactly.  Writing the call
  -- keys into one would be lines that assert nothing and read as though
  -- they did.
  | Just n <- csStandard cs = unlines (comments ++ ["standard:  " ++ n])
  | otherwise = unlines $
       comments
    ++ [ "source:    " ++ p | Just p <- [csSource cs] ]
    ++ [ "seconds:   " ++ show s | Just s <- [csSeconds cs] ]
    ++ [ "role:      " ++ (case csRole cs of { Answer -> "answer"; Originate -> "originate" })
       , "modes:     " ++ intercalate "," (map standardName (csModes cs))
       , "v8:        " ++ (if csV8 cs then "yes" else "no") ]
    ++ [ "mnp:       " ++ show c | Just c <- [csMnp cs] ]
    ++ [ "connect:   " ++ csConnect cs ]
    ++ [ "retrains:  " ++ show n | n <- csRetrains cs ]
    ++ [ "expect:    " ++ e | e <- expects ]
    ++ [ "tolerance: " ++ show (csTolerance cs) ]
  where
    comments = [ "# " ++ c | c <- csComment cs ]
    expects = if null (csExpect cs) then [""] else csExpect cs

-- | The modem a fixture is replayed through.  This is the assembly that
-- used to be written out in the executable, the corpus and the replay
-- command separately, all three setting 'hcV8' and 'mcMnp' the same way
-- by hand.
callSpecConfig :: Double -> CallSpec -> ModemConfig
callSpecConfig fs cs = cfg0
  { mcHandshake = (mcHandshake cfg0) { hcV8 = csV8 cs }
  , mcMnp = fmap (\c -> (defaultMnpConfig 2400 (csRole cs == Originate)) { mnClass = c })
                 (csMnp cs)
  }
  where cfg0 = defaultModemConfig fs (csRole cs) (csModes cs)

-- | What a run of the modem connected at, in the spelling @connect:@
-- uses.  The minter writes this and the corpus compares against it, so
-- there is one function and they cannot disagree about the format.
connectLine :: [(Double, ModemEvent)] -> String
connectLine evs = case [ (s, l) | (_, EvConnected s l) <- evs ] of
  ((s, l) : _) -> show s ++ " " ++ show (round (linkBitRate l) :: Int)
  [] -> "none"

-- | How many times the call retrained (V.32 §5.5).
retrainCount :: [(Double, ModemEvent)] -> Int
retrainCount evs = length [ () | (_, EvRetrain _) <- evs ]

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])    -> [a]
  (a, _ : b) -> a : splitOn c b
