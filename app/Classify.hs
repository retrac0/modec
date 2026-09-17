-- | @modec classify@: what answered each recorded call, read from the
-- audio, set beside what the modem logged at the time and summed up per
-- number against the lists the numbers came from.
--
-- The recordings directory holds two generations of call.  Calls placed
-- through @modec dial@ and the Hayes port have a @STEM.log@ written by
-- "CallLog", whose last line is the modem's outcome and whose header
-- names the number.  The earlier sweep of the BBS list wrote only audio,
-- named after the board and the configuration tried, so their number is
-- recovered by matching that name against the candidate lists.
module Classify
  ( ClassifyOpts (..)
  , runClassify
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, replicateM_, unless, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortOn, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeFileName, (</>))
import System.IO
import Text.Printf (printf)

import Modec.Classify
import Modec.DSP (Signal, resampleTo)
import Modec.Wav

data ClassifyOpts = ClassifyOpts
  { coTruth      :: Bool
  , coByNumber   :: Bool
  , coAll        :: Bool
  , coQuiet      :: Bool
  , coCandidates :: [FilePath]
  , coPaths      :: [FilePath]
  }

-- | One recording and what is known about it besides its audio.
data Call = Call
  { clPath   :: FilePath
  , clStem   :: String
  , clNumber :: Maybe String
  , clLogged :: Maybe String    -- ^ the modem's outcome, if it wrote one
  , clTruth  :: Maybe String    -- ^ the coarse class that outcome implies
  }

data Result = Result
  { rsCall  :: Call
  , rsClass :: Either String CallClass
  , rsEv    :: Maybe Evidence
  }

runClassify :: ClassifyOpts -> IO ()
runClassify o = do
  let paths = if null (coPaths o) then ["recordings"] else coPaths o
  wavs <- concat <$> mapM expand paths
  cands <- concat <$> mapM readCandidates (coCandidates o)
  calls <- mapM (describeCall cands) wavs
  let wanted = [ c | c <- calls, coAll o || not (isBench c) ]
  results <- classifyAll (not (coQuiet o)) wanted
  when (coTruth o) (truthReport results)
  when (coByNumber o) (numberReport cands results)
  listenReport results

-- | A WAV names itself; a directory contributes its received-audio WAVs,
-- the top level only.
expand :: FilePath -> IO [FilePath]
expand p = do
  dir <- doesDirectoryExist p
  if not dir
    then return [p]
    else do
      fs <- listDirectory p
      return (sort [ p </> f | f <- fs, ".wav" `isSuffixOf` f, not ("-tx.wav" `isSuffixOf` f) ])

-- | Bench and loopback calls: the Asterisk extensions, modec calling
-- itself, voip.ms's echo test and the made-up number.
isBench :: Call -> Bool
isBench c = maybe False (not . ("+" `isPrefixOf`)) (clNumber c)

describeCall :: [Candidate] -> FilePath -> IO Call
describeCall cands path = do
  let stem = dropExtension path
      base = takeFileName stem
      -- YYYYMMDDTHHMMSS-rest
      rest = case break (== '-') base of
        (ts, '-' : r) | length ts == 15, 'T' `elem` ts -> r
        _ -> base
  hasLog <- doesFileExist (stem ++ ".log")
  logLines <- if hasLog then lines <$> readFileStrict (stem ++ ".log") else return []
  let header = case logLines of
        (h : _) | "#" `isPrefixOf` h -> listToMaybe (reverse (words h))
        _ -> Nothing
      logged = listToMaybe [ drop (length marker) t
                           | l <- reverse logLines, t <- take 1 [ t | t <- tails l, marker `isPrefixOf` t ] ]
      marker = "call ended: "
      heardAnswer = any ("answer tone" `isInfixOf`) logLines
      number = case header of
        Just h -> Just (normalise h)
        Nothing
          | all (\ch -> isDigit ch || ch == '+') rest -> Just (normalise rest)
          | otherwise -> fmap cdNumber (bySlug cands rest)
      truth = case logged of
        Just l -> truthOf heardAnswer l
        Nothing -> case number >>= \n -> listToMaybe [ c | c <- cands, cdNumber c == n ] of
          Just c | cdNeverAnswered c -> Just "unanswered"
          _ -> Nothing
  return (Call path stem number logged truth)
  where
    readFileStrict f = do
      s <- readFile f
      _ <- evaluate (length s)
      return s

-- | The coarse class a logged outcome stands for, when it stands for
-- one.  A modem that failed after hearing an answer tone was still
-- talking to a modem; one that failed without hearing one says nothing.
truthOf :: Bool -> String -> Maybe String
truthOf heardAnswer l
  | "connected" `isPrefixOf` l = Just "modem"
  | "carrier lost" `isPrefixOf` l = Just "modem"
  | "failed" `isPrefixOf` l = if heardAnswer then Just "modem" else Nothing
  | "congestion" `isPrefixOf` l = Just "congestion"
  | "busy" `isPrefixOf` l = Just "busy"
  | "no answer" `isPrefixOf` l = Just "unanswered"
  | otherwise = Nothing

-- | The classes a truth label compares against.  The modem logging "no
-- answer" cannot tell ringing from a line that carried no audio at all.
coarse :: CallClass -> String
coarse c = case c of
  NoAnswer -> "unanswered"
  NoAudio -> "unanswered"
  Silence -> "unanswered"
  _ -> className c

-- | Classify every call, on as many threads as the RTS was given, and
-- print each as it completes if asked to.  Results come back in the
-- order the calls were given.
classifyAll :: Bool -> [Call] -> IO [Result]
classifyAll verbose calls = do
  caps <- getNumCapabilities
  queue <- newMVar (zip [0 :: Int ..] calls)
  out <- newMVar M.empty
  lock <- newMVar ()
  done <- newEmptyMVar
  let worker = do
        next <- modifyMVar queue (\q -> return (drop 1 q, listToMaybe q))
        case next of
          Nothing -> putMVar done ()
          Just (i, c) -> do
            r <- classifyOne c
            modifyMVar_ out (return . M.insert i r)
            when verbose (withMVar lock (\_ -> putStrLn (resultLine r) >> hFlush stdout))
            worker
  replicateM_ caps (forkIO worker)
  replicateM_ caps (takeMVar done)
  M.elems <$> readMVar out

classifyOne :: Call -> IO Result
classifyOne c = do
  r <- try $ do
    (fs, rx) <- load (clPath c)
    let txPath = clStem c ++ "-tx.wav"
    hasTx <- doesFileExist txPath
    tx <- if hasTx then Just . snd <$> load txPath else return Nothing
    let (cls, ev) = classifyCall fs tx rx
    _ <- evaluate (length (describeEvidence ev))
    return (cls, ev)
  return $ case r of
    Left e -> Result c (Left (show (e :: SomeException))) Nothing
    Right (cls, ev) -> Result c (Right cls) (Just ev)
  where
    load p = do
      w <- readWav p
      let fs0 = fromIntegral (wavRate w)
          x = if wavRate w == 8000 then wavSamples w else resampleTo fs0 8000 (wavSamples w)
      return (8000, x) :: IO (Double, Signal)

resultLine :: Result -> String
resultLine r =
  printf "%-44s %-13s %-30s %-28s %s"
    (takeFileName (clPath c)) (fromMaybe "?" (clNumber c))
    (either ("error: " ++) describeClass (rsClass r))
    (maybe "" ("logged: " ++) (clLogged c))
    (maybe "" describeEvidence (rsEv r))
  where c = rsCall r

-- | Verdicts against the modem's own outcomes: a confusion table, then
-- every call the two disagree on.
truthReport :: [Result] -> IO ()
truthReport rs = do
  let judged = [ (t, coarse cls, r) | r <- rs, Just t <- [clTruth (rsCall r)], Right cls <- [rsClass r] ]
      truths = nub (sort [ t | (t, _, _) <- judged ])
      preds = nub (sort [ p | (_, p, _) <- judged ])
      count t p = length [ () | (t', p', _) <- judged, t' == t, p' == p ]
      agree = length [ () | (t, p, _) <- judged, t == p ]
  putStrLn ""
  printf "Against the logged outcome: %d of %d agree\n\n" agree (length judged)
  printf "%-14s" "logged \\ heard"
  forM_ preds (printf "%13s")
  putStrLn ""
  forM_ truths $ \t -> do
    printf "%-14s" t
    forM_ preds (\p -> printf "%13s" (let n = count t p in if n == 0 then "." else show n))
    putStrLn ""
  let differ = [ r | (t, p, r) <- judged, t /= p ]
  unless (null differ) $ do
    putStrLn "\nDisagreements:"
    mapM_ (putStrLn . ("  " ++) . resultLine) differ

-- | The calls a person should listen to: every voice, every call the
-- rules could not place, and every file that would not read.
listenReport :: [Result] -> IO ()
listenReport rs = do
  let listen = [ r | r <- rs, case rsClass r of
                                Right Voice -> True
                                Right Unclassified -> True
                                Left _ -> True
                                _ -> False ]
  unless (null listen) $ do
    putStrLn "\nListen to these:"
    forM_ listen $ \r -> putStrLn ("  " ++ resultLine r)

-- | One line per number: its name on the lists, how often it was
-- called, what was heard each time, and the best of it.  Then the
-- candidates never called at all.
numberReport :: [Candidate] -> [Result] -> IO ()
numberReport cands rs = do
  let byNum = M.fromListWith (flip (++))
                [ (n, [cls]) | r <- rs, Just n <- [clNumber (rsCall r)], Right cls <- [rsClass r] ]
      nameOf n = maybe "" cdName (listToMaybe [ c | c <- cands, cdNumber c == n ])
  putStrLn "\nBy number:"
  forM_ (sortOn (\(n, cs) -> (rank (best cs), n)) (M.toList byNum)) $ \(n, cs) -> do
    let tally = M.toList (M.fromListWith (+) [ (className c, 1 :: Int) | c <- cs ])
    printf "  %-13s %-26s %-13s %2d calls: %s\n" n (take 26 (nameOf n)) (className (best cs)) (length cs)
      (unwords [ printf "%s %d" k v | (k, v) <- tally ] :: String)
  let called = M.keys byNum
      never = nub [ c | c <- cands, cdNumber c `notElem` called ]
  unless (null cands) $ do
    printf "\nOn the lists and never called: %d\n" (length (nub (map cdNumber never)))
    forM_ (M.toList (M.fromListWith (flip (++)) [ (cdSource c, [c]) | c <- never ])) $ \(src, cs) -> do
      printf "  %s\n" src
      forM_ (nubOn cdNumber cs) $ \c -> printf "    %-13s %s\n" (cdNumber c) (cdName c)
  where
    best = head . sortOn rank
    nubOn f = foldr (\x acc -> if any ((== f x) . f) acc then acc else x : acc) [] . reverse
    rank c = case c of
      Modem _ -> 0 :: Int
      Fax _ -> 1
      Voice -> 2
      SpecialInfo _ -> 3
      Congestion -> 4
      Busy -> 5
      NoAnswer -> 6
      Silence -> 7
      NoAudio -> 8
      Unclassified -> 9

-- | A number from a candidate list, with the name beside it.
data Candidate = Candidate
  { cdNumber        :: String
  , cdName          :: String
  , cdSource        :: FilePath
  , cdNeverAnswered :: Bool
  } deriving (Eq)

-- | Every North American number in a list, spelled @+1NNNNNNNNNN@,
-- @NNN-NNN-NNNN@ or @1-NNN-NNN-NNNN@.  A number's name is the text
-- before it on its line, back to the previous comma or colon; a name
-- that wrapped onto the line before is looked for there; and a number
-- that opens a table row is named by the columns after it.  A paragraph
-- that says a number never answered marks every number in it.
readCandidates :: FilePath -> IO [Candidate]
readCandidates f = do
  ok <- doesFileExist f
  if not ok
    then hPutStrLn stderr ("no such candidate list: " ++ f) >> return []
    else concatMap perParagraph . paragraphs . lines <$> readFile f
  where
    paragraphs ls = case break (all isSpace) ls of
      (p, []) -> [p]
      (p, _ : rest) -> p : paragraphs rest
    perParagraph ls =
      let never = any (\l -> any (`isInfixOf` map toLower l) ["never answered", "never picked up"]) ls
      in concat (zipWith (perLine never) ("" : ls) ls)
    perLine never prevLine l = reverse (snd (foldl step ("", []) (numbersIn l)))
      where
        step (lineName, acc) (n, before, after) =
          let name = pickName prevLine lineName before after
          in (if null lineName then name else lineName, Candidate n name f never : acc)
    -- the text before the number; else, for "Name (+1...)" broken across
    -- lines, the end of the line before; else the name an earlier number
    -- on this line had; else the table columns after the number
    pickName prevLine lineName before after
      | named seg = seg
      | '(' `elem` before, named wrapped = wrapped
      | not (null lineName) = lineName
      | otherwise = take 40 (columns after)
      where
        seg = lastSegment before
        wrapped = lastSegment prevLine
    lastSegment t = trim (filter (`notElem` "()") (reverse (takeWhile (`notElem` ",:") (reverse t))))
    named t = any isAlpha t
    columns t = intercalate ", " (filter (not . null) (map trim (splitOn2 (takeWhile (/= ',') (trim t)))))
    -- columns in these tables are separated by two or more spaces
    splitOn2 t = case breakOn2 t of
      (a, Nothing) -> [a]
      (a, Just rest) -> a : splitOn2 rest
    breakOn2 t = go "" t
      where
        go acc (' ' : ' ' : rest) = (reverse acc, Just (dropWhile (== ' ') rest))
        go acc (ch : rest) = go (ch : acc) rest
        go acc [] = (reverse acc, Nothing)

-- | Numbers in a line, each with the text before it (since the previous
-- number) and the rest of the line after it.
numbersIn :: String -> [(String, String, String)]
numbersIn = go ""
  where
    go _ [] = []
    go before s@(ch : rest)
      | Just (n, rest') <- plus s = (n, before, rest') : go "" rest'
      | not (endsDigit before), Just (n, rest') <- dashed s = (n, before, rest') : go "" rest'
      | otherwise = go (before ++ [ch]) rest
    endsDigit b = not (null b) && isDigit (last b)
    plus ('+' : '1' : r) | let d = takeWhile isDigit r, length d == 10 = Just ("+1" ++ d, drop 10 r)
    plus _ = Nothing
    dashed ('1' : '-' : r) | Just m <- ten r = Just m
    dashed r = ten r
    ten s = case s of
      (a : b : c : '-' : d : e : g : '-' : h : i : j : k : r)
        | all isDigit [a, b, c, d, e, g, h, i, j, k], not (take 1 r /= "" && isDigit (head r)) ->
            Just ("+1" ++ [a, b, c, d, e, g, h, i, j, k], r)
      _ -> Nothing

-- | The longest candidate name whose slug begins a sweep file's name.
bySlug :: [Candidate] -> String -> Maybe Candidate
bySlug cands rest =
  -- sortOn is stable, so among equally long names the list given first
  -- wins: a board's main line in bbslist.txt over its alternates
  listToMaybe (sortOn (negate . length . slug . cdName)
    [ c | c <- cands, let s = slug (cdName c), not (null s)
        , s == rest || (s ++ "-") `isPrefixOf` rest ])

slug :: String -> String
slug = collapse . map (\ch -> if isAlphaNum ch then toLower ch else '-') . filter (`notElem` "'’")
  where
    collapse = trimDash . foldr (\ch acc -> if ch == '-' && take 1 acc == "-" then acc else ch : acc) []
    trimDash = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

normalise :: String -> String
normalise n
  | "+" `isPrefixOf` n = n
  | length n == 10, all isDigit n = "+1" ++ n
  | otherwise = n

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
