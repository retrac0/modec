-- | Per-call recordings.
--
-- One directory, three files per call, all named after the moment the
-- call was placed and the number it was placed to:
--
-- > recordings/20260906T152425-14692003550.wav   received audio
-- > recordings/20260906T152425-14692003550.log   what the modem said
-- > recordings/calls.log                         one line per call
--
-- The WAV and the log share a stem so a recording can never be separated
-- from the account of what produced it, which is the whole point: a
-- recording tells you what came down the line, and only the log says what
-- the modem made of it.  The index is appended to as calls end, so it
-- survives a crash mid-call with everything up to that call intact.
module CallLog
  ( CallRec (..)
  , callRecStart
  , callRecWrite
  , callRecSay
  , callRecEnd
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum)
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, diffUTCTime, formatTime,
                  getCurrentTime, getZonedTime, zonedTimeToUTC)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO
import Text.Printf (printf)

import Modec.Wav (WavWriter, closeWav, openWav16Mono, wavAppendRaw)

data CallRec = CallRec
  { crNumber :: String
  , crStem   :: FilePath        -- ^ path without extension; .wav and .log hang off it
  , crDir    :: FilePath
  , crWav    :: WavWriter
  , crLog    :: Handle
  , crStart  :: UTCTime        -- ^ for durations
  , crPlaced :: ZonedTime       -- ^ for names and the index, in local time
  }

-- | Anything that is not a digit, letter or '+' becomes '-', so a number
-- as dialled ("+1 469 200 3550", "ATDT" leftovers, a SIP URI) still makes
-- one filename component.
sanitise :: String -> String
sanitise s = case filter (/= '-') cleaned of
  [] -> "unknown"
  _  -> cleaned
  where
    cleaned = map (\c -> if isAlphaNum c || c == '+' then c else '-') s

-- | Open the recording and log for a call being placed now.
callRecStart :: FilePath -> String -> Int -> IO (Maybe CallRec)
callRecStart dir number rate = do
  -- Local time throughout: these names are read by a person sitting in
  -- front of the machine that placed the call, next to a shell whose ls
  -- prints local time too.
  t <- getZonedTime
  let stamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%S" t
      stem = dir </> (stamp ++ "-" ++ sanitise number)
  r <- try $ do
    createDirectoryIfMissing True dir
    w <- openWav16Mono (stem ++ ".wav") rate
    h <- openFile (stem ++ ".log") WriteMode
    hSetBuffering h LineBuffering
    hPutStrLn h ("# " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" t ++ "  " ++ number)
    return (CallRec number stem dir w h (zonedTimeToUTC t) t)
  case r of
    Right c -> return (Just c)
    Left e -> do
      hPutStrLn stderr ("modec: cannot record this call: " ++ show (e :: IOException))
      return Nothing

-- | Received audio, exactly as it arrived.
callRecWrite :: CallRec -> B.ByteString -> IO ()
callRecWrite c bs = wavAppendRaw (crWav c) bs

-- | A line of the modem's own commentary, stamped with how far into the
-- call it happened.  The offsets are what make the log readable next to
-- the recording: both start at the same instant.
callRecSay :: CallRec -> String -> IO ()
callRecSay c msg = do
  now <- getCurrentTime
  let dt = realToFrac (diffUTCTime now (crStart c)) :: Double
  hPutStrLn (crLog c) (printf "%7.2f  %s" dt msg)

-- | Close the call's files and add it to the index.
callRecEnd :: CallRec -> String -> IO ()
callRecEnd c outcome = do
  now <- getCurrentTime
  let dt = realToFrac (diffUTCTime now (crStart c)) :: Double
  callRecSay c ("call ended: " ++ outcome)
  closeWav (crWav c)
  hClose (crLog c)
  appendFile (crDir c </> "calls.log") $
    printf "%s  %-18s %6.1fs  %-28s %s\n"
      (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (crPlaced c))
      (crNumber c) dt outcome (crStem c ++ ".wav")
