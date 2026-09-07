-- | @modec dial NUMBER@: one command to place a call.
--
-- Everything the modem needs to reach a telephone line over SIP is
-- either derivable or has one sensible answer, so none of it is asked
-- for.  This module fills in what the longer @modec modem@ form makes
-- you spell out:
--
-- * baresip is started if nothing is already listening on its control
--   port, and stopped again on the way out if we were the ones who
--   started it;
-- * the domain comes from the account baresip is registered with;
-- * audio runs through the PipeWire loopback pair the supplied baresip
--   configuration names;
-- * the terminal is the DTE, in raw mode so what you type reaches the
--   far end a character at a time and is echoed once rather than twice;
-- * the number is dialled for you, and the call is recorded.
module Dial
  ( DialOpts (..)
  , runDial
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, bracket_, try)
import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Network.Socket
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO
import System.Posix.Terminal
import System.Posix.Types (Fd (..))
import System.Process

import Modem

data DialOpts = DialOpts
  { dNumber :: String            -- ^ as dialled: digits, or a full SIP URI
  , dCtrl   :: String            -- ^ baresip ctrl_tcp address, host:port
  , dDomain :: Maybe String      -- ^ SIP domain; taken from the account when absent
  , dLoop   :: String            -- ^ PipeWire loopback prefix
  , dListen :: Maybe Int         -- ^ put the DTE on this telnet port instead of the terminal
  , dLaunch :: Bool              -- ^ start baresip if the control port does not answer
  , dStay   :: Bool              -- ^ keep the AT prompt after the call instead of exiting
  , dModem  :: ModemOpts         -- ^ the modem's own settings, from the shared options
  }

say :: String -> IO ()
say s = hPutStrLn stderr ("modec: " ++ s)

runDial :: DialOpts -> IO ()
runDial o = do
  domain <- case dDomain o of
    Just d -> return d
    Nothing -> do
      d <- accountDomain
      case d of
        Just d' -> say ("SIP domain " ++ d' ++ " (from ~/.baresip/accounts)") >> return d'
        Nothing -> do
          say "no SIP domain: pass --sip-domain, or put an account in ~/.baresip/accounts"
          return ""
  withBaresip o $ do
    let mo = (dModem o)
          { moHayes = True
          , moSip = Just (dCtrl o)
          , moSipDomain = domain
          , moAudio = AudioSipLoop (dLoop o)
          , moData = maybe DataStdio DataListen (dListen o)
          , moDial = Just (dNumber o)
          , moHangupExits = not (dStay o)
          }
    case dListen o of
      Just p -> say ("the modem is on telnet port " ++ show p ++ "; dialling " ++ dNumber o)
      Nothing -> say ("dialling " ++ dNumber o ++ " -- +++ATH hangs up, ctrl-C leaves")
    (if dListen o == Nothing then withRawTty else id) (runModem mo)

-- | Run the body with baresip up, starting one if the control port does
-- not already answer.  A baresip we started is stopped again; one that
-- was already there is left alone, since it is not ours to close.
withBaresip :: DialOpts -> IO a -> IO a
withBaresip o body = do
  up <- ctrlPortOpen (dCtrl o)
  if up
    then say ("using the baresip already listening on " ++ dCtrl o) >> body
    else if not (dLaunch o)
      then do
        say ("nothing is listening on " ++ dCtrl o ++ " and --no-launch was given")
        body
      else bracket start stop (const body)
  where
    start = do
      let logPath = maybe "baresip.log" (</> "baresip.log") (moRecordDir (dModem o))
      mapM_ (createDirectoryIfMissing True) (moRecordDir (dModem o))
      from <- fileSizeOr0 logPath
      h <- openFile logPath AppendMode
      say ("starting baresip (its output goes to " ++ logPath ++ ")")
      ph <- (\(_, _, _, p) -> p) <$>
              createProcess (proc "baresip" []) { std_in = NoStream, std_out = UseHandle h
                                                , std_err = UseHandle h }
      ready <- waitForCtrl (dCtrl o) 15
      unless ready $ say ("baresip did not open " ++ dCtrl o ++ " within 15 s")
      registered <- waitForRegistration logPath from 25
      if registered
        then say "SIP account registered"
        else do
          say "baresip did not report a registration within 25 s; dialling anyway"
          say ("the last of " ++ logPath ++ " says:")
          tailOf logPath from 6 >>= mapM_ (\l -> say ("  " ++ l))
      return ph
    stop ph = say "stopping baresip" >> terminateProcess ph >> void' (waitForProcess ph)
    void' a = a >> return ()

-- | Wait for the account to finish registering.
--
-- The control port opens well before that happens, and a number dialled
-- in between is not refused by the trunk but by baresip itself, with
-- "could not find UA" -- which reaches the sweep as a call that was
-- placed and never answered, when in truth it was never placed.  A
-- fixed pause was what stood here, and a fixed pause is a guess: two
-- seconds was enough on a warm start and not enough on a cold one.
--
-- What is waited for instead is baresip saying so.  It prints the
-- registrar's answer to its log, and the answer to a registration that
-- took is 200 OK, so that is the line to look for.  Failures are left
-- to the timeout rather than matched: the strings baresip prints for
-- them vary by version and by what went wrong, and guessing at them
-- wrongly would turn a slow registration into a reported failure.
waitForRegistration :: FilePath -> Integer -> Int -> IO Bool
waitForRegistration path from = go
  where
    go n
      | n <= 0 = return False
      | otherwise = do
          ls <- linesSince path from
          if any ("200 OK" `isInfixOf`) ls
            then return True
            else threadDelay 1000000 >> go (n - 1)

-- | Everything written to a file after byte @from@, as lines.
linesSince :: FilePath -> Integer -> IO [String]
linesSince path from = do
  r <- try $ bracket (openFile path ReadMode) hClose $ \h -> do
    hSetBinaryMode h True
    size <- hFileSize h
    if size <= from then return [] else do
      hSeek h AbsoluteSeek from
      s <- hGetContents h
      length s `seq` return (lines s)
  return (either (\e -> const [] (e :: IOException)) id r)

-- | The last @n@ lines written after byte @from@, for a report.
tailOf :: FilePath -> Integer -> Int -> IO [String]
tailOf path from n = do
  ls <- filter (not . all isSpace) <$> linesSince path from
  return (drop (max 0 (length ls - n)) ls)

fileSizeOr0 :: FilePath -> IO Integer
fileSizeOr0 path = do
  ok <- doesFileExist path
  if ok then either (\e -> const 0 (e :: IOException)) id <$> try (getFileSize path)
        else return 0

waitForCtrl :: String -> Int -> IO Bool
waitForCtrl addr = go
  where
    go n
      | n <= 0 = return False
      | otherwise = do
          ok <- ctrlPortOpen addr
          if ok then return True else threadDelay 500000 >> go (n - 1)

-- | Can something be connected to on the control address right now?
ctrlPortOpen :: String -> IO Bool
ctrlPortOpen addr = do
  let (host, portS) = break (== ':') addr
      port = if null portS then "4444" else drop 1 portS
      host' = if null host then "127.0.0.1" else host
  r <- try $ do
    ai <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host') (Just port)
    bracket (openSocket ai) close (\s -> connect s (addrAddress ai))
  return (either (\e -> const False (e :: IOException)) (const True) r)

-- | The domain of the first account in @~/.baresip/accounts@: the part
-- after the @\@@ of @\<sip:user\@domain\>@.
accountDomain :: IO (Maybe String)
accountDomain = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> return Nothing
    Just h -> do
      let path = h </> ".baresip" </> "accounts"
      ok <- doesFileExist path
      if not ok then return Nothing else do
        ls <- lines <$> readFile path
        return (firstJust (map parse ls))
  where
    parse l0 =
      let l = dropWhile isSpace l0
      in if "#" `isPrefixOf` l || not ("<sip:" `isPrefixOf` l) then Nothing
         else case break (== '@') (drop 5 l) of
           (_, '@' : rest) -> case takeWhile (`notElem` ">;") rest of
             [] -> Nothing
             d -> Just d
           _ -> Nothing
    firstJust xs = case [ x | Just x <- xs ] of
      (x : _) -> Just x
      [] -> Nothing

-- | The terminal as a DTE: no line discipline and no echo, and no
-- translation of anything on its way to the line, because every byte
-- typed here is going somewhere a modem and a far end have already
-- agreed what the bytes mean.
--
-- 'MapCRtoLF' is the one that matters and the one that is on by default
-- on every terminal: with it, the return key arrives as a line feed,
-- which a BBS waiting for a carriage return ignores completely and an
-- AT command parser does not accept either.  The rest are the same
-- argument -- ^S and ^Q are characters to send, not flow control, and
-- the high bit belongs to whatever is using it.
--
-- What comes back the other way is a different question, and the answer
-- is the opposite one: see the note on 'ProcessOutput' below.
--
-- 'KeyboardInterrupts' stays on, which is the one deliberate exception:
-- ctrl-C leaves rather than reaching the far end, as the banner says.
-- Restored however the call ends.
withRawTty :: IO a -> IO a
withRawTty body = do
  isTty <- queryTerminal (Fd 0)
  if not isTty then body else do
    saved <- getTerminalAttributes (Fd 0)
    -- Everything a raw mode turns off except output processing, which
    -- stays on.  The usual recipe (cfmakeraw) clears that too, and it
    -- is wrong here: with it clear the terminal loses ONLCR, so a bare
    -- line feed from the far end moves the cursor down a line without
    -- returning it to the left margin, and a banner walks off the right
    -- of the screen in a staircase.  Plenty of boards send bare line
    -- feeds, and the ones that send CR LF are unharmed -- the extra
    -- carriage return ONLCR inserts lands on a margin the CR already
    -- reached.  Byte transparency on the way out is not wanted here in
    -- any case: this end of the line is a person reading a screen, and
    -- anything that needs the bytes untouched asks for --listen and
    -- gets them over telnet instead.
    let off = [ EnableEcho, ProcessInput, ExtendedFunctions
              , MapCRtoLF, MapLFtoCR, IgnoreCR
              , StartStopInput, StartStopOutput
              , StripHighBit, CheckParity, MarkParityErrors, InterruptOnBreak ]
        raw = flip withTime 0 . flip withMinInput 1
            $ foldl withoutMode saved off
    bracket_ (setTerminalAttributes (Fd 0) raw Immediately)
             (setTerminalAttributes (Fd 0) saved Immediately)
             (hSetBuffering stdin NoBuffering >> hSetBuffering stdout NoBuffering >> body)
