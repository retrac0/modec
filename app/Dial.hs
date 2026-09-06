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
import Data.List (isPrefixOf)
import Network.Socket
import System.Directory (createDirectoryIfMissing, doesFileExist)
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
      h <- openFile logPath AppendMode
      say ("starting baresip (its output goes to " ++ logPath ++ ")")
      ph <- (\(_, _, _, p) -> p) <$>
              createProcess (proc "baresip" []) { std_in = NoStream, std_out = UseHandle h
                                                , std_err = UseHandle h }
      ready <- waitForCtrl (dCtrl o) 15
      unless ready $ say ("baresip did not open " ++ dCtrl o ++ " within 15 s")
      -- the control port opens before the account has registered, and
      -- dialling before then is refused by the trunk, not by baresip
      threadDelay 2000000
      return ph
    stop ph = say "stopping baresip" >> terminateProcess ph >> void' (waitForProcess ph)
    void' a = a >> return ()

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

-- | The terminal as a DTE: no line discipline and no echo, because the
-- modem does both itself.  Restored however the call ends.
withRawTty :: IO a -> IO a
withRawTty body = do
  isTty <- queryTerminal (Fd 0)
  if not isTty then body else do
    saved <- getTerminalAttributes (Fd 0)
    let raw = flip withoutMode EnableEcho
            . flip withoutMode ProcessInput
            . flip withoutMode ExtendedFunctions
            $ saved
    bracket_ (setTerminalAttributes (Fd 0) raw Immediately)
             (setTerminalAttributes (Fd 0) saved Immediately)
             (hSetBuffering stdin NoBuffering >> hSetBuffering stdout NoBuffering >> body)
