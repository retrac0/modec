{-# LANGUAGE OverloadedStrings #-}
-- | The line as a serial port: a voice-mode modem streaming samples.
--
-- A Conexant-class USB modem with @AT+FCLASS=8@ is a telephone-line
-- sound card on a CDC-ACM port.  This opens the port, gets the modem
-- streaming with the dialogue in "Modec.Voice", and offers the bytes
-- either way with the in-band framing taken off and put on.  What the
-- bytes mean as samples is not decided here: the caller wraps the
-- reader and writer in a format, as it does for a pipe.
--
-- The port is opened twice, once to read and once to write, which is
-- how the executable already treats a FIFO pair and keeps the two
-- directions from sharing a lock.  Both opens are non-blocking, so
-- that a port whose carrier-detect line is not asserted does not hang
-- the open, and so that a read can be interrupted at hang-up.
module Serial (withSerial) where

import Control.Concurrent
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import System.Exit (exitFailure)
import System.IO
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), defaultFileFlags, fdToHandle, openFd)
import System.Posix.Terminal
import System.Posix.Types (Fd)
import System.Timeout (timeout)

import Modec.Sample
import Modec.Standards (Role (..))
import Modec.Voice

-- | Open the modem, get it streaming in the format, and hand the body a
-- reader of exactly-this-many bytes (short at the end of the stream)
-- and a writer.  Both are the sample stream without its shielding.
withSerial :: FilePath -> SampleFormat -> Int -> Role -> (String -> IO ())
           -> ((Int -> IO B.ByteString) -> (B.ByteString -> IO ()) -> IO a) -> IO a
withSerial dev fmt rate role say body = do
  code <- case vsmCode fmt of
    Just c -> return c
    Nothing -> do
      say ("a voice-mode modem has no " ++ formatName fmt ++ "; it offers "
           ++ unwords [ formatName f | f <- [minBound .. maxBound], vsmCode f /= Nothing ])
      exitFailure
  when (rate /= 8000) $ do
    say "the modem's codec runs at 8000 Hz: --audio-serial needs --rate 8000"
    exitFailure
  bracket (openPort dev) (\(hr, hw) -> ignore (hClose hr) >> ignore (hClose hw)) $ \(hr, hw) -> do
    say ("audio: " ++ dev ++ ", " ++ show rate ++ " Hz " ++ formatName fmt ++ " (voice mode)")
    resync hr hw
    dialogue hr hw say (voiceSetup code role)
    say "the line is streaming"
    inbox <- newInbox
    under <- newIORef (0 :: Int)
    over <- newIORef (0 :: Int)
    let pump st = do
          r <- try (B.hGetSome hr 4096) :: IO (Either IOException B.ByteString)
          case r of
            Right bs | not (B.null bs) -> do
              let (st', d) = dleDecode st bs
              forM_ (dlEvents d) $ \c -> case c of
                0x75 -> modifyIORef' under (+ 1)
                0x6F -> modifyIORef' over (+ 1)
                _ -> say ("line: " ++ describeVoiceEvent c)
              unless (B.null (dlPayload d)) $ deliver inbox (dlPayload d)
              case dlRest d of
                Nothing -> pump st'
                Just _ -> say "the modem ended the stream" >> finish inbox
            _ -> finish inbox
    tid <- forkIO (pump dleInit)
    let wr bs = ignore (B.hPut hw (dleEncode bs))
    body (readBytes inbox) wr `finally` do
      killThread tid
      u <- readIORef under
      o <- readIORef over
      when (u + o > 0) $ say (show u ++ " underruns, " ++ show o ++ " overruns on the port")
      hangUp hr hw
  where
    ignore act = void (try act :: IO (Either IOException ()))

    -- Back to command mode and on hook, best effort: this runs at exit,
    -- possibly with the modem already gone.
    hangUp hr hw = do
      ignore (B.hPut hw dleLeaveDuplex)
      forM_ voiceTeardown $ \st -> case st of
        Send cmd _ -> do
          ignore (B.hPut hw (cmd <> "\r"))
          void (timeout 2000000 (finalResult hr))
        WaitFor _ -> return ()

-- | Leave whatever state the last run died in, before saying @AT@.
--
-- A bench session is interrupted mid-stream constantly, and a modem
-- still in the voice duplex state answers the next run's first @AT@
-- with the tail of the old one -- @CONNECT@, or a sample byte that is
-- no result code at all -- so every second run failed until this was
-- here.  The two escapes mean nothing to a modem already in command
-- mode; the bare carriage return ends any half-typed line they leave,
-- and the drain eats whatever all of it echoed.
resync :: Handle -> Handle -> IO ()
resync hr hw = do
  ignoreIO (B.hPut hw dleEtx)
  threadDelay 200000
  ignoreIO (B.hPut hw dleLeaveDuplex)
  threadDelay 200000
  ignoreIO (B.hPut hw "\r")
  threadDelay 100000
  drain
  where
    ignoreIO act = void (try act :: IO (Either IOException ()))
    drain = do
      r <- timeout 150000 (try (B.hGetSome hr 4096) :: IO (Either IOException B.ByteString))
      case r of
        Just (Right bs) | not (B.null bs) -> drain
        _ -> return ()

-- | Reading and writing handles on the device, in raw mode when it is
-- a terminal.  A FIFO or a file is taken as it is, which is what lets a
-- fake stand in for the modem.
openPort :: FilePath -> IO (Handle, Handle)
openPort dev = do
  let flags = defaultFileFlags { noctty = True, nonBlock = True }
  fdr <- openFd dev ReadOnly flags
  isTty <- queryTerminal fdr
  when isTty (configure fdr)
  fdw <- openFd dev WriteOnly flags
  hr <- fdToHandle fdr
  hw <- fdToHandle fdw
  forM_ [hr, hw] $ \h -> hSetBinaryMode h True >> hSetBuffering h NoBuffering
  return (hr, hw)

-- | Eight bits, no parity, nothing interpreted, and -- the one that
-- matters -- no XON/XOFF: 0x11 and 0x13 are sample values.
configure :: Fd -> IO ()
configure fd = do
  a <- getTerminalAttributes fd
  let off = [ EnableEcho, EchoErase, EchoKill, EchoLF
            , ProcessInput, ProcessOutput, ExtendedFunctions, KeyboardInterrupts
            , MapCRtoLF, MapLFtoCR, IgnoreCR, StartStopInput, StartStopOutput
            , StripHighBit, EnableParity, CheckParity, MarkParityErrors
            , InterruptOnBreak, TwoStopBits ]
      on = [LocalMode, ReadEnable]
      raw = (`withTime` 0) . (`withMinInput` 1)
          . (`withOutputSpeed` B115200) . (`withInputSpeed` B115200) . (`withBits` 8)
          $ foldl withoutMode (foldl withMode a on) off
  setTerminalAttributes fd raw Immediately

-- | Run the set-up, one step at a time, and say what went wrong where
-- if it does: bring-up reads this like a checklist.
dialogue :: Handle -> Handle -> (String -> IO ()) -> [AtStep] -> IO ()
dialogue hr hw say = mapM_ step
  where
    step (Send cmd oks) = attempt (if cmd == "AT" then 3 else 1 :: Int)
      where
        attempt n = do
          B.hPut hw (cmd <> "\r")
          r <- timeout 5000000 (finalResult hr)
          case r of
            Just (Just res) | res `elem` oks -> return ()
            Just (Just res) -> failWith ("the modem answered " ++ BC.unpack cmd ++ " with " ++ BC.unpack res)
            Just Nothing -> failWith ("the port closed during " ++ BC.unpack cmd)
            Nothing | n > 1 -> attempt (n - 1)
                    | otherwise -> failWith ("no answer to " ++ BC.unpack cmd
                                             ++ " in 5 s: is this the modem's port, and does AT+FCLASS=? list 8?")
    step (WaitFor res) = do
      say ("waiting for " ++ BC.unpack res)
      let go = do
            r <- finalResult hr
            case r of
              Just got | got == res -> return ()
                       | otherwise -> go
              Nothing -> failWith ("the port closed while waiting for " ++ BC.unpack res)
      go
    failWith m = say m >> exitFailure

-- | The next final result on the port, skipping echo and information
-- text; 'Nothing' at end of file.
finalResult :: Handle -> IO (Maybe B.ByteString)
finalResult h = do
  ml <- readLine h
  case ml of
    Nothing -> return Nothing
    Just l -> maybe (finalResult h) (return . Just) (atFinal l)

-- | A line, a byte at a time so nothing past its end is taken: the
-- sample stream starts right after the result that announces it.
readLine :: Handle -> IO (Maybe B.ByteString)
readLine h = go mempty
  where
    go acc = do
      r <- try (B.hGetSome h 1) :: IO (Either IOException B.ByteString)
      case r of
        Right b | not (B.null b) -> if b == "\n" then return (Just acc) else go (acc <> b)
        _ -> return (if B.null acc then Nothing else Just acc)

-- | Payload from the pump thread, read out in exact amounts.
data Inbox = Inbox (Chan B.ByteString) (IORef B.ByteString) (IORef Bool)

newInbox :: IO Inbox
newInbox = Inbox <$> newChan <*> newIORef B.empty <*> newIORef False

deliver :: Inbox -> B.ByteString -> IO ()
deliver (Inbox ch _ _) = writeChan ch

-- | An empty chunk is the end.
finish :: Inbox -> IO ()
finish (Inbox ch _ _) = writeChan ch B.empty

readBytes :: Inbox -> Int -> IO B.ByteString
readBytes (Inbox ch restRef endRef) n = readIORef restRef >>= go
  where
    go acc
      | B.length acc >= n = do
          let (now, later) = B.splitAt n acc
          writeIORef restRef later
          return now
      | otherwise = do
          ended <- readIORef endRef
          if ended
            then writeIORef restRef B.empty >> return acc
            else do
              chunk <- readChan ch
              if B.null chunk then writeIORef endRef True >> go acc else go (acc <> chunk)
