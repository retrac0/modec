{-# LANGUAGE ScopedTypeVariables #-}
-- | The live modem: audio in and out through PipeWire (pw-cat) or raw
-- 16-bit little-endian mono pipes, bytes through a telnet socket or
-- stdio.  The main loop is paced by the audio input, one block at a
-- time; the pure modem in "Modec.Modem" does all the work.
module Modem
  ( AudioIO (..)
  , DataIO (..)
  , ModemOpts (..)
  , runModem
  ) where

import Control.Concurrent
import Control.Exception (SomeException, bracket, try)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Storable as VS
import Network.Socket
import qualified Network.Socket.ByteString as NB
import System.Exit (exitFailure)
import System.IO
import System.Process
import System.Posix.IO (OpenMode (..), defaultFileFlags, fdToHandle, openFd)

import Modec.DSP (Signal)
import Modec.Handshake
import Modec.Modem
import Modec.Telnet

data AudioIO
  = AudioPipewire (Maybe String)     -- ^ optional pw-cat target node
  | AudioFiles FilePath FilePath     -- ^ raw s16le mono: input, output (files or FIFOs)
  | AudioStdio                       -- ^ raw s16le mono on stdin/stdout

data DataIO
  = DataListen Int                   -- ^ telnet server on this port, one connection
  | DataConnect String Int           -- ^ telnet client
  | DataStdio                        -- ^ raw bytes on stdin/stdout (no telnet)

data ModemOpts = ModemOpts
  { moRate     :: Int
  , moBlockMs  :: Int
  , moRole     :: Role
  , moStandard :: Maybe Standard
  , moNoHandshake :: Bool
  , moAudio    :: AudioIO
  , moData     :: DataIO
  , moAmp      :: Double
  }

logMsg :: String -> IO ()
logMsg s = hPutStrLn stderr ("modec: " ++ s)

runModem :: ModemOpts -> IO ()
runModem o = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  let fs = fromIntegral (moRate o)
      blockN = moRate o * moBlockMs o `div` 1000
      cfg = (defaultModemConfig fs (moRole o) (moStandard o)) { mcNoHandshake = moNoHandshake o, mcTxAmp = moAmp o }
  when (moNoHandshake o && moStandard o == Nothing) $ do
    logMsg "--no-handshake needs --standard bell103 or v21"
    exitFailure
  withAudio (moAudio o) (moRate o) (moRole o) $ \ain aout ->
    withData (moData o) $ \recvBytes sendBytes -> do
      stRef <- newIORef (modemInit cfg)
      -- output leads input by one block: two modems joined by pipes would
      -- otherwise each wait for the other's first block
      B.hPut aout (encodeS16 (VS.replicate blockN 0))
      hFlush aout
      let loop = do
            raw <- B.hGet ain (2 * blockN)
            if B.length raw < 2 * blockN
              then logMsg "audio input ended"
              else do
                pending <- recvBytes
                st <- readIORef stRef
                let (st', audio, rxBytes, events) = modemStep cfg st (decodeS16 raw) (B.unpack pending)
                writeIORef stRef st'
                B.hPut aout (encodeS16 audio)
                hFlush aout
                unless (null rxBytes) $ sendBytes (B.pack rxBytes)
                forM_ events $ \ev -> case ev of
                  EvConnected s link -> logMsg ("CONNECT " ++ show s ++ " " ++ show link)
                  EvDropped -> logMsg "NO CARRIER"
                  EvFailed why -> logMsg ("connection failed: " ++ why)
                let finished = any isFinal events
                unless finished loop
          isFinal EvDropped = True
          isFinal (EvFailed _) = True
          isFinal _ = False
      loop

decodeS16 :: B.ByteString -> Signal
decodeS16 bs = VS.generate (B.length bs `div` 2) $ \i ->
  let lo = fromIntegral (B.index bs (2 * i)) :: Int
      hi = fromIntegral (B.index bs (2 * i + 1)) :: Int
      v = lo + hi * 256
      s = if v >= 32768 then v - 65536 else v
  in fromIntegral s / 32768

encodeS16 :: Signal -> B.ByteString
encodeS16 x = BL.toStrict (BB.toLazyByteString (VS.foldr (\v acc -> BB.int16LE (toI16 v) <> acc) mempty x))
  where
    toI16 :: Double -> Int16
    toI16 v = round (max (-1) (min 1 v) * 32767)

-- | Open the audio input and output handles.
withAudio :: AudioIO -> Int -> Role -> (Handle -> Handle -> IO a) -> IO a
withAudio aio rate role body = case aio of
  AudioStdio -> body stdin stdout
  AudioFiles i o -> do
    -- Blocking POSIX opens (GHC's openFile opens FIFOs non-blocking and
    -- fails with ENXIO when no reader exists yet).  Each FIFO open waits
    -- for the peer's open of the other end, so two cross-connected modems
    -- must open in opposite orders: the answerer input first, the caller
    -- output first.
    let openIn = openFd i ReadOnly defaultFileFlags >>= fdToHandle
        openOut = openFd o WriteOnly defaultFileFlags >>= fdToHandle
    (hi, ho) <- case role of
      Answer -> do { a <- openIn; b <- openOut; return (a, b) }
      Originate -> do { b <- openOut; a <- openIn; return (a, b) }
    hSetBinaryMode hi True
    hSetBinaryMode ho True
    hSetBuffering ho NoBuffering
    r <- body hi ho
    hClose hi
    hClose ho
    return r
  AudioPipewire target -> do
    let common = ["--raw", "--rate", show rate, "--channels", "1", "--format", "s16", "--latency", "20ms"]
                 ++ maybe [] (\t -> ["--target", t]) target
        rec = (proc "pw-cat" (["--record"] ++ common ++ ["-"])) { std_out = CreatePipe, std_err = Inherit }
        play = (proc "pw-cat" (["--playback"] ++ common ++ ["-"])) { std_in = CreatePipe, std_err = Inherit }
    bracket (createProcess rec) cleanup $ \r ->
      bracket (createProcess play) cleanup $ \pl -> case (r, pl) of
        ((_, Just hin, _, _), (Just hout, _, _, _)) -> do
          hSetBinaryMode hin True
          hSetBinaryMode hout True
          hSetBuffering hout NoBuffering
          logMsg ("pw-cat record/playback at " ++ show rate ++ " Hz")
          body hin hout
        _ -> logMsg "could not start pw-cat" >> exitFailure
  where
    cleanup (mi, mo, _, ph) = do
      mapM_ hClose mi
      mapM_ hClose mo
      terminateProcess ph
      _ <- waitForProcess ph
      return ()

-- | Provide a non-blocking receiver of pending inbound bytes and a sender.
withData :: DataIO -> (IO B.ByteString -> (B.ByteString -> IO ()) -> IO a) -> IO a
withData dio body = case dio of
  DataStdio -> do
    buf <- newBuffer
    _ <- forkIO (pump stdin buf)
    body (drain buf) (\bs -> B.hPut stdout bs >> hFlush stdout)
  DataListen port -> withSocketsDo $ do
    addr <- head <$> getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }) Nothing (Just (show port))
    bracket (openSocket addr) close $ \lsock -> do
      setSocketOption lsock ReuseAddr 1
      bind lsock (addrAddress addr)
      listen lsock 1
      logMsg ("listening on port " ++ show port)
      bracket (fst <$> accept lsock) close $ \sock -> do
        logMsg "client connected"
        telnetSession sock body
  DataConnect host port -> withSocketsDo $ do
    addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just (show port))
    bracket (openSocket addr) close $ \sock -> do
      connect sock (addrAddress addr)
      logMsg ("connected to " ++ host ++ ":" ++ show port)
      telnetSession sock body
  where
    pump h buf = do
      r <- try (B.hGetSome h 4096) :: IO (Either SomeException B.ByteString)
      case r of
        Right bs | not (B.null bs) -> push buf bs >> pump h buf
        _ -> return ()

-- | Telnet framing over a connected socket.
telnetSession :: Socket -> (IO B.ByteString -> (B.ByteString -> IO ()) -> IO a) -> IO a
telnetSession sock body = do
  let (ts0, hello) = telnetHello telnetInit
  NB.sendAll sock hello
  tsRef <- newMVar ts0
  buf <- newBuffer
  sendLock <- newMVar ()
  let send bs = withMVar sendLock (\_ -> NB.sendAll sock bs)
      reader = do
        r <- try (NB.recv sock 4096) :: IO (Either SomeException B.ByteString)
        case r of
          Right bs | not (B.null bs) -> do
            (payload, reply) <- modifyMVar tsRef $ \ts ->
              let (ts', p, rep) = telnetDecode ts bs in return (ts', (p, rep))
            unless (B.null reply) (send reply)
            unless (B.null payload) (push buf payload)
            reader
          _ -> logMsg "data connection closed"
  _ <- forkIO reader
  body (drain buf) (send . telnetEncode)

-- | Pending inbound bytes, appended by a reader thread and drained by
-- the audio-paced main loop.
newBuffer :: IO (IORef [B.ByteString])
newBuffer = newIORef []

push :: IORef [B.ByteString] -> B.ByteString -> IO ()
push buf bs = atomicModifyIORef' buf (\xs -> (bs : xs, ()))

drain :: IORef [B.ByteString] -> IO B.ByteString
drain buf = B.concat . reverse <$> atomicModifyIORef' buf (\xs -> ([], xs))
