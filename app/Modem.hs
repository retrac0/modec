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
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO
import System.Process
import Data.List (isSuffixOf)
import System.Posix.IO (OpenMode (..), defaultFileFlags, fdToHandle, openFd)

import Modec.DSP (Signal, rms)
import Modec.Handshake
import Modec.Baresip
import Modec.Dtmf
import Modec.Hayes
import Modec.Modem
import Modec.V22 (Rate (..), rxEvmEstimate, rxOnes2400Run, rxSpsEstimate)
import Modec.Telnet

data AudioIO
  = AudioPipewire (Maybe String) Bool  -- ^ optional pw-cat target node (numeric id), capture the sink monitor
  | AudioSipLoop String              -- ^ PipeWire loopback pair for a softphone; the prefix names the nodes
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
  , moMax1200  :: Bool
  , moNoV8bis  :: Bool
  , moHayes    :: Bool
  , moSip      :: Maybe String       -- ^ baresip ctrl_tcp address host:port
  , moSipDomain :: String
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
      cfg0 = defaultModemConfig fs (moRole o) (moStandard o)
      cfg = cfg0 { mcNoHandshake = moNoHandshake o, mcTxAmp = moAmp o, mcHandshake = (mcHandshake cfg0) { hcAllow2400 = not (moMax1200 o), hcV8bis = not (moNoV8bis o) } }
  when (moNoHandshake o && moStandard o == Nothing) $ do
    logMsg "--no-handshake needs --standard bell103 or v21"
    exitFailure
  -- connect to the softphone control port before the audio and the DTE,
  -- so that control is up whatever order the peers start in
  sip <- case moSip o of
    Nothing -> return Nothing
    Just addr -> Just <$> sipConnect addr
  withAudio (moAudio o) (moRate o) (moRole o) $ \ain aout ->
    withData (moData o) $ \recvBytes sendBytes -> do
      trace <- (/= Nothing) <$> lookupEnv "MODEC_TRACE"
      blockRef <- newIORef (0 :: Int)
      -- output leads input by one block: two modems joined by pipes would
      -- otherwise each wait for the other's first block
      B.hPut aout (encodeS16 (VS.replicate blockN 0))
      hFlush aout
      let cfgFor role = let c0 = defaultModemConfig fs role (moStandard o)
                        in c0 { mcNoHandshake = moNoHandshake o, mcTxAmp = moAmp o
                              , mcHandshake = (mcHandshake c0) { hcAllow2400 = not (moMax1200 o), hcV8bis = not (moNoV8bis o) } }
          traceStep st st' = when trace $ do
            k <- readIORef blockRef
            writeIORef blockRef (k + 1)
            when (modemTxCmd st' /= modemTxCmd st || k `mod` 25 == 0) $
              logMsg (show (fromIntegral (k * blockN) / fs :: Double) ++ " tx " ++ show (modemTxCmd st') ++ " " ++ v22Info st')
          report ev = case ev of
            EvConnected s link -> logMsg ("CONNECT " ++ show s ++ " " ++ show link)
            EvDropped -> logMsg "NO CARRIER"
            EvFailed why -> logMsg ("connection failed: " ++ why)
      if not (moHayes o) && sip == Nothing
        then do
          -- plain mode: one call in the configured role, then exit
          stRef <- newIORef (modemInit cfg)
          let loop = do
                raw <- B.hGet ain (2 * blockN)
                if B.length raw < 2 * blockN
                  then logMsg "audio input ended (the capture stream stopped)"
                  else do
                    pending <- recvBytes
                    st <- readIORef stRef
                    let (st', audio, rxBytes, events) = modemStep cfg st (decodeS16 raw) (B.unpack pending)
                    writeIORef stRef st'
                    traceStep st st'
                    unless trace $ modifyIORef' blockRef (+ 1)
                    B.hPut aout (encodeS16 audio)
                    hFlush aout
                    unless (null rxBytes) $ sendBytes (B.pack rxBytes)
                    mapM_ report events
                    unless (any isFinal events) loop
          loop
        else do
          -- Hayes mode: an AT command interpreter controls calls on the line
          hayesRef <- newIORef hayesInit
          lineRef <- newIORef LineIdle
          energyRef <- newIORef (0 :: Int)
          sipLineRef <- newIORef (sipLineInit (moSipDomain o))
          logMsg (case sip of
                    Nothing -> "Hayes command mode (ATD to dial, ATA to answer, ATH to hang up)"
                    Just _ -> "Hayes command mode over SIP (baresip at " ++ maybe "" id (moSip o) ++ ")")
          let tNow = do
                k <- readIORef blockRef
                return (fromIntegral (k * blockN) / fs :: Double)
              rateOf link = case link of
                FskLink {} -> 300
                V22Link _ _ R1200 -> 1200
                V22Link _ _ R2400 -> 2400 :: Int
              modemEvent ev = do
                hs <- readIORef hayesRef
                let (hs', out) = hayesEvent hs ev
                writeIORef hayesRef hs'
                unless (B.null out) $ sendBytes out
              loop = do
                raw <- B.hGet ain (2 * blockN)
                if B.length raw < 2 * blockN
                  then logMsg "audio input ended (the capture stream stopped)"
                  else do
                    t <- tNow
                    modifyIORef' blockRef (+ 0)
                    pending <- recvBytes
                    hs0 <- readIORef hayesRef
                    let (hs1, back, fwd, acts) = hayesInput t hs0 pending
                        (hs2, tickOut) = hayesTick t hs1
                    writeIORef hayesRef hs2
                    unless (B.null back) $ sendBytes back
                    unless (B.null tickOut) $ sendBytes tickOut
                    -- SIP: Hayes actions and baresip events go through the line controller
                    sipActs <- case sip of
                      Nothing -> return []
                      Just cl -> do
                        evs <- sipDrain cl
                        sl0 <- readIORef sipLineRef
                        let (sl1, as1) = foldl (\(s, acc) a -> let (s', xs) = sipLineHayes s a in (s', acc ++ xs)) (sl0, []) acts
                            (sl2, as2) = foldl (\(s, acc) e -> let (s', xs) = sipLineEvent t s e in (s', acc ++ xs)) (sl1, []) evs
                            (sl3, as3) = sipLineTick t sl2
                        writeIORef sipLineRef sl3
                        return (as1 ++ as2 ++ as3)
                    forM_ sipActs $ \sa -> case sa of
                      SipCommand c params -> maybe (return ()) (\cl -> sipSend cl c params) sip
                      SipStartModem role -> do
                        logMsg ("SIP call up, modem role " ++ show role)
                        writeIORef lineRef (LineCall (modemInit (cfgFor role)) (cfgFor role))
                      SipStopModem -> writeIORef lineRef LineIdle
                      SipToDte ev -> modemEvent ev
                    forM_ (if sip == Nothing then acts else []) $ \a -> do
                      line <- readIORef lineRef
                      case a of
                        ActDial s -> do
                          logMsg ("dialling " ++ s)
                          writeIORef lineRef (LineDialing (dtmfDialSignal fs (0.5 * moAmp o) (map toUpperC s)))
                        ActAnswer -> do
                          logMsg "answering"
                          writeIORef lineRef (LineCall (modemInit (cfgFor Answer)) (cfgFor Answer))
                        ActHangup -> case line of
                          LineIdle -> return ()
                          _ -> logMsg "on hook" >> writeIORef lineRef LineIdle
                        ActOnline -> return ()
                    line <- readIORef lineRef
                    let rxBlock = decodeS16 raw
                        online = hayesOnline hs2
                    case line of
                      LineIdle -> do
                        -- a calling signal (any sustained energy) while idle rings the DTE (not in SIP mode: baresip rings)
                        let loud = sip == Nothing && rms rxBlock > 0.01
                        n <- readIORef energyRef
                        let n' = if loud then n + 1 else 0
                        writeIORef energyRef n'
                        when (n' == 25) $ do
                          modemEvent EvRing
                          hs <- readIORef hayesRef
                          when (hayesAutoAnswer hs) $ do
                            logMsg "auto-answer"
                            writeIORef lineRef (LineCall (modemInit (cfgFor Answer)) (cfgFor Answer))
                        when (n' > 25) $ writeIORef energyRef 0
                        B.hPut aout (encodeS16 (VS.replicate blockN 0))
                      LineDialing sig -> do
                        let (now, rest) = VS.splitAt blockN sig
                            block = now VS.++ VS.replicate (blockN - VS.length now) 0
                        B.hPut aout (encodeS16 block)
                        writeIORef lineRef (if VS.null rest then LineCall (modemInit (cfgFor Originate)) (cfgFor Originate) else LineDialing rest)
                      LineCall st c -> do
                        let (st', audio, rxBytes, events) = modemStep c st rxBlock (if online then B.unpack fwd else [])
                        traceStep st st'
                        B.hPut aout (encodeS16 audio)
                        when (online && not (null rxBytes)) $ sendBytes (B.pack rxBytes)
                        writeIORef lineRef (LineCall st' c)
                        forM_ events $ \ev -> do
                          report ev
                          case ev of
                            EvConnected _ link -> modemEvent (EvConnect (rateOf link))
                            EvDropped -> modemEvent EvNoCarrier >> writeIORef lineRef LineIdle
                            EvFailed _ -> modemEvent EvNoCarrier >> writeIORef lineRef LineIdle
                    hFlush aout
                    modifyIORef' blockRef (+ 1)
                    loop
          loop
  where
    isFinal EvDropped = True
    isFinal (EvFailed _) = True
    isFinal _ = False
    toUpperC ch = if ch >= 'a' && ch <= 'z' then toEnum (fromEnum ch - 32) else ch

-- | Connection to baresip's ctrl_tcp module: a reader thread decodes
-- netstring-framed JSON into a queue; commands go out with tokens.
data SipClient = SipClient Socket (IORef [BsMessage]) (IORef Int)

instance Eq SipClient where
  _ == _ = True

sipConnect :: String -> IO SipClient
sipConnect addr = withSocketsDo $ do
  let (host, portS) = break (== ':') addr
      port = if null portS then "4444" else drop 1 portS
  ai <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just (if null host then "127.0.0.1" else host)) (Just port)
  sock <- openSocket ai
  connect sock (addrAddress ai)
  queue <- newIORef []
  tok <- newIORef 0
  let reader buf = do
        r <- try (NB.recv sock 4096) :: IO (Either SomeException B.ByteString)
        case r of
          Right bs | not (B.null bs) -> do
            let (msgs, rest) = netstringDecode (buf <> bs)
            forM_ msgs $ \m -> case decodeBsMessage m of
              Just (BsResponse okk dat _) -> unless okk (logMsg ("baresip: " ++ dat))
              Just ev@(BsEvent _ typ param _) -> do
                logMsg ("baresip event " ++ typ ++ (if null param then "" else " " ++ param))
                atomicModifyIORef' queue (\q -> (q ++ [ev], ()))
              _ -> return ()
            reader rest
          _ -> logMsg "baresip control connection closed"
  _ <- forkIO (reader B.empty)
  logMsg ("connected to baresip control at " ++ addr)
  return (SipClient sock queue tok)

sipSend :: SipClient -> String -> String -> IO ()
sipSend (SipClient sock _ tok) cmd params = do
  n <- atomicModifyIORef' tok (\k -> (k + 1, k))
  logMsg ("baresip <- " ++ cmd ++ (if null params then "" else " " ++ params))
  r <- try (NB.sendAll sock (commandJson cmd params ("m" ++ show n))) :: IO (Either SomeException ())
  either (\e -> logMsg ("baresip send failed: " ++ show e)) return r

sipDrain :: SipClient -> IO [BsMessage]
sipDrain (SipClient _ queue _) = atomicModifyIORef' queue (\q -> ([], q))

-- | The line in Hayes mode: idle, dialling (DTMF audio left to play), or
-- a call in progress with its modem state and configuration.
data Line
  = LineIdle
  | LineDialing Signal
  | LineCall ModemState ModemConfig

-- | True when PipeWire offers a capture device that is not a monitor of
-- an output.  Uses pactl; if that is unavailable, assume there is one.
hasCaptureDevice :: IO Bool
hasCaptureDevice = do
  r <- try (readProcess "pactl" ["list", "short", "sources"] "") :: IO (Either SomeException String)
  return $ case r of
    Left _ -> True
    Right out -> any real (lines out)
  where
    real l = case drop 1 (words l) of
      (name : _) -> not (".monitor" `isSuffixOf` name)
      _ -> False

v22Info :: ModemState -> String
v22Info st = case modemV22Rx st of
  (Just (ch, r), rate) -> "v22rx " ++ show ch ++ " " ++ show rate ++ " evm " ++ show (rxEvmEstimate r) ++ " ones2400 " ++ show (rxOnes2400Run r) ++ " sps " ++ show (rxSpsEstimate r)
  _ -> ""

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
  AudioSipLoop prefix -> do
    -- two loopbacks: modec plays into <prefix>-to-sip whose other side is the
    -- Audio/Source <prefix>-line (the softphone captures it); the softphone
    -- plays into sip-to-<prefix> whose other side is <prefix>-sip-line
    -- (modec captures it).  Node classes are exactly what baresip accepts.
    let lb name sink src = proc "pw-loopback"
          [ "-n", name
          , "--capture-props", "{ media.class = Audio/Sink node.name = " ++ sink ++ " node.description = \"" ++ sink ++ "\" }"
          , "--playback-props", "{ media.class = Audio/Source node.name = " ++ src ++ " node.description = \"" ++ src ++ "\" }" ]
        toSip = prefix ++ "-to-sip"; lineSrc = prefix ++ "-line"
        fromSip = "sip-to-" ++ prefix; sipSrc = prefix ++ "-sip-line"
        common = ["--raw", "--rate", show rate, "--channels", "1", "--format", "s16", "--latency", "100ms"]
        rec = (proc "pw-cat" (["--record", "--target", sipSrc, "-P", "{ node.name = " ++ prefix ++ "-rx }"] ++ common ++ ["-"])) { std_out = CreatePipe, std_err = Inherit }
        play = (proc "pw-cat" (["--playback", "--target", toSip, "-P", "{ node.name = " ++ prefix ++ "-tx }"] ++ common ++ ["-"])) { std_in = CreatePipe, std_err = Inherit }
    bracket (createProcess (lb (prefix ++ "-lb1") toSip lineSrc)) cleanup $ \_ ->
      bracket (createProcess (lb (prefix ++ "-lb2") fromSip sipSrc)) cleanup $ \_ -> do
        threadDelay 800000   -- let the loopback nodes appear before targeting them
        bracket (createProcess rec) cleanup $ \r ->
          bracket (createProcess play) cleanup $ \pl -> case (r, pl) of
            ((_, Just hin, _, _), (Just hout, _, _, _)) -> do
              hSetBinaryMode hin True
              hSetBinaryMode hout True
              hSetBuffering hout NoBuffering
              logMsg ("PipeWire loopbacks: " ++ toSip ++ " -> " ++ lineSrc ++ " (softphone source), " ++ fromSip ++ " -> " ++ sipSrc)
              body hin hout
            _ -> logMsg "could not start pw-cat" >> exitFailure
  AudioPipewire target monitor0 -> do
    -- On a machine with no capture device the only thing to record is the
    -- output's monitor; pw-cat cannot auto-connect to that, and a failed
    -- capture stream would end the audio-paced loop before anything is
    -- heard.  Fall back to monitor capture rather than dying silently.
    haveCapture <- hasCaptureDevice
    let monitor = monitor0 || (target == Nothing && not haveCapture)
    when (monitor && not monitor0) $
      logMsg "no capture device: recording the playback monitor (the modem hears its own tones)"
    let common = ["--raw", "--rate", show rate, "--channels", "1", "--format", "s16", "--latency", "100ms"]
                 ++ maybe [] (\t -> ["--target", t]) target
        -- pw-cat wants a numeric node id as target; stream.capture.sink records a sink's monitor
        recExtra = if monitor then ["-P", "{ stream.capture.sink = true }"] else []
        rec = (proc "pw-cat" (["--record"] ++ recExtra ++ common ++ ["-"])) { std_out = CreatePipe, std_err = Inherit }
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
