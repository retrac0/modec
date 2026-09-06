{-# LANGUAGE ScopedTypeVariables #-}
-- | The live modem: audio in and out through PipeWire (pw-cat) or raw
-- 16-bit little-endian mono pipes, bytes through a telnet socket or
-- stdio.  The main loop is paced by the audio input, one block at a
-- time; the pure modem in "Modec.Modem" does all the work.
module Modem
  ( AudioIO (..)
  , DataIO (..)
  , ModemOpts (..)
  , defaultModemOpts
  , runModem
  ) where

import Control.Concurrent
import Numeric (showFFloat)
import Control.Exception (IOException, bracket, finally, throwTo, try)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.IORef
import qualified Data.Vector.Storable as VS
import Network.Socket
import qualified Network.Socket.ByteString as NB
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO
import System.Process
import System.Posix.IO (OpenMode (..), defaultFileFlags, fdToHandle, openFd)
import System.Posix.Signals (Handler (..), installHandler, sigTERM)

import CallLog
import Modec.DSP (Signal, rms)
import Modec.Handshake
import Modec.Baresip
import Modec.Dtmf
import Modec.Hayes
import Modec.Modem
import Modec.Standards (fskBaud, fskName)
import Modec.Mnp (MnpConfig (..), MnpEvent (..), defaultMnpConfig)
import Modec.Pipewire
import Modec.V8 (describeMenu)
import Modec.V22 (Rate (..), rxEvmEstimate, rxOnes2400Run, rxSpsEstimate)
import Modec.Telnet
import Modec.Wav (closeWav, openWav16Mono, wavAppendRaw)

data AudioIO
  = AudioPipewire (Maybe String) (Maybe String) Bool
    -- ^ input and output device specifications (node id, name or a
    -- substring of either; 'Nothing' means the PipeWire default), and
    -- whether to record the output's monitor instead of an input
  | AudioSipLoop String              -- ^ PipeWire loopback pair for a softphone; the prefix names the nodes
  | AudioFiles FilePath FilePath     -- ^ raw s16le mono: input, output (files or FIFOs)
  | AudioStdio                       -- ^ raw s16le mono on stdin/stdout

-- | The audio interface the main loop sees.  Reading is the clock: a
-- short read means the capture stream stopped, and 'aiRestart' offers to
-- put it back (only the PipeWire backends can).
data AudioIf = AudioIf
  { aiRead    :: Int -> IO B.ByteString
  , aiWrite   :: B.ByteString -> IO ()
  , aiRestart :: IO Bool
  }

data DataIO
  = DataListen Int                   -- ^ telnet server on this port, one connection
  | DataConnect String Int           -- ^ telnet client
  | DataStdio                        -- ^ raw bytes on stdin/stdout (no telnet)

data ModemOpts = ModemOpts
  { moRate     :: Int
  , moBlockMs  :: Int
  , moRole     :: Role
  , moModes    :: [Standard]        -- ^ modes to negotiate, best first
  , moNoHandshake :: Bool
  , moNoV8bis  :: Bool
  , moV8       :: Bool
  , moV8All    :: Bool
  , moMaxEvm   :: Double
  , moMnp      :: Maybe Int          -- ^ highest MNP class to offer (2, 3 or 4)
  , moMnpTrt   :: Double             -- ^ round trip the retransmission timer allows for
  , moMnpProbes :: Int               -- ^ link requests sent before giving up
  , moMnpProbeGap :: Double          -- ^ seconds between them
  , moHayes    :: Bool
  , moSip      :: Maybe String       -- ^ baresip ctrl_tcp address host:port
  , moSipDomain :: String
  , moAudio    :: AudioIO
  , moData     :: DataIO
  , moAmp      :: Double
  , moRecordRx :: Maybe FilePath   -- ^ write everything received to this WAV
  , moRecordTx :: Maybe FilePath   -- ^ write everything transmitted to this WAV
  , moRecordDir :: Maybe FilePath  -- ^ record each call separately here (see "CallLog")
  , moDial     :: Maybe String     -- ^ dial this as soon as the line is ready
  , moHangupExits :: Bool          -- ^ leave when the call does, rather than back to AT
  }

-- | The settings a call is placed with when nothing says otherwise.
-- Anything that builds a 'ModemOpts' starts here and overrides what it
-- cares about, so a new option cannot be forgotten by a caller.
defaultModemOpts :: ModemOpts
defaultModemOpts = ModemOpts
  { moRate = 8000, moBlockMs = 20, moRole = Originate, moModes = allStandards
  , moNoHandshake = False, moNoV8bis = False, moV8 = False, moV8All = False
  , moMaxEvm = 1.0, moMnp = Nothing, moMnpTrt = 0.5, moMnpProbes = 6, moMnpProbeGap = 2.5
  , moHayes = False, moSip = Nothing, moSipDomain = ""
  , moAudio = AudioSipLoop "modec", moData = DataStdio, moAmp = 0.5
  , moRecordRx = Nothing, moRecordTx = Nothing, moRecordDir = Just "recordings"
  , moDial = Nothing, moHangupExits = False }

logMsg :: String -> IO ()
logMsg s = hPutStrLn stderr ("modec: " ++ s)

runModem :: ModemOpts -> IO ()
runModem o = do
  -- SIGTERM must unwind rather than kill the process outright, or child
  -- processes are orphaned and recordings are left unterminated
  mainTid <- myThreadId
  _ <- installHandler sigTERM (Catch (throwTo mainTid ExitSuccess)) Nothing
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  let fs = fromIntegral (moRate o)
      blockN = moRate o * moBlockMs o `div` 1000
      cfg0 = defaultModemConfig fs (moRole o) (moModes o)
      cfg = cfg0 { mcNoHandshake = moNoHandshake o, mcTxAmp = moAmp o, mcMaxEvm = moMaxEvm o, mcMnp = mnpCfg, mcHandshake = (mcHandshake cfg0) { hcV8bis = not (moNoV8bis o), hcV8 = moV8 o || moV8All o, hcV8OfferAll = moV8All o } }
      -- The rate and whether the link can go synchronous belong to the
      -- link rather than to the command line, so those two are left for
      -- Modec.Modem to fill in once the call is established.
      mnpCfg = fmap (\cls -> (defaultMnpConfig 1200 True)
                       { mnClass = cls, mnTrt = moMnpTrt o
                       , mnLrTries = max 1 (moMnpProbes o), mnT401Lr = moMnpProbeGap o })
                    (moMnp o)
  when (moNoHandshake o && length (moModes o) /= 1) $ do
    logMsg "--no-handshake needs exactly one mode, e.g. --standard v22"
    exitFailure
  when (null (moModes o)) $ do
    logMsg "no modes enabled"
    exitFailure
  -- connect to the softphone control port before the audio and the DTE,
  -- so that control is up whatever order the peers start in
  sip <- case moSip o of
    Nothing -> return Nothing
    Just addr -> Just <$> sipConnect addr
  withAudio (moAudio o) (moRate o) (moRole o) $ \ai ->
    withData (moData o) $ \recvBytes sendBytes -> do
      trace <- (/= Nothing) <$> lookupEnv "MODEC_TRACE"
      blockRef <- newIORef (0 :: Int)
      -- output leads input by one block: two modems joined by pipes would
      -- otherwise each wait for the other's first block
      aiWrite ai (encodeS16 (VS.replicate blockN 0))
      restarts <- newIORef (0 :: Int)
      -- optional session recordings, useful for checking what a VoIP trunk
      -- does to modem tones (modec detect / probe read them back)
      recRx <- mapM (\f -> logMsg ("recording received audio to " ++ f) >> openWav16Mono f (moRate o)) (moRecordRx o)
      recTx <- mapM (\f -> logMsg ("recording transmitted audio to " ++ f) >> openWav16Mono f (moRate o)) (moRecordTx o)
      -- The session recordings above are one file for however long the
      -- process runs.  This is the other kind: one recording and one log
      -- per call, named for when it was placed and what it dialled.
      callRef <- newIORef (Nothing :: Maybe CallRec)
      outcomeRef <- newIORef "no answer"
      let say msg = do
            logMsg msg
            mc <- readIORef callRef
            mapM_ (\c -> callRecSay c msg) mc
          beginCall number = case moRecordDir o of
            Nothing -> return ()
            Just dir -> do
              endCall                      -- a redial without a hangup
              mc <- callRecStart dir number (moRate o)
              writeIORef callRef mc
              writeIORef outcomeRef "no answer"
              mapM_ (\c -> logMsg ("recording this call to " ++ crStem c ++ ".wav")) mc
          endCall = do
            mc <- readIORef callRef
            case mc of
              Nothing -> return ()
              Just c -> do
                outcome <- readIORef outcomeRef
                callRecEnd c outcome
                writeIORef callRef Nothing
      let readBlock = do
            bs <- aiRead ai (2 * blockN)
            mapM_ (\w -> wavAppendRaw w bs) recRx
            mc <- readIORef callRef
            mapM_ (\c -> callRecWrite c bs) mc
            return bs
          writeBlock bs = do
            mapM_ (\w -> wavAppendRaw w bs) recTx
            aiWrite ai bs
          closeRecordings = endCall >> mapM_ closeWav recRx >> mapM_ closeWav recTx
          -- The capture stream stopped (device unplugged, pw-cat killed,
          -- the peer closed a FIFO).  Try to put it back a few times
          -- before giving up, so a glitching USB interface does not end
          -- the session.
          audioLost = do
            n <- readIORef restarts
            if n >= 3
              then logMsg "audio input ended (giving up after 3 restarts)" >> return False
              else do
                logMsg "audio input ended (the capture stream stopped)"
                ok <- aiRestart ai
                if ok
                  then do
                    writeIORef restarts (n + 1)
                    logMsg ("audio restarted (attempt " ++ show (n + 1) ++ ")")
                    aiWrite ai (encodeS16 (VS.replicate blockN 0))
                    return True
                  else return False
      let lineNode = case moAudio o of
            AudioSipLoop prefix -> Just (prefix ++ "-line")
            _ -> Nothing
          cfgFor role = let c0 = defaultModemConfig fs role (moModes o)
                        in c0 { mcNoHandshake = moNoHandshake o, mcTxAmp = moAmp o
                              , mcMaxEvm = moMaxEvm o, mcMnp = mnpCfg
                              , mcHandshake = (mcHandshake c0) { hcV8bis = not (moNoV8bis o), hcV8 = moV8 o || moV8All o, hcV8OfferAll = moV8All o } }
          traceStep st st' = when trace $ do
            k <- readIORef blockRef
            writeIORef blockRef (k + 1)
            when (modemTxCmd st' /= modemTxCmd st || k `mod` 25 == 0) $
              logMsg (show (fromIntegral (k * blockN) / fs :: Double) ++ " tx " ++ show (modemTxCmd st') ++ " " ++ v22Info st')
          report ev = case ev of
            EvConnected st link -> do
              writeIORef outcomeRef ("connected " ++ show st ++ " " ++ describeRate link)
              say ("CONNECT " ++ show st ++ " " ++ describeRate link ++ ", " ++ describeChannels link)
              when trace (logMsg (show link))
            EvDropped -> writeIORef outcomeRef "carrier lost" >> say "NO CARRIER"
            EvFailed why -> writeIORef outcomeRef ("failed: " ++ why) >> say ("connection failed: " ++ why)
            EvV8Menu m -> say ("V.8 far end offers: " ++ describeMenu m)
            EvMnp (MnpUp cls k n401) ->
              say ("MNP class " ++ show cls ++ ", " ++ show k ++ " outstanding frames, N401 " ++ show n401)
            EvMnp MnpTransparentFallback -> say "no error correction: the far end did not answer"
            EvMnp (MnpDown why) -> say ("MNP link down: " ++ why)
      if not (moHayes o) && sip == Nothing
        then do
          -- plain mode: one call in the configured role, then exit
          stRef <- newIORef (modemInit cfg)
          let loop = do
                raw <- readBlock
                if B.length raw < 2 * blockN
                  then audioLost >>= \ok -> when ok loop
                  else do
                    pending <- recvBytes
                    st <- readIORef stRef
                    let (st', audio, rxBytes, events) = modemStep cfg st (decodeS16 raw) (B.unpack pending)
                    writeIORef stRef st'
                    traceStep st st'
                    unless trace $ modifyIORef' blockRef (+ 1)
                    writeBlock (encodeS16 audio)
                    unless (null rxBytes) $ sendBytes (B.pack rxBytes)
                    mapM_ report events
                    unless (any isFinal events) loop
          loop `finally` closeRecordings
        else do
          -- Hayes mode: an AT command interpreter controls calls on the line
          hayesRef <- newIORef hayesInit
          lineRef <- newIORef LineIdle
          -- A number given on the command line is typed in for the user,
          -- once the line has had a moment to settle; from there on it is
          -- an ordinary Hayes call and everything else behaves the same.
          dialRef <- newIORef (moDial o)
          -- commands the modem types on the DTE's behalf, in the same
          -- stream as anything the DTE types itself
          injectRef <- newIORef B.empty
          doneRef <- newIORef False
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
              carrierGone = do
                modemEvent EvNoCarrier
                writeIORef lineRef LineIdle
                -- over SIP the call itself is still up until baresip says
                -- otherwise, and hanging up is what ends the recording
                if sip == Nothing
                  then do
                    endCall
                    when (moHangupExits o) (writeIORef doneRef True)
                  -- the trunk holds the call open after the modem tones
                  -- stop, so leaving means hanging up first
                  else when (moHangupExits o) $
                         modifyIORef' injectRef (<> BC.pack "ATH\r")
              modemEvent ev = do
                hs <- readIORef hayesRef
                let (hs', out) = hayesEvent hs ev
                writeIORef hayesRef hs'
                unless (B.null out) $ sendBytes out
              loop = do
                raw <- readBlock
                if B.length raw < 2 * blockN
                  then do
                    -- losing the line drops any call in progress
                    line <- readIORef lineRef
                    case line of
                      LineCall {} -> modemEvent EvNoCarrier >> writeIORef lineRef LineIdle
                      _ -> return ()
                    ok <- audioLost
                    when ok loop
                  else do
                    t <- tNow
                    modifyIORef' blockRef (+ 0)
                    typed <- recvBytes
                    toDial <- readIORef dialRef
                    injected <- atomicModifyIORef' injectRef (\b -> (B.empty, b))
                    pending <- case toDial of
                      Just n | t >= 1.0 -> do
                        writeIORef dialRef Nothing
                        return (injected <> BC.pack ("ATDT" ++ n ++ "\r") <> typed)
                      _ -> return (injected <> typed)
                    hs0 <- readIORef hayesRef
                    let (hs1, back, fwd, acts) = hayesInput t hs0 pending
                        (hs2, tickOut) = hayesTick t hs1
                    writeIORef hayesRef hs2
                    unless (B.null back) $ sendBytes back
                    unless (B.null tickOut) $ sendBytes tickOut
                    -- Every call gets its own recording, whether it was
                    -- dialled here or answered from the line, and whether
                    -- it goes out over SIP or over the audio device.
                    forM_ acts $ \a -> case a of
                      ActDial n -> beginCall (dialledNumber n)
                      ActAnswer -> beginCall "incoming"
                      _ -> return ()
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
                        say ("SIP call up, modem role " ++ show role)
                        -- PipeWire may have linked the default microphone into
                        -- the softphone's capture alongside our line, which
                        -- would put room noise on the wire; take it out now and
                        -- again once the stream has settled
                        forM_ lineNode $ \ln -> void $ forkIO $ forM_ [0, 1000000] $ \d -> do
                          threadDelay d
                          stray <- pruneCompetingInputs ln
                          forM_ stray $ \l ->
                            logMsg ("removed stray audio link into " ++ plDst l ++ " from " ++ plSrc l)
                        writeIORef lineRef (LineCall (modemInit (cfgFor role)) (cfgFor role))
                      SipStopModem -> do
                        writeIORef lineRef LineIdle
                        endCall
                        when (moHangupExits o) (writeIORef doneRef True)
                      SipToDte ev -> modemEvent ev
                    forM_ (if sip == Nothing then acts else []) $ \a -> do
                      line <- readIORef lineRef
                      case a of
                        ActDial s -> do
                          say ("dialling " ++ s)
                          writeIORef lineRef (LineDialing (dtmfDialSignal fs (0.5 * moAmp o) (map toUpperC s)))
                        ActAnswer -> do
                          say "answering"
                          writeIORef lineRef (LineCall (modemInit (cfgFor Answer)) (cfgFor Answer))
                        ActHangup -> case line of
                          LineIdle -> return ()
                          _ -> do
                            say "on hook"
                            writeIORef lineRef LineIdle
                            endCall
                            when (moHangupExits o) (writeIORef doneRef True)
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
                        writeBlock (encodeS16 (VS.replicate blockN 0))
                      LineDialing sig -> do
                        let (now, rest) = VS.splitAt blockN sig
                            block = now VS.++ VS.replicate (blockN - VS.length now) 0
                        writeBlock (encodeS16 block)
                        writeIORef lineRef (if VS.null rest then LineCall (modemInit (cfgFor Originate)) (cfgFor Originate) else LineDialing rest)
                      LineCall st c -> do
                        let (st', audio, rxBytes, events) = modemStep c st rxBlock (if online then B.unpack fwd else [])
                        traceStep st st'
                        writeBlock (encodeS16 audio)
                        when (online && not (null rxBytes)) $ sendBytes (B.pack rxBytes)
                        writeIORef lineRef (LineCall st' c)
                        forM_ events $ \ev -> do
                          report ev
                          case ev of
                            EvConnected _ link -> modemEvent (EvConnect (rateOf link))
                            EvDropped -> carrierGone
                            EvFailed _ -> carrierGone
                            -- reported to the log by `report`; the DTE
                            -- has no Hayes result code for a V.8 menu,
                            -- and none for the error-correcting protocol
                            -- either until the CONNECT message carries it
                            EvV8Menu _ -> return ()
                            EvMnp (MnpUp cls _ _) ->
                              modemEvent (EvProtocol ("MNP CLASS " ++ show cls))
                            EvMnp _ -> return ()
                    modifyIORef' blockRef (+ 1)
                    done <- readIORef doneRef
                    unless done loop
          loop `finally` closeRecordings
  where
    -- What the index line calls the speed.  An asymmetric link has two,
    -- and naming only one of them would be a lie by omission.
    describeRate link = case link of
      FskLink tx rx | fskBaud tx == fskBaud rx -> show (round (fskBaud tx) :: Int) ++ " bit/s"
                    | otherwise -> show (round (fskBaud rx) :: Int) ++ "/" ++ show (round (fskBaud tx) :: Int) ++ " bit/s"
      V22Link _ _ R1200 -> "1200 bit/s"
      V22Link _ _ R2400 -> "2400 bit/s"
    -- which way round the link runs, in the terms the standard uses
    describeChannels link = case link of
      FskLink tx rx -> "sending " ++ fskName tx ++ ", hearing " ++ fskName rx
      V22Link tx rx _ -> "sending " ++ show tx ++ ", hearing " ++ show rx
    isFinal EvDropped = True
    isFinal (EvFailed _) = True
    isFinal _ = False
    toUpperC ch = if ch >= 'a' && ch <= 'z' then toEnum (fromEnum ch - 32) else ch
    -- ATDT4695551212 reaches here as "T4695551212": the dial string keeps
    -- the tone/pulse/wait modifiers, and a recording should be named
    -- after the number, not after how the dialler was told to send it
    dialledNumber = dropWhile (`elem` " ,TPWtpw")

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
        r <- try (NB.recv sock 4096) :: IO (Either IOException B.ByteString)
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
  r <- try (NB.sendAll sock (commandJson cmd params ("m" ++ show n))) :: IO (Either IOException ())
  either (\e -> logMsg ("baresip send failed: " ++ show e)) return r

sipDrain :: SipClient -> IO [BsMessage]
sipDrain (SipClient _ queue _) = atomicModifyIORef' queue (\q -> ([], q))

-- | The line in Hayes mode: idle, dialling (DTMF audio left to play), or
-- a call in progress with its modem state and configuration.
data Line
  = LineIdle
  | LineDialing Signal
  | LineCall ModemState ModemConfig

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

-- | An audio interface backed by two handles that cannot be restarted.
handleIf :: Handle -> Handle -> AudioIf
handleIf hi ho = AudioIf
  { aiRead = B.hGet hi
  , aiWrite = \bs -> B.hPut ho bs >> hFlush ho
  , aiRestart = return False
  }

-- | A running pw-cat pair: capture pipe, playback pipe, and the two
-- child processes.
data PwPair = PwPair Handle Handle ProcessHandle ProcessHandle

-- | What 'createProcess' returns.
type ProcResult = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | Run a record/playback pw-cat pair, giving the body an interface that
-- can respawn it.  Reads and writes go through an 'IORef' so that a
-- restart is invisible to the caller.
withPwCatPair :: [String] -> [String] -> (AudioIf -> IO a) -> IO a
withPwCatPair recArgs playArgs body = do
  ref <- newIORef Nothing
  let spawn = do
        r <- try (createProcess (proc "pw-cat" recArgs) { std_out = CreatePipe, std_err = Inherit }) :: IO (Either IOException ProcResult)
        p <- try (createProcess (proc "pw-cat" playArgs) { std_in = CreatePipe, std_err = Inherit }) :: IO (Either IOException ProcResult)
        case (r, p) of
          (Right (_, Just hin, _, rph), Right (Just hout, _, _, pph)) -> do
            hSetBinaryMode hin True
            hSetBinaryMode hout True
            hSetBuffering hout NoBuffering
            writeIORef ref (Just (PwPair hin hout rph pph))
            return True
          _ -> do
            logMsg ("could not start pw-cat" ++ hint r)
            return False
      hint :: Either IOException ProcResult -> String
      hint (Left e) = ": " ++ show e
      hint _ = ""
      -- say why the capture stopped, when the child has already exited
      reportDeaths = do
        m <- readIORef ref
        forM_ m $ \(PwPair _ _ rph pph) -> do
          rc <- getProcessExitCode rph
          pc <- getProcessExitCode pph
          forM_ rc $ \c -> logMsg ("pw-cat (capture) exited: " ++ show c)
          forM_ pc $ \c -> logMsg ("pw-cat (playback) exited: " ++ show c)
      stop = do
        m <- readIORef ref
        writeIORef ref Nothing
        forM_ m $ \(PwPair hin hout rph pph) -> do
          ignore (hClose hin)
          ignore (hClose hout)
          ignore (terminateProcess rph)
          ignore (terminateProcess pph)
          ignore (void (waitForProcess rph))
          ignore (void (waitForProcess pph))
      ignore :: IO () -> IO ()
      ignore act = void (try act :: IO (Either IOException ()))
      rd n = do
        m <- readIORef ref
        case m of
          Nothing -> return B.empty
          Just (PwPair hin _ _ _) -> do
            -- IOException only: an asynchronous exception here is a
            -- shutdown request and must propagate, or the loop would treat
            -- it as audio loss and restart instead of exiting
            r <- try (B.hGet hin n) :: IO (Either IOException B.ByteString)
            return (either (const B.empty) id r)
      wr bs = do
        m <- readIORef ref
        forM_ m $ \(PwPair _ hout _ _) -> ignore (B.hPut hout bs >> hFlush hout)
      restart = do
        reportDeaths
        stop
        threadDelay 300000
        spawn
  started <- spawn
  unless started exitFailure
  body (AudioIf rd wr restart) `finally` stop

-- | Name a resolved device for the log.
nodeLabel :: Maybe PwNode -> String
nodeLabel Nothing = "PipeWire default"
nodeLabel (Just n) = pnName n ++ " (id " ++ show (pnId n) ++ ")"

-- | Resolve an optional device specification, exiting with a listing if
-- it names nothing or is ambiguous.
resolveOpt :: Maybe String -> PwClass -> IO (Maybe PwNode)
resolveOpt Nothing _ = return Nothing
resolveOpt (Just spec) want = do
  r <- resolveNode spec want
  case r of
    Right n -> return (Just n)
    Left why -> do
      logMsg why
      logMsg "list the devices with: modec devices"
      exitFailure

-- | Open the audio interface.
withAudio :: AudioIO -> Int -> Role -> (AudioIf -> IO a) -> IO a
withAudio aio rate role body = case aio of
  AudioStdio -> body (handleIf stdin stdout)
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
    body (handleIf hi ho) `finally` (ignoreIO (hClose hi) >> ignoreIO (hClose ho))
  AudioSipLoop prefix -> do
    -- Two loopbacks: modec plays into <prefix>-to-sip whose other side is
    -- the Audio/Source <prefix>-line (the softphone captures it); the
    -- softphone plays into sip-to-<prefix> whose other side is
    -- <prefix>-sip-line (modec captures it).  Those classes are exactly
    -- what baresip's PipeWire module accepts.
    let lb name sink src = proc "pw-loopback"
          [ "-n", name
          , "--capture-props", "{ media.class = Audio/Sink node.name = " ++ sink ++ " node.description = \"" ++ sink ++ "\" " ++ noRestore ++ " }"
          , "--playback-props", "{ media.class = Audio/Source node.name = " ++ src ++ " node.description = \"" ++ src ++ "\" " ++ noRestore ++ " }" ]
        toSip = prefix ++ "-to-sip"; lineSrc = prefix ++ "-line"
        fromSip = "sip-to-" ++ prefix; sipSrc = prefix ++ "-sip-line"
    bracket (createProcess (lb (prefix ++ "-lb1") toSip lineSrc)) cleanupProc $ \_ ->
      bracket (createProcess (lb (prefix ++ "-lb2") fromSip sipSrc)) cleanupProc $ \_ -> do
        -- wait for the nodes rather than guessing how long they take
        missing <- waitForNodes [toSip, lineSrc, fromSip, sipSrc] 5
        unless (null missing) $ do
          logMsg ("pw-loopback did not create: " ++ unwords missing)
          logMsg "is pipewire running, and is pw-loopback installed?"
          exitFailure
        logMsg ("PipeWire loopbacks: " ++ toSip ++ " -> " ++ lineSrc ++ " (softphone source), " ++ fromSip ++ " -> " ++ sipSrc)
        withPwCatPair
          (["--record", "--target", sipSrc, "-P", streamProps (prefix ++ "-rx")] ++ common ++ ["-"])
          (["--playback", "--target", toSip, "-P", streamProps (prefix ++ "-tx")] ++ common ++ ["-"])
          (withGainCheck [prefix ++ "-rx", prefix ++ "-tx"] body)
  AudioPipewire inSpec outSpec monitor0 -> do
    -- With no capture device the only thing to record is an output's
    -- monitor; pw-cat cannot auto-connect to that, and a failed capture
    -- stream would end the audio-paced loop before anything is heard.
    haveCapture <- hasCaptureDevice
    let monitor = monitor0 || (inSpec == Nothing && not haveCapture)
    when (monitor && not monitor0) $
      logMsg "no capture device: recording the playback monitor (the modem hears its own tones)"
    outN <- resolveOpt outSpec PwSink
    -- in monitor mode the capture target is a sink, so it follows the output
    inN <- if monitor then maybe (resolveOpt inSpec PwSink) (return . Just) outN
                      else resolveOpt inSpec PwSource
    let target = maybe [] (\n -> ["--target", show (pnId n)])
        recArgs = ["--record", "-P", streamPropsWith "modec-rx"
                     (if monitor then ["stream.capture.sink = true"] else [])]
                  ++ target inN ++ common ++ ["-"]
        playArgs = ["--playback", "-P", streamProps "modec-tx"] ++ target outN ++ common ++ ["-"]
    logMsg ("audio in: " ++ (if monitor then "monitor of " else "") ++ nodeLabel inN
            ++ ", out: " ++ nodeLabel outN ++ ", " ++ show rate ++ " Hz")
    withPwCatPair recArgs playArgs (withGainCheck ["modec-rx", "modec-tx"] body)
  where
    common = ["--raw", "--rate", show rate, "--channels", "1", "--format", "s16", "--latency", "100ms"]
    -- WirePlumber restores per-application volumes from its
    -- stream-properties state, and every pw-cat stream on the machine
    -- shares the application name "pw-cat".  A single slider drag in a
    -- mixer therefore attenuates the modem's transmit permanently, on a
    -- control nothing in a call would lead you to inspect.  A send level
    -- is part of the modulation, not a listening preference, so opt out
    -- of the restore and take a name of our own.
    noRestore = "state.restore-props = false"
    streamProps name = streamPropsWith name []
    streamPropsWith name extra =
      "{ node.name = " ++ name ++ " application.name = modec " ++ noRestore
        ++ concatMap (' ' :) extra ++ " }"
    -- Confirm the opt-out took: a graph we do not control could still
    -- put a volume on the stream, and silently sending 8 dB low is worse
    -- than a warning nobody needs.
    withGainCheck names act aif = do
      _ <- forkIO $ do
        threadDelay 800000
        bad <- attenuatedNodes names
        mapM_ (\(n, v, m) -> logMsg ("warning: " ++ n ++ " is " ++
                (if m then "muted" else showFFloat (Just 1) (20 * logBase 10 v) "" ++ " dB")
                ++ "; audio levels will be wrong (reset it in a mixer)")) bad
      act aif
    ignoreIO act = void (try act :: IO (Either IOException ()))
    cleanupProc (mi, mo, _, ph) = do
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
      r <- try (B.hGetSome h 4096) :: IO (Either IOException B.ByteString)
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
        r <- try (NB.recv sock 4096) :: IO (Either IOException B.ByteString)
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
