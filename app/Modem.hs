{-# LANGUAGE ScopedTypeVariables #-}
-- | The live modem: audio in and out through PipeWire (pw-cat) or raw
-- mono pipes in any of "Modec.Sample"'s formats, bytes through a telnet
-- socket or stdio.  The main loop is paced by the audio input, one block at a
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
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Vector.Storable as VS
import Network.Socket
import qualified Network.Socket.ByteString as NB
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Version (showVersion)
import Paths_modec (version)
import System.IO
import System.Process
import System.Posix.IO (OpenMode (..), defaultFileFlags, fdToHandle, openFd)
import System.Posix.Signals (Handler (..), installHandler, sigTERM)

import CallLog
import Modec.Link
import Modec.DSP (Signal, rms)
import Modec.Handshake
import Modec.Baresip
import Modec.Dtmf
import Modec.Progress
import Modec.Hayes
import Modec.Modem
import Modec.Standards
import qualified Modec.V32 as V32
import Modec.Mnp (MnpConfig (..), MnpEvent (..), defaultMnpConfig)
import Modec.Pipewire
import PipewireIO
import Modec.V8 (describeMenu)
import Modec.V22 (rxEvmEstimate, rxOnes2400Run, rxSpsEstimate)
import Modec.Telnet
import Modec.Sample
import Modec.Session
import Serial (withSerial)
import Modec.Wav (closeWav, openWav16Mono, wavAppend)

data AudioIO
  = AudioPipewire (Maybe String) (Maybe String) Bool
    -- ^ input and output device specifications (node id, name or a
    -- substring of either; 'Nothing' means the PipeWire default), and
    -- whether to record the output's monitor instead of an input
  | AudioSipLoop String              -- ^ PipeWire loopback pair for a softphone; the prefix names the nodes
  | AudioFiles FilePath FilePath     -- ^ raw mono in 'moFormat': input, output (files or FIFOs)
  | AudioStdio                       -- ^ raw mono in 'moFormat' on stdin/stdout
  | AudioSerial FilePath             -- ^ a voice-mode modem on this serial port: the line itself

-- | The audio interface the main loop sees.  Reading is the clock: a
-- short read means the capture stream stopped, and 'aiRestart' offers to
-- put it back (only the PipeWire backends can).
--
-- Samples, not bytes: what the bytes on a device mean is settled here,
-- once, by the format the backend was opened with, and the loop above
-- never sees them.
data AudioIf = AudioIf
  { aiRead    :: Int -> IO Signal        -- ^ this many samples
  , aiWrite   :: Signal -> IO ()
  , aiRestart :: IO Bool
  }

-- | An interface over a byte reader and writer, in a format.
sampleIf :: SampleFormat -> (Int -> IO B.ByteString) -> (B.ByteString -> IO ()) -> IO Bool -> AudioIf
sampleIf fmt rd wr restart = AudioIf
  { aiRead = \n -> decodeSamples fmt <$> rd (n * bytesPerSample fmt)
  , aiWrite = wr . encodeSamples fmt
  , aiRestart = restart
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
  , moV8       :: Bool
  , moProbe    :: Bool
  , moV8All    :: Bool
  , moMaxEvm   :: Double
  , moV32Rates :: Maybe V32Rate  -- ^ hold V.32 to one rate
  , moMnp      :: Maybe Int          -- ^ highest MNP class to offer (2, 3 or 4)
  , moMnpTrt   :: Double             -- ^ round trip the retransmission timer allows for
  , moMnpProbes :: Int               -- ^ link requests sent before giving up
  , moMnpProbeGap :: Double          -- ^ seconds between them
  , moHayes    :: Bool
  , moSip      :: Maybe String       -- ^ baresip ctrl_tcp address host:port
  , moSipDomain :: String
  , moAudio    :: AudioIO
  , moFormat   :: Maybe SampleFormat  -- ^ what the audio device or pipe carries; 'Nothing' takes the backend's own default
  , moData     :: DataIO
  , moAmp      :: Double
  , moRecordRx :: Maybe FilePath   -- ^ write everything received to this WAV
  , moRecordTx :: Maybe FilePath   -- ^ write everything transmitted to this WAV
  , moRecordDir :: Maybe FilePath  -- ^ record each call separately here (see "CallLog")
  , moAutoType :: Maybe String     -- ^ typed on the DTE's behalf once the line is ready
  , moBanner   :: Bool             -- ^ greet the far end with what we connected at
  , moHangupExits :: Bool          -- ^ leave when the call does, rather than back to AT
  , moIgnoreBusy :: Bool          -- ^ stay on the line through busy, congestion and SIT
  }

-- | The settings a call is placed with when nothing says otherwise.
-- Anything that builds a 'ModemOpts' starts here and overrides what it
-- cares about, so a new option cannot be forgotten by a caller.
defaultModemOpts :: ModemOpts
defaultModemOpts = ModemOpts
  { moRate = 8000, moBlockMs = 20, moRole = Originate, moModes = allStandards
  , moNoHandshake = False, moV8 = False, moV8All = False, moProbe = False
  , moMaxEvm = 1.0, moV32Rates = Nothing, moMnp = Nothing, moMnpTrt = 0.5, moMnpProbes = 6, moMnpProbeGap = 2.5
  , moHayes = False, moSip = Nothing, moSipDomain = ""
  , moAudio = AudioSipLoop "modec", moFormat = Nothing, moData = DataStdio, moAmp = 0.5
  , moRecordRx = Nothing, moRecordTx = Nothing, moRecordDir = Just "recordings"
  , moAutoType = Nothing, moBanner = False, moHangupExits = False, moIgnoreBusy = False }

logMsg :: String -> IO ()
logMsg s = hPutStrLn stderr ("modec: " ++ s)

-- | The format the audio moves in: what was asked for, else what the
-- backend naturally carries -- s16 for every sound card and pipe, and
-- for a voice-mode modem mu-law.
--
-- The 14-bit linear @+VSM=133@ offers is the better front end on paper
-- and unusable duplex in fact.  A CX93001 has one throughput budget
-- shared by both directions, and it is about 30.4 kB/s.  Receiving
-- alone, 14-bit runs at its full 16.5 kB/s with a single underrun; it
-- is holding a carrier at the same time that breaks it, because 16
-- kB/s each way is 32 kB/s and that does not fit.  Measured, with only
-- the transmit load varied: at 4, 8 and 12 kB/s out the receive side
-- still gets its full 16.5 kB/s, and at 16 kB/s out it falls to 14.4
-- and the modem reports 5921 buffer underruns.  Mu-law is 8 kB/s each
-- way, 16 kB/s the pair, half the budget, and it runs clean -- and
-- 'Modec.G711' decodes it exactly.
--
-- Neither knob that looks like the answer is one.  The DTE rate is not
-- the throttle: B115200 through B921600 all measure the same 14.4
-- kB/s, and so does @+VPR@ at 0, 48 and 96, which is what CDC-ACM
-- ignoring its own line coding looks like.  Ask for @--audio-format
-- pcm14@ when only receiving, or on a dongle with a wider budget.
audioFormat :: ModemOpts -> SampleFormat
audioFormat o = case (moFormat o, moAudio o) of
  (Just f, _) -> f
  (Nothing, AudioSerial _) -> Ulaw
  _ -> S16

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
      -- One builder, used by both the plain path and the SIP one.  They
      -- were two, kept in step by hand, and they were not in step: the
      -- SIP path -- which is every real call -- silently dropped
      -- --max-evm, and then --probe, because each was added to whichever
      -- one the author happened to be looking at.
      configFor role =
        let c0 = defaultModemConfig fs role (moModes o)
        in c0 { mcNoHandshake = moNoHandshake o, mcProbe = moProbe o
              , mcTxAmp = moAmp o, mcMaxEvm = moMaxEvm o
              , mcV32Rates = v32Offered o, mcMnp = mnpCfg
              , mcHandshake = (mcHandshake c0)
                  { hcV8 = moV8 o || moV8All o
                  , hcV8OfferAll = moV8All o } }
      -- The rate and whether the link can go synchronous belong to the
      -- link rather than to the command line, so those two are left for
      -- Modec.Modem to fill in once the call is established.
      mnpCfg = fmap (\cls -> (defaultMnpConfig 1200 True)
                       { mnClass = cls, mnTrt = moMnpTrt o
                       , mnLrTries = max 1 (moMnpProbes o), mnT401Lr = moMnpProbeGap o })
                    (moMnp o)
  when (moNoHandshake o && length (moModes o) /= 1) $ do
    logMsg "--no-handshake needs exactly one mode, e.g. --mode v22"
    exitFailure
  when (null (moModes o)) $ do
    logMsg "no modes enabled"
    exitFailure
  -- connect to the softphone control port before the audio and the DTE,
  -- so that control is up whatever order the peers start in
  sip <- case moSip o of
    Nothing -> return Nothing
    Just addr -> Just <$> sipConnect addr
  withAudio (moAudio o) (audioFormat o) (moRate o) (moRole o) $ \ai ->
    withData (moData o) $ \recvBytes sendBytes -> do
      trace <- (/= Nothing) <$> lookupEnv "MODEC_TRACE"
      blockRef <- newIORef (0 :: Int)
      -- Output leads input.  One block was enough for two modems joined
      -- by pipes, which would otherwise each wait for the other's first
      -- block, and one block is not enough for PipeWire.  Both pw-cat
      -- streams run on a 100 ms quantum: capture hands over 100 ms at a
      -- time, and playback asks for 100 ms at a time, on two clocks that
      -- have no reason to agree.  Feeding playback exactly what capture
      -- delivered leaves it one block ahead on a good cycle and short on
      -- a bad one, and every short cycle is an xrun: pw-top counted one a
      -- second on modec-tx, and each one is a hole in our carrier that the
      -- far end reads as a start bit -- 0xFE at 300 bit/s, a scrambled
      -- byte at 2400.  Measured on this machine, 50 ms of cushion laid
      -- down at the right moment is already enough for pw-top to count
      -- no xruns at all over a call; 100 ms is that with margin, at a
      -- cost on our transmit that no handshake timer here notices.
      -- MODEC_TX_LEAD_MS overrides it.
      -- The cushion has to be laid down once capture is actually
      -- flowing.  Playback connects before capture does, and anything
      -- written before the first block arrives is drained by those
      -- first empty cycles while we are still blocked on the read; a
      -- cushion written then is gone before the call starts, and the
      -- rest of the call runs with none.  One block goes out now, so two
      -- modems joined by pipes do not each wait for the other's first
      -- block; the cushion follows the first block in.
      leadMs <- maybe 100 read <$> lookupEnv "MODEC_TX_LEAD_MS"
      let leadBlocks = max 1 ((leadMs + moBlockMs o - 1) `div` moBlockMs o) :: Int
          lead = VS.replicate (blockN * leadBlocks) 0
      aiWrite ai (VS.replicate blockN 0)
      primed <- newIORef False
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
      -- The greeting the answering side sends, and the two things it is
      -- composed from that are known before the link can carry it.
      bannerRef <- newIORef B.empty
      connRef <- newIORef ([] :: [String])
      peerRef <- newIORef ""
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
            x <- aiRead ai blockN
            p <- readIORef primed
            unless p $ do
              writeIORef primed True
              aiWrite ai lead
            mapM_ (\w -> wavAppend w x) recRx
            mc <- readIORef callRef
            mapM_ (\c -> callRecWrite c x) mc
            return x
          writeBlock x = do
            mapM_ (\w -> wavAppend w x) recTx
            mc <- readIORef callRef
            mapM_ (\c -> callRecWriteTx c x) mc
            aiWrite ai x
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
                    aiWrite ai (VS.replicate blockN 0)
                    writeIORef primed False
                    return True
                  else return False
      let lineNode = case moAudio o of
            AudioSipLoop prefix -> Just (prefix ++ "-line")
            _ -> Nothing
          cfgFor = configFor
          -- A call we placed watches the line for what the network
          -- plays back at it; a call we answered does not.
          startCall role = LineCall (modemInit (cfgFor role)) (cfgFor role)
            (if role == Originate then Just (progressRxInit fs defaultProgressParams) else Nothing)
          -- The block index is handed in rather than read, which is what
          -- keeps a trace from being able to move the clock it is
          -- tracing.  It used to read it, and the two loops compensated
          -- differently -- one skipped its own increment when tracing,
          -- the other did not -- so with MODEC_TRACE set the Hayes clock
          -- counted two blocks per block and every guard time in
          -- Modec.Hayes ran at double speed.
          traceStep k st st' = when trace $
            when (modemTxCmd st' /= modemTxCmd st || k `mod` 25 == 0) $
              logMsg (show (fromIntegral (k * blockN) / fs :: Double) ++ " tx " ++ show (modemTxCmd st') ++ " " ++ v22Info st')
          -- Every five seconds of a V.32 call, what the receiver and the
          -- canceller think of the line.  All four of these accessors
          -- existed and nothing called them, so a live call produced a
          -- recording and a list of phases and no numbers at all -- and
          -- the numbers are the whole of what a call is for once it is
          -- connecting at all.
          telemetry k st' = do
            let evm = maybe "" (printf "decision error %.4f" . abs) (modemV32Evm st')
                delay = maybe "" (printf ", echo at %.0f ms" . (\d -> fromIntegral d / (fs / 1000) :: Double))
                              (modemEchoDelay st')
                erle = maybe "" (printf ", return loss %.1f dB") (modemEchoErle st')
                line = intercalate ", " (filter (not . null) [evm, dropComma delay, dropComma erle])
                dropComma x = case x of { (',' : ' ' : r) -> r; _ -> x }
            -- Only once there is something to say.  A V.32 call has a
            -- canceller from the first block, so reporting whenever one
            -- exists prints "return loss 0.0 dB" every five seconds
            -- through a handshake that has not started cancelling
            -- anything.
            when (k `mod` 250 == 249 && (modemV32Evm st' /= Nothing
                                         || modemEchoDelay st' /= Nothing)) $
              say ("line: " ++ line)

          -- An answering modem with no service behind it still owes the
          -- caller a word about what it just agreed to.  Held until the
          -- link can carry it: with MNP that means waiting for the
          -- protocol, because its class is part of the answer.
          sendBanner ec = when (moBanner o) $ do
            conn <- readIORef connRef
            peer <- readIORef peerRef
            now <- getCurrentTime
            let ls = [ "", "modec " ++ showVersion version ++ " -- software modem", "" ]
                     ++ conn
                     ++ [ "error correction: " ++ ec ]
                     ++ [ "caller: " ++ peer | not (null peer) ]
                     ++ [ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" now
                        , "", "Nothing is listening behind this modem yet.", "" ]
            writeIORef bannerRef (BC.pack (concatMap (++ "\r\n") ls))
          report ev = case ev of
            EvConnected st link -> do
              writeIORef outcomeRef ("connected " ++ show st ++ " " ++ linkRateName link)
              say ("CONNECT " ++ show st ++ " " ++ linkRateName link ++ ", " ++ linkChannels link)
              -- "this end" because the banner is read at the other one,
              -- where "sending HighChannel" would otherwise look like a
              -- description of the reader's own side
              writeIORef connRef [ "CONNECT " ++ show st ++ " " ++ linkRateName link
                                 , "this end: " ++ linkChannels link ]
              -- with no protocol to wait for, the carrier is the boundary
              when (moMnp o == Nothing) (sendBanner "none")
              when trace (logMsg (show link))
            EvDropped -> writeIORef outcomeRef "carrier lost" >> say "NO CARRIER"
            EvFailed why -> writeIORef outcomeRef ("failed: " ++ why) >> say ("connection failed: " ++ why)
            EvV8Menu m -> say ("V.8 far end offers: " ++ describeMenu m)
            EvMnp (MnpUp cls k n401) -> do
              say ("MNP class " ++ show cls ++ ", " ++ show k ++ " outstanding frames, N401 " ++ show n401)
              sendBanner ("MNP class " ++ show cls)
            EvMnp MnpTransparentFallback -> do
              say "no error correction: the far end did not answer"
              sendBanner "none (the far end did not answer)"
            EvMnp (MnpDown why) -> say ("MNP link down: " ++ why)
            -- 5.5 is invisible from the terminal by design -- nothing has
            -- ended and the DTE is not told -- so the call log is the
            -- only place it shows up at all.
            EvRetrain why -> say ("retraining the link: " ++ case why of
              RetrainLocal -> "this receiver could not read the line"
              RetrainFarEnd -> "the far end asked")
            EvRate r -> say ("now running at " ++ show (rateBitRate r) ++ " bit/s")
      -- One loop, in Modec.Session.  What a plain call and a Hayes
      -- session disagree about is a Controller, and the line state is
      -- the same in both: a plain call is a session that starts in
      -- LineCall and never leaves it.
      lineRef <- newIORef (startCall (moRole o))
      let session = Session
            { seFs = fs, seBlockN = blockN
            , seRead = readBlock, seWrite = writeBlock
            , seRecv = recvBytes, seSend = sendBytes
            , seSay = say
            , seObserve = \k st st' -> traceStep k st st' >> telemetry k st'
            , seLost = audioLost
            , seStartCall = startCall
            , seParams = defaultProgressParams
            , seLine = lineRef, seBanner = bannerRef, seBlock = blockRef
            }
      if not (moHayes o) && sip == Nothing
        then do
          -- plain mode: one call in the configured role, then exit
          doneRef <- newIORef False
          let co = quietController
                { coTurn = \_ -> do
                    pending <- recvBytes
                    return (Turn pending True)
                , coEvent = \ev -> report ev >> when (isFinal ev) (writeIORef doneRef True)
                , coProgress = \e -> when (refused (peKind e)) $ do
                    writeIORef outcomeRef (shortName (peKind e))
                    unless (moIgnoreBusy o) (writeIORef doneRef True)
                  -- An audio restart does not end a plain call: the modem
                  -- state is still good and the far end is still there.
                , coCarrier = return ()
                , coDone = readIORef doneRef
                }
          runLoop session co `finally` closeRecordings
        else do
          -- Hayes mode: an AT command interpreter controls calls on the line
          writeIORef lineRef LineIdle
          hayesRef <- newIORef hayesInit
          -- A command the invocation implies is typed in for the user once
          -- the line has had a moment to settle -- DT<number> for `dial`,
          -- S0=1 for `answer`.  From there on it is an ordinary Hayes
          -- session and everything else behaves the same.
          autoTypeRef <- newIORef (moAutoType o)
          -- commands the modem types on the DTE's behalf, in the same
          -- stream as anything the DTE types itself
          injectRef <- newIORef B.empty
          doneRef <- newIORef False
          energyRef <- newIORef (0 :: Int)
          sipLineRef <- newIORef (sipLineInit (moSipDomain o))
          logMsg (case sip of
                    Nothing -> "Hayes command mode (ATD to dial, ATA to answer, ATH to hang up)"
                    Just _ -> "Hayes command mode over SIP (baresip at " ++ maybe "" id (moSip o) ++ ")")
          let -- The number in the CONNECT result code, which is what the
              -- DTE is told it is talking at.  Not 'linkBitRate': a
              -- terminal on a V.23 call is told 300, not the 75 bit/s
              -- its own direction crawls back at.
              rateOf link = case link of
                FskLink {} -> 300
                V22Link _ _ R1200 -> 1200
                V22Link _ _ R2400 -> 2400
                V32Link _ r -> rateBitRate r :: Int
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

              turn t = do
                typed <- recvBytes
                toType <- readIORef autoTypeRef
                injected <- atomicModifyIORef' injectRef (\b -> (B.empty, b))
                pending <- case toType of
                  Just cmd | t >= 1.0 -> do
                    writeIORef autoTypeRef Nothing
                    return (injected <> BC.pack ("AT" ++ cmd ++ "\r") <> typed)
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
                  -- over SIP the answer is begun on CALL_ESTABLISHED
                  -- instead, which is also where the caller's name is
                  -- known and where an auto-answer arrives at all
                  ActAnswer | sip == Nothing -> beginCall "incoming"
                  _ -> return ()
                -- SIP: Hayes actions and baresip events go through the line controller
                sipActs <- case sip of
                  Nothing -> return []
                  Just cl -> do
                    evs <- sipDrain cl
                    sl0 <- readIORef sipLineRef
                    -- S0 lives in the Hayes state, which the line
                    -- controller cannot see; push it in each block so
                    -- an incoming call can answer itself
                    let sl0' = sipLineSetAuto (hayesAutoAnswer hs2) sl0
                        (sl1, as1) = foldl (\(sl, acc) a -> let (sl', xs) = sipLineHayes sl a in (sl', acc ++ xs)) (sl0', []) acts
                        (sl2, as2) = foldl (\(sl, acc) e -> let (sl', xs) = sipLineEvent t sl e in (sl', acc ++ xs)) (sl1, []) evs
                        (sl3, as3) = sipLineTick t sl2
                    writeIORef sipLineRef sl3
                    return (as1 ++ as2 ++ as3)
                forM_ sipActs $ \sa -> case sa of
                  SipCommand c params -> maybe (return ()) (\cl -> sipSend cl c params) sip
                  SipStartModem role -> do
                    say ("SIP call up, modem role " ++ show role)
                    peer <- sipLinePeer <$> readIORef sipLineRef
                    -- cleared on a call we placed, or a banner sent on
                    -- the next one would name whoever rang before it
                    writeIORef peerRef (if role == Answer then peer else "")
                    when (role == Answer) (beginCall (callerName peer))
                    -- PipeWire may have linked the default microphone into
                    -- the softphone's capture alongside our line, which
                    -- would put room noise on the wire; take it out now and
                    -- again once the stream has settled
                    forM_ lineNode $ \ln -> void $ forkIO $ forM_ [0, 1000000] $ \d -> do
                      threadDelay d
                      stray <- pruneCompetingInputs ln
                      forM_ stray $ \l ->
                        logMsg ("removed stray audio link into " ++ plDst l ++ " from " ++ plSrc l)
                    writeIORef lineRef (startCall role)
                  SipStopModem -> do
                    writeIORef lineRef LineIdle
                    endCall
                    when (moHangupExits o) (writeIORef doneRef True)
                  SipToDte ev -> do
                    modemEvent ev
                    -- A dial that closes without ever being
                    -- established never reached the far end at all,
                    -- and that is worth telling apart from a far end
                    -- that did not pick up: the modem heard nothing
                    -- because there was no call, not because the line
                    -- was quiet.
                    when (ev == EvNoAnswer) $ do
                      writeIORef outcomeRef "no SIP call: the trunk never answered"
                      say "no SIP call: baresip got no answer to its INVITE"
                      say "if numbers that used to answer all do this, restart baresip -- a long-lived one can stop getting call responses while its registration still succeeds"
                      endCall
                      when (moHangupExits o) (writeIORef doneRef True)
                forM_ (if sip == Nothing then acts else []) $ \a -> do
                  line <- readIORef lineRef
                  case a of
                    ActDial d -> do
                      say ("dialling " ++ d)
                      writeIORef lineRef (LineDialing (dtmfDialSignal fs (0.5 * moAmp o) (map toUpperC d)))
                    ActAnswer -> do
                      say "answering"
                      writeIORef lineRef (startCall Answer)
                    ActHangup -> case line of
                      LineIdle -> return ()
                      _ -> do
                        say "on hook"
                        writeIORef lineRef LineIdle
                        endCall
                        when (moHangupExits o) (writeIORef doneRef True)
                    ActOnline -> return ()
                return (Turn fwd (hayesOnline hs2))

              -- a calling signal (any sustained energy) while idle rings
              -- the DTE (not in SIP mode: baresip rings)
              idle rxBlock = do
                let loud = sip == Nothing && rms rxBlock > 0.01
                n <- readIORef energyRef
                let n' = if loud then n + 1 else 0
                writeIORef energyRef n'
                when (n' == 25) $ do
                  modemEvent EvRing
                  hs <- readIORef hayesRef
                  when (hayesAutoAnswer hs) $ do
                    logMsg "auto-answer"
                    writeIORef lineRef (startCall Answer)
                when (n' > 25) $ writeIORef energyRef 0

              onEvent ev = do
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
                  -- 5.5 is not a Hayes result code.  The call is up
                  -- throughout a retrain and the DTE is told nothing,
                  -- which is the point of it; 'report' has already put
                  -- both in the log.
                  --
                  -- This case was missing, and every event here is
                  -- matched without a catch-all, so the first retrain on
                  -- a live call killed the modem outright: dialled a
                  -- board that connected at 9600 trellis and asked for a
                  -- retrain a second later, and the process died of a
                  -- non-exhaustive pattern with the call still up.
                  -- Nothing in the suite could see it -- the tests drive
                  -- Modec.Modem and never this.
                  EvRetrain _ -> return ()
                  EvRate _ -> return ()

              onProgress e = when (refused (peKind e)) $
                if moIgnoreBusy o
                  then writeIORef outcomeRef (shortName (peKind e))
                  else do
                    writeIORef outcomeRef (shortName (peKind e))
                    modemEvent EvBusy
                    writeIORef lineRef LineIdle
                    endCall
                    when (moHangupExits o) (writeIORef doneRef True)

              -- Losing the line drops any call in progress.
              lineLost = do
                line <- readIORef lineRef
                case line of
                  LineCall {} -> modemEvent EvNoCarrier >> writeIORef lineRef LineIdle
                  _ -> return ()

          let co = Controller
                { coTurn = turn, coEvent = onEvent, coProgress = onProgress
                , coCarrier = lineLost, coIdle = idle, coDone = readIORef doneRef }
          runLoop session co `finally` closeRecordings
  where
    isFinal EvDropped = True
    isFinal (EvFailed _) = True
    isFinal _ = False
    toUpperC ch = if ch >= 'a' && ch <= 'z' then toEnum (fromEnum ch - 32) else ch
    -- A recording is named after who called, not after the whole URI:
    -- the host is the same on every call and only makes the stem longer.
    callerName u = case break (== '@') (drop 1 (dropWhile (/= ':') u)) of
      (user, _) | not (null user) -> user
      _ -> "incoming"
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
--
-- | The call progress tones that mean the network has refused the
-- call.  Ringing and dial tone are news, not refusals, and a call goes
-- on through them.
refused :: Progress -> Bool
refused k = case k of
  Busy -> True
  Reorder -> True
  Sit _ -> True
  _ -> False

-- | What to write in the call log when one of them ends a call.
shortName :: Progress -> String
shortName k = case k of
  Busy -> "busy"
  Reorder -> "congestion: no circuit"
  Sit _ -> "special information tone: the call did not complete"
  _ -> "call progress"

v22Info :: ModemState -> String
v22Info st = case modemV22Rx st of
  (Just (ch, r), rate) -> "v22rx " ++ show ch ++ " " ++ show rate ++ " evm " ++ show (rxEvmEstimate r) ++ " ones2400 " ++ show (rxOnes2400Run r) ++ " sps " ++ show (rxSpsEstimate r)
  _ -> ""

-- | An audio interface backed by two handles that cannot be restarted.
handleIf :: SampleFormat -> Handle -> Handle -> AudioIf
handleIf fmt hi ho = sampleIf fmt (B.hGet hi) (\bs -> B.hPut ho bs >> hFlush ho) (return False)

-- | A running pw-cat pair: capture pipe, playback pipe, and the two
-- child processes.
data PwPair = PwPair Handle Handle ProcessHandle ProcessHandle

-- | What 'createProcess' returns.
type ProcResult = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | Run a record/playback pw-cat pair, giving the body an interface that
-- can respawn it.  Reads and writes go through an 'IORef' so that a
-- restart is invisible to the caller.
withPwCatPair :: SampleFormat -> [String] -> [String] -> (AudioIf -> IO a) -> IO a
withPwCatPair fmt recArgs playArgs body = do
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
  body (sampleIf fmt rd wr restart) `finally` stop

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
withAudio :: AudioIO -> SampleFormat -> Int -> Role -> (AudioIf -> IO a) -> IO a
withAudio aio fmt rate role body = lookupEnv "MODEC_PW_LATENCY" >>= \pwLatencyEnv -> withAudio' aio fmt rate role body pwLatencyEnv

-- | The name pw-cat gives a format, for the ones it has.  PipeWire
-- converts whatever the device does to what a stream asks for, so a
-- format it lacks is not a device it cannot reach, only a name.
pwCatFormat :: SampleFormat -> Maybe String
pwCatFormat f = case f of
  U8 -> Just "u8"; S8 -> Just "s8"; S16 -> Just "s16"; S32 -> Just "s32"; F32 -> Just "f32"
  _ -> Nothing

withAudio' :: AudioIO -> SampleFormat -> Int -> Role -> (AudioIf -> IO a) -> Maybe String -> IO a
withAudio' aio fmt rate role body pwLatencyEnv = case aio of
  AudioStdio -> body (handleIf fmt stdin stdout)
  AudioSerial dev -> withSerial dev fmt rate role logMsg $ \rd wr ->
    body (sampleIf fmt rd wr (return False))
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
    body (handleIf fmt hi ho) `finally` (ignoreIO (hClose hi) >> ignoreIO (hClose ho))
  AudioSipLoop prefix -> do
    requirePwFormat
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
        withPwCatPair fmt
          (["--record", "--target", sipSrc, "-P", streamProps (prefix ++ "-rx")] ++ common ++ ["-"])
          (["--playback", "--target", toSip, "-P", streamProps (prefix ++ "-tx")] ++ common ++ ["-"])
          (withGainCheck [prefix ++ "-rx", prefix ++ "-tx"] body)
  AudioPipewire inSpec outSpec monitor0 -> do
    requirePwFormat
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
            ++ ", out: " ++ nodeLabel outN ++ ", " ++ show rate ++ " Hz " ++ formatName fmt)
    withPwCatPair fmt recArgs playArgs (withGainCheck ["modec-rx", "modec-tx"] body)
  where
    common = ["--raw", "--rate", show rate, "--channels", "1", "--format", pwFmt, "--latency", pwLatency]
    pwFmt = maybe "s16" id (pwCatFormat fmt)
    requirePwFormat = case pwCatFormat fmt of
      Just _ -> return ()
      Nothing -> do
        logMsg ("pw-cat cannot carry " ++ formatName fmt ++ " audio; PipeWire converts whatever the device"
                ++ " does to the format a stream asks for, so ask for s16 (or u8, s8, s32, f32)")
        exitFailure
    -- The quantum both pw-cat streams run on.  Overridable while the
    -- right figure is being found: MODEC_PW_LATENCY=50ms.
    pwLatency = maybe "100ms" id pwLatencyEnv
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


-- | The V.32 rate signal this modem sends: everything it offers by
-- default, or the single rate @--v32-rate@ pins it to.  Pinning is how
-- 7200, 12000 and 14400 are reached, since they are not in the default
-- offer, and how a rate can be held down to see what a line will carry.
-- | Nothing means take the rates from the modes, which is what --mode
-- v32 and --mode v32bis are for; --v32-rate overrides both.
v32Offered :: ModemOpts -> Maybe V32.RateSeq
v32Offered o = fmap V32.chosenRate (moV32Rates o)
