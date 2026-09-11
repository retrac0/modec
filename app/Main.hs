module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Options.Applicative
import System.IO
import Text.Printf (printf, hPrintf)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))

import Dial
import FakeDongle
import qualified Modec.Channel as Ch
import Modec.Link
import Modec.Echo (EchoConfig (..))
import Modec.Modem
import Modec.Replay
import Modec.Fixture
import Modec.Detect
import Modec.V32Start (v32Timeline)
import Modec.Dtmf
import Modec.Progress
import Modec.Pipewire (describeNodes)
import PipewireIO (pwAudioNodes)
import Modec.DSP
import Modem
import Modec.FSK
import Modec.Standards
import Modec.Baudot
import Modec.Stream
import Modec.Sample
import Modec.Wav

-- | Which band the offline @decode@/@encode@ tools work in.  This is not
-- 'Role': it has an 'BandAuto' that picks by measuring the file, and the
-- text-telephone tone pairs it selects among are shared by both
-- directions, so there is no calling end to be.
data Band = BandLow | BandHigh | BandAuto deriving (Eq, Show)

-- | Which tone pair those tools use.  This is not 'Standard' either: the
-- 5-bit text telephone modes are not modes the modem can negotiate, and
-- nothing here is a mode -- it is a pair of frequencies and a baud rate.
data Tones = TBell103 | TV21 | TV23 | TTty45 | TTty50 deriving (Eq, Show)

data Cmd
  = Decode Tones Band Double Input
  | Encode Tones Band Int Double SampleFormat FilePath
  | Probe Input
  | Detect Input
  | V32Trace Bool Input      -- ^ True = we were the answering modem
  | Replay ReplayOpts
  | ProgressOf Input
  | DtmfOf Input
  | RunModem ModemOpts
  | DialOut DialOpts
  | AnswerIn AnswerOpts
  | ListDevices
  | FakeDongleCmd FakeOpts

-- | Reading a recording back through the whole modem, and optionally
-- minting a test fixture out of what came back.
data ReplayOpts = ReplayOpts
  { roModes   :: [Standard]
  , roAnswer  :: Bool
  , roV8      :: Bool
  , roMnp     :: Maybe Int
  , roMaxEvm  :: Double
  , roMaxEvmV32 :: Double
  , roSeconds :: Maybe Double
  , roLine    :: Bool
  , roImpair  :: [String]
  , roChannel :: Maybe String
  , roMint    :: Maybe String
  , roDir     :: FilePath
  , roInput   :: Input
  }

-- | A recording for the offline tools: a WAV, or a headerless file in
-- a named format, and optionally brought to another sample rate before
-- anything looks at it.
data Input = Input
  { inPath    :: FilePath
  , inRate    :: Maybe Int          -- ^ resample to this; the file's own rate otherwise
  , inRaw     :: Maybe SampleFormat -- ^ no header, mono samples in this format
  , inRawRate :: Int                -- ^ and at this rate
  }

inputP :: Parser Input
inputP = Input
  <$> argument str (metavar "FILE")
  <*> optional (option auto (long "rate" <> metavar "HZ"
        <> help "resample the recording to this rate before the modem sees it (default: run at the file's own rate)"))
  <*> optional (option (maybeReader formatNamed) (long "raw" <> metavar "FMT"
        <> help ("the file has no header and holds mono samples in this format: " ++ unwords formatNames)))
  <*> option auto (long "raw-rate" <> value 8000 <> showDefault <> metavar "HZ" <> help "the sample rate of a --raw file")

-- | The samples, their rate, and a description for the log.
readInput :: Input -> IO (Double, Signal, String)
readInput inp = do
  (fs0, x0, what) <- case inRaw inp of
    Just fmt -> do
      bs <- B.readFile (inPath inp)
      return (fromIntegral (inRawRate inp), decodeSamples fmt bs, "raw " ++ describeFormat fmt)
    Nothing -> do
      w <- readWav (inPath inp)
      return (fromIntegral (wavRate w), wavSamples w, show (wavChannels w) ++ " channels, " ++ describeFormat (wavFormat w))
  return $ case inRate inp of
    Just r | fromIntegral r /= fs0 ->
      (fromIntegral r, resampleTo fs0 (fromIntegral r) x0, what ++ ", resampled from " ++ show (round fs0 :: Int) ++ " Hz")
    _ -> (fs0, x0, what)

channelP :: Parser Band
channelP =
      flag' BandLow (long "originate" <> help "low band (calling modem transmits)")
  <|> flag' BandHigh (long "answer" <> help "high band (answering modem transmits)")
  <|> pure BandAuto

stdP :: Parser Tones
stdP =
      flag' TV21 (long "v21" <> help "use V.21 tones instead of Bell 103")
  <|> flag' TV23 (long "v23" <> help "V.23 duplex: --answer is the 1200 bit/s forward channel, --originate the 75 bit/s backward one")
  <|> flag' TTty45 (long "tty45" <> help "5-bit text telephone (TTY/TDD), 1400/1800 Hz at 45.45 baud: Baudot text, not bytes. --originate and --answer do not apply, the two directions share one tone pair")
  <|> flag' TTty50 (long "tty50" <> help "the same at 50 baud, as sold outside North America")
  <|> pure TBell103

cmdP :: Parser Cmd
cmdP = hsubparser
  (  command "decode" (info decodeP (progDesc "Demodulate a WAV file to bytes on stdout"))
  <> command "encode" (info encodeP (progDesc "Modulate stdin bytes to a WAV file"))
  <> command "probe"  (info probeP  (progDesc "Report tone energies in a WAV file"))
  <> command "detect" (info detectP (progDesc "Identify the FSK standard/channel and tone sequence in a WAV file"))
  <> command "v32trace" (info v32traceP (progDesc "Read a V.32 start-up back out of a recording, as a timeline"))
  <> command "replay" (info replayP (progDesc "Run the whole modem over a recording: the call timeline on stderr, the bytes on stdout"))
  <> command "progress" (info progressP (progDesc "Report the call progress tones in a WAV file: dial tone, ringing, busy, congestion, special information tone"))
  <> command "dtmf"  (info dtmfP   (progDesc "Report the DTMF digits in a WAV file"))
  <> command "dial"   (info dialP   (progDesc "Dial a number over SIP and hand the call to this terminal"))
  <> command "answer" (info answerP (progDesc "Register over SIP and answer incoming calls, greeting the caller"))
  <> command "modem"  (info modemP  (progDesc "Run a live modem: audio via PipeWire or raw pipes, data via telnet"))
  <> command "devices" (info (pure ListDevices) (progDesc "List the PipeWire audio devices usable with --pw-in / --pw-out"))
  <> command "fake-dongle" (info fakeP (progDesc "Fake a voice-mode USB modem on a pseudo-terminal, with a modem behind it, for --audio-serial"))
  )
  where
    decodeP = Decode <$> stdP <*> channelP
      <*> option auto (long "squelch" <> value 0.01 <> showDefault <> help "min tone amplitude, full scale = 1")
      <*> inputP
    encodeP = Encode <$> stdP <*> channelP
      <*> option auto (long "rate" <> value 8000 <> showDefault <> help "sample rate")
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "amplitude")
      <*> option (maybeReader formatNamed) (long "format" <> value S16 <> metavar "FMT"
             <> help ("sample format, default s16: " ++ unwords formatNames))
      <*> strOption (short 'o' <> long "output" <> metavar "FILE.wav")
    probeP = Probe <$> inputP
    detectP = Detect <$> inputP
    v32traceP = V32Trace <$> switch (long "answer" <> help "we were the answering modem (default: calling)")
                         <*> inputP
    replayP = Replay <$> (ReplayOpts
      <$> modesP
      <*> switch (long "answer" <> help "we were the answering modem (default: calling)")
      <*> switch (long "v8" <> help "V.8 was in use on the call")
      <*> (flag' (Just 4) (long "mnp" <> help "MNP error correction was in use")
           <|> option (fmap Just auto) (long "mnp-class" <> metavar "N" <> help "as --mnp, offering only up to class N")
           <|> pure Nothing)
      <*> option auto (long "max-evm" <> value 1.0 <> showDefault <> metavar "E"
             <> help "stop passing bytes to the DTE above this decision error (V.22 family)")
      <*> option auto (long "max-evm-v32" <> value 0.5 <> showDefault <> metavar "E"
             <> help "the same gate for V.32, as a fraction of the constellation's own margin")
      <*> optional (option auto (long "seconds" <> metavar "S" <> help "stop after this much of the recording"))
      <*> switch (long "line" <> help "report the receiver's decision error and symbol timing twice a second")
      <*> many (strOption (long "impair" <> metavar "K=V"
             <> help "degrade the recording first, on top of --channel. Line: snr, freq, rate, gain, dc, band, clip, hum, seed. Analogue: softclip, harm2, harm3, sing, singgain, wobble, phasejit. Time: jitter, slips, wow, flutter. Digital span: ulaw, alaw, biterr, loss, burst, stuck. Transient: impulse, hits. Repeatable"))
      <*> optional (strOption (long "channel" <> metavar "NAME"
             <> help "start from a named channel rather than an ideal one: ideal, telephone, voip, long-loop, handset, tape, noisy"))
      <*> optional (strOption (long "mint" <> metavar "NAME"
             <> help "write NAME.wav (trimmed to --seconds), NAME.txt (this decode) and NAME.call into the fixture directory"))
      <*> strOption (long "fixture-dir" <> value "test/fixtures/live" <> showDefault <> metavar "DIR")
      <*> inputP)
    progressP = ProgressOf <$> inputP
    dtmfP = DtmfOf <$> inputP
    fakeP = FakeDongleCmd <$> (FakeOpts
      <$> option (maybeReader formatNamed) (long "format" <> value Pcm14 <> metavar "FMT"
             <> help "the +VSM format the dongle streams in: pcm14, ulaw, alaw, u8 or s8 (default pcm14)")
      <*> flag Originate Answer (long "far-answers"
             <> help "the modem behind the line answers rather than calls, so nothing rings")
      <*> (modesP <|> pure allStandards)
      <*> strOption (long "say" <> value "" <> metavar "TEXT" <> help "what the far end sends once connected")
      <*> optional (strOption (long "heard" <> metavar "FILE" <> help "write what the far end received here (default: stderr)"))
      <*> optional (strOption (long "record" <> metavar "FILE.wav" <> help "record what arrived over the port, as 16-bit PCM"))
      <*> optional (strOption (long "play" <> metavar "FILE.wav" <> help "stream this recording instead of a modem's audio"))
      <*> option auto (long "seconds" <> value 30 <> showDefault <> help "end the stream after this much audio"))
    modemP = RunModem <$> (ModemOpts
      <$> option auto (long "rate" <> value 8000 <> showDefault <> help "sample rate")
      <*> option auto (long "block-ms" <> value 20 <> showDefault <> help "audio block length")
      <*> (flag' Answer (long "answer" <> help "answering side") <|> flag Originate Originate (long "originate" <> help "calling side (default)"))
      <*> (option (maybeReader modesReader)
             (long "mode" <> metavar "LIST"
              <> help "comma-separated modes to negotiate, best first: bell103,v21,v23,bell212a,v22,v22bis,v32,v32bis (default: all but v32 and v32bis)")
                <|> pure allStandards)
      <*> switch (long "no-handshake" <> help "go straight to data mode with the given standard")
      <*> switch (long "v8" <> help "V.8: answer with ANSam and exchange CM/JM capability menus")
      <*> switch (long "probe" <> help "do not place a call: hold a V.32 carrier and measure the echo path of whatever is on the line")
      <*> switch (long "v8-offer-all" <> help "implies --v8; advertise every V.8 modulation so the far end's menu comes back in full. A survey option: the mode it then selects will not be one this modem can run")
      <*> option auto (long "max-evm" <> value 1.0 <> showDefault <> metavar "E"
             <> help "stop passing bytes to the DTE when the receiver's decision error exceeds this; a good link sits near 0.01 and 20 dB SNR near 0.35, while a converging or collapsing carrier runs past 1. Raise it to pass noisy data through, lower it to pass only what is trustworthy")
      <*> optional (option (maybeReader v32RateReader) (long "v32-rate" <> metavar "BPS"
            <> help "hold V.32 to one rate: 4800, 7200, 9600, 9600t, 12000 or 14400. 7200, 12000 and 14400 are not in the default offer"))
      <*> (flag' (Just 4) (long "mnp" <> help "MNP error correction (ITU-T V.42 Annex A): frames the data, checks it, and asks again for whatever the line damaged. Classes 2 to 4; falls through to an unprotected connection if the far end does not answer")
           <|> option (fmap Just auto) (long "mnp-class" <> metavar "N" <> help "as --mnp, but offering only up to class N: 2 start-stop framing, 3 synchronous framing, 4 adds the data phase optimization and adaptive frame sizing")
           <|> pure Nothing)
      <*> option auto (long "mnp-round-trip" <> value 0.5 <> showDefault <> metavar "S"
                       <> help "seconds of round trip the MNP retransmission timer allows for. Raise it on a path that buffers: a satellite hop, or a loopback through FIFOs, where a reply can take longer than the timer and every frame looks lost")
      <*> option auto (long "mnp-probes" <> value 6 <> showDefault <> metavar "N"
                       <> help "how many MNP link requests to send before deciding the far end has no error correction")
      <*> option auto (long "mnp-probe-interval" <> value 2.5 <> showDefault <> metavar "S"
                       <> help "seconds between those link requests. A far end running V.42's detection phase abandons it 750 ms into the data phase, so probing early matters as much as probing often")
      <*> switch (long "hayes" <> help "Hayes AT command mode on the data side (ATD, ATA, ATH, +++)")
      <*> optional (strOption (long "sip" <> metavar "HOST:PORT"
                               <> help "drive baresip over its ctrl_tcp module (implies --hayes): ATD dials a SIP call, ATA answers, RING on incoming. `modec dial` sets this up for you"))
      <*> strOption (long "sip-domain" <> value "" <> metavar "DOMAIN" <> help "domain appended to dialled numbers (sip:NUMBER@DOMAIN)")
      <*> audioP
      <*> optional (option (maybeReader formatNamed) (long "audio-format" <> metavar "FMT"
             <> help ("what the audio device or pipe carries: " ++ unwords formatNames
                      ++ ". Default s16. PipeWire converts, so pw-cat takes only the linear ones")))
      <*> dataP
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "transmit amplitude")
      <*> optional (strOption (long "record-rx" <> metavar "FILE.wav" <> help "also record the whole session's received audio to one WAV, start to finish"))
      <*> optional (strOption (long "record-tx" <> metavar "FILE.wav" <> help "as --record-rx, for transmitted audio"))
      <*> recordDirP
      <*> pure Nothing      -- moAutoType: `modem` types nothing on your behalf
      <*> switch (long "banner"
                  <> help "on connecting, send the far end a line or two about what was negotiated. `modec answer` sets this")
      <*> pure False        -- moHangupExits
      <*> ignoreBusyP
      <*> switch (long "ans-plain" <> help "answer tone without V.25 phase reversals, so an echo canceller in the path (an ATA's) stays on instead of standing down for us; for paths whose reflection returns too late for modec's own canceller")
      <*> option auto (long "line-every" <> value 250 <> showDefault <> metavar "BLOCKS" <> help "blocks between 'line:' reports of the V.32 receiver's decision error and the canceller's return loss (250 is five seconds at 20 ms blocks); 12 shows what happens in the second after CONNECT")
      <*> option auto (long "max-evm-v32" <> value 0.5 <> showDefault <> metavar "E" <> help "V.32: pass bytes, and do not retrain, while the receiver's decision error is under this fraction of the rate's decision margin. Raising it does not make a link readable: at 14400 an unlocked receiver reports the same error as a locked one, because the grid is that dense")
      <*> option (maybeReader snrSchedule) (long "line-snr" <> value [] <> metavar "SPEC"
             <> help "put noise on the line, in dB of signal to noise: a number for the whole call, or a schedule like 40@0,24@20,16@35 -- 40 dB from the call coming up, 24 dB from twenty seconds in, 16 dB from thirty-five. The same measure Modec.Channel and modec-bench use, so the numbers compare. This is how to make a call step its rate down on purpose")
      <*> option (maybeReader lineDir) (long "line-snr-dir" <> value (True, True) <> metavar "DIR"
             <> help "which way the noise goes: rx (only what this modem hears, so this end retrains), tx (only what the far end hears, so it does), or both (default)")
      <*> option auto (long "line-seed" <> value 1 <> showDefault <> metavar "N"
             <> help "which noise realisation the line uses")
      <*> many (strOption (long "impair" <> metavar "K=V"
             <> help "impair the line as `modec replay --impair` does, live: the same keys (freq, rate, gain, dc, band, clip, hum, echo, softclip, harm2, harm3, sing, wobble, phasejit, jitter, slips, wow, flutter, ulaw, alaw, biterr, loss, burst, stuck, impulse, hits, dropout), run a block at a time with their state carried across blocks. Repeatable. Filters arrive late by half their length, which a modem cannot see"))
      <*> optional (strOption (long "channel" <> metavar "NAME"
             <> help "start from a named line profile (voip, longloop, carbon, tape, switched) and apply --impair on top"))
      <*> option auto (long "echo-data-mu" <> value 0.001 <> showDefault <> metavar "MU"
             <> help "V.32: the echo canceller's step while the far end is talking, once its data-mode search has aimed it and set its taps. The taps come from the search; this only holds them. The far end's signal is noise to the update, so keep it small; 0 leaves the taps to the search alone")
      <*> flag True False (long "no-echo-data"
             <> help "V.32: do not search for or cancel our own echo once the far end is talking. What the canceller did before it could, and the thing to compare against")
      <*> switch (long "aid-b1"
             <> help "V.32: train the receiver on the far end's B1, whose 128 symbols are predictable from the scrambler, instead of reading them decision-directed. It predicts them exactly and is refused unless it can corroborate that against the B1 already received -- but it does not reliably lower the error rate, so it is off; see docs/reference-modem.md"))
    -- A modem hangs up when the network answers a call with a busy
    -- tone, congestion or the special information tone that precedes a
    -- recorded announcement, and says BUSY.  This is how to sit and
    -- listen to one instead.
    -- "24" is 24 dB for the whole call; "40@0,24@20" is 40 dB until
    -- twenty seconds after the call came up and 24 dB after that.
    snrSchedule s = case s of
      "" -> Nothing
      _ -> mapM one (splitOn ',' s)
      where
        one w = case break (== '@') w of
          (d, "") -> (,) 0 <$> readMaybe d
          (d, _ : t) -> (,) <$> readMaybe t <*> readMaybe d
        readMaybe t = case reads t of { [(v, "")] -> Just (v :: Double); _ -> Nothing }
    lineDir s = case s of
      "rx" -> Just (True, False)
      "tx" -> Just (False, True)
      "both" -> Just (True, True)
      _ -> Nothing
    ignoreBusyP = switch (long "ignore-busy"
      <> help "stay on the line when the far end returns busy, congestion or a special information tone, instead of hanging up and reporting BUSY")
    -- Every call is recorded and logged under this directory, named for
    -- when it was placed and what it dialled; --no-record is the way to
    -- ask for a call that leaves nothing behind.
    recordDirP =
          flag' Nothing (long "no-record" <> help "do not record calls")
      <|> (Just <$> strOption (long "record-dir" <> metavar "DIR" <> value "recordings" <> showDefault
                               <> help "per-call recordings and logs go here, plus a calls.log index"))
    dialP = DialOut <$> (DialOpts
      <$> argument str (metavar "NUMBER" <> help "digits as you would dial them, or a full sip: URI")
      <*> strOption (long "sip" <> metavar "HOST:PORT" <> value "127.0.0.1:4444" <> showDefault
                     <> help "baresip's ctrl_tcp address")
      <*> optional (strOption (long "sip-domain" <> metavar "DOMAIN"
                               <> help "domain to dial into (default: the one in ~/.baresip/accounts)"))
      <*> strOption (long "audio-sip-loop" <> metavar "PREFIX" <> value "modec" <> showDefault
                     <> help "PipeWire loopback pair shared with the softphone")
      <*> optional (option auto (long "listen" <> metavar "PORT"
                                 <> help "put the modem on a telnet port instead of this terminal"))
      <*> flag True False (long "no-launch" <> help "expect baresip to be running already")
      <*> switch (long "stay" <> help "keep the AT prompt when the call ends, instead of exiting")
      <*> modemP')
    answerP = AnswerIn <$> (AnswerOpts
      <$> strOption (long "sip" <> metavar "HOST:PORT" <> value "127.0.0.1:4444" <> showDefault
                     <> help "baresip's ctrl_tcp address")
      <*> optional (strOption (long "sip-domain" <> metavar "DOMAIN"
                               <> help "domain we answer for (default: the one in ~/.baresip/accounts)"))
      <*> strOption (long "audio-sip-loop" <> metavar "PREFIX" <> value "modec" <> showDefault
                     <> help "PipeWire loopback pair shared with the softphone")
      <*> optional (option auto (long "listen" <> metavar "PORT"
                                 <> help "put the modem on a telnet port instead of this terminal"))
      <*> flag True False (long "no-launch" <> help "expect baresip to be running already")
      <*> modemP')
    -- The modem's own settings, shared with `modec modem`: how to
    -- negotiate, whether to error-correct, what to record.  Everything
    -- about how the call is carried is decided by `dial` itself.
    modemP' = mkDialModem
      <$> modesP
      <*> switch (long "v8" <> help "V.8: exchange CM/JM capability menus before the modem start-up")
      <*> switch (long "v8-offer-all" <> help "implies --v8; advertise every V.8 modulation so the far end's menu comes back in full")
      <*> dialMnpP
      <*> option auto (long "max-evm" <> value 1.0 <> showDefault <> metavar "E"
                       <> help "stop passing bytes to the DTE when the receiver's decision error exceeds this")
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "transmit amplitude")
      <*> recordDirP
      <*> ignoreBusyP
      <*> switch (long "probe"
                  <> help "do not place a call: hold a V.32 carrier and measure the echo path")
      <*> optional (option (maybeReader v32RateReader) (long "v32-rate" <> metavar "BPS"
            <> help "hold V.32 to one rate: 4800, 7200, 9600, 9600t, 12000 or 14400"))
    mkDialModem modes v8 v8all mnp evm amp rdir ignoreBusy probe v32rate = defaultModemOpts
      { moModes = modes, moV8 = v8, moV8All = v8all
      , moMnp = mnp, moMaxEvm = evm, moAmp = amp, moRecordDir = rdir
      , moIgnoreBusy = ignoreBusy, moProbe = probe, moV32Rates = v32rate }
    modesP =
          option (maybeReader modesReader)
            (long "mode" <> metavar "LIST"
             <> help "comma-separated modes to negotiate, best first: bell103,v21,v23,bell212a,v22,v22bis,v32,v32bis (default: all but v32 and v32bis)")
      <|> pure allStandards
    -- Dialling asks for error correction the way a modem with its
    -- factory settings does.  An unprotected call over a VoIP trunk
    -- delivers the odd corrupt character in the direction it transmits,
    -- and there is nothing downstream that can tell a corrupt character
    -- from one that was typed; a call that negotiates MNP does not.
    dialMnpP =
          flag' Nothing (long "no-mnp" <> help "no error correction: hand over whatever arrives, errors and all")
      <|> flag' (Just 4) (long "mnp" <> help "MNP error correction (ITU-T V.42 Annex A), classes 2 to 4. On by default here; the flag is for saying so")
      <|> option (fmap Just auto) (long "mnp-class" <> metavar "N" <> help "offer only up to class N: 2 start-stop framing, 3 synchronous framing, 4 adds adaptive frame sizing")
      <|> pure (Just 4)
    modesReader s = case s of
      "auto" -> Just allStandards
      "all" -> Just allStandards
      _ -> mapM modeReader (splitOn ',' s)
    v32RateReader m = case m of
      "4800" -> Just V32R4800
      "7200" -> Just V32R7200
      "9600" -> Just V32R9600
      "9600t" -> Just V32R9600T
      "12000" -> Just V32R12000
      "14400" -> Just V32R14400
      _ -> Nothing
    modeReader = standardNamed
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest
    audioP =
          flag' () (long "audio-pipewire" <> help "capture and play through pw-cat") *> pipewireP
      <|> AudioFiles <$> strOption (long "audio-in" <> metavar "RAW" <> help "headerless mono audio to read, in --audio-format (a file or a FIFO)")
                     <*> strOption (long "audio-out" <> metavar "RAW" <> help "and to write")
      <|> AudioSipLoop <$> strOption (long "audio-sip-loop" <> metavar "PREFIX" <> value "modec" <> help "PipeWire loopback pair for a softphone (nodes PREFIX-to-sip / PREFIX-line and sip-to-PREFIX / PREFIX-sip-line)")
      <|> flag' AudioStdio (long "audio-stdio" <> help "headerless mono audio on stdin/stdout, in --audio-format")
      <|> AudioSerial <$> strOption (long "audio-serial" <> metavar "DEV"
             <> help "a voice-mode USB modem (AT+FCLASS=8) on this serial port, e.g. /dev/ttyACM0: the line itself, at 8000 Hz. --audio-format defaults to ulaw here; pcm14 is finer but a CX93001 cannot carry it without losing samples")
    pipewireP = mkPw
      <$> optional (strOption (long "pw-in" <> metavar "DEV" <> help "capture device: node id, name, or part of either (see: modec devices)"))
      <*> optional (strOption (long "pw-out" <> metavar "DEV" <> help "playback device: node id, name, or part of either"))
      <*> optional (strOption (long "pw-target" <> metavar "DEV" <> help "shorthand setting both --pw-in and --pw-out"))
      <*> switch (long "pw-monitor" <> help "capture the output's monitor instead of an input (hear and receive your own tones)")
    mkPw i o both mon = AudioPipewire (maybe both Just i) (maybe both Just o) mon
    dataP =
          DataListen <$> option auto (long "listen" <> metavar "PORT" <> help "telnet server")
      <|> (DataConnect <$> strOption (long "connect" <> metavar "HOST") <*> option auto (long "port" <> metavar "PORT" <> value 23))
      <|> flag' DataStdio (long "data-stdio" <> help "raw bytes on stdin/stdout")

specFor :: Tones -> Band -> FskSpec
specFor TBell103 BandLow = bell103Originate
specFor TBell103 _       = bell103Answer
specFor TV21 BandLow     = v21Channel1
specFor TV21 _           = v21Channel2
specFor TV23 BandLow     = v23Backward
specFor TV23 _           = v23Forward
specFor TTty45 _         = tdd45
specFor TTty50 _         = tdd50

-- | The 5-bit text telephone modes, which are a different pipeline and
-- not merely different tones: a carrierless line, a 5-bit character at
-- 1.5 stop bits, and Baudot text rather than transparent bytes.
isTty :: Tones -> Bool
isTty s = s == TTty45 || s == TTty50


-- | Mean tone energy for a spec over the whole file, used to pick a channel.
bandEnergy :: Double -> FskSpec -> Signal -> Double
bandEnergy fs spec x =
  let (em, es) = discriminate fs spec x
  in VS.sum em + VS.sum es

main :: IO ()
main = do
  cmd <- execParser (info (cmdP <**> helper) (fullDesc <> progDesc "modec: software audio modem"))
  case cmd of
    Decode std ch squelch inp -> do
      (fs, x, _) <- readInput inp
      let spec = case ch of
            _ | isTty std -> specFor std BandLow     -- one pair, both directions
            BandAuto ->
              let o = specFor std BandLow
                  a = specFor std BandHigh
              in if bandEnergy fs o x >= bandEnergy fs a x then o else a
            _ -> specFor std ch
      when (ch == BandAuto && not (isTty std)) $ hPutStrLn stderr ("auto-selected " ++ fskName spec)
      hSetBinaryMode stdout True
      if isTty std
        then do
          let codes = concatStage (fskDiscriminator fs spec defaultDemodParams { dpSquelch = squelch }
                                   >>> fskBurstDeframer fs spec tddFraming
                                         defaultDemodParams { dpSquelch = squelch } defaultBurstParams)
                                  [x, flushSilence fs spec]
          B.hPut stdout (B.pack (snd (baudotDecode baudotRxInit codes)))
        else B.hPut stdout (B.pack (demodulate fs spec framing8N1 defaultDemodParams { dpSquelch = squelch } x))
    Encode std ch rate amp fmt out -> do
      hSetBinaryMode stdin True
      bytes <- B.getContents
      let spec = specFor std (if ch == BandAuto then BandLow else ch)
          fs = fromIntegral rate
          codes = snd (baudotEncode baudotTxInit (B.unpack bytes))
          burst = (Off, 0.2) : keyedBurst tddFraming (fskBaud spec) defaultBurst codes ++ [(Off, 0.3)]
      writeWavMono fmt out rate $ if isTty std
        then txFilter fs spec (VS.map (* amp) (modulateKeyed fs spec burst))
        else encodeBytes fs spec framing8N1 amp 0.5 0.2 (B.unpack bytes)
    FakeDongleCmd fo -> runFakeDongle fo
    ListDevices -> do
      ns <- pwAudioNodes
      if null ns
        then putStrLn "no PipeWire audio devices found (is pipewire running, and is pw-dump installed?)"
        else putStr (describeNodes ns)
    RunModem mo -> runModem mo
    DialOut d -> runDial d
    AnswerIn a -> runAnswer a
    V32Trace answered inp -> do
      (fs, x, _) <- readInput inp
      let dir = if answered then Answer else Originate
      forM_ (v32Timeline fs dir x) $ \(t, what) ->
        printf "%8.3f  %s\n" t what
    Replay ro -> runReplay ro
    Detect inp -> do
      (fs, x, _) <- readInput inp
      putStrLn "FSK channel scores (fraction of frames dominated by the channel's tones):"
      forM_ (detectFsk fs x) $ \(s, sc) -> printf "  %-18s %.3f\n" (fskName s) sc
      putStrLn "Tone runs longer than 100 ms:"
      forM_ [ r | r <- toneRunsWith diagnosticToneBank fs x, trEnd r - trStart r >= 0.1 ] $ \r ->
        printf "  %7.3f - %7.3f s  %s\n" (trStart r) (trEnd r) (maybe "silence / no dominant tone" (\f -> printf "%.0f Hz" f) (trTone r) :: String)
    ProgressOf inp -> do
      (fs, x, _) <- readInput inp
      let evs = callProgress fs x
      if null evs
        then putStrLn "no call progress tone recognised"
        else mapM_ (putStrLn . describeProgress) evs
    DtmfOf inp -> do
      (fs, x, _) <- readInput inp
      let ds = dtmfDecode fs defaultDtmfParams x
      if null ds
        then putStrLn "no DTMF digits"
        else do
          forM_ ds $ \d ->
            printf "%7.3f s  %c  %4.0f ms  amplitude %.3f\n" (ddStart d) (ddChar d) (ddDuration d * 1000) (ddLevel d)
          putStrLn ("dialled: " ++ map ddChar ds)
    Probe inp -> do
      (fs, x, what) <- readInput inp
      let n = VS.length x
          len = round (fs * 0.02) :: Int
          tones = [ ("bell103 orig space", 1070), ("bell103 orig mark", 1270)
                  , ("bell103 ans space", 2025), ("bell103 ans mark", 2225)
                  , ("v21 ch1 mark", 980), ("v21 ch1 space", 1180)
                  , ("v21 ch2 mark", 1650), ("v21 ch2 space", 1850)
                  , ("v23 back mark", 390), ("v23 back space", 450), ("v23 fwd mark", 1300)
                  , ("tty mark", 1400), ("tty space", 1800)
                  , ("v25 answer tone", 2100), ("v22 low carrier", 1200), ("v22 high carrier", 2400) ]
      printf "%s: %d Hz, %s, %.2f s, rms %.4f\n" (inPath inp) (round fs :: Int) what
        (fromIntegral n / fs :: Double) (rms x)
      forM_ tones $ \(name, f) -> do
        let e = toneEnergy fs f len x
            amps = VS.map (toneAmplitude len) e
            peak = VS.maximum amps
            mean = VS.sum amps / fromIntegral (max 1 n)
        printf "  %-20s %6.0f Hz  mean amp %.4f  peak amp %.4f\n" (name :: String) f mean peak

-- | Read a recording back through the whole modem.  The call timeline
-- goes to stderr and the bytes to stdout, so a replay reads like the
-- call log it reproduces; @--mint@ additionally writes the three files a
-- corpus fixture is made of.
runReplay :: ReplayOpts -> IO ()
runReplay ro = do
  (fs, samples, _) <- readInput (roInput ro)
  let
      -- The same assembly the corpus uses, so a replay from the command
      -- line and the test that replays the fixture it mints are the same
      -- modem.  Only the two decision-error gates are the command's own.
      cfg = (callSpecConfig fs (replaySpec ro))
              { mcMaxEvm = roMaxEvm ro, mcMaxEvmV32 = roMaxEvmV32 ro }
      trimmed = case roSeconds ro of
        Nothing -> samples
        Just s -> VS.take (round (s * fs)) samples
      x = Ch.applyChannel fs (impairments (roChannel ro) (roImpair ro)) trimmed
      rc = (defaultReplayConfig cfg)
             { rcEvery = if roLine ro then Just 0.5 else Nothing }
      r = replay rc x
  forM_ (rrPhases r) $ \(t, ph) -> hPrintf stderr "  %6.2f  %s\n" t ph
  forM_ (rrLine r) $ \(t, evm, sps) ->
    hPrintf stderr "  %6.2f  evm %7.4f  sps %8.5f\n" t evm sps
  forM_ (rrEcho r) $ \(t, lag, erle) ->
    hPrintf stderr "  %6.2f  echo %s  return loss %5.1f dB\n" t
      (maybe "unaimed" (\l -> "at " ++ show (round (fromIntegral l / (fs / 1000) :: Double) :: Int) ++ " ms") lag) erle
  forM_ (rrEvents r) $ \(t, e) -> hPrintf stderr "  %6.2f  %s\n" t (describeEvent e)
  hPrintf stderr "%d bytes\n" (length (rrBytes r))
  hSetBinaryMode stdout True
  B.hPut stdout (B.pack (rrBytes r))
  case roMint ro of
    Nothing -> return ()
    Just name -> mint ro r name (round fs) trimmed

describeEvent :: ModemEvent -> String
describeEvent e = case e of
  EvConnected s l -> "CONNECT " ++ show s ++ " " ++ show (round (linkBitRate l) :: Int) ++ " bit/s"
  EvDropped -> "NO CARRIER"
  EvFailed why -> "failed: " ++ why
  EvV8Menu _ -> "V.8 menu"
  EvMnp m -> "MNP " ++ show m
  -- A replayed V.32 recording can retrain, and this case falling through
  -- crashed the replay rather than printing a line about it.  Every
  -- renderer of this type must be total; there is more than one of them.
  EvRetrain _ -> "retraining"
  EvRate r -> "now " ++ show (rateBitRate r) ++ " bit/s"

replaySpec :: ReplayOpts -> CallSpec
-- | The fixture spec a replay is running under.  'mint' fills in what
-- the run turned out to do; this is what it was asked to do.
replaySpec ro = emptyCallSpec
  { csSeconds = roSeconds ro
  , csRole    = if roAnswer ro then Answer else Originate
  , csModes   = roModes ro
  , csV8      = roV8 ro
  , csMnp     = roMnp ro
  }

-- | Write the three files a corpus fixture is made of: the recording
-- trimmed to what the test needs, this decode as the reference, and a
-- spec saying how to replay it.  The spec's @expect:@ line is left for a
-- human, because what the far end really sent is not something a decode
-- can assert about itself.
mint :: ReplayOpts -> ReplayResult -> String -> Int -> Signal -> IO ()
mint ro r name rate trimmed = do
  createDirectoryIfMissing True dir
  writeWav16Mono (dir </> name ++ ".wav") rate trimmed
  B.writeFile (dir </> name ++ ".txt") (B.pack (rrBytes r))
  writeFile (dir </> name ++ ".call") (renderCallSpec spec)
  hPutStrLn stderr ("minted " ++ dir </> name ++ ".{wav,txt,call} -- now write its expect: line by hand")
  where
    dir = roDir ro
    conn = connectLine (rrEvents r)
    spec = (replaySpec ro)
      { csComment   = [takeFileName (inPath (roInput ro))]
      , csConnect   = conn
      , csRetrains  = [retrainCount (rrEvents r) | conn /= "none"]
      , csTolerance = 0
      }


