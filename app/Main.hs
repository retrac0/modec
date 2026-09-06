module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Options.Applicative
import System.IO
import Text.Printf (printf)

import Dial
import Modec.Detect
import Modec.Pipewire (describeNodes, pwAudioNodes)
import Modec.DSP
import qualified Modec.Handshake as H
import Modem
import Modec.FSK
import Modec.Standards
import Modec.Wav

data Channel = Originate | Answer | Auto deriving (Eq, Show)
data Std = Bell103 | V21 | V23 deriving (Eq, Show)

data Cmd
  = Decode Std Channel Double FilePath
  | Encode Std Channel Int Double FilePath
  | Probe FilePath
  | Detect FilePath
  | RunModem ModemOpts
  | DialOut DialOpts
  | ListDevices

channelP :: Parser Channel
channelP =
      flag' Originate (long "originate" <> help "low band (calling modem transmits)")
  <|> flag' Answer (long "answer" <> help "high band (answering modem transmits)")
  <|> pure Auto

stdP :: Parser Std
stdP =
      flag' V21 (long "v21" <> help "use V.21 tones instead of Bell 103")
  <|> flag' V23 (long "v23" <> help "V.23 duplex: --answer is the 1200 bit/s forward channel, --originate the 75 bit/s backward one")
  <|> pure Bell103

cmdP :: Parser Cmd
cmdP = hsubparser
  (  command "decode" (info decodeP (progDesc "Demodulate a WAV file to bytes on stdout"))
  <> command "encode" (info encodeP (progDesc "Modulate stdin bytes to a WAV file"))
  <> command "probe"  (info probeP  (progDesc "Report tone energies in a WAV file"))
  <> command "detect" (info detectP (progDesc "Identify the FSK standard/channel and tone sequence in a WAV file"))
  <> command "dial"   (info dialP   (progDesc "Dial a number over SIP and hand the call to this terminal"))
  <> command "modem"  (info modemP  (progDesc "Run a live modem: audio via PipeWire or raw pipes, data via telnet"))
  <> command "devices" (info (pure ListDevices) (progDesc "List the PipeWire audio devices usable with --pw-in / --pw-out"))
  )
  where
    decodeP = Decode <$> stdP <*> channelP
      <*> option auto (long "squelch" <> value 0.01 <> showDefault <> help "min tone amplitude, full scale = 1")
      <*> argument str (metavar "FILE.wav")
    encodeP = Encode <$> stdP <*> channelP
      <*> option auto (long "rate" <> value 8000 <> showDefault <> help "sample rate")
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "amplitude")
      <*> strOption (short 'o' <> long "output" <> metavar "FILE.wav")
    probeP = Probe <$> argument str (metavar "FILE.wav")
    detectP = Detect <$> argument str (metavar "FILE.wav")
    modemP = RunModem <$> (ModemOpts
      <$> option auto (long "rate" <> value 8000 <> showDefault <> help "sample rate")
      <*> option auto (long "block-ms" <> value 20 <> showDefault <> help "audio block length")
      <*> (flag' H.Answer (long "answer" <> help "answering side") <|> flag H.Originate H.Originate (long "originate" <> help "calling side (default)"))
      <*> (option (maybeReader modesReader)
             (long "mode" <> metavar "LIST"
              <> help "comma-separated modes to negotiate, best first: bell103,v21,bell212a,v22,v22bis (default: all)")
                <|> pure H.allStandards)
      <*> switch (long "no-handshake" <> help "go straight to data mode with the given standard")
      <*> switch (long "no-v8bis" <> help "skip the V.8bis capabilities exchange")
      <*> switch (long "v8" <> help "V.8: answer with ANSam and exchange CM/JM capability menus")
      <*> switch (long "v8-offer-all" <> help "implies --v8; advertise every V.8 modulation so the far end's menu comes back in full. A survey option: the mode it then selects will not be one this modem can run")
      <*> option auto (long "max-evm" <> value 1.0 <> showDefault <> metavar "E"
             <> help "stop passing bytes to the DTE when the receiver's decision error exceeds this; a good link sits near 0.01 and 20 dB SNR near 0.35, while a converging or collapsing carrier runs past 1. Raise it to pass noisy data through, lower it to pass only what is trustworthy")
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
      <*> dataP
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "transmit amplitude")
      <*> optional (strOption (long "record-rx" <> metavar "FILE.wav" <> help "also record the whole session's received audio to one WAV, start to finish"))
      <*> optional (strOption (long "record-tx" <> metavar "FILE.wav" <> help "as --record-rx, for transmitted audio"))
      <*> recordDirP
      <*> pure Nothing
      <*> pure False)
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
    -- The modem's own settings, shared with `modec modem`: how to
    -- negotiate, whether to error-correct, what to record.  Everything
    -- about how the call is carried is decided by `dial` itself.
    modemP' = mkDialModem
      <$> modesP
      <*> switch (long "v8" <> help "V.8: exchange CM/JM capability menus before the modem start-up")
      <*> switch (long "v8-offer-all" <> help "implies --v8; advertise every V.8 modulation so the far end's menu comes back in full")
      <*> switch (long "no-v8bis" <> help "skip the V.8bis capabilities exchange")
      <*> dialMnpP
      <*> option auto (long "max-evm" <> value 1.0 <> showDefault <> metavar "E"
                       <> help "stop passing bytes to the DTE when the receiver's decision error exceeds this")
      <*> option auto (long "amp" <> value 0.5 <> showDefault <> help "transmit amplitude")
      <*> recordDirP
    mkDialModem modes v8 v8all noV8bis mnp evm amp rdir = defaultModemOpts
      { moModes = modes, moV8 = v8, moV8All = v8all, moNoV8bis = noV8bis
      , moMnp = mnp, moMaxEvm = evm, moAmp = amp, moRecordDir = rdir }
    modesP =
          option (maybeReader modesReader)
            (long "mode" <> metavar "LIST"
             <> help "comma-separated modes to negotiate, best first: bell103,v21,v23,bell212a,v22,v22bis (default: all)")
      <|> pure H.allStandards
    mnpP =
          flag' (Just 4) (long "mnp" <> help "MNP error correction (ITU-T V.42 Annex A), classes 2 to 4")
      <|> option (fmap Just auto) (long "mnp-class" <> metavar "N" <> help "as --mnp, but offering only up to class N")
      <|> pure Nothing
    -- Dialling asks for error correction the way a modem with its
    -- factory settings does.  An unprotected call over a VoIP trunk
    -- delivers the odd corrupt character in the direction it transmits,
    -- and there is nothing downstream that can tell a corrupt character
    -- from one that was typed; a call that negotiates MNP does not.
    dialMnpP =
          flag' Nothing (long "no-mnp" <> help "no error correction: hand over whatever arrives, errors and all")
      <|> option (fmap Just auto) (long "mnp-class" <> metavar "N" <> help "offer only up to class N: 2 start-stop framing, 3 synchronous framing, 4 adds adaptive frame sizing")
      <|> pure (Just 4)
    modesReader s = case s of
      "auto" -> Just H.allStandards
      "all" -> Just H.allStandards
      _ -> mapM modeReader (splitOn ',' s)
    modeReader m = case m of
      "bell103" -> Just H.Bell103
      "v21" -> Just H.V21
      "v23" -> Just H.V23
      "bell212a" -> Just H.Bell212A
      "bell212" -> Just H.Bell212A
      "v22" -> Just H.V22
      "v22bis" -> Just H.V22bis
      _ -> Nothing
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest
    audioP =
          flag' () (long "audio-pipewire" <> help "capture and play through pw-cat") *> pipewireP
      <|> AudioFiles <$> strOption (long "audio-in" <> metavar "RAW") <*> strOption (long "audio-out" <> metavar "RAW")
      <|> AudioSipLoop <$> strOption (long "audio-sip-loop" <> metavar "PREFIX" <> value "modec" <> help "PipeWire loopback pair for a softphone (nodes PREFIX-to-sip / PREFIX-line and sip-to-PREFIX / PREFIX-sip-line)")
      <|> flag' AudioStdio (long "audio-stdio" <> help "raw s16le mono audio on stdin/stdout")
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

specFor :: Std -> Channel -> FskSpec
specFor Bell103 Originate = bell103Originate
specFor Bell103 _        = bell103Answer
specFor V21 Originate    = v21Channel1
specFor V21 _            = v21Channel2
specFor V23 Originate    = v23Backward
specFor V23 _            = v23Forward

-- | Mean tone energy for a spec over the whole file, used to pick a channel.
bandEnergy :: Double -> FskSpec -> Signal -> Double
bandEnergy fs spec x =
  let (em, es) = discriminate fs spec x
  in VS.sum em + VS.sum es

main :: IO ()
main = do
  cmd <- execParser (info (cmdP <**> helper) (fullDesc <> progDesc "modec: software audio modem"))
  case cmd of
    Decode std ch squelch path -> do
      w <- readWav path
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
          spec = case ch of
            Auto ->
              let o = specFor std Originate
                  a = specFor std Answer
              in if bandEnergy fs o x >= bandEnergy fs a x then o else a
            _ -> specFor std ch
      when (ch == Auto) $ hPutStrLn stderr ("auto-selected " ++ fskName spec)
      hSetBinaryMode stdout True
      B.hPut stdout (B.pack (demodulate fs spec framing8N1 defaultDemodParams { dpSquelch = squelch } x))
    Encode std ch rate amp out -> do
      hSetBinaryMode stdin True
      bytes <- B.getContents
      let spec = specFor std (if ch == Auto then Originate else ch)
          fs = fromIntegral rate
      writeWav16Mono out rate (encodeBytes fs spec framing8N1 amp 0.5 0.2 (B.unpack bytes))
    ListDevices -> do
      ns <- pwAudioNodes
      if null ns
        then putStrLn "no PipeWire audio devices found (is pipewire running, and is pw-dump installed?)"
        else putStr (describeNodes ns)
    RunModem mo -> runModem mo
    DialOut d -> runDial d
    Detect path -> do
      w <- readWav path
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
      putStrLn "FSK channel scores (fraction of frames dominated by the channel's tones):"
      forM_ (detectFsk fs x) $ \(s, sc) -> printf "  %-18s %.3f\n" (fskName s) sc
      putStrLn "Tone runs longer than 100 ms:"
      forM_ [ r | r <- toneRunsWith diagnosticToneBank fs x, trEnd r - trStart r >= 0.1 ] $ \r ->
        printf "  %7.3f - %7.3f s  %s\n" (trStart r) (trEnd r) (maybe "silence / no dominant tone" (\f -> printf "%.0f Hz" f) (trTone r) :: String)
    Probe path -> do
      w <- readWav path
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
          n = VS.length x
          len = round (fs * 0.02) :: Int
          tones = [ ("bell103 orig space", 1070), ("bell103 orig mark", 1270)
                  , ("bell103 ans space", 2025), ("bell103 ans mark", 2225)
                  , ("v21 ch1 mark", 980), ("v21 ch1 space", 1180)
                  , ("v21 ch2 mark", 1650), ("v21 ch2 space", 1850)
                  , ("v25 answer tone", 2100), ("v22 low carrier", 1200), ("v22 high carrier", 2400) ]
      printf "%s: %d Hz, %d channels, %.2f s, rms %.4f\n" path (wavRate w) (wavChannels w)
        (fromIntegral n / fs :: Double) (rms x)
      forM_ tones $ \(name, f) -> do
        let e = toneEnergy fs f len x
            amps = VS.map (toneAmplitude len) e
            peak = VS.maximum amps
            mean = VS.sum amps / fromIntegral (max 1 n)
        printf "  %-20s %6.0f Hz  mean amp %.4f  peak amp %.4f\n" (name :: String) f mean peak
