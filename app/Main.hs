module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Options.Applicative
import System.IO
import Text.Printf (printf)

import Modec.DSP
import Modec.FSK
import Modec.Standards
import Modec.Wav

data Channel = Originate | Answer | Auto deriving (Eq, Show)
data Std = Bell103 | V21 deriving (Eq, Show)

data Cmd
  = Decode Std Channel Double FilePath
  | Encode Std Channel Int Double FilePath
  | Probe FilePath

channelP :: Parser Channel
channelP =
      flag' Originate (long "originate" <> help "low band (calling modem transmits)")
  <|> flag' Answer (long "answer" <> help "high band (answering modem transmits)")
  <|> pure Auto

stdP :: Parser Std
stdP = flag Bell103 V21 (long "v21" <> help "use V.21 tones instead of Bell 103")

cmdP :: Parser Cmd
cmdP = hsubparser
  (  command "decode" (info decodeP (progDesc "Demodulate a WAV file to bytes on stdout"))
  <> command "encode" (info encodeP (progDesc "Modulate stdin bytes to a WAV file"))
  <> command "probe"  (info probeP  (progDesc "Report tone energies in a WAV file"))
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

specFor :: Std -> Channel -> FskSpec
specFor Bell103 Originate = bell103Originate
specFor Bell103 _        = bell103Answer
specFor V21 Originate    = v21Channel1
specFor V21 _            = v21Channel2

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
      B.hPut stdout (B.pack (demodulate fs spec framing8N1 (DemodParams squelch) x))
    Encode std ch rate amp out -> do
      hSetBinaryMode stdin True
      bytes <- B.getContents
      let spec = specFor std (if ch == Auto then Originate else ch)
          fs = fromIntegral rate
      writeWav16Mono out rate (encodeBytes fs spec framing8N1 amp 0.5 0.2 (B.unpack bytes))
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
