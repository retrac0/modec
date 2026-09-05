-- | Impairment sweep for the FSK modem: modulate a payload, push it
-- through a list of channel conditions, and report byte errors.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Options.Applicative
import Text.Printf (printf)

import Modec.Channel
import Modec.DSP
import Modec.FSK
import Modec.Metrics
import Modec.Standards
import Modec.Async
import Modec.V22

data Opts = Opts
  { oRate    :: Double
  , oBytes   :: Int
  , oChannel :: String
  , oGain    :: Double
  , oWindow  :: String
  }

optsP :: Parser Opts
optsP = Opts
  <$> option auto (long "rate" <> value 8000 <> showDefault)
  <*> option auto (long "bytes" <> value 400 <> showDefault <> help "payload length")
  <*> strOption (long "channel" <> value "answer" <> showDefault <> help "answer | originate | v21 | v22low | v22high")
  <*> option auto (long "timing-gain" <> value 0.5 <> showDefault)
  <*> strOption (long "window" <> value "rect" <> showDefault <> help "hann | rect")

payloadBytes :: Int -> Int -> [Word8]
payloadBytes salt n = [fromIntegral ((i * 7919 + salt * 104729 + 13) `mod` 256) | i <- [1 .. n]]

main :: IO ()
main = do
  o <- execParser (info (optsP <**> helper) (fullDesc <> progDesc "modec impairment sweep"))
  let fs = oRate o
      isV22 = oChannel o == "v22low" || oChannel o == "v22high"
      v22ch = if oChannel o == "v22low" then LowChannel else HighChannel
      v22other = if v22ch == LowChannel then HighChannel else LowChannel
      (spec, other) = case oChannel o of
        "originate" -> (bell103Originate, bell103Answer)
        "v21"       -> (v21Channel2, v21Channel1)
        _           -> (bell103Answer, bell103Originate)
      params = defaultDemodParams { dpTimingGain = oGain o, dpWindow = if oWindow o == "hann" then Hann else Rect }
      payload = payloadBytes 1 (oBytes o)
      framed bs = replicate 120 True ++ frameBits framing8N1 bs ++ replicate 120 True
      clean = if isV22 then v22Modulate fs v22ch 0.5 (framed payload)
                       else encodeBytes fs spec framing8N1 0.5 0.2 0.2 payload
      adjacent = if isV22 then v22Modulate fs v22other 0.5 (framed (payloadBytes 2 (oBytes o)))
                          else encodeBytes fs other framing8N1 0.5 0.05 0.2 (payloadBytes 2 (oBytes o))
      decode sig = if isV22
                     then let bits = v22Demodulate fs v22ch sig
                              -- skip the start-up bits before the descrambler has synchronised
                              (_, bytes) = asyncRxBits (asyncRxInit framing8N1) (drop 60 bits)
                          in bytes
                     else demodulate fs spec framing8N1 params sig
      name = if isV22 then "V.22 " ++ show v22ch else fskName spec
      bp = Just (300, 3400)
      base = idealChannel { chBandpass = bp }
      conditions :: [(String, Signal -> Signal)]
      conditions =
        [ ("clean", id)
        , ("bandpass 300-3400", applyChannel fs base)
        ] ++
        [ (printf "bandpass, SNR %2.0f dB" s, applyChannel fs base { chSnrDb = Just s }) | s <- [30, 20, 15, 12, 10, 8, 6] ] ++
        [ (printf "rate offset %+.1f %%" (r * 100), applyChannel fs base { chRateOffset = r }) | r <- [0.01, -0.01, 0.03, -0.03, 0.05] ] ++
        [ (printf "freq offset %+.0f Hz" f, applyChannel fs base { chFreqOffsetHz = f }) | f <- [7, -7, 30] ] ++
        [ ("sine jitter 3 smp @ 2 Hz", applyChannel fs base { chJitter = SineJitter 3 2 })
        , ("walk jitter 0.05/smp max 10", applyChannel fs base { chJitter = WalkJitter 0.05 10 })
        , ("slips 4 smp every 0.3 s", applyChannel fs base { chJitter = Slips 0.3 4 })
        , ("dropouts 20 ms p=0.01", applyChannel fs base { chDropout = Just (0.02, 0.01) })
        , ("dropouts 20 ms p=0.05", applyChannel fs base { chDropout = Just (0.02, 0.05) })
        , ("echo 5 ms -12 dB", applyChannel fs base { chEcho = Just (0.005, fromDb (-12)) })
        , ("delay distortion 1 ms at band edges", applyChannel fs base { chDelayDist = 1 })
        , ("delay distortion 3 ms at band edges", applyChannel fs base { chDelayDist = 3 })
        , ("adjacent channel +10 dB", applyChannel fs base . mixAt 10 adjacent)
        , ("adjacent channel +20 dB", applyChannel fs base . mixAt 20 adjacent)
        , ("adjacent channel +30 dB", applyChannel fs base . mixAt 30 adjacent)
        , ("adjacent +10 dB, clip 0.3", applyChannel fs base { chClip = Just 0.3 } . mixAt 10 adjacent)
        , ("level -40 dBFS", applyChannel fs base { chGain = fromDb (-40) / 0.5 })
        , ("level -50 dBFS", applyChannel fs base { chGain = fromDb (-50) / 0.5 })
        , ("hum 60 Hz amp 0.3", applyChannel fs base { chHum = Just (60, 0.3) })
        , ("dc offset 0.3", applyChannel fs base { chDcOffset = 0.3 })
        , ("realistic acoustic: adj +20, SNR 25, rate 0.5 %, 3 Hz, jitter"
          , applyChannel fs base { chSnrDb = Just 25, chRateOffset = 0.005, chFreqOffsetHz = 3, chJitter = SineJitter 2 1 } . mixAt 20 adjacent)
        , ("VoIP: SNR 35, slips, dropouts p=0.005, rate 0.1 %"
          , applyChannel fs base { chSnrDb = Just 35, chJitter = Slips 0.5 8, chDropout = Just (0.02, 0.005), chRateOffset = 0.001 })
        ]
  printf "%s at %.0f Hz, %d bytes, timing gain %.2f, window %s\n" name fs (oBytes o) (oGain o) (oWindow o)
  forM_ conditions $ \(cname, f) -> do
    let sig = f clean
        got = decode sig
        d = editDistance payload got
    printf "  %-62s %5d errors  (%5.1f %%)  rms %.3f\n" cname d (100 * fromIntegral d / fromIntegral (oBytes o) :: Double) (rms sig)
    VS.length sig `seq` return ()
