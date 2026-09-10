-- | Bell 103, V.21 and V.23: the frequency-shift modems.
module Suite.Fsk (roundTrip, Payload (..), ShortPayload (..), propertyTests, errorRateTests, channelTests, chunkTests, detectTests, resampleTests) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Modec.Channel
import Modec.Detect
import Modec.DSP
import Modec.Metrics
import Modec.Stream
import Modec.FSK
import Modec.Standards
import Modec.Wav
import Corpus

roundTrip :: Double -> FskSpec -> Double -> Int -> [Word8] -> Bool
roundTrip fs spec noise seed bytes =
  let sig = addNoise seed noise (encodeBytes fs spec framing8N1 0.5 0.1 0.1 bytes)
  in demodulate fs spec framing8N1 defaultDemodParams sig == bytes

newtype Payload = Payload [Word8] deriving Show

instance Arbitrary Payload where
  arbitrary = Payload <$> resize 40 (listOf arbitrary)
  shrink (Payload bs) = map Payload (shrink bs)

-- | A handful of bytes rather than forty, for the sample rates where the
-- front end costs O(fs^2) and every byte is paid for at that price.
newtype ShortPayload = ShortPayload [Word8] deriving Show

instance Arbitrary ShortPayload where
  arbitrary = ShortPayload <$> resize 8 (listOf arbitrary)
  shrink (ShortPayload bs) = map ShortPayload (shrink bs)

propertyTests :: TestTree
propertyTests = testGroup "self round trips"
  [ testProperty "bell103 answer 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 bell103Answer 0 1 bs
  , testProperty "bell103 originate 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 bell103Originate 0 2 bs
  , testProperty "v21 ch2 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 v21Channel2 0 3 bs
  -- The prefilter has 2*round(fs/25)+1 taps, so its length grows with the
  -- sample rate and so does the number of samples to push through it:
  -- the front end costs O(fs^2), and 48 kHz is 35 times the work of
  -- 8 kHz.  These two cases exist to show the demodulator is
  -- rate-independent, which a handful of short payloads settles; the
  -- payload space itself is covered at 8 kHz above, at a thirty-fifth of
  -- the price.  Left at the default this one test was 73% of the suite,
  -- and even capped it was the longest single case in a parallel run.
  , testProperty "bell103 answer 48 kHz clean" $ withMaxSuccess 8 $
      \(ShortPayload bs) -> roundTrip 48000 bell103Answer 0 4 bs
  , testProperty "bell103 answer 11025 Hz clean" $ withMaxSuccess 25 $
      \(ShortPayload bs) -> roundTrip 11025 bell103Answer 0 5 bs
  -- Amplitude 0.5 against sigma 0.2 of white noise at 8 kHz is about 5 dB
  -- SNR in the full band and an Eb/N0 of roughly 16 dB.  An ideal
  -- non-coherent FSK detector would have a bit error rate near 1e-9
  -- there; this receiver measures 2 to 3 dB worse than ideal, which is an
  -- ordinary implementation loss for one that also has to find the bit
  -- clock and track a threshold.  Measured over 12000 characters at this
  -- level it makes no errors, so the property holds -- but the margin is
  -- a few dB, not the nine orders of magnitude the ideal figure suggests,
  -- and tightening the noise a little will start to break it.
  , testProperty "bell103 answer 8 kHz noisy" $ \(Payload bs) (Positive seed) -> roundTrip 8000 bell103Answer 0.2 seed bs
  ]

-- | At Eb/N0 of about 11 dB (sigma 0.35 against amplitude 0.5 at 8 kHz) theory
-- gives a BER near 5e-4 for orthogonal non-coherent FSK.  Bell 103 tones are
-- only 200 Hz apart at 300 baud (not orthogonal), and measured BER of the
-- discriminator at ideal timing is about 2e-3, i.e. 2 % of bytes.  The
-- deframer's first-zero-crossing edge estimate adds timing jitter and
-- roughly doubles that (measured about 5 %).  Require under 8 %: this
-- catches a broken discriminator or deframer without being flaky, and
-- should be tightened when the deframer gets a better timing estimator.
errorRateTests :: TestTree
errorRateTests = testGroup "error rate under heavy noise"
  [ testCase "bell103 answer 8 kHz, sigma 0.35" $ do
      let payload = [ fromIntegral ((i * 7919 + 13) `mod` 256) | i <- [1 .. 2000 :: Int] ]
          sig = addNoise 42 0.35 (encodeBytes 8000 bell103Answer framing8N1 0.5 0.1 0.1 payload)
          got = demodulate 8000 bell103Answer framing8N1 defaultDemodParams sig
          d = editDistance payload got
      assertBool ("byte edit distance " ++ show d ++ " of " ++ show (length payload)) (d < 160)
  ]

-- | The streaming receiver must give the same bytes however the input is chunked.
chunkTests :: TestTree
chunkTests = testGroup "chunk invariance"
  [ testCase ("bell103_ans_8k_noisy.wav in chunks of " ++ show c) $ do
      w <- readWav (fixtureDir </> "bell103_ans_8k_noisy.wav")
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
          whole = demodulate fs bell103Answer framing8N1 defaultDemodParams x
          chunks = [ VS.slice i (min c (VS.length x - i)) x | i <- [0, c .. VS.length x - 1] ] ++ [flushSilence fs bell103Answer]
          streamed = concatStage (fskReceiver fs bell103Answer framing8N1 defaultDemodParams) chunks
      assertEqual "bytes" whole streamed
  | c <- [7, 160, 1000, 4096] ]

-- | The two fixtures recorded at sound-card rates decode the same
-- brought to 8 kHz as they do natively -- which is what makes --rate a
-- way to run any recording through the path everything else is
-- measured at.
resampleTests :: TestTree
resampleTests = testGroup "recordings at other rates, brought to 8 kHz"
  [ testCase name $ do
      w <- readWav (fixtureDir </> name)
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
          native = demodulate fs spec framing8N1 defaultDemodParams x
          at8k = demodulate 8000 spec framing8N1 defaultDemodParams (resampleTo fs 8000 x)
      assertBool "decodes at all" (not (null native))
      assertEqual "the same bytes" native at8k
  | (name, spec) <- [("bell103_ans_48k.wav", bell103Answer), ("bell103_orig_44k1_long.wav", bell103Originate)] ]

-- | Conditions the Bell 103 receiver must survive without a single error.
channelTests :: TestTree
channelTests = testGroup "channel impairments (must be error free)"
  [ cond "telephone band, SNR 20 dB" base { chSnrDb = Just 20 } id
  , cond "rate offset +2 %" base { chRateOffset = 0.02 } id
  , cond "rate offset -2 %" base { chRateOffset = -0.02 } id
  , cond "frequency offset +7 Hz" base { chFreqOffsetHz = 7 } id
  , cond "frequency offset -7 Hz" base { chFreqOffsetHz = -7 } id
  , cond "sine jitter 3 samples at 2 Hz" base { chJitter = SineJitter 3 2 } id
  , cond "adjacent channel +20 dB" base (mixAt 20 adjacent)
  , cond "level -40 dBFS" base { chGain = fromDb (-40) / 0.5 } id
  , cond "hum 60 Hz" base { chHum = Just (60, 0.3) } id
  , cond "delay distortion 3 ms at band edges" base { chDelayDist = 3 } id
  , cond "realistic acoustic coupling" base { chSnrDb = Just 25, chRateOffset = 0.005, chFreqOffsetHz = 3, chJitter = SineJitter 2 1 } (mixAt 20 adjacent)
  ]
  where
    fs = 8000
    base = idealChannel { chBandpass = Just (300, 3400) }
    payload = [ fromIntegral ((i * 7919 + 13) `mod` 256) | i <- [1 .. 300 :: Int] ]
    clean = encodeBytes fs bell103Answer framing8N1 0.5 0.2 0.2 payload
    adjacent = encodeBytes fs bell103Originate framing8N1 0.5 0.05 0.2 [ fromIntegral (i * 31) | i <- [1 .. 300 :: Int] ]
    cond name ch pre = testCase name $ do
      let got = demodulate fs bell103Answer framing8N1 defaultDemodParams (applyChannel fs ch (pre clean))
      assertEqual "decoded bytes" payload got

detectTests :: TestTree
detectTests = testGroup "detection" $
  [ testCase ("detectFsk " ++ fskName s) $ do
      let sig = encodeBytes 8000 s framing8N1 0.3 0.1 0.1 [ fromIntegral (i * 37) | i <- [1 .. 60 :: Int] ]
      case detectFsk 8000 (applyChannel 8000 (telephoneChannel 25) sig) of
        ((best, score) : _) -> do
          assertEqual "standard" (fskName s) (fskName best)
          assertBool ("score " ++ show score) (score > 0.4)
        [] -> assertFailure "no candidates"
  | s <- fskStandards ] ++
  [ testCase "toneRuns finds a 3 s answer tone" $ do
      let fs = 8000
          sig = VS.concat [ VS.replicate 8000 0
                          , VS.generate 24000 (\i -> 0.3 * sin (2 * pi * 2100 * fromIntegral i / fs))
                          , VS.replicate 600 0
                          , encodeBytes fs v21Channel2 framing8N1 0.3 0.5 0.1 [65, 66, 67] ]
          runs = toneRuns fs sig
          ans = [ r | r@(ToneRun (Just 2100) _ _) <- runs ]
      assertBool ("runs " ++ show (take 6 runs)) (case ans of
        (ToneRun _ t0 t1 : _) -> t0 > 0.9 && t0 < 1.1 && (t1 - t0) > 2.9 && (t1 - t0) < 3.1
        _ -> False)
  ]
