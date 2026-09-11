-- | The channel simulator a block at a time, against the whole-signal
-- one it is a rendering of.
--
-- Where a stage has no state -- gain, clipping, hum, a codec -- the
-- two must agree to the sample.  Where it has a filter, the streaming
-- one is late by the filter's group delay and must agree once that is
-- taken out.  Where it warps time with an interpolator, the two use
-- different interpolators and are asked to agree closely rather than
-- exactly.  And where a stage draws its own randomness, the block
-- version draws differently on purpose -- one seed per block, or every
-- block would carry the same noise -- so those are asked for the
-- statistic, not the sample.
module Suite.LiveChannel (liveChannelTests) where

import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit

import Modec.DSP
import Modec.Channel
import Modec.Channel.Live

fs :: Double
fs = 8000

-- | A second of three tones and a little noise: something with a
-- spectrum, that is not periodic in a block.
probe :: Signal
probe = VS.zipWith (+) tones (gaussianNoise 9 n 0.02)
  where
    n = 8000
    tones = VS.generate n $ \i ->
      let t = fromIntegral i / fs
      in 0.3 * sin (2 * pi * 700 * t) + 0.25 * sin (2 * pi * 1300 * t + 1) + 0.2 * sin (2 * pi * 2100 * t + 2)

-- | The block version, over 20 ms blocks.
stream :: Channel -> Signal -> Signal
stream ch x = VS.concat (go (liveInit fs ch) (chunks x))
  where
    chunks v | VS.null v = []
             | otherwise = let (a, b) = VS.splitAt 160 v in a : chunks b
    go _ [] = []
    go st (c : cs) = let (st', y) = liveStep fs ch st c in y : go st' cs

maxDiff :: Signal -> Signal -> Double
maxDiff a b = VS.maximum (VS.map abs (VS.zipWith (-) a b))

-- | Normalised correlation at zero lag.
corr :: Signal -> Signal -> Double
corr a b = VS.sum (VS.zipWith (*) a b) / sqrt (VS.sum (VS.map (^ (2 :: Int)) a) * VS.sum (VS.map (^ (2 :: Int)) b))

-- | Where the energy of a signal sits, to the nearest hertz, by the
-- coarsest possible means: the frequency that correlates best.
peakHz :: Signal -> Double
peakHz x = snd (maximum [ (mag f, f) | f <- [900, 901 .. 1100] ])
  where
    n = VS.length x
    mag f = let c = VS.sum (VS.imap (\i v -> v * cos (2 * pi * f * fromIntegral i / fs)) x)
                s = VS.sum (VS.imap (\i v -> v * sin (2 * pi * f * fromIntegral i / fs)) x)
            in c * c + s * s + 0 * fromIntegral n

tone1k :: Signal
tone1k = VS.generate 16000 (\i -> 0.4 * sin (2 * pi * 1000 * fromIntegral i / fs))

liveChannelTests :: TestTree
liveChannelTests = testGroup "the channel simulator, a block at a time"
  [ testCase "with nothing asked of the line it is the identity, to the sample" $
      stream idealChannel probe @?= probe

  , testCase "the stateless stages agree with the whole-signal ones exactly" $ do
      let ch = idealChannel { chGain = 0.5, chDcOffset = 0.01, chClip = Just 0.2
                            , chHum = Just (50, 0.02), chNonlin = Just (SoftClip 2) }
      maxDiff (stream ch probe) (applyChannel fs ch probe) < 1e-12 @? "differs"

  , testCase "a single-tap echo agrees exactly" $ do
      let ch = idealChannel { chEcho = Just (0.02, 0.3) }
      maxDiff (stream ch probe) (applyChannel fs ch probe) < 1e-12 @? "differs"

  , testCase "the band-pass agrees once its group delay is taken out" $ do
      let ch = idealChannel { chBandpass = Just (300, 3400) }
          d = liveLatency fs ch
          live = stream ch probe
          whole = applyChannel fs ch probe
          n = VS.length probe
          a = VS.slice d (n - 2 * d) live
          b = VS.slice 0 (n - 2 * d) whole
      d > 0 @? "no latency reported"
      maxDiff a b < 1e-9 @? ("differs by " ++ show (maxDiff a b))

  , testCase "a frequency offset moves a tone by that much" $ do
      let ch = idealChannel { chFreqOffsetHz = 37 }
      abs (peakHz (VS.drop 1000 (stream ch tone1k)) - 1037) <= 1 @? "tone did not move"

  , testCase "a fast far clock reads a tone high by the same fraction" $ do
      let ch = idealChannel { chRateOffset = 0.03 }
      abs (peakHz (VS.drop 2000 (stream ch tone1k)) - 1030) <= 1 @? "tone did not move"

  , testCase "sinusoidal jitter is the same warp, arriving late" $ do
      -- the whole-signal warp reads a delay of 0 to 2a; the live one is
      -- biased so its interpolator always has samples either side, and
      -- 'liveLatency' says by how much.  Taking that out, the two are
      -- the same signal to within the interpolators' disagreement.
      let ch = idealChannel { chJitter = SineJitter 2 3 }
          d = liveLatency fs ch
          n = VS.length probe
          live = VS.slice d (n - 2 * d) (stream ch probe)
          whole = VS.slice 0 (n - 2 * d) (applyChannel fs ch probe)
      d > 0 @? "no latency reported for a warped line"
      corr live whole > 0.999 @? ("warps disagree, correlation " ++ show (corr live whole))

  , testCase "a jittered line is not flattened where its delay goes to zero" $ do
      -- what a clamp instead of a bias did: the trough of every cycle
      -- held at one delay, which is a distortion rather than a warp.
      let ch = idealChannel { chJitter = SineJitter 2 3 }
          d = liveLatency fs ch
          live = VS.slice d 6000 (stream ch tone1k)
          whole = VS.slice 0 6000 (applyChannel fs ch tone1k)
      corr live whole > 0.999 @? "a pure tone came through differently"

  , testCase "G.711 agrees exactly: encode, decode, nothing else" $ do
      let ch = idealChannel { chCodec = Just Ulaw }
      maxDiff (stream ch probe) (applyChannel fs ch probe) < 1e-12 @? "differs"

  , testCase "a loss chain that never enters its bad state loses nothing" $ do
      -- not the same as changing nothing: naming a loss without naming a
      -- codec puts the span's default G.711 in the path, here as much as
      -- in the whole-signal version, so the comparison is against that.
      let ch = idealChannel { chLoss = Just (Loss 0.02 0.0 1.0 Silence) }
      stream ch probe @?= applyChannel fs ch probe

  , testCase "a loss chain that never leaves it goes silent" $ do
      let ch = idealChannel { chLoss = Just (Loss 0.02 1.0 0.0 Silence) }
      VS.maximum (VS.map abs (stream ch probe)) == 0 @? "not silent"

  , testCase "noise lands at the ratio asked for, against the signal's level" $ do
      let ch = idealChannel { chSnrDb = Just 20 }
          y = stream ch tone1k
          resid = VS.zipWith (-) y tone1k
          got = 20 * logBase 10 (rms tone1k / rms (VS.drop 800 resid))
      abs (got - 20) < 1.5 @? ("got " ++ show got ++ " dB")
  ]
