{-# LANGUAGE BangPatterns #-}
-- | Small, dependency-free DSP primitives over unboxed 'Double' vectors.
--
-- Offline (whole-vector) helpers used by the channel simulator, the
-- tests and the probe tool.  The modem receiver itself is built from the
-- streaming stages in "Modec.FSK" and friends; those reuse the window
-- and tone helpers here.
module Modec.DSP
  ( Signal
    -- * Windows and correlators
  , movingSum
  , rectWindow
  , hannWindow
  , windowDot
  , toneEnergy
  , toneEnergyW
  , toneAmplitude
  , toneAmplitudeW
    -- * Levels
  , rms
  , db
  , fromDb
    -- * Noise
  , gaussianNoise
  , addNoise
    -- * FIR filters
  , firLowpass
  , firBandpass
  , firHilbert
  , fir
  , firCentered
  , firStream
    -- * Interpolation and time warping
  , sampleAt
  , resampleBy
  , variableDelay
  , frequencyShift
  ) where

import qualified Data.Vector.Storable as VS

-- | Real-valued sample stream, nominally in [-1, 1].
type Signal = VS.Vector Double

-- | Sliding-window sum of the last @len@ samples (window ends at the
-- current index, truncated at the start of the vector).  Uses prefix
-- sums, so it costs O(n) regardless of window length.
movingSum :: Int -> Signal -> Signal
movingSum len x = VS.generate n go
  where
    n = VS.length x
    s = VS.scanl' (+) 0 x
    go i =
      let j = max 0 (i + 1 - len)
      in VS.unsafeIndex s (i + 1) - VS.unsafeIndex s j

rectWindow :: Int -> VS.Vector Double
rectWindow len = VS.replicate len 1

-- | Periodic-free (symmetric) Hann window, never zero at the ends.
hannWindow :: Int -> VS.Vector Double
hannWindow len = VS.generate len $ \k ->
  0.5 - 0.5 * cos (2 * pi * (fromIntegral k + 1) / (fromIntegral len + 1))

-- | Weighted sliding window: @out[i] = sum_k w[k] * x[i - (L-1) + k]@,
-- with zeros before the start of @x@.  Output has the length of @x@.
windowDot :: VS.Vector Double -> Signal -> Signal
windowDot w x = VS.generate n $ \i -> VS.sum (VS.zipWith (*) w (VS.slice i l xp))
  where
    l = VS.length w
    n = VS.length x
    xp = VS.replicate (l - 1) 0 VS.++ x

-- | Energy of a complex correlation against a tone of frequency @f@ Hz
-- at sample rate @fs@, over a rectangular sliding window of @len@
-- samples.  For a tone of amplitude @a@ exactly at @f@ the value is
-- approximately @(a * len / 2)^2@; see 'toneAmplitude'.
toneEnergy :: Double -> Double -> Int -> Signal -> Signal
toneEnergy fs f len x = VS.zipWith (\a b -> a * a + b * b) (movingSum len re) (movingSum len im)
  where
    (re, im) = mixTone fs f x

-- | Like 'toneEnergy' with an arbitrary window; scale with 'toneAmplitudeW'.
toneEnergyW :: Double -> Double -> VS.Vector Double -> Signal -> Signal
toneEnergyW fs f w x = VS.zipWith (\a b -> a * a + b * b) (windowDot w re) (windowDot w im)
  where
    (re, im) = mixTone fs f x

mixTone :: Double -> Double -> Signal -> (Signal, Signal)
mixTone fs f x = (VS.imap (\n v -> v * cos (w * fromIntegral n)) x, VS.imap (\n v -> v * sin (w * fromIntegral n)) x)
  where w = 2 * pi * f / fs

-- | Convert a rectangular-window 'toneEnergy' value back to an
-- estimated tone amplitude.
toneAmplitude :: Int -> Double -> Double
toneAmplitude len = toneAmplitudeW (fromIntegral len)

-- | Convert a 'toneEnergyW' value back to an amplitude, given the sum of
-- the window weights.
toneAmplitudeW :: Double -> Double -> Double
toneAmplitudeW wsum e = 2 * sqrt e / wsum

rms :: Signal -> Double
rms x
  | VS.null x = 0
  | otherwise = sqrt (VS.sum (VS.map (\v -> v * v) x) / fromIntegral (VS.length x))

db :: Double -> Double
db v = 20 * logBase 10 (max 1e-30 v)

fromDb :: Double -> Double
fromDb d = 10 ** (d / 20)

-- | @n@ samples of deterministic white Gaussian noise with standard
-- deviation @sigma@ (Box-Muller over a 64-bit LCG seeded with @seed@).
-- Repeatable, not good, not cryptographic.
gaussianNoise :: Int -> Int -> Double -> Signal
gaussianNoise seed n sigma = VS.unfoldrN n step (fromIntegral seed * 2654435761 + 1 :: Integer)
  where
    step !st =
      let s1 = lcg st
          s2 = lcg s1
          u1 = max 1e-12 (unit s1)
          u2 = unit s2
          g = sqrt (-2 * log u1) * cos (2 * pi * u2)
      in Just (sigma * g, s2)
    lcg s = (s * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (64 :: Int))
    unit s = fromIntegral (s `div` 2048) / 9007199254740992

addNoise :: Int -> Double -> Signal -> Signal
addNoise seed sigma x = VS.zipWith (+) x (gaussianNoise seed (VS.length x) sigma)

sinc :: Double -> Double
sinc t
  | t == 0 = 1
  | otherwise = sin (pi * t) / (pi * t)

blackman :: Int -> VS.Vector Double
blackman n = VS.generate n $ \k ->
  let a = 2 * pi * fromIntegral k / fromIntegral (n - 1)
  in 0.42 - 0.5 * cos a + 0.08 * cos (2 * a)

oddTaps :: Int -> Int
oddTaps t = if even t then t + 1 else t

-- | Windowed-sinc low-pass FIR, unity DC gain, @taps@ coefficients (made odd).
firLowpass :: Double -> Double -> Int -> VS.Vector Double
firLowpass fs fc taps0 = VS.map (/ s) h
  where
    taps = oddTaps taps0
    m = fromIntegral (taps `div` 2) :: Double
    fcn = 2 * fc / fs
    h = VS.zipWith (*) (blackman taps) (VS.generate taps $ \k -> fcn * sinc (fcn * (fromIntegral k - m)))
    s = VS.sum h

-- | Band-pass as the difference of two low-passes.
firBandpass :: Double -> Double -> Double -> Int -> VS.Vector Double
firBandpass fs f1 f2 taps = VS.zipWith (-) (firLowpass fs f2 taps) (firLowpass fs f1 taps)

-- | Windowed ideal Hilbert transformer (90 degree phase shift), odd length.
firHilbert :: Int -> VS.Vector Double
firHilbert taps0 = VS.zipWith (*) (blackman taps) (VS.generate taps h)
  where
    taps = oddTaps taps0
    m = taps `div` 2
    h k = let d = k - m in if odd d then 2 / (pi * fromIntegral d) else 0

-- | Causal FIR: @out[i] = sum_k h[k] x[i-k]@.
fir :: VS.Vector Double -> Signal -> Signal
fir h = windowDot (VS.reverse h)

-- | Streaming FIR: @hrev@ is the reversed kernel, @hist@ the last
-- (taps-1) input samples.  Returns the output for the chunk (delayed by
-- the group delay) and the new history.
firStream :: VS.Vector Double -> Signal -> Signal -> (Signal, Signal)
firStream hrev hist chunk = (VS.generate n out, VS.drop n ext)
  where
    n = VS.length chunk
    t = VS.length hrev
    ext = hist VS.++ chunk
    out i = go 0 0
      where
        go !k !acc
          | k >= t = acc
          | otherwise = go (k + 1) (acc + VS.unsafeIndex hrev k * VS.unsafeIndex ext (i + k))

-- | FIR with the group delay of an odd-length symmetric kernel removed,
-- so the output lines up sample-for-sample with the input.
firCentered :: VS.Vector Double -> Signal -> Signal
firCentered h x = VS.slice m n (fir h (x VS.++ VS.replicate m 0))
  where
    m = VS.length h `div` 2
    n = VS.length x

-- | Band-limited interpolation at fractional index @t@ (Lanczos, a = 6).
-- Samples outside the vector read as zero.
sampleAt :: Signal -> Double -> Double
sampleAt x t = go (-5) 0
  where
    n = VS.length x
    i0 = floor t :: Int
    fr = t - fromIntegral i0
    a = 6
    go !k !acc
      | k > 6 = acc
      | otherwise =
          let i = i0 + k
              v = if i < 0 || i >= n then 0 else VS.unsafeIndex x i
              u = fromIntegral k - fr
              l = if abs u < a then sinc u * sinc (u / a) else 0
          in go (k + 1) (acc + v * l)

-- | Read the signal at a rate @ratio@ times the original: @ratio > 1@
-- shortens the signal (as seen by a receiver whose clock runs slow).
resampleBy :: Double -> Signal -> Signal
resampleBy ratio x = VS.generate m (\i -> sampleAt x (fromIntegral i * ratio))
  where m = max 0 (floor (fromIntegral (VS.length x - 1) / ratio) + 1)

-- | Time-varying delay: @out[i] = x(i - d i)@ with @d@ in samples.
variableDelay :: (Int -> Double) -> Signal -> Signal
variableDelay d x = VS.generate (VS.length x) (\i -> sampleAt x (fromIntegral i - d i))

-- | Shift every frequency component by @df@ Hz (single-sideband
-- modulation via a Hilbert transformer), as an FDM carrier system would.
frequencyShift :: Double -> Double -> Signal -> Signal
frequencyShift fs df x = VS.izipWith (\n v q -> v * cos (w * fromIntegral n) - q * sin (w * fromIntegral n)) x xq
  where
    w = 2 * pi * df / fs
    taps = 2 * round (fs / 125) + 1 :: Int
    xq = firCentered (firHilbert taps) x
