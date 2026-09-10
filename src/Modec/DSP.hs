{-# LANGUAGE BangPatterns #-}
-- | Small, dependency-free DSP primitives over unboxed 'Double' vectors.
--
-- Offline (whole-vector) helpers used by the channel simulator, the
-- tests and the probe tool.  The modem receiver itself is built from the
-- streaming stages in "Modec.FSK" and friends; those reuse the window
-- and tone helpers here.
module Modec.DSP
  ( Signal
  , chunksOf
    -- * Windows and correlators
  , movingSum
  , rectWindow
  , hannWindow
  , windowDot
  , toneEnergy
  , toneEnergyW
  , mixDownAt
  , goertzel
  , toneAmplitude
  , toneAmplitudeW
    -- * Levels
  , rms
  , db
  , fromDb
    -- * Noise and test sources
  , gaussianNoise
  , uniformNoise
  , addNoise
  , prbs
    -- * FIR filters
  , firLowpass
  , firBandpass
  , firHilbert
  , fir
  , firCentered
  , firStream
  , delayDistortionKernel
    -- * Pulse shaping
  , rrcPulse
  , rrcKernel
    -- * Phase
  , wrapPi
  , wrapTwoPi
    -- * Interpolation and time warping
  , sampleAt
  , sampleAtFast
  , cubicAt
  , resampleBy
  , resampleTo
  , variableDelay
  , frequencyShift
  ) where

import Data.Bits (shiftL, testBit, (.&.), (.|.))
import qualified Data.Vector.Storable as VS

-- | Real-valued sample stream, nominally in [-1, 1].
type Signal = VS.Vector Double

-- | Cut a signal into blocks of @n@ samples, the last one short.  A
-- streaming receiver is defined by its behaviour not depending on where
-- these fall, so the offline helpers that drive one over a whole
-- recording all have to cut it up somewhere; they cut it up here.
chunksOf :: Int -> Signal -> [Signal]
chunksOf n x
  | n <= 0 = [x]
  | VS.null x = []
  | otherwise = VS.take n x : chunksOf n (VS.drop n x)

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

-- | Mix a block down to complex baseband against a tone of @w@ radians
-- per sample, starting from global sample index @n0@: multiply by
-- @exp(-j w n)@ and return the in-phase and quadrature parts.
--
-- The offset is the point of it.  A streaming receiver takes the phase
-- of its own local carrier from the running sample count, so a block
-- boundary is not an event it can see -- and every receiver here that
-- does so had written these two lines out by hand.
mixDownAt :: Double -> Int -> Signal -> (Signal, Signal)
mixDownAt w n0 x =
  ( VS.imap (\i v -> v * cos (w * fromIntegral (n0 + i))) x
  , VS.imap (\i v -> negate v * sin (w * fromIntegral (n0 + i))) x )

mixTone :: Double -> Double -> Signal -> (Signal, Signal)
mixTone fs f = mixDownAt (2 * pi * f / fs) 0

-- | Squared magnitude of one DFT bin at @f@ over a whole block, by
-- Goertzel's recurrence: one multiply and two adds per sample, and no
-- per-sample sine or cosine.  'toneEnergy' slides a window along a
-- signal and costs a complex mixer per sample; this is the cheaper
-- shape for a detector that only wants one number per block, which is
-- how the DTMF receiver reads its eight tones.
--
-- The scaling is the same as 'toneEnergy' with a rectangular window of
-- the block length -- a tone of amplitude @a@ exactly at @f@ gives
-- @(a n \/ 2)^2@ -- so 'toneAmplitude' converts it back.  Off-bin tones
-- read low: the response follows the rectangular window's sinc, which
-- first nulls at @fs \/ n@ away.
goertzel :: Double -> Double -> Signal -> Double
goertzel fs f x = go 0 0 0
  where
    n = VS.length x
    k = 2 * cos (2 * pi * f / fs)
    go !i !s1 !s2
      | i >= n = s1 * s1 + s2 * s2 - k * s1 * s2
      | otherwise = go (i + 1) (VS.unsafeIndex x i + k * s1 - s2) s1

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

-- | @n@ deterministic samples uniform on [0, 1), from the same
-- generator.  Impairments that happen or do not -- a lost frame, a bit
-- error, an impulse arriving -- need a coin rather than a bell curve,
-- and the alternative to having one is pushing Gaussians through an erf
-- approximation, which "Modec.Channel" was doing.
uniformNoise :: Int -> Int -> Signal
uniformNoise seed n = VS.unfoldrN n step (fromIntegral seed * 2654435761 + 1 :: Integer)
  where
    step !st = let s' = lcg st in Just (unit s', s')
    lcg s = (s * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (64 :: Int))
    unit s = fromIntegral (s `div` 2048) / 9007199254740992

addNoise :: Int -> Double -> Signal -> Signal
addNoise seed sigma x = VS.zipWith (+) x (gaussianNoise seed (VS.length x) sigma)

-- | Maximal-length pseudo-random binary sequence from the generating
-- polynomial @1 + x^-q + x^-p@: a @p@-stage Fibonacci shift register
-- tapped at stages @p@ and @q@, started at all ones.  ITU-T O.152, the
-- test pattern the reference implementations use, is @(11, 9)@ and
-- repeats every 2047 bits.
--
-- Deterministic, so a test can compare a demodulated stream against the
-- pattern that produced it without carrying the bits around.
prbs :: (Int, Int) -> Int -> [Bool]
prbs (p, q) n = take n (go mask)
  where
    mask = (1 `shiftL` p) - 1 :: Int
    go !reg =
      let b = testBit reg (p - 1) /= testBit reg (q - 1)
          reg' = ((reg `shiftL` 1) .|. (if b then 1 else 0)) .&. mask
      in b : go reg'

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

-- | All-pass FIR whose group delay rises parabolically from the band
-- centre to @edgeMs@ milliseconds extra at the band edges (300 and
-- 3400 Hz), the classic telephone-line delay distortion that a modem
-- equaliser has to undo.  Designed by frequency sampling: the phase is
-- the integral of the group delay, the impulse response the inverse
-- transform, windowed to @taps@ coefficients (made odd).
delayDistortionKernel :: Double -> Double -> Int -> VS.Vector Double
delayDistortionKernel fs edgeMs taps0 = VS.zipWith (*) (blackman taps) raw
  where
    taps = oddTaps taps0
    m = taps `div` 2
    nf = 2048 :: Int
    fc = 1850
    halfBand = 1550
    k = edgeMs / 1000 / (halfBand * halfBand)          -- seconds per Hz^2
    -- phase(f) = 2 pi * integral of tau(f) df, tau(f) = k (f - fc)^2 (extra delay only)
    phase f = 2 * pi * k * ((f - fc) ^ (3 :: Int)) / 3
    raw = VS.generate taps $ \i ->
      let n = fromIntegral (i - m)
          -- real part of the inverse DFT of exp(-j phase(f)) over positive frequencies, doubled
          s = sum [ cos (2 * pi * f * n / fs - phase f) | j <- [0 .. nf - 1], let f = fromIntegral j * fs / 2 / fromIntegral nf ]
      in s / fromIntegral nf

-- | Root-raised-cosine impulse response with roll-off @b@, at time @t@
-- in symbol periods.  The two removable singularities (t = 0 and
-- t = 1\/4b) are given their limits.
rrcPulse :: Double -> Double -> Double
rrcPulse b t
  | abs t < 1e-9 = 1 - b + 4 * b / pi
  | abs (abs t - 1 / (4 * b)) < 1e-9 =
      b / sqrt 2 * ((1 + 2 / pi) * sin (pi / (4 * b)) + (1 - 2 / pi) * cos (pi / (4 * b)))
  | otherwise =
      (sin (pi * t * (1 - b)) + 4 * b * t * cos (pi * t * (1 + b))) / (pi * t * (1 - (4 * b * t) ^ (2 :: Int)))

-- | Sampled root-raised-cosine kernel for a matched filter, unit energy:
-- @rrcKernel fs baud rollOff span@, spanning @span@ symbols each side.
rrcKernel :: Double -> Double -> Double -> Double -> VS.Vector Double
rrcKernel fs baud rollOff span_ = VS.map (/ norm) raw
  where
    sps = fs / baud
    half = round (span_ * sps) :: Int
    raw = VS.generate (2 * half + 1) (\i -> rrcPulse rollOff (fromIntegral (i - half) / sps))
    norm = sqrt (VS.sum (VS.map (\v -> v * v) raw))

-- | Fold a phase into [-pi, pi).  For a phase /error/, where the whole
-- point is that a step across the branch cut is a small correction and
-- not a full turn backwards.
wrapPi :: Double -> Double
wrapPi x = x - 2 * pi * fromIntegral (round (x / (2 * pi)) :: Int)

-- | Fold a phase into [0, 2*pi).  For an oscillator carried across
-- blocks, where the only job is to keep the accumulator from growing
-- until its absolute precision no longer resolves a sample.
wrapTwoPi :: Double -> Double
wrapTwoPi x = x - 2 * pi * fromIntegral (floor (x / (2 * pi)) :: Int)

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

-- | The same interpolation as 'sampleAt', from a table.
--
-- 'sampleAt' evaluates twelve Lanczos weights per output sample, and
-- each one costs two sines: about six hundred nanoseconds a sample,
-- which is more than the rest of a channel simulation put together.
-- The weights depend only on the fractional part, so quantise that to
-- 1\/1024 of a sample and there are only 1024 kernels to have.  Building
-- them costs a quarter of a millisecond, once, and the interpolation
-- becomes twelve multiply-adds.
--
-- The error that quantisation buys is 1\/1024 of a sample of timing,
-- which at 2400 Hz is a tenth of a degree of phase -- far below what
-- any receiver here can see, and far below what Catmull-Rom would cost.
-- ('cubicAt' is cheaper still, but its response droops 2 dB and its
-- phase 7 degrees at 2400 Hz; that is harmless at a fixed delay, which
-- is why the timing loops use it, and not harmless at all under a
-- delay that moves, because it turns a pure delay modulation into an
-- amplitude and phase modulation nobody asked for.)
sampleAtFast :: Signal -> Double -> Double
sampleAtFast x t = go 0 0
  where
    n = VS.length x
    i0 = floor t :: Int
    fr = t - fromIntegral i0
    base = min (delayPhases - 1) (floor (fr * fromIntegral delayPhases)) * 12
    go !k !acc
      | k > 11 = acc
      | otherwise =
          let i = i0 - 5 + k
              v = if i < 0 || i >= n then 0 else VS.unsafeIndex x i
          in go (k + 1) (acc + v * VS.unsafeIndex delayBank (base + k))

delayPhases :: Int
delayPhases = 1024

-- | 1024 fractional positions of the twelve weights 'sampleAt' uses.
-- A top-level constant so it is built once for the life of the program
-- rather than once per call.
delayBank :: VS.Vector Double
delayBank = VS.generate (delayPhases * 12) $ \i ->
  let (ph, k) = i `divMod` 12
      fr = fromIntegral ph / fromIntegral delayPhases
      u = fromIntegral (k - 5 :: Int) - fr
      a = 6
  in if abs u < a then sinc u * sinc (u / a) else 0
{-# NOINLINE delayBank #-}

-- | Four-point cubic (Catmull-Rom) interpolation at fractional index
-- @t@.  Unlike 'sampleAt' this reads without bounds checks, so the
-- caller must keep one sample before and two after @t@ in range; that is
-- what makes it cheap enough for a per-symbol timing loop.
cubicAt :: Signal -> Double -> Double
cubicAt v t =
  let i = floor t :: Int
      mu = t - fromIntegral i
      p0 = VS.unsafeIndex v (i - 1); p1 = VS.unsafeIndex v i
      p2 = VS.unsafeIndex v (i + 1); p3 = VS.unsafeIndex v (i + 2)
      a0 = -0.5 * p0 + 1.5 * p1 - 1.5 * p2 + 0.5 * p3
      a1 = p0 - 2.5 * p1 + 2 * p2 - 0.5 * p3
      a2 = -0.5 * p0 + 0.5 * p2
  in ((a0 * mu + a1) * mu + a2) * mu + p1

-- | Read the signal at a rate @ratio@ times the original: @ratio > 1@
-- shortens the signal (as seen by a receiver whose clock runs slow).
resampleBy :: Double -> Signal -> Signal
resampleBy ratio x = VS.generate m (\i -> sampleAt x (fromIntegral i * ratio))
  where m = max 0 (floor (fromIntegral (VS.length x - 1) / ratio) + 1)

-- | The signal at another sample rate.
--
-- The modem runs at whatever rate its input has, and nothing here
-- needs this to decode a 48 kHz recording.  What it buys is running
-- one at the rate everything else here is measured at, and at a
-- thirty-fifth of the cost, since the FSK front end is O(fs^2).
-- Going down, the band above the new Nyquist would fold back in, so
-- it is removed first: a low-pass at 0.45 of the new rate, its
-- transition a tenth of that, and its delay taken back out so the
-- result lines up with the original in time.
resampleTo :: Double -> Double -> Signal -> Signal
resampleTo from to x
  | from == to = x
  | to > from = resampleBy (from / to) x
  | otherwise = resampleBy (from / to) (VS.drop half (fir h (x VS.++ VS.replicate half 0)))
  where
    h = firLowpass from (0.45 * to) (round (55 * from / to))
    half = VS.length h `div` 2

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
