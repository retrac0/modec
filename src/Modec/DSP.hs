{-# LANGUAGE BangPatterns #-}
-- | Small, dependency-free DSP primitives over unboxed 'Double' vectors.
--
-- Everything here is offline (whole-vector) for now.  The modem core is
-- kept pure so the same code runs against WAV fixtures in the test suite
-- and against live audio at runtime; a chunked/streaming variant with
-- explicit carried state will replace the whole-vector helpers later.
module Modec.DSP
  ( Signal
  , movingSum
  , toneEnergy
  , toneAmplitude
  , rms
  , addNoise
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

-- | Energy of a complex correlation against a tone of frequency @f@ Hz
-- at sample rate @fs@, over a sliding window of @len@ samples.
--
-- For a full-scale tone of amplitude @a@ exactly at @f@ the value is
-- approximately @(a * len / 2)^2@; see 'toneAmplitude'.
toneEnergy :: Double -> Double -> Int -> Signal -> Signal
toneEnergy fs f len x = VS.zipWith (\a b -> a * a + b * b) (movingSum len re) (movingSum len im)
  where
    w = 2 * pi * f / fs
    re = VS.imap (\n v -> v * cos (w * fromIntegral n)) x
    im = VS.imap (\n v -> v * sin (w * fromIntegral n)) x

-- | Convert a 'toneEnergy' value back to an estimated tone amplitude.
toneAmplitude :: Int -> Double -> Double
toneAmplitude len e = 2 * sqrt e / fromIntegral len

rms :: Signal -> Double
rms x
  | VS.null x = 0
  | otherwise = sqrt (VS.sum (VS.map (\v -> v * v) x) / fromIntegral (VS.length x))

-- | Add deterministic white Gaussian noise with standard deviation
-- @sigma@ (Box-Muller over a 64-bit LCG seeded with @seed@).  Used by
-- the tests; not cryptographic, not even good, just repeatable.
addNoise :: Int -> Double -> Signal -> Signal
addNoise seed sigma x = VS.zipWith (+) x noise
  where
    n = VS.length x
    noise = VS.unfoldrN n step (fromIntegral seed * 2654435761 + 1 :: Integer)
    step !st =
      let s1 = lcg st
          s2 = lcg s1
          u1 = max 1e-12 (unit s1)
          u2 = unit s2
          g = sqrt (-2 * log u1) * cos (2 * pi * u2)
      in Just (sigma * g, s2)
    lcg s = (s * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (64 :: Int))
    unit s = fromIntegral (s `div` 2048) / 9007199254740992
