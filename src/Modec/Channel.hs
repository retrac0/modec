-- | Telephone channel impairment simulator.  Pure and deterministic
-- (everything random derives from 'chSeed'), so tests and benches are
-- repeatable.  Impairments are applied in the order listed in
-- 'applyChannel'.
module Modec.Channel
  ( Channel (..)
  , Jitter (..)
  , idealChannel
  , telephoneChannel
  , applyChannel
  , mixAt
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP

data Jitter
  = NoJitter
  | SineJitter { jAmpSamples :: Double, jFreqHz :: Double }
    -- ^ wow/flutter style sinusoidal delay modulation
  | WalkJitter { jStepSamples :: Double, jMaxSamples :: Double }
    -- ^ random walk of the delay, clamped
  | Slips { jEverySec :: Double, jSamples :: Double }
    -- ^ jitter-buffer style: the delay toggles by @jSamples@ every @jEverySec@
  deriving (Show)

data Channel = Channel
  { chSeed         :: !Int
  , chGain         :: !Double                 -- ^ linear gain, 1 = unchanged
  , chDcOffset     :: !Double
  , chEcho         :: !(Maybe (Double, Double))  -- ^ (delay seconds, linear gain)
  , chBandpass     :: !(Maybe (Double, Double))  -- ^ (low Hz, high Hz)
  , chFreqOffsetHz :: !Double                 -- ^ FDM carrier frequency offset
  , chRateOffset   :: !Double                 -- ^ clock offset as a fraction; +0.01 = far clock 1 % fast
  , chJitter       :: !Jitter
  , chDropout      :: !(Maybe (Double, Double))  -- ^ (block seconds, probability a block is zeroed)
  , chClip         :: !(Maybe Double)         -- ^ hard clip level
  , chHum          :: !(Maybe (Double, Double))  -- ^ (Hz, amplitude)
  , chSnrDb        :: !(Maybe Double)         -- ^ AWGN, full-band SNR relative to the signal at that point
  } deriving (Show)

idealChannel :: Channel
idealChannel = Channel
  { chSeed = 1, chGain = 1, chDcOffset = 0, chEcho = Nothing, chBandpass = Nothing
  , chFreqOffsetHz = 0, chRateOffset = 0, chJitter = NoJitter, chDropout = Nothing
  , chClip = Nothing, chHum = Nothing, chSnrDb = Nothing }

-- | A plain but realistic analogue line: 300-3400 Hz band, given SNR.
telephoneChannel :: Double -> Channel
telephoneChannel snr = idealChannel { chBandpass = Just (300, 3400), chSnrDb = Just snr }

-- | Add @other@ to @x@ at @levelDb@ relative to the RMS of @x@.
mixAt :: Double -> Signal -> Signal -> Signal
mixAt levelDb other x = VS.zipWith (+) x (VS.map (* g) (VS.take (VS.length x) (other VS.++ VS.replicate (VS.length x) 0)))
  where g = rms x / max 1e-12 (rms other) * fromDb levelDb

applyChannel :: Double -> Channel -> Signal -> Signal
applyChannel fs ch =
    noise . hum . clip . dropout . rate . jitter . freqOff . bandpass . echo . gainDc
  where
    seed k = chSeed ch * 7919 + k

    gainDc = VS.map (\v -> v * chGain ch + chDcOffset ch)

    echo x = case chEcho ch of
      Nothing -> x
      Just (secs, g) ->
        let d = round (secs * fs)
        in VS.imap (\i v -> v + (if i >= d then g * VS.unsafeIndex x (i - d) else 0)) x

    bandpass x = case chBandpass ch of
      Nothing -> x
      Just (f1, f2) -> firCentered (firBandpass fs f1 f2 (2 * round (fs / 40) + 1)) x

    freqOff x
      | chFreqOffsetHz ch == 0 = x
      | otherwise = frequencyShift fs (chFreqOffsetHz ch) x

    jitter x = case chJitter ch of
      NoJitter -> x
      SineJitter a f -> variableDelay (\i -> a + a * sin (2 * pi * f * fromIntegral i / fs)) x
      WalkJitter stepS mx ->
        let steps = gaussianNoise (seed 2) (VS.length x) stepS
            walk = VS.scanl' (\d s -> max (-mx) (min mx (d + s))) 0 steps
        in variableDelay (\i -> mx + VS.unsafeIndex walk i) x
      Slips every s ->
        let per = max 1 (round (every * fs)) :: Int
        in variableDelay (\i -> s * fromIntegral ((i `div` per) `mod` 2)) x

    rate x
      | chRateOffset ch == 0 = x
      | otherwise = resampleBy (1 + chRateOffset ch) x

    dropout x = case chDropout ch of
      Nothing -> x
      Just (secs, prob) ->
        let blk = max 1 (round (secs * fs)) :: Int
            nb = VS.length x `div` blk + 1
            u = VS.map (\g -> 0.5 + 0.5 * erfApprox (g / sqrt 2)) (gaussianNoise (seed 3) nb 1)
        in VS.imap (\i v -> if VS.unsafeIndex u (i `div` blk) < prob then 0 else v) x

    clip x = case chClip ch of
      Nothing -> x
      Just c -> VS.map (max (-c) . min c) x

    hum x = case chHum ch of
      Nothing -> x
      Just (f, a) -> VS.imap (\i v -> v + a * sin (2 * pi * f * fromIntegral i / fs)) x

    noise x = case chSnrDb ch of
      Nothing -> x
      Just snr -> addNoise (seed 4) (rms x / fromDb snr) x

-- | Abramowitz-Stegun style erf approximation, plenty for a coin flip.
erfApprox :: Double -> Double
erfApprox z =
  let t = 1 / (1 + 0.3275911 * abs z)
      y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-z * z)
  in if z >= 0 then y else -y
