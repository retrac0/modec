-- | Telephone channel impairment simulator.  Pure and deterministic
-- (everything random derives from 'chSeed'), so tests and benches are
-- repeatable.  Impairments are applied in the order listed in
-- 'applyChannel'.
module Modec.Channel
  ( Channel (..)
  , Jitter (..)
  , Nonlinearity (..)
  , Codec (..)
  , Conceal (..)
  , Loss (..)
  , Stuck (..)
  , Impulse (..)
  , Hits (..)
  , idealChannel
  , telephoneChannel
  , applyChannel
  , mixAt
  , echoPath
  ) where

import Data.Bits (shiftL, xor, (.&.))
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.DSP
import Modec.G711

data Jitter
  = NoJitter
  | SineJitter { jAmpSamples :: Double, jFreqHz :: Double }
    -- ^ wow/flutter style sinusoidal delay modulation
  | WalkJitter { jStepSamples :: Double, jMaxSamples :: Double }
    -- ^ random walk of the delay, clamped
  | Slips { jEverySec :: Double, jSamples :: Double }
    -- ^ jitter-buffer style: the delay toggles by @jSamples@ every @jEverySec@
  deriving (Show)

-- | A memoryless nonlinearity: the same input sample always gives the
-- same output sample, so it makes harmonics and intermodulation but no
-- delay.  Between them these cover what an overdriven amplifier, a
-- saturating transformer or a carbon microphone does to a signal.
data Nonlinearity
  = SoftClip Double
    -- ^ @tanh (d*x) \/ tanh d@, normalised so full scale stays full
    -- scale.  Odd symmetric, so it makes only odd harmonics: the third,
    -- the fifth, and nothing at twice the fundamental.
  | Polynomial Double Double
    -- ^ @x + a2*x^2 - a3*x^3@.  The second-order term is the one that
    -- matters on a V.22 call: twice the low channel's 1200 Hz carrier is
    -- 2400 Hz, which is the high channel's carrier, so an asymmetric
    -- device puts one direction's energy directly onto the other's.
  deriving (Show)

-- | How a digital span carries a sample.
data Codec
  = Ulaw          -- ^ G.711 µ-law: North America, and every call in @recordings@
  | Alaw          -- ^ G.711 A-law: everywhere else
  | LinearBits Int  -- ^ uniform quantisation to @n@ bits, for contrast
  deriving (Show)

-- | What a receiver puts on the line in place of a frame that never
-- arrived.  It matters: a jitter buffer that repeats the last frame
-- hands a demodulator a plausible carrier with the wrong phase, which
-- is a different problem from handing it silence.
data Conceal = Silence | HoldLast | RepeatFrame
  deriving (Eq, Show)

-- | Frame loss as a Gilbert-Elliott chain.  Real loss arrives in runs --
-- a queue overflows, a route flaps -- and a per-frame coin flip of the
-- same average rate does far less damage, because a modem rides out one
-- lost frame and loses lock over five.
data Loss = Loss
  { lsFrameSec :: Double  -- ^ frame length; 0.02 for an RTP packet
  , lsToBad    :: Double  -- ^ probability of entering the bad state, per frame
  , lsToGood   :: Double  -- ^ probability of leaving it; mean burst is 1\/this
  , lsConceal  :: Conceal
  } deriving (Show)

-- | A run of PCM codes forced to one value: a span that has lost frame
-- alignment, or an alarm indication.  Held at the code level rather
-- than the sample level because that is where the effect lives -- in
-- µ-law @0xFF@ is silence and @0x00@ is full-scale negative, and
-- \"stuck at all ones\" therefore means the quietest thing on the line
-- rather than the loudest.
data Stuck = Stuck
  { stEverySec :: Double
  , stForSec   :: Double
  , stCode     :: Word8
  } deriving (Show)

-- | Impulse noise: the classic switched-network impairment, counted
-- rather than averaged (ITU-T O.71 counts excursions above a threshold
-- in fifteen minutes).  Each arrival rings, because a click on a line
-- reaches the receiver through the line's own bandwidth rather than as
-- a delta.
data Impulse = Impulse
  { imPerSec   :: Double  -- ^ mean arrivals per second
  , imAmp      :: Double  -- ^ peak amplitude of an arrival
  , imRingHz   :: Double  -- ^ what it rings at
  , imDecaySec :: Double  -- ^ time constant of the ring-down
  } deriving (Show)

-- | Transient hits, the impairment a line records as a count rather
-- than a level.  One mechanism, three names: a gain hit is a few dB for
-- a few milliseconds, a dropout is the same thing twenty dB down.  The
-- thresholds the specs use are >3 dB for a gain hit and >12 dB for a
-- dropout, both lasting >4 ms.
data Hits = Hits
  { hitPerSec :: Double  -- ^ mean arrivals per second
  , hitForSec :: Double  -- ^ how long one lasts
  , hitGainDb :: Double  -- ^ level change while it lasts; negative is a dropout
  } deriving (Show)

data Channel = Channel
  { chSeed         :: !Int
  , chGain         :: !Double                 -- ^ linear gain, 1 = unchanged
  , chDcOffset     :: !Double
  , chEcho         :: !(Maybe (Double, Double))  -- ^ (delay seconds, linear gain)
  , chBandpass     :: !(Maybe (Double, Double))  -- ^ (low Hz, high Hz)
  , chDelayDist    :: !Double                 -- ^ extra group delay at the band edges, ms (0 = none)
  , chFreqOffsetHz :: !Double                 -- ^ FDM carrier frequency offset
  , chRateOffset   :: !Double                 -- ^ clock offset as a fraction; +0.01 = far clock 1 % fast
  , chJitter       :: !Jitter
  , chDropout      :: !(Maybe (Double, Double))  -- ^ (block seconds, probability a block is zeroed)
  , chClip         :: !(Maybe Double)         -- ^ hard clip level
  , chHum          :: !(Maybe (Double, Double))  -- ^ (Hz, amplitude)
  , chSnrDb        :: !(Maybe Double)         -- ^ AWGN, full-band SNR relative to the signal at that point
    -- * The handset and the loop
  , chNonlin       :: !(Maybe Nonlinearity)
  , chEchoTaps     :: ![(Double, Double)]
    -- ^ (delay in samples, linear gain) -- a dispersive hybrid return.
    -- 'chEcho' is one tap at a whole number of samples, which a linear
    -- FIR cancels exactly; a canceller measured against that reports a
    -- number it will not repeat on a telephone line.
    -- * The digital span
  , chCodec        :: !(Maybe Codec)
  , chBitError     :: !(Maybe Double)   -- ^ probability of a bit flip per PCM code
  , chStuck        :: !(Maybe Stuck)
  , chLoss         :: !(Maybe Loss)
  , chSlip         :: !(Maybe (Double, Int))
    -- ^ (every N seconds, frames) -- a jitter buffer under- or
    -- overrunning: positive repeats a 20 ms frame, negative drops one.
    -- A splice, not a delay, which is what makes it different from
    -- 'Slips'.
    -- * Transients
  , chImpulse      :: !(Maybe Impulse)
  , chHits         :: !(Maybe Hits)
  } deriving (Show)

idealChannel :: Channel
idealChannel = Channel
  { chSeed = 1, chGain = 1, chDcOffset = 0, chEcho = Nothing, chBandpass = Nothing, chDelayDist = 0
  , chFreqOffsetHz = 0, chRateOffset = 0, chJitter = NoJitter, chDropout = Nothing
  , chClip = Nothing, chHum = Nothing, chSnrDb = Nothing
  , chNonlin = Nothing, chEchoTaps = []
  , chCodec = Nothing, chBitError = Nothing, chStuck = Nothing, chLoss = Nothing
  , chSlip = Nothing
  , chImpulse = Nothing, chHits = Nothing }

-- | A plain but realistic analogue line: 300-3400 Hz band, given SNR.
telephoneChannel :: Double -> Channel
telephoneChannel snr = idealChannel { chBandpass = Just (300, 3400), chSnrDb = Just snr }

-- | Add @other@ to @x@ at @levelDb@ relative to the RMS of @x@.
mixAt :: Double -> Signal -> Signal -> Signal
mixAt levelDb other x = VS.zipWith (+) x (VS.map (* g) (VS.take (VS.length x) (other VS.++ VS.replicate (VS.length x) 0)))
  where g = rms x / max 1e-12 (rms other) * fromDb levelDb

-- | The order is a signal path, read right to left: what the
-- transmitter does to the signal, then the loop, the hybrid, the line,
-- the carrier system, the digital span, and finally what the receiver's
-- own front end adds.  New impairments are inserted at the point where
-- the mechanism lives; the stages that were here before keep their
-- order, and every stage is the identity when its field is unset, so
-- adding one cannot move a result that was already measured.
applyChannel :: Double -> Channel -> Signal -> Signal
applyChannel fs ch =
    noise . hum . clip . impulse . dropout . hits . slip . span_ . rate . jitter
      . freqOff . delayDist . bandpass . echo . nonlin . gainDc
  where
    -- Seeds are per impairment so that turning one on does not change
    -- what another one does.  2, 3 and 4 belong to the walk jitter, the
    -- dropout and the AWGN and must stay where they are: renumbering
    -- them would silently change the noise realisation of every test in
    -- the suite.
    seed k = chSeed ch * 7919 + k

    gainDc = VS.map (\v -> v * chGain ch + chDcOffset ch)

    nonlin x = case chNonlin ch of
      Nothing -> x
      Just (SoftClip d)
        | d <= 0 -> x
        | otherwise -> let k = tanh d in VS.map (\v -> tanh (d * v) / k) x
      Just (Polynomial a2 a3) ->
        VS.map (\v -> v + a2 * v * v - a3 * v * v * v) x

    -- One tap at a whole number of samples, several taps at fractional
    -- ones, or both.  Each reflects the signal arriving at the hybrid,
    -- not each other.
    echo x = case (chEcho ch, chEchoTaps ch) of
      (Nothing, []) -> x
      (e, taps) ->
        let zero = VS.replicate (VS.length x) 0
            one = case e of
              Nothing -> zero
              Just (secs, g) ->
                let d = round (secs * fs)
                in VS.imap (\i _ -> if i >= d then g * VS.unsafeIndex x (i - d) else 0) x
            many = if null taps then zero else echoPath taps x
        in VS.zipWith3 (\a b c -> a + b + c) x one many

    bandpass x = case chBandpass ch of
      Nothing -> x
      Just (f1, f2) -> firCentered (firBandpass fs f1 f2 (2 * round (fs / 40) + 1)) x

    delayDist x
      | chDelayDist ch == 0 = x
      | otherwise = firCentered (delayDistortionKernel fs (chDelayDist ch) (2 * round (fs * 0.02) + 1)) x

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

    -- The digital span: encode once, damage the codes, decode once.
    -- Working at the code level is the point -- a flipped exponent bit
    -- or a stuck byte means what it means on a real span, and neither
    -- can be expressed as something done to a sample.
    span_ x
      | not spanOn = x
      | otherwise = losses (VS.map dec (stuckAt (bitErrors (VS.map enc x))))
      where
        spanOn = case (chCodec ch, chBitError ch, chStuck ch, chLoss ch) of
          (Nothing, Nothing, Nothing, Nothing) -> False
          _ -> True
        -- A bit error needs bits, so asking for one without naming a
        -- codec gets the one this project's calls actually went through.
        (enc, dec, bits) = codecOf (maybe Ulaw id (chCodec ch))

    codecOf c = case c of
      Ulaw -> (fromIntegral . ulawEncode, ulawDecode . fromIntegral, 8 :: Int)
      Alaw -> (fromIntegral . alawEncode, alawDecode . fromIntegral, 8)
      LinearBits n ->
        let half = 2 ^^ (n - 1) :: Double
            lo = negate (round half) :: Int
            hi = round half - 1
        in ( \v -> max lo (min hi (round (v * half)))
           , \k -> fromIntegral k / half
           , n )

    bitErrors cs = case chBitError ch of
      Nothing -> cs
      Just p ->
        let n = VS.length cs
            u = uniformNoise (seed 5) (2 * n)
            (_, _, bits) = codecOf (maybe Ulaw id (chCodec ch))
        in VS.imap (\i c ->
             if VS.unsafeIndex u (2 * i) < p
               then let b = min (bits - 1) (floor (VS.unsafeIndex u (2 * i + 1) * fromIntegral bits))
                    in c `xor` (1 `shiftL` b)
               else c) cs

    stuckAt cs = case chStuck ch of
      Nothing -> cs
      Just (Stuck every for code) ->
        let per = max 1 (round (every * fs)) :: Int
            len = max 1 (round (for * fs)) :: Int
        in VS.imap (\i c -> if i `mod` per < len then fromIntegral code else c) cs

    -- Gilbert-Elliott: a two-state chain over frames, so loss arrives in
    -- runs of mean length 1 / lsToGood rather than one frame at a time.
    losses y = case chLoss ch of
      Nothing -> y
      Just (Loss frameSec toBad toGood how) ->
        let n = VS.length y
            blk = max 1 (round (frameSec * fs)) :: Int
            nf = (n + blk - 1) `div` blk
            u = uniformNoise (seed 6) nf
            walk b i = if b then VS.unsafeIndex u i < toGood else VS.unsafeIndex u i < toBad
            bad = VS.fromList (map fromEnum (drop 1 (scanl step False [0 .. nf - 1]))) :: VS.Vector Int
            step b i = if b then not (walk True i) else walk False i
            -- the most recent frame that arrived, for the concealers
            lastGood = VS.postscanl' (\g f -> if VS.unsafeIndex bad f == 0 then f else g)
                                     0 (VS.enumFromN (0 :: Int) nf)
        in VS.generate n $ \i ->
             let f = i `div` blk
             in if VS.unsafeIndex bad f == 0 then VS.unsafeIndex y i else
                  let g = VS.unsafeIndex lastGood f in case how of
                    Silence -> 0
                    HoldLast -> at (min (n - 1) (g * blk + blk - 1))
                    RepeatFrame -> at (g * blk + i `mod` blk)
          where at j = if j < 0 || j >= VS.length y then 0 else VS.unsafeIndex y j

    -- A buffer that under- or overruns splices rather than stretches:
    -- the source index jumps a whole frame every so often, and the
    -- receiver meets a discontinuity rather than a drift.
    slip x = case chSlip ch of
      Nothing -> x
      Just (_, 0) -> x
      Just (every, k) ->
        let n = VS.length x
            blk = max 1 (round (0.02 * fs)) :: Int
            per = max 1 (round (every * fs)) :: Int
        in VS.generate n $ \i ->
             let j = i - (i `div` per) * k * blk
             in if j < 0 || j >= n then 0 else VS.unsafeIndex x j

    -- Gain hits and dropouts: the same mechanism at two depths.  Time
    -- is cut into slots of the hit's own length and a slot either is a
    -- hit or is not, which is a Poisson process whenever the hits are
    -- rare compared with the slot.
    hits x = case chHits ch of
      Nothing -> x
      Just (Hits perSec forSec gainDb) ->
        let blk = max 1 (round (forSec * fs)) :: Int
            nb = VS.length x `div` blk + 1
            u = uniformNoise (seed 8) nb
            p = perSec * forSec
            g = fromDb gainDb
        in VS.imap (\i v -> if VS.unsafeIndex u (i `div` blk) < p then g * v else v) x

    -- Impulse noise.  Arrivals are Poisson; each one is a decaying
    -- sinusoid rather than a delta, because a click reaches the receiver
    -- through the line's own bandwidth and arrives ringing.
    impulse x = case chImpulse ch of
      Nothing -> x
      Just (Impulse perSec amp ringHz decay)
        | perSec <= 0 || amp == 0 -> x
        | otherwise ->
            let n = VS.length x
                u = uniformNoise (seed 7) n
                p = perSec / fs
                deltas = VS.generate n (\i -> if VS.unsafeIndex u i < p then 1 else 0)
                taps = max 1 (round (6 * decay * fs)) :: Int
                raw = VS.generate taps $ \k ->
                        let t = fromIntegral k / fs
                        in exp (negate t / decay) * sin (2 * pi * ringHz * t)
                peak = max 1e-12 (VS.maximum (VS.map abs raw))
                kern = VS.map (\v -> amp * v / peak) raw
            in VS.zipWith (+) x (fir kern deltas)

-- | An echo path with several taps at fractional delays -- what a
-- hybrid actually returns.  'chEcho' is a single real tap at a whole
-- number of samples, which a linear FIR cancels exactly; a canceller
-- measured against that reports a number it will not repeat on a
-- telephone line.
--
-- Returns the reflection alone, not the signal plus its reflection.
echoPath :: [(Double, Double)] -> Signal -> Signal
echoPath taps x = VS.generate (VS.length x) $ \i ->
  sum [ g * sampleAt x (fromIntegral i - d) | (d, g) <- taps ]

-- | Abramowitz-Stegun style erf approximation, plenty for a coin flip.
erfApprox :: Double -> Double
erfApprox z =
  let t = 1 / (1 + 0.3275911 * abs z)
      y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-z * z)
  in if z >= 0 then y else -y
