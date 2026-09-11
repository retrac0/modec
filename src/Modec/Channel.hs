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
  , voipTrunk
  , longLoop
  , carbonHandset
  , tapeArchive
  , noisySwitched
  , profile
  , applyChannel
  , mixAt
  , echoPath
    -- * Shared with the block-at-a-time version
  , erfApprox
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
  | WowFlutter [(Double, Double)]
    -- ^ Speed variation, as @(deviation as a fraction, Hz)@ components
    -- summed: wow below about 6 Hz, flutter above it, scrape higher
    -- still.  The units are the ones DIN 45507 uses, because this is a
    -- transport impairment -- a cassette, an answering machine, an
    -- acoustically coupled recording -- and not something a telephone
    -- circuit does.  A line's equivalents are 'chFreqOffsetHz' and
    -- 'chRateOffset', which are separate and stay separate.
    --
    -- The delay is the integral of the speed deviation, not the
    -- deviation itself, so a component of amplitude @A@ at @f@ Hz swings
    -- the delay by @A*fs\/(2*pi*f)@ samples either way: the same one
    -- percent is 25 samples at 0.5 Hz and a fifth of a sample at 30 Hz.
    -- What stays fixed is the frequency deviation, @A@ times the
    -- carrier, whatever the rate -- one percent is ±18 Hz on V.32's
    -- 1800 Hz carrier, which is past where a call stops connecting at
    -- all.  Real cassette wow and flutter is 0.1 to 0.3 percent.
    --
    -- 'SineJitter' is this impairment held by the other end: it makes
    -- the /delay/ sinusoidal rather than its integral, so @SineJitter 3
    -- 2@ at 8 kHz is 0.47 percent of speed deviation.  Components are
    -- summed before interpolating rather than cascaded, so that however
    -- many there are the signal is resampled once.
  deriving (Show)

-- | A memoryless nonlinearity: the same input sample always gives the
-- same output sample, so it makes harmonics and intermodulation but no
-- delay.  Between them these cover what an overdriven amplifier, a
-- saturating transformer or a carbon microphone does to a signal.
data Nonlinearity
  = SoftClip Double
    -- ^ @tanh (d*x) \/ d@: unity gain for a small signal, saturating at
    -- @1\/d@ for a large one, which is what a saturating amplifier does.
    -- Odd symmetric, so it makes only odd harmonics -- the third, the
    -- fifth, and nothing at twice the fundamental.
    --
    -- Normalising by @tanh d@ instead would hold full scale at full
    -- scale, and would be wrong: its small-signal gain is @d \/ tanh d@,
    -- which is 3 at a drive of 3 and 5 at a drive of 5.  That is an
    -- amplifier, not a distortion, and every level-dependent stage
    -- downstream would quietly be measuring something else.
  | Polynomial Double Double
    -- ^ @x + a2*x^2 - a3*x^3@, with the mean removed, and @a3@ held at
    -- or below 1\/3.
    --
    -- The second-order term is the one that matters on a V.22 call.
    -- Twice the low channel's 1200 Hz carrier is 2400 Hz, the high
    -- channel's carrier; and with both directions on the line the
    -- second-order /intermodulation/ is worse still, because the
    -- difference product of 2400 and 1200 lands back on 1200 -- 6 dB
    -- above the harmonic, and on the originating receiver rather than
    -- the answering one.
    --
    -- Two constraints, both of which bite.  Past @a3 = 1\/3@ the curve
    -- folds: its slope reaches zero inside the range and the output gets
    -- /quieter/ as the input grows, which is not a distorting device but
    -- a broken one, so @a3@ is clamped.  And @a2*x^2@ has a mean of
    -- @a2*rms^2@, a DC offset a transformer-coupled amplifier would not
    -- pass, so it is subtracted.
    --
    -- For a calibration: a sine of amplitude @A@ comes out with its
    -- second harmonic at @a2*A\/2@ and its third at @a3*A^2\/4@ relative
    -- to the fundamental.
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
-- Worth knowing before writing a test against 'RepeatFrame': at 8 kHz
-- a 20 ms frame is exactly 160 samples, so repeating one advances any
-- carrier that is a multiple of 50 Hz by a whole number of cycles.
-- Every carrier in this project is -- 1070, 1270, 1650, 1800, 2025,
-- 2100, 2225, 2400 -- so frame repetition introduces no carrier phase
-- step at all.  The damage is to the data and to symbol timing, and a
-- test looking for a phase hit will measure exactly zero.
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
  , chSnrDb        :: !(Maybe Double)
    -- ^ AWGN, full-band SNR against the signal /as it arrives at this
    -- stage/, which is the last one.  So the ratio is what is held
    -- fixed, not the noise power: a channel that compresses, companded
    -- or resonant, is quieter by the time the noise is added, and its
    -- noise is quieter with it.  Two conditions at \"SNR 12 dB\" are
    -- therefore not the same noise floor unless everything upstream of
    -- them matches.
    -- * The handset and the loop
  , chNonlin       :: !(Maybe Nonlinearity)
  , chWobble       :: !(Maybe (Double, Double))
    -- ^ (peak deviation in Hz, rate in Hz) -- a carrier supply that will
    -- not sit still, which is 'chFreqOffsetHz' with a wobble on it
    -- rather than a constant.
  , chPhaseJitter  :: !(Maybe (Double, Double))
    -- ^ (degrees peak to peak, Hz) -- phase modulation of the carrier
    -- at power-line rates, 20 to 300 Hz, which is how the ITU specifies
    -- it and how a line is measured.  Ten degrees peak to peak is the
    -- limit for a good circuit; a few degrees is ordinary.
    --
    -- Not the same impairment as wow, and not interchangeable with it.
    -- A delay modulation shifts every component in proportion to its
    -- frequency; carrier phase jitter shifts every component by the same
    -- angle.  For a tone the two are indistinguishable, but V.32
    -- occupies 600 to 3000 Hz, and standing in for one with the other
    -- would put five times more jitter at the top of that band than at
    -- the bottom.
    --
    -- Sinusoidal phase modulation of @b@ radians peak at @f@ is
    -- identical to frequency modulation of @b*f@ Hz peak, so this and
    -- 'chWobble' are one mechanism with two dials: 15 degrees peak to
    -- peak at 60 Hz is 0.131 rad, which is 7.9 Hz of deviation -- next
    -- to the 7 Hz of /static/ offset the V.32 tests treat as a hard
    -- case, which is why a realistic setting is a few degrees.
  , chSing         :: !(Maybe (Double, Double))
    -- ^ (round-trip delay in seconds, loop gain) -- a four-wire circuit
    -- close to its singing margin, where the signal goes round the loop
    -- through two hybrids and comes back at almost the level it left.
    -- It rings, which is the point: the response is a comb with peaks
    -- every 1\/delay Hz and a ring-down of @gain^k@.
    --
    -- This, and not a resonator, is how a telephone circuit rings.  A
    -- single resonant pole pair was the obvious thing to reach for and
    -- it models nothing here: bridged taps are notches and they are a
    -- DSL problem anyway, and loading coils are a low-pass ladder that
    -- 'chBandpass' and 'chDelayDist' already stand in for.  Feedback
    -- round a loop is the mechanism that is really there.
    --
    -- The gain is clamped below 1: at 1 the circuit is not ringing, it
    -- is oscillating, and the simulation would never return.
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
  , chNonlin = Nothing, chWobble = Nothing, chPhaseJitter = Nothing
  , chSing = Nothing, chEchoTaps = []
  , chCodec = Nothing, chBitError = Nothing, chStuck = Nothing, chLoss = Nothing
  , chSlip = Nothing
  , chImpulse = Nothing, chHits = Nothing }

-- | A plain but realistic analogue line: 300-3400 Hz band, given SNR.
telephoneChannel :: Double -> Channel
telephoneChannel snr = idealChannel { chBandpass = Just (300, 3400), chSnrDb = Just snr }

-- | The path every recording in @test\/fixtures\/live@ came through:
-- G.711 on a voip.ms trunk, a POTS line and an ATA at the far end.
--
-- Calibrated rather than guessed.  Take the steadiest tone in a
-- recording and measure how far it stands above everything else in the
-- band: the codec-free loopback in @docs\/recordings\/v32@ manages 34 dB,
-- and real trunk calls manage 21.6 dB (a V.22bis call) and 16.8 dB (a
-- V.32 one).  Companding alone accounts for about 37 dB, so it is not
-- the whole story -- the rest is the resampling the capture path does
-- and the analogue loop at the far end, which is why there is noise
-- here as well as a codec.
--
-- The loss rate is deliberately far below anything that would break a
-- call.  A 20 ms frame is twelve symbols at V.22's 600 baud and
-- forty-eight at V.32's 2400, so a lost frame is not recoverable
-- whatever conceals it; one percent loss takes down every call there
-- is.  What is interesting at this rate is whether the receiver comes
-- back, and how fast.
voipTrunk :: Channel
voipTrunk = idealChannel
  { chBandpass = Just (300, 3400)
  , chCodec = Just Ulaw
  , chLoss = Just (Loss 0.02 0.0002 0.5 RepeatFrame)
  , chSnrDb = Just 24
  }

-- | A long subscriber loop: quiet, band-limited hard, and with the
-- group delay a loading-coil ladder leaves at the band edges.
longLoop :: Channel
longLoop = idealChannel
  { chBandpass = Just (300, 3000)
  , chDelayDist = 3
  , chGain = fromDb (-12)
  , chSnrDb = Just 26
  }

-- | A call held in front of a handset rather than wired to a line: a
-- carbon microphone's asymmetric distortion, a narrow band, and mains
-- hum picked up on the way.
carbonHandset :: Channel
carbonHandset = idealChannel
  { chNonlin = Just (Polynomial 0.12 0.25)
  , chBandpass = Just (400, 2800)
  , chHum = Just (60, 0.02)
  , chSnrDb = Just 22
  }

-- | Modem audio recovered from a cassette.  Wow at the capstan's
-- rotation, flutter above it, and scrape flutter higher still; 0.25
-- percent unweighted is an ordinary domestic deck, and rather more than
-- a telephone line ever does.  Not a channel: a medium.
tapeArchive :: Channel
tapeArchive = idealChannel
  { chJitter = WowFlutter [(0.0015, 0.9), (0.001, 6), (0.0005, 33)]
  , chBandpass = Just (300, 3400)
  , chSnrDb = Just 32
  , chDropout = Just (0.02, 0.001)
  }

-- | A switched connection having a bad day: impulse noise from
-- switching, transient hits, hum, and a moderate noise floor.
noisySwitched :: Channel
noisySwitched = idealChannel
  { chBandpass = Just (300, 3400)
  , chImpulse = Just (Impulse 3 0.25 1400 0.002)
  , chHits = Just (Hits 0.5 0.006 (-4))
  , chHum = Just (50, 0.01)
  , chSnrDb = Just 24
  }

-- | Look a profile up by name, for a command line.
profile :: String -> Maybe Channel
profile n = lookup n
  [ ("ideal", idealChannel), ("telephone", telephoneChannel 25)
  , ("voip", voipTrunk), ("long-loop", longLoop), ("handset", carbonHandset)
  , ("tape", tapeArchive), ("noisy", noisySwitched) ]

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
      . wobble . freqOff . delayDist . bandpass . sing . echo . nonlin . gainDc
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
        | otherwise -> VS.map (\v -> tanh (d * v) / d) x
      Just (Polynomial a2 a30) ->
        let a3 = min (1 / 3) a30
            y = VS.map (\v -> v + a2 * v * v - a3 * v * v * v) x
            dc = if VS.null y then 0 else VS.sum y / fromIntegral (VS.length y)
        in VS.map (subtract dc) y

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
      WowFlutter comps ->
        let ms = [ (a * fs / (2 * pi * f), f) | (a, f) <- comps, f > 0, a /= 0 ]
            -- enough bias to keep the read inside the signal, and clear
            -- of the interpolator's six-sample reach
            d0 = 2 * sum (map fst ms) + 6
        in if null ms then x else
             VS.generate (VS.length x) $ \i ->
               let t = fromIntegral i / fs
                   d = d0 - sum [ m * (1 - cos (2 * pi * f * t)) | (m, f) <- ms ]
               in sampleAtFast x (fromIntegral i - d)

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

    -- Frequency and phase modulation of the carrier, which are the same
    -- thing written two ways, so they share one phase and one pass.
    -- 'frequencyShift' does this with a fixed rate; the oscillator is
    -- what has to change, not the Hilbert pair it mixes against.
    --
    -- The Hilbert is 129 taps at 8 kHz, so the first and last 64 samples
    -- are computed against a truncated kernel and the unwanted sideband
    -- is not cancelled there.  Anything measuring instantaneous phase
    -- has to discard them.
    wobble x = case (chWobble ch, chPhaseJitter ch) of
      (Nothing, Nothing) -> x
      (w, j) ->
        let n = VS.length x
            xq = firCentered (firHilbert (2 * round (fs / 125) + 1)) x
            ph = VS.generate n $ \i ->
                   let t = fromIntegral i / fs
                       fm = case w of
                         Just (dev, rate) | rate > 0 -> (dev / rate) * (1 - cos (2 * pi * rate * t))
                         _ -> 0
                       pm = case j of
                         Just (deg, rate) -> (deg * pi / 360) * sin (2 * pi * rate * t)
                         _ -> 0
                   in fm + pm
        in VS.izipWith (\i v q -> let p = VS.unsafeIndex ph i
                                  in v * cos p - q * sin p) x xq

    -- A signal that goes round the loop and comes back, over and over.
    -- 'VS.constructN' is what makes it expressible: each output sample
    -- can read the ones already written, which is exactly what feedback
    -- needs and what none of the other stages do.
    sing x = case chSing ch of
      Nothing -> x
      Just (secs, g0) ->
        let d = max 1 (round (secs * fs)) :: Int
            g = max (-0.95) (min 0.95 g0)
            n = VS.length x
        in VS.constructN n $ \acc ->
             let i = VS.length acc
                 back = if i >= d then VS.unsafeIndex acc (i - d) else 0
             in VS.unsafeIndex x i + g * back

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
