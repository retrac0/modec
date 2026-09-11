{-# LANGUAGE BangPatterns #-}
-- | A quadrature amplitude modulation pump with nothing baked in: the
-- baud rate, carrier, pulse shaping and constellation all arrive as
-- parameters.  Both of this modem's passband receivers are this one --
-- V.32 and V.32bis at 2400 baud through "Modec.V32Pump", V.22 and
-- V.22bis at 600 through "Modec.V22".
--
-- This module's header used to argue the other way: that V.22 was a
-- sibling rather than a refactor, because its loop gains, roll-off and
-- tap count are measured numbers for five working modes and none of
-- them is right at 2400 baud.  Every word of that is still true, and
-- none of it was ever an argument for a second copy of the machine --
-- the constants are exactly what 'QamParams' and 'QamRxCfg' are for.
-- What the fork actually cost was that the AGC, the Gardner update, the
-- carrier loop and the T\/2 LMS existed twice, byte for byte, so a fix
-- to one receiver did not reach the other.
--
-- Merging them turned up four differences that nobody had chosen: the
-- two receivers started from different initial conditions, counted a
-- symbol as bad at a different error from the one they froze the
-- equaliser at, gated the timing loop on different evidence, and
-- disagreed about whether a restart empties the equaliser's delay line.
-- Each is a field now ('QamRxSeed', 'qrEvmBad', 'qrSteerAt', 
-- 'qrResetLine'), each defaulting to what V.32 always did, and each
-- carrying what it is for -- which is more than any of them had while
-- there were two loops for a reader to notice the difference between.
--
-- The receiver is a chain of: complex downconversion at the nominal
-- carrier, a matched root-raised-cosine filter, Gardner symbol timing
-- with cubic interpolation, automatic gain, a decision-directed carrier
-- phase and frequency loop, and a T\/2-spaced complex LMS equaliser.
-- What it does /not/ contain is any notion of what the points mean:
-- the caller supplies a slicer and its inverse, so the trellis decoder
-- of "Modec.V32" can sit outside and take its time without the carrier
-- loop having to wait for it.
module Modec.QAM
  ( -- * Parameters
    QamParams (..)
  , QamRxCfg (..)
  , defaultRxCfg
  , samplesPerSymbol
    -- * Transmitter
  , QamTxState
  , qamTxInit
  , qamTxBlock
  , qamTxSymbolsFor
    -- * Receiver
  , QamRxState
  , qamRxInit
  , qamRxBlock
  , qamReceiver
  , qamReceiverFrom
  , QamSym (..)
  , qamRxEvm
  , qamRxSps
  , agcSettleSyms
  , qamRxPower
  , qamRxTheta
  , qamRxFreq
  , qamRxReset
  , qamRxTiming
  , qamRxUnlock
  , quarterTurns
  , qamRxEnergy
  , qamRxPrevSym
    -- * Training on symbols that are known rather than decided
  , qamRxRef
  , qamRxRefLeft
    -- * Starting a receiver somewhere other than at rest
  , QamRxSeed (..)
  , defaultSeed
  , qamRxInitWith
    -- * What a restart predicate sees
  , QamTap (..)
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP
import Modec.Stream (Stage (..))

-- | The line parameters of a pump.
data QamParams = QamParams
  { qpFs      :: !Double   -- ^ sample rate
  , qpBaud    :: !Double   -- ^ symbol rate
  , qpCarrier :: !Double   -- ^ carrier frequency
  , qpRollOff :: !Double   -- ^ root-raised-cosine roll-off
  , qpSpan    :: !Double   -- ^ pulse span each side, in symbols
  , qpGuard   :: Maybe (Double, Double)
    -- ^ a guard tone added to the transmitted signal: its frequency in
    -- Hz and its amplitude relative to the block's.  V.22's answering
    -- modem puts 1800 Hz on the high channel at half amplitude
    -- (§2.4\/V.22bis); V.32 has none.
    --
    -- It is summed inside the sample expression rather than added as a
    -- second vector afterwards, which is not fussiness: the two differ
    -- in the last bit, and this signal is compared against a recording
    -- of what the code used to emit.
  } deriving (Eq, Show)

samplesPerSymbol :: QamParams -> Double
samplesPerSymbol p = qpFs p / qpBaud p

-- | Receiver tuning, and the constellation it decides against.
data QamRxCfg = QamRxCfg
  { qrKp        :: !Double  -- ^ Gardner proportional gain, acquiring
  , qrKi        :: !Double  -- ^ Gardner integral gain, acquiring (tracks the far clock)
  , qrKpTrack   :: !Double  -- ^ the same, once the loop is on the pulse
  , qrKiTrack   :: !Double  -- ^ the same, once the loop is on the pulse
  , qrTrackAt   :: !Double  -- ^ decision error power under which the timing loop narrows (0: never)
  , qrClamp     :: !Double  -- ^ limit on the timing error estimate
  , qrThKp      :: !Double  -- ^ carrier loop proportional gain, per symbol
  , qrThKi      :: !Double  -- ^ carrier loop integral gain, per symbol
  , qrEqMu      :: !Double  -- ^ LMS step
  , qrEqTaps    :: !Int     -- ^ equaliser taps, T/2 spaced
  , qrAgcRate   :: !Double  -- ^ step of the power estimate the gain divides by
  , qrAgcSettled :: !Double -- ^ the same after 'agcSettleSyms' symbols
  , qrLockAt    :: !Double  -- ^ decision error power under which the receiver counts as converged
  , qrLoopGate  :: !Double  -- ^ once converged, a symbol further than this (squared) from any point steers nothing
  , qrEvmFreeze :: !Double  -- ^ stop adapting above this decision error power
  , qrEvmGiveUp :: !Int     -- ^ symbols of bad decisions before starting over
  , qrAdapt     :: !Bool    -- ^ let the equaliser and the watchdog move at all
  , qrTrack     :: !Bool    -- ^ let the carrier loop follow the decisions
  , qrPower     :: !Double  -- ^ mean square of the constellation (the AGC target)
  , qrSlice     :: (Double, Double) -> Int          -- ^ nearest point, as an index
  , qrPoint     :: Int -> (Double, Double)          -- ^ that index back to a point
    -- The fields below exist so that V.22 can be this receiver too.
    -- Each reproduces one measured difference between the two pumps as a
    -- parameter, and each default is what V.32 always did.
  , qrSteerAt   :: !Int
    -- ^ distinct decisions among the last eight before the timing loop
    -- is steered at all.  A steady tone is one point and says nothing
    -- about where in the symbol the samples fall; V.32 wants two, V.22
    -- steers unconditionally.
  , qrAdaptAt   :: !Int
    -- ^ ...and before the equaliser adapts.  A one- or two-point signal
    -- has a singular autocorrelation and runs the taps away.
  , qrAdaptRun  :: !Int
    -- ^ consecutive identical phase steps that also stop the equaliser:
    -- V.22's way of asking the same question of unscrambled ones.
  , qrFreqFf    :: !Double
    -- ^ weight of the measured phase-step deviation in the frequency
    -- estimate, or 0 for none.  The differential path sees the carrier
    -- offset directly, as the signed deviation of each step from the
    -- nearest quarter turn; V.22 uses it to pull the estimate in during
    -- 1200 bit/s training so the decision-directed loop only tracks the
    -- residual.
  , qrFreqFfRun :: !Int     -- ^ ...suppressed while the step has been constant this long
  , qrEvmBad    :: !(Maybe Double)
    -- ^ decision error power a symbol is counted as bad above, towards
    -- 'qrEvmGiveUp'; 'Nothing' means 'qrEvmFreeze', which is what it
    -- always was.
  , qrRestartOn :: QamTap -> Bool
    -- ^ start the coherent path over on this symbol, in addition to the
    -- give-up count.  Mid-block, because it has to take effect for the
    -- rest of the block: V.22 restarts on the 93rd symbol of the
    -- answerer's unscrambled ones.
  , qrResetLine :: !Bool
    -- ^ whether a restart also empties the equaliser's delay line.  It
    -- holds recent input, so keeping it means the first outputs after a
    -- restart mix the old signal with the new, and clearing it means the
    -- equaliser spends its span with nothing behind it.  V.32 clears;
    -- V.22 keeps.  Both were the unexamined consequence of where each
    -- fork happened to write its reset, and each is what its own
    -- measurements were taken against.
  }

-- | What 'qrRestartOn' is shown, per symbol.
data QamTap = QamTap
  { qtStep    :: !Int      -- ^ this symbol's phase step, in quarter turns
  , qtStepRun :: !Int      -- ^ how many symbols running it has been the same
  , qtEvm     :: !Double   -- ^ the tracked decision error power
  , qtSyms    :: !Int      -- ^ symbols since the receiver last started over
  }

-- | Gains that work at 2400 baud with a unit-mean-power constellation.
--
-- The carrier loop's integral gain is deliberately small.  A trellis
-- decoder integrates over its whole traceback, so residual phase jitter
-- costs it far more than it costs an uncoded slicer that judges each
-- symbol alone: at V.22's gains the coded 9600 alternative was losing
-- 453 symbols where the uncoded one lost none, which is the coding gain
-- running backwards.  It was the integral term doing that, not the
-- proportional one, which took a fuzzing sweep to separate: halving both
-- fixed the trellis and quietly shrank the carrier pull-in range to
-- about -18 to +15 Hz, so a 1 % clock offset -- which moves the carrier
-- 18 Hz, since resampling moves every frequency -- acquired in one
-- direction and wound the loop up the wrong way in the other.  A
-- proportional gain of 0.08 with the integral left at 0.0015 acquires
-- both, follows timing wander a good deal better, and leaves the trellis
-- alone.
-- The equaliser spans 31 T\/2 taps, about 15 symbols or 6.5 ms, which is
-- the group delay a telephone connection actually smears a 2400 baud
-- signal over; V.22's 15 taps cover the same milliseconds at a quarter
-- of the rate and would cover a quarter of the distortion here.
defaultRxCfg :: ((Double, Double) -> Int) -> (Int -> (Double, Double)) -> QamRxCfg
defaultRxCfg slice point = QamRxCfg
  { qrKp = 0.12, qrKi = 0.0015
  , qrKpTrack = 0.12, qrKiTrack = 0.0015, qrTrackAt = 0
  , qrClamp = 2
  , qrThKp = 0.08, qrThKi = 0.0015
  , qrEqMu = 0.002, qrEqTaps = 31
  , qrAgcRate = 0.05, qrAgcSettled = 0.05
  , qrLockAt = 1 / 0, qrLoopGate = 1 / 0
  , qrEvmFreeze = 0.4, qrEvmGiveUp = 200, qrAdapt = True, qrTrack = True, qrPower = 1
  , qrSlice = slice, qrPoint = point
  , qrSteerAt = 2, qrAdaptAt = 3, qrAdaptRun = maxBound
  , qrFreqFf = 0, qrFreqFfRun = maxBound
  , qrEvmBad = Nothing, qrRestartOn = const False, qrResetLine = True }

-- | Transmitter state.  Symbols are held on a fractional clock and the
-- pulse is evaluated per output sample, so no sample rate divides the
-- baud rate evenly and none has to.
data QamTxState = QamTxState
  { txSymClock :: !Double
  , txSymT0    :: !Double
  , txSymbols  :: [(Double, Double)]
  , txCarrier  :: !Double
  , txGuard    :: !Double   -- ^ guard tone phase, if 'qpGuard' asks for one
  }

qamTxInit :: QamTxState
qamTxInit = QamTxState 0 0 [] 0 0

-- | How many symbols a block of @n@ samples will consume.  Useful when
-- the symbols are expensive to produce, or come from a coder that must
-- not be run speculatively.
qamTxSymbolsFor :: QamParams -> Int -> QamTxState -> Int
qamTxSymbolsFor p n st = length (takeWhile (<= limit) clocks)
  where
    sps = samplesPerSymbol p
    limit = fromIntegral n + qpSpan p * sps
    clocks = iterate (+ sps) (txSymClock st)

-- | Generate @n@ samples at amplitude @amp@, drawing symbols from the
-- given list and returning whatever is left of it.
qamTxBlock :: QamParams -> Double -> Int -> [(Double, Double)] -> QamTxState
           -> (QamTxState, Signal, [(Double, Double)])
qamTxBlock p amp n syms0 st0 = (st', sig, rest)
  where
    sps = samplesPerSymbol p
    wc = 2 * pi * qpCarrier p / qpFs p
    span_ = qpSpan p
    wg = case qpGuard p of
      Just (f, _) -> 2 * pi * f / qpFs p
      Nothing -> 0

    -- pull symbols until the pulse tails of every sample in this block
    -- are covered
    fill st syms
      | txSymClock st > fromIntegral n + span_ * sps = (st, syms)
      | otherwise = case syms of
          [] -> (st, [])
          (s : more) ->
            let st1 = st { txSymbols = txSymbols st ++ [s]
                         , txSymClock = txSymClock st + sps
                         , txSymT0 = if null (txSymbols st) then txSymClock st else txSymT0 st }
            in fill st1 more
    (stF, rest) = fill st0 syms0

    symsRe = VS.fromList (map fst (txSymbols stF))
    symsIm = VS.fromList (map snd (txSymbols stF))
    nSyms = VS.length symsRe
    t0s = txSymT0 stF

    sig = VS.generate n $ \i ->
      let t = fromIntegral i
          kLo = max 0 (ceiling ((t - span_ * sps - t0s) / sps))
          kHi = min (nSyms - 1) (floor ((t + span_ * sps - t0s) / sps))
          accum !k !a !b
            | k > kHi = (a, b)
            | otherwise =
                let g = rrcPulse (qpRollOff p) ((t - (t0s + fromIntegral k * sps)) / sps)
                in accum (k + 1) (a + g * VS.unsafeIndex symsRe k) (b + g * VS.unsafeIndex symsIm k)
          (re, im) = accum kLo 0 0
          th = txCarrier stF + wc * t
          g = case qpGuard p of
            Just (_, ga) -> ga * sin (txGuard stF + wg * t)
            Nothing -> 0
      in amp * (re * cos th - im * sin th + g)

    dropN = max 0 (floor ((fromIntegral n - (span_ + 1) * sps - t0s) / sps)) :: Int
    st' = stF
      { txSymClock = txSymClock stF - fromIntegral n
      , txSymT0 = t0s + fromIntegral dropN * sps - fromIntegral n
      , txSymbols = drop dropN (txSymbols stF)
      , txCarrier = wrapTwoPi (txCarrier stF + wc * fromIntegral n)
      , txGuard = wrapTwoPi (txGuard stF + wg * fromIntegral n) }

-- | One decided symbol.
data QamSym = QamSym
  { qsPoint    :: !(Double, Double)  -- ^ equalised and derotated
  , qsIndex    :: !Int               -- ^ the immediate decision
  , qsError    :: !Double            -- ^ squared distance to it
  , qsRaw      :: !(Double, Double)  -- ^ before gain, carrier and equaliser
  , qsStep     :: !Int
    -- ^ phase advance from the previous raw symbol, in quarter turns:
    -- the differential decision, which owes nothing to the carrier loop
  , qsDev      :: !Double
    -- ^ that advance's signed deviation from the nearest quarter turn,
    -- in radians: the carrier offset, seen directly
  } deriving (Eq, Show)

-- | Phase advance from one symbol to the next, in quarter turns.
--
-- Taken from 'qsRaw', so it owes nothing to the carrier loop -- which
-- matters more than it sounds.  A decision-directed loop shown a signal
-- that alternates by a quarter turn every symbol will follow the
-- alternation rather than the carrier, absorbing part of it and leaving
-- a quarter-turn signal looking like a half-turn one.  The difference
-- between consecutive symbols is immune to that, and to the four-fold
-- ambiguity the loop settles into, which is why V.32 encodes its
-- start-up and its rate signals as quadrant changes in the first place.
quarterTurns :: [QamSym] -> (Double, Double) -> ([Int], (Double, Double))
quarterTurns syms prev0 = go prev0 syms []
  where
    go prev [] acc = (reverse acc, prev)
    go (pr, pim) (sy : rest) acc =
      let (yr, yi) = qsRaw sy
          dr = yr * pr + yi * pim
          di = yi * pr - yr * pim
          k = (round (atan2 di dr / (pi / 2)) :: Int) `mod` 4
      in go (yr, yi) rest (k : acc)

data QamRxState = QamRxState
  { rxN       :: !Int
  , rxHistRe  :: !Signal
  , rxHistIm  :: !Signal
  , rxPrevRe  :: !Signal
  , rxPrevIm  :: !Signal
  , rxTau     :: !Double
  , rxSps     :: !Double
  , rxPrevSym :: !(Double, Double)
  , rxPower_  :: !Double
  , rxTheta   :: !Double
  , rxFreq    :: !Double
  , rxEqRe    :: !Signal
  , rxEqIm    :: !Signal
  , rxLineRe  :: !Signal
  , rxLineIm  :: !Signal
  , rxEvm_    :: !Double
  , rxBad     :: !Int
  , rxRecent  :: [Int]
  , rxLocked  :: !Bool   -- ^ the error has once been under 'qrLockAt'
  , rxSyms    :: !Int    -- ^ symbols since the receiver last started over
  , rxTiming  :: !Bool   -- ^ the timing loop has been on the pulse and may narrow
  , rxLastStep :: !Int   -- ^ the previous symbol's phase step
  , rxStepRun :: !Int    -- ^ symbols running the step has been the same
  , rxEnergy  :: !Double -- ^ mean matched-filter power over the last block
  , rxRef     :: [(Double, Double)]
    -- ^ Points the far end is known to have sent, oldest first, one per
    -- symbol, consumed as they are used.  Empty is the ordinary
    -- decision-directed receiver and is bit for bit what it always was.
    -- See 'qamRxRef'.
  , rxSeed    :: QamRxSeed  -- ^ where 'qamRxReset' returns to
  }

-- | The values a receiver starts from.  Two pumps that are one machine
-- did not start it in the same place: V.22 began a symbol later, with
-- the previous symbol at (1, 0) and the error estimate at 1, and the
-- first Gardner error and first differential step follow from those.
data QamRxSeed = QamRxSeed
  { srTau0   :: !Double            -- ^ added to the timing offset, in samples
  , srPower0 :: !Double            -- ^ the power estimate
  , srEvm0   :: !Double            -- ^ the error estimate, here and after every reset
  , srPrev0  :: !(Double, Double)  -- ^ the previous raw symbol
  } deriving (Eq, Show)

-- | At rest, which is where V.32 always started.
defaultSeed :: QamRxSeed
defaultSeed = QamRxSeed 0 0 0 (0, 0)

qamRxInit :: QamParams -> QamRxCfg -> QamRxState
qamRxInit p cfg = qamRxInitWith p cfg defaultSeed

qamRxInitWith :: QamParams -> QamRxCfg -> QamRxSeed -> QamRxState
qamRxInitWith p cfg seed = QamRxState
  { rxN = 0
  , rxHistRe = VS.replicate hist 0, rxHistIm = VS.replicate hist 0
  , rxPrevRe = VS.replicate carry 0, rxPrevIm = VS.replicate carry 0
  , rxTau = fromIntegral carry + srTau0 seed, rxSps = sps
  , rxPrevSym = srPrev0 seed, rxPower_ = srPower0 seed
  , rxTheta = 0, rxFreq = 0
  , rxEqRe = centreTap, rxEqIm = VS.replicate taps 0
  , rxLineRe = VS.replicate taps 0, rxLineIm = VS.replicate taps 0
  , rxEvm_ = srEvm0 seed, rxBad = 0, rxRecent = [], rxLocked = False, rxSyms = 0
  , rxTiming = False
  , rxLastStep = 0, rxStepRun = 0, rxEnergy = 0, rxSeed = seed, rxRef = [] }
  where
    sps = samplesPerSymbol p
    taps = qrEqTaps cfg
    hist = VS.length (kernel p) - 1
    carry = 4 + ceiling sps
    centreTap = VS.generate taps (\i -> if i == 2 * (taps `div` 4) then 1 else 0)

-- | Restart the carrier loop and the equaliser, keeping symbol timing.
-- The start-up sequence has several points where the far end stops and
-- starts again, and carrying a stale carrier phase across one of those
-- costs longer than starting over.
qamRxReset :: QamParams -> QamRxCfg -> QamRxState -> QamRxState
qamRxReset _ cfg st = st
  { rxTheta = 0, rxFreq = 0
  , rxEqRe = VS.generate taps (\i -> if i == 2 * (taps `div` 4) then 1 else 0)
  , rxEqIm = VS.replicate taps 0
  , rxLineRe = if qrResetLine cfg then VS.replicate taps 0 else rxLineRe st
  , rxLineIm = if qrResetLine cfg then VS.replicate taps 0 else rxLineIm st
  , rxEvm_ = srEvm0 (rxSeed st), rxBad = 0, rxRecent = [], rxLocked = False, rxSyms = 0
  , rxTiming = False }
  where taps = qrEqTaps cfg

-- | Forget that the receiver has ever locked, keeping everything it has
-- learned.  For a handover: the carrier, timing and equaliser trained
-- on one constellation are exactly what the next one should start
-- from, but whether they read the next one is not yet known, and the
-- things that hang off having locked -- the slow gain, the tight freeze
-- -- would otherwise be applied before it has had the chance.
qamRxUnlock :: QamRxState -> QamRxState
qamRxUnlock st = st { rxLocked = False, rxSyms = 0 }

kernel :: QamParams -> VS.Vector Double
kernel p = rrcKernel (qpFs p) (qpBaud p) (qpRollOff p) (qpSpan p)

-- | Hand the receiver the points the far end is known to be sending,
-- oldest first.  They are consumed one per symbol and replace the
-- receiver's own decision as what the carrier loop and the equaliser
-- train against; when they run out it is decision-directed again.
--
-- Appended, not replaced: a caller topping the queue up each block
-- cannot lose the ones the last block did not reach.
qamRxRef :: [(Double, Double)] -> QamRxState -> QamRxState
qamRxRef ps st = st { rxRef = rxRef st ++ ps }

-- | How many known points are still queued.
qamRxRefLeft :: QamRxState -> Int
qamRxRefLeft = length . rxRef

qamRxEvm :: QamRxState -> Double
qamRxEvm = rxEvm_

-- | Mean matched-filter power over the last block.
qamRxEnergy :: QamRxState -> Double
qamRxEnergy = rxEnergy

-- | The last raw symbol, before gain, carrier or equaliser.  What a
-- differential decision on the next block's first symbol is measured
-- against.
qamRxPrevSym :: QamRxState -> (Double, Double)
qamRxPrevSym = rxPrevSym

-- | How long the gain chases its own input before settling down.  A
-- fifth of a second at 2400 baud: long enough for a receiver starting
-- cold to find the level, short enough to be over inside B1 and the
-- first bytes for one that was handed a trained receiver.
agcSettleSyms :: Int
agcSettleSyms = 512

-- | Whether the timing loop has narrowed: see 'qrTrackAt'.  For tests
-- and for tracing, where it is the first thing to ask of a receiver
-- that is reading the line well and still getting the answer wrong.
qamRxTiming :: QamRxState -> Bool
qamRxTiming = rxTiming

qamRxSps :: QamRxState -> Double
qamRxSps = rxSps

qamRxPower :: QamRxState -> Double
qamRxPower = rxPower_

qamRxTheta :: QamRxState -> Double
qamRxTheta = rxTheta

qamRxFreq :: QamRxState -> Double
qamRxFreq = rxFreq

-- | The receiver as a stream stage, so it composes with the rest of the
-- chain and does not care how the audio is cut up.  The configuration is
-- fixed for the life of the stage; a start-up that has to change slicer
-- part way through drives 'qamRxBlock' directly instead.
qamReceiver :: QamParams -> QamRxCfg -> Stage Signal [QamSym]
qamReceiver p cfg = qamReceiverFrom p cfg (qamRxInit p cfg)

-- | 'qamReceiver' resuming from a receiver that has already run.
qamReceiverFrom :: QamParams -> QamRxCfg -> QamRxState -> Stage Signal [QamSym]
qamReceiverFrom p cfg st0 = Stage st0 (\st chunk -> qamRxBlock p cfg chunk st)

-- | Demodulate one block, returning the symbols it completed.
qamRxBlock :: QamParams -> QamRxCfg -> Signal -> QamRxState -> (QamRxState, [QamSym])
qamRxBlock p cfg chunk st0 = (st', symsOut)
  where
    n = VS.length chunk
    wc = 2 * pi * qpCarrier p / qpFs p
    n0 = rxN st0
    -- Phase continuity across blocks comes from the global sample index,
    -- so a block boundary is not an event the receiver can see.
    (mixRe, mixIm) = mixDownAt wc n0 chunk
    hrev = VS.reverse (kernel p)
    (mfRe, histRe') = firStream hrev (rxHistRe st0) mixRe
    (mfIm, histIm') = firStream hrev (rxHistIm st0) mixIm
    extRe = rxPrevRe st0 VS.++ mfRe
    extIm = rxPrevIm st0 VS.++ mfIm
    len = VS.length extRe
    taps = qrEqTaps cfg
    nominalSps = samplesPerSymbol p

    go st syms
      | floor (rxTau st) + 2 >= len = (st, reverse syms)
      | rxTau st - rxSps st / 2 < 1 = go st { rxTau = rxTau st + rxSps st } syms
      | otherwise =
          let tau = rxTau st
              sps = rxSps st
              yr = cubicAt extRe tau; yi = cubicAt extIm tau
              hr = cubicAt extRe (tau - sps / 2); hi = cubicAt extIm (tau - sps / 2)
              (pr, pim) = rxPrevSym st
              -- The differential decision: this symbol against the
              -- last, before gain, carrier or equaliser.  The same
              -- arithmetic as 'quarterTurns', per symbol, because three
              -- things inside the loop act on it for the *next* symbol
              -- -- the frequency feed-forward, the equaliser gate and
              -- the restart predicate -- and could not be fed from
              -- outside.
              dr = yr * pr + yi * pim
              di = yi * pr - yr * pim
              ang = atan2 di dr
              step = (round (ang / (pi / 2)) :: Int) `mod` 4
              dev = ang - fromIntegral (round (ang / (pi / 2)) :: Int) * (pi / 2)
              stepRun = if step == rxLastStep st then rxStepRun st + 1 else 0
              -- The gain's own noise.  The power estimate is an
              -- exponential mean of the symbol samples, and the gain is
              -- the square root of its reciprocal: on the four training
              -- states, all at one amplitude, the estimate is exact, and
              -- on a hundred and twenty-eight points spread over nine
              -- amplitudes a twenty-symbol mean of them wanders by
              -- several percent -- which multiplies straight into every
              -- symbol as a decision error of about 0.05, at any signal
              -- to noise ratio at all, and nothing downstream can take
              -- it back out.  Measured on the bench at 45 dB: 0.063 with
              -- the fast estimate, 0.040 with a slow one.  The fast one
              -- is still what acquisition needs -- a receiver starting
              -- cold has to find the level before it can read anything --
              -- so it runs for 'agcSettleSyms' and the slow one after.
              -- Elapsed symbols and not "once it has read the line":
              -- with the fast estimate running, the error at 14400 never
              -- gets down to where a lock would be declared, because the
              -- fast estimate is most of the error.
              pwA = if rxSyms st >= agcSettleSyms then qrAgcSettled cfg else qrAgcRate cfg
              pw = (1 - pwA) * rxPower_ st + pwA * (yr * yr + yi * yi)
              eRaw = ((yr - pr) * hr + (yi - pim) * hi) / max 1e-9 pw
              e = if pw < 1e-5 then 0 else max (negate (qrClamp cfg)) (min (qrClamp cfg) eRaw)
              -- Acquiring and tracking, at two bandwidths.
              --
              -- The Gardner detector's output is not zero at the right
              -- sampling instant: it is a difference between symbols
              -- times a sample between them, so it carries the data as
              -- well as the timing, and the wider the spread of
              -- amplitudes in the constellation the more of it is data.
              -- A proportional gain puts that straight into the sampling
              -- instant, one symbol at a time, and the interpolator
              -- turns it back into a decision error that is white,
              -- isotropic, and the same absolute size whatever
              -- constellation is being read.
              --
              -- Which is why it was invisible.  Measured with no channel
              -- at all, at this gain: 0.028 at 9600 trellis, where the
              -- points are 0.22 apart, and 0.053 at 14400, where they
              -- are 0.11 -- 13 % of the decision margin, and 48 % of
              -- it.  Adding a telephone line and 34 dB of noise moved
              -- the second of those to 0.055.  The receiver's own timing
              -- loop was most of what stood between 14400 and the
              -- terminal, and no amount of line would have shown it.
              --
              -- It cannot simply be turned down: a receiver has to find
              -- the pulse before it can sit on it, and at a quarter of
              -- this gain a cold start takes a second where it took a
              -- quarter of one.  So it is turned down once, when the
              -- decisions say the loop is on the pulse -- 'qrTrackAt' --
              -- and turned back up by 'qamRxReset' and nothing else.
              -- The handover to the data constellation deliberately does
              -- not re-open it: the constellation changes there, the
              -- symbol clock does not, and the seam is the last place
              -- that wants a wide timing loop.
              (kpNow, kiNow) | rxTiming st = (qrKpTrack cfg, qrKiTrack cfg)
                             | otherwise = (qrKp cfg, qrKi cfg)
              -- And steered only by a signal that moves, which is
              -- 'moving' below.  A steady tone is one point: there is no
              -- transition in it, so what the detector reports is the
              -- carrier, and a loop that integrates that walks its clock
              -- away from the far end's for as long as the tone lasts.
              -- Measured across the answering ladder, which spends seven
              -- seconds on tones before it hears TRN: the symbol rate
              -- estimate had wandered 1400 ppm off by the time there was
              -- anything to read, and the restarts carry it forward,
              -- because 'qamRxReset' keeps the timing on purpose.  At
              -- the acquiring gain that is pulled back inside a second
              -- and nobody notices; a narrower loop wears it for
              -- thousands of symbols, which is a cost with nothing
              -- bought by it -- the wander was never information.
              steer = if moving then e else 0
              sps' = sps - kiNow * steer
              tau' = tau + max (0.5 * sps) (sps' - kpNow * steer)

              agc = if pw < 1e-9 then 0 else sqrt (qrPower cfg / pw)
              th = rxTheta st
              c = cos th; s = sin th
              rot a b = (agc * (a * c + b * s), agc * (b * c - a * s))
              (zmr, zmi) = rot hr hi
              (zr, zi) = rot yr yi
              lineRe = VS.cons zr (VS.cons zmr (VS.take (taps - 2) (rxLineRe st)))
              lineIm = VS.cons zi (VS.cons zmi (VS.take (taps - 2) (rxLineIm st)))
              ur = VS.sum (VS.zipWith (-) (VS.zipWith (*) (rxEqRe st) lineRe) (VS.zipWith (*) (rxEqIm st) lineIm))
              ui = VS.sum (VS.zipWith (+) (VS.zipWith (*) (rxEqRe st) lineIm) (VS.zipWith (*) (rxEqIm st) lineRe))

              idx = qrSlice cfg (ur, ui)
              (px, py) = qrPoint cfg idx
              -- What the loops train against.  Decision-directed, that
              -- is the nearest point -- which is also the thing being
              -- measured, so on a constellation dense enough for the
              -- decisions to be wrong the error that steers the carrier
              -- and the equaliser is partly the receiver's own mistakes,
              -- and the loops settle around them.  Where the far end's
              -- symbols are known -- §5.4.2's B1 is 128 of them, and a
              -- far end idling sends thousands -- training on the truth
              -- instead removes that term.  'rxRef' carries them.
              (ax, ay, ref') = case rxRef st of
                (q : qs) -> (fst q, snd q, qs)
                []       -> (px, py, [])
              phErr = atan2 (ui * ax - ur * ay) (ur * ax + ui * ay)
              errR = ax - ur; errI = ay - ui
              -- The decision error stays the decision's, so everything
              -- that reads it -- the lock, the gates, the byte gate and
              -- the retrain timer above this module -- is asking the
              -- same question it was before.  Only the training is aided.
              dErrR = px - ur; dErrI = py - ui
              err2 = dErrR * dErrR + dErrI * dErrI
              evm = 0.98 * rxEvm_ st + 0.02 * err2
              locked = pw > 1e-5
              -- 'qrTrack' holds the carrier loop where 'qrAdapt' holds
              -- the equaliser.  They were one flag, which meant they
              -- were one flag in name only: qrAdapt reached the taps and
              -- the watchdog and never the phase, so a receiver told to
              -- stop adapting went on steering its carrier by the
              -- difference between what arrived and the nearest of four
              -- points -- while the far end had already moved to
              -- thirty-two or a hundred and twenty-eight of them, and
              -- every one of those differences was meaningless.
              --
              -- theta still advances by freq' while held.  Stopping it
              -- outright is the tempting mistake: over B1's 128 symbols
              -- a residual 7 Hz -- which 2.1/V.32 obliges us to work
              -- through -- turns the constellation by 134 degrees.
              -- A symbol that is nowhere near any point, on a receiver
              -- that has been reading the line, is not a symbol of this
              -- constellation, and what it says about the carrier or
              -- the channel is noise.  Where that matters is the seam at
              -- the end of the start-up: the far end's E is four points
              -- and its B1 is the agreed constellation, and the
              -- four-point receiver reads a block or so of B1 before E
              -- is recognised as complete.  Forty-eight symbols of phase
              -- error against the nearest of four states, on a signal
              -- spread over a hundred and twenty-eight, kicked the
              -- frequency estimate by over a hertz -- which the rate's
              -- receiver, arriving to guesswork, could not see to undo.
              --
              -- Only once locked: on a channel with real distortion the
              -- early errors are large because the equaliser has not
              -- converged yet, and a receiver that refused to learn from
              -- those would never converge at all.  So the lock has to
              -- mean converged -- 'qrLockAt', not 'qrEvmFreeze' -- and
              -- not merely "still adapting".
              good = not (rxLocked st) || err2 < qrLoopGate cfg
              -- Guarded on the weight rather than multiplied by it, so
              -- a receiver with none is bit for bit what it was.
              freqFf | qrFreqFf cfg > 0 && locked && stepRun < qrFreqFfRun cfg
                         = (1 - qrFreqFf cfg) * rxFreq st + qrFreqFf cfg * dev
                     | otherwise = rxFreq st
              freq' = if locked && qrTrack cfg && good then freqFf + qrThKi cfg * phErr else rxFreq st
              theta' | not locked = th
                     | qrTrack cfg && good = wrapPi (th + freq' + qrThKp cfg * phErr)
                     | otherwise = wrapPi (th + freq')

              -- The start-up signals are one point, or two alternating:
              -- their autocorrelation is singular and an LMS equaliser
              -- fed one runs its taps away.  TRN draws on all four
              -- states, which is exactly why the Recommendation trains
              -- the equaliser with it and not with S.
              recent = take 8 (idx : rxRecent st)
              varied = length (distinct recent) >= qrAdaptAt cfg
              -- What the timing loop needs to steer on is weaker than
              -- what the equaliser needs to adapt on, and the difference
              -- is the conditioning signal.  S is an alternating pair:
              -- two points, a quarter turn every symbol, which is a
              -- singular autocorrelation for a least-squares equaliser
              -- and the easiest thing in the world to recover a symbol
              -- clock from.  Holding the timing loop to 'varied' as well
              -- froze it right through S, and the calling ladder then
              -- failed to recognise the conditioning signal at all.
              moving = length (distinct recent) >= qrSteerAt cfg
              lineP = max 1e-6 (VS.sum (VS.zipWith (\a b -> a * a + b * b) lineRe lineIm) / fromIntegral taps)
              -- Converged, as against merely adapting.  'qrEvmFreeze'
              -- is where the decisions stop being worth learning from at
              -- all; 'qrLockAt' is far tighter -- where the receiver is
              -- reading the line properly, so that a symbol landing well
              -- outside means something rather than being one more it
              -- has not got right yet.  Not in the first symbols either
              -- way: the error is an exponential mean starting from
              -- nothing, and a receiver that latched on that would call
              -- itself converged before it had read a thing.
              latched = rxLocked st || (evm < qrLockAt cfg && rxSyms st >= 64)
              -- The same shape of question as 'latched', asked of the
              -- timing loop and answered once for the call.  A decision
              -- error this small is only reached by a receiver whose
              -- sampling instant is right, so it is evidence about the
              -- timing whichever constellation it was measured on --
              -- which is what lets the four-point start-up hand the data
              -- pump a narrow loop it could not have earned for itself.
              --
              -- 'varied' for the same reason the equaliser wants it, and
              -- it matters more here.  A steady tone is one point: every
              -- decision is right, the error goes to the noise floor,
              -- and none of it says anything about where in the symbol
              -- the samples are being taken -- there is no symbol.  The
              -- calling modem's AA is such a tone, and a loop that
              -- narrowed on it went into the rest of the start-up four
              -- times slower at pulling its clock back, which is how an
              -- answering modem that had been reading the caller
              -- perfectly failed to find E.
              timing' = rxTiming st
                || (qrTrackAt cfg > 0 && evm < qrTrackAt cfg && varied && rxSyms st >= 64)
              mu = if qrAdapt cfg && locked && evm < qrEvmFreeze cfg && varied && good
                       && stepRun < qrAdaptRun cfg
                     then qrEqMu cfg / lineP else 0
              eqRe' = VS.zipWith3 (\w lr li -> w + mu * (errR * lr + errI * li)) (rxEqRe st) lineRe lineIm
              eqIm' = VS.zipWith3 (\w lr li -> w + mu * (errI * lr - errR * li)) (rxEqIm st) lineRe lineIm

              -- The freeze above keeps a good equaliser from adapting on
              -- rubbish; on its own it also keeps a bad one from ever
              -- adapting back, because once the taps are wrong the
              -- decisions are wrong and the error stays over the
              -- threshold for good.  So count how long it has been bad
              -- and, past that, start the coherent path over.  Without
              -- this a receiver handed a signal it cannot read -- the far
              -- end still finishing its start-up, say -- is ruined by it
              -- permanently rather than for as long as it lasts.
              badAt = maybe (qrEvmFreeze cfg) id (qrEvmBad cfg)
              bad = if qrAdapt cfg && locked && evm > badAt then rxBad st + 1 else 0
              st1 = st { rxTau = tau'
                       , rxSps = max (0.9 * nominalSps) (min (1.1 * nominalSps) sps')
                       , rxPrevSym = (yr, yi), rxPower_ = pw
                       , rxTheta = theta', rxFreq = freq'
                       , rxEqRe = eqRe', rxEqIm = eqIm'
                       , rxLineRe = lineRe, rxLineIm = lineIm
                       , rxEvm_ = evm, rxBad = bad, rxRecent = recent
                       , rxLocked = latched, rxSyms = rxSyms st + 1
                       , rxTiming = timing'
                       , rxLastStep = step, rxStepRun = stepRun
                       , rxRef = ref' }
              st2 = if bad >= qrEvmGiveUp cfg || qrRestartOn cfg (QamTap step stepRun evm (rxSyms st + 1))
                      then qamRxReset p cfg st1 else st1
          in go st2 (QamSym (ur, ui) idx err2 (yr, yi) step dev : syms)

    (stSym, symsOut) = go st0 []
    carry = VS.length (rxPrevRe st0)
    keepFrom = max 0 (len - carry)
    -- What the matched filter put out this block, on average: the
    -- handshake's evidence that anything is on this channel at all.
    energy = if n == 0 then 0
             else (VS.sum (VS.map (\v -> v * v) mfRe) + VS.sum (VS.map (\v -> v * v) mfIm)) / fromIntegral n
    st' = stSym
      { rxN = n0 + n
      , rxEnergy = energy
      , rxHistRe = histRe', rxHistIm = histIm'
      , rxPrevRe = VS.drop keepFrom extRe, rxPrevIm = VS.drop keepFrom extIm
      , rxTau = rxTau stSym - fromIntegral keepFrom }

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x : xs) = x : distinct (filter (/= x) xs)
