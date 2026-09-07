{-# LANGUAGE BangPatterns #-}
-- | A quadrature amplitude modulation pump with nothing baked in: the
-- baud rate, carrier, pulse shaping and constellation all arrive as
-- parameters.  "Modec.V22" is the same machine wired for 600 baud and
-- one constellation, and is deliberately left alone -- its loop gains,
-- roll-off and tap count are measured numbers for five modes that work,
-- and none of them is right at 2400 baud.  What is shared between the
-- two is the shape, not the constants, so this is a sibling rather than
-- a refactor.
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
  , qamRxPower
  , qamRxReset
  , quarterTurns
    -- * Phase reversal tracking
  , RevTracker
  , revInit
  , revRearm
  , revBlock
  , revLevel
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
  } deriving (Eq, Show)

samplesPerSymbol :: QamParams -> Double
samplesPerSymbol p = qpFs p / qpBaud p

-- | Receiver tuning, and the constellation it decides against.
data QamRxCfg = QamRxCfg
  { qrKp        :: !Double  -- ^ Gardner proportional gain
  , qrKi        :: !Double  -- ^ Gardner integral gain (tracks the far clock)
  , qrClamp     :: !Double  -- ^ limit on the timing error estimate
  , qrThKp      :: !Double  -- ^ carrier loop proportional gain, per symbol
  , qrThKi      :: !Double  -- ^ carrier loop integral gain, per symbol
  , qrEqMu      :: !Double  -- ^ LMS step
  , qrEqTaps    :: !Int     -- ^ equaliser taps, T/2 spaced
  , qrEvmFreeze :: !Double  -- ^ stop adapting above this decision error power
  , qrEvmGiveUp :: !Int     -- ^ symbols of bad decisions before starting over
  , qrPower     :: !Double  -- ^ mean square of the constellation (the AGC target)
  , qrSlice     :: (Double, Double) -> Int          -- ^ nearest point, as an index
  , qrPoint     :: Int -> (Double, Double)          -- ^ that index back to a point
  }

-- | Gains that work at 2400 baud with a unit-mean-power constellation.
-- The carrier loop is deliberately slower than V.22's.  A trellis
-- decoder integrates over its whole traceback, so residual phase jitter
-- costs it far more than it costs an uncoded slicer that judges each
-- symbol alone: at V.22's gains the coded 9600 alternative was losing
-- 453 symbols where the uncoded one lost none, which is the coding gain
-- running backwards.  Halving them puts both at zero.
-- The equaliser spans 31 T\/2 taps, about 15 symbols or 6.5 ms, which is
-- the group delay a telephone connection actually smears a 2400 baud
-- signal over; V.22's 15 taps cover the same milliseconds at a quarter
-- of the rate and would cover a quarter of the distortion here.
defaultRxCfg :: ((Double, Double) -> Int) -> (Int -> (Double, Double)) -> QamRxCfg
defaultRxCfg slice point = QamRxCfg
  { qrKp = 0.12, qrKi = 0.0015, qrClamp = 2
  , qrThKp = 0.03, qrThKi = 0.0015
  , qrEqMu = 0.002, qrEqTaps = 31
  , qrEvmFreeze = 0.4, qrEvmGiveUp = 200, qrPower = 1
  , qrSlice = slice, qrPoint = point }

-- | Transmitter state.  Symbols are held on a fractional clock and the
-- pulse is evaluated per output sample, so no sample rate divides the
-- baud rate evenly and none has to.
data QamTxState = QamTxState
  { txSymClock :: !Double
  , txSymT0    :: !Double
  , txSymbols  :: [(Double, Double)]
  , txCarrier  :: !Double
  }

qamTxInit :: QamTxState
qamTxInit = QamTxState 0 0 [] 0

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
      in amp * (re * cos th - im * sin th)

    dropN = max 0 (floor ((fromIntegral n - (span_ + 1) * sps - t0s) / sps)) :: Int
    st' = stF
      { txSymClock = txSymClock stF - fromIntegral n
      , txSymT0 = t0s + fromIntegral dropN * sps - fromIntegral n
      , txSymbols = drop dropN (txSymbols stF)
      , txCarrier = wrapTwoPi (txCarrier stF + wc * fromIntegral n) }

-- | One decided symbol.
data QamSym = QamSym
  { qsPoint    :: !(Double, Double)  -- ^ equalised and derotated
  , qsIndex    :: !Int               -- ^ the immediate decision
  , qsError    :: !Double            -- ^ squared distance to it
  , qsRaw      :: !(Double, Double)  -- ^ before gain, carrier and equaliser
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
  }

qamRxInit :: QamParams -> QamRxCfg -> QamRxState
qamRxInit p cfg = QamRxState
  { rxN = 0
  , rxHistRe = VS.replicate hist 0, rxHistIm = VS.replicate hist 0
  , rxPrevRe = VS.replicate carry 0, rxPrevIm = VS.replicate carry 0
  , rxTau = fromIntegral carry, rxSps = sps
  , rxPrevSym = (0, 0), rxPower_ = 0
  , rxTheta = 0, rxFreq = 0
  , rxEqRe = centreTap, rxEqIm = VS.replicate taps 0
  , rxLineRe = VS.replicate taps 0, rxLineIm = VS.replicate taps 0
  , rxEvm_ = 0, rxBad = 0, rxRecent = [] }
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
  , rxLineRe = VS.replicate taps 0, rxLineIm = VS.replicate taps 0
  , rxEvm_ = 0, rxBad = 0, rxRecent = [] }
  where taps = qrEqTaps cfg

kernel :: QamParams -> VS.Vector Double
kernel p = rrcKernel (qpFs p) (qpBaud p) (qpRollOff p) (qpSpan p)

qamRxEvm :: QamRxState -> Double
qamRxEvm = rxEvm_

qamRxSps :: QamRxState -> Double
qamRxSps = rxSps

qamRxPower :: QamRxState -> Double
qamRxPower = rxPower_

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
    mixRe = VS.imap (\i v -> v * cos (wc * fromIntegral (n0 + i))) chunk
    mixIm = VS.imap (\i v -> negate v * sin (wc * fromIntegral (n0 + i))) chunk
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
              pw = 0.95 * rxPower_ st + 0.05 * (yr * yr + yi * yi)
              eRaw = ((yr - pr) * hr + (yi - pim) * hi) / max 1e-9 pw
              e = if pw < 1e-5 then 0 else max (negate (qrClamp cfg)) (min (qrClamp cfg) eRaw)
              sps' = sps - qrKi cfg * e
              tau' = tau + max (0.5 * sps) (sps' - qrKp cfg * e)

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
              phErr = atan2 (ui * px - ur * py) (ur * px + ui * py)
              errR = px - ur; errI = py - ui
              err2 = errR * errR + errI * errI
              evm = 0.98 * rxEvm_ st + 0.02 * err2
              locked = pw > 1e-5
              freq' = if locked then rxFreq st + qrThKi cfg * phErr else rxFreq st
              theta' = if locked then wrapPi (th + freq' + qrThKp cfg * phErr) else th

              -- The start-up signals are one point, or two alternating:
              -- their autocorrelation is singular and an LMS equaliser
              -- fed one runs its taps away.  TRN draws on all four
              -- states, which is exactly why the Recommendation trains
              -- the equaliser with it and not with S.
              recent = take 8 (idx : rxRecent st)
              varied = length (distinct recent) > 2
              lineP = max 1e-6 (VS.sum (VS.zipWith (\a b -> a * a + b * b) lineRe lineIm) / fromIntegral taps)
              mu = if locked && evm < qrEvmFreeze cfg && varied then qrEqMu cfg / lineP else 0
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
              bad = if locked && evm > qrEvmFreeze cfg then rxBad st + 1 else 0
              st1 = st { rxTau = tau'
                       , rxSps = max (0.9 * nominalSps) (min (1.1 * nominalSps) sps')
                       , rxPrevSym = (yr, yi), rxPower_ = pw
                       , rxTheta = theta', rxFreq = freq'
                       , rxEqRe = eqRe', rxEqIm = eqIm'
                       , rxLineRe = lineRe, rxLineIm = lineIm
                       , rxEvm_ = evm, rxBad = bad, rxRecent = recent }
              st2 = if bad >= qrEvmGiveUp cfg then qamRxReset p cfg st1 else st1
          in go st2 (QamSym (ur, ui) idx err2 (yr, yi) : syms)

    (stSym, symsOut) = go st0 []
    carry = VS.length (rxPrevRe st0)
    keepFrom = max 0 (len - carry)
    st' = stSym
      { rxN = n0 + n
      , rxHistRe = histRe', rxHistIm = histIm'
      , rxPrevRe = VS.drop keepFrom extRe, rxPrevIm = VS.drop keepFrom extIm
      , rxTau = rxTau stSym - fromIntegral keepFrom }

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x : xs) = x : distinct (filter (/= x) xs)

-- | Finds the instant a steady tone reverses phase, to the sample.
--
-- The whole of V.32's start-up turns on phase reversals: the calling
-- modem's AA and CC are both a steady 1800 Hz tone and differ only by
-- 180 degrees, and the answering modem's AC and CA are the same trick at
-- 600 and 3000 Hz.  A magnitude tone bank cannot see any of it -- the
-- amplitude is identical either side of the event -- which is why V.32
-- signal detection does not go through "Modec.Detect" and why no 1800 Hz
-- bin is added to it.
--
-- Sample accuracy is not a refinement here but a requirement: §5.4.1
-- fixes the turnaround from hearing a reversal to sending one at
-- 64 +/- 2 symbol periods, which at 2400 baud is 26.67 +/- 0.83 ms,
-- and the handshake state machine only runs every 20 ms.  So the
-- receiver timestamps the event and the transmitter is given a sample
-- index to act on, rather than the tick being asked to do something it
-- cannot.
--
-- The method is a sliding coherent correlation against the tone.  Its
-- projection onto the phase established before the event runs from
-- strongly positive to strongly negative, and the crossing, interpolated
-- between the two straddling samples, is the reversal.
data RevTracker = RevTracker
  { rtW      :: !Double          -- ^ radians per sample at the tone
  , rtN      :: !Int             -- ^ global sample index
  , rtWin    :: !Int
  , rtHist   :: [(Double, Double)]  -- ^ recent mixed samples, newest first
  , rtAcc    :: !(Double, Double)   -- ^ running sum over the window
  , rtRef    :: !(Maybe (Double, Double))  -- ^ phase before the event
  , rtProj   :: !Double
  , rtLevel  :: !Double
  , rtPow    :: !Double          -- ^ tracked mean square of the input
  , rtSeen   :: !Bool            -- ^ the tone has been steady in this phase
  , rtHold   :: !Int             -- ^ samples to wait before reporting again
  }

revInit :: Double -> Double -> RevTracker
revInit fs f = RevTracker
  { rtW = 2 * pi * f / fs
  , rtN = 0
  -- A fixed 5 ms, not a fixed number of cycles.  Sizing the window by
  -- the tone's own period gives the high tones too little frequency
  -- resolution to reject the low ones: three cycles of 3000 Hz is 8
  -- samples, over which a 2100 Hz answer tone does not average away at
  -- all, and the tracker reads it as its own.
  , rtWin = max 8 (round (fs / 200))
  , rtHist = [], rtAcc = (0, 0)
  , rtRef = Nothing, rtProj = 0, rtLevel = 0, rtPow = 0, rtSeen = False, rtHold = 0 }

-- | Forget what has been heard so far, but not what time it is.
--
-- The start-up hands a tracker a different signal several times over,
-- and the phase reference it established for the last one is worse than
-- useless for the next.  The sample counter has to survive, though: the
-- round trip is the difference between two reversal timestamps taken
-- either side of a re-arm, and restarting the clock between them
-- measures a negative delay.
revRearm :: RevTracker -> RevTracker
revRearm t = t
  { rtHist = [], rtAcc = (0, 0), rtRef = Nothing
  , rtProj = 0, rtLevel = 0, rtPow = 0, rtSeen = False, rtHold = 0 }

-- | How much of what is arriving is this tone, from 0 to about 0.71.
--
-- The measurement is the coherent correlation divided by the signal's
-- own root mean square, which is the only form of it that means
-- anything: an absolute threshold says \"this is loud\", and at any
-- realistic signal to noise ratio noise alone will clear it.  A single
-- tone reads about 0.71, either sideband of the alternating AC signal
-- about 0.5, and white noise about one over the square root of the
-- window length -- around 0.16 here.
revLevel :: RevTracker -> Double
revLevel = rtLevel

-- | Feed a block; returns the global sample indices at which the tone
-- reversed phase.
revBlock :: Signal -> RevTracker -> (RevTracker, [Int])
revBlock chunk st0 = go 0 st0 []
  where
    n = VS.length chunk
    go !i st acc
      | i >= n = (st, reverse acc)
      | otherwise =
          let t = rtN st
              v = VS.unsafeIndex chunk i
              c = cos (rtW st * fromIntegral t)
              sn = sin (rtW st * fromIntegral t)
              p = (v * c, negate v * sn)
              hist' = take (rtWin st) (p : rtHist st)
              (ar, ai) = foldl (\(x, y) (a, b) -> (x + a, y + b)) (0, 0) hist'
              mag = sqrt (ar * ar + ai * ai) / fromIntegral (rtWin st)
              pow = 0.995 * rtPow st + 0.005 * (v * v)
              -- until there is something on the line at all, the ratio
              -- is meaningless rather than large: an empty line divided
              -- by an empty line must not read as a tone
              lvl = if pow > 1e-12
                      then 0.98 * rtLevel st + 0.02 * (mag / sqrt pow)
                      else 0
              -- everything below is gated on the band actually holding
              -- this tone, not on the line being loud
              tone = lvl > 0.25 && pow > 1e-10
              full = length hist' >= rtWin st
              -- the phase to measure against: whatever was established
              -- before, adopted once the tone is steady
              ref = case rtRef st of
                Just r | rtHold st > 0 -> Just r
                Just r -> Just r
                Nothing | full && tone -> Just (ar / (mag * fromIntegral (rtWin st)), ai / (mag * fromIntegral (rtWin st)))
                _ -> Nothing
              proj = case ref of
                Just (rr, ri) -> (ar * rr + ai * ri) / fromIntegral (rtWin st)
                Nothing -> 0
              -- The correlation does not step from one phase to the other:
              -- the window slides across the event over its own length,
              -- so the projection walks down through zero.  The event is
              -- that crossing, and it only counts if the tone had been
              -- steady in the old phase first -- which is what rtSeen
              -- records, and what stops noise from ringing the bell.
              seen = rtSeen st || (full && tone && proj > 0.5 * mag)
              crossed = full && rtHold st == 0 && seen && tone
                        && rtProj st > 0 && proj <= 0
              st1 = st { rtN = t + 1, rtHist = hist', rtAcc = (ar, ai)
                       , rtRef = if crossed then Nothing else ref
                       , rtProj = proj, rtLevel = lvl, rtPow = pow
                       , rtSeen = not crossed && seen && tone
                       , rtHold = if crossed then rtWin st * 2 else max 0 (rtHold st - 1) }
          in if crossed
               -- the crossing lies between this sample and the last;
               -- the correlation is linear across it, so interpolate
               -- Interpolate between the two samples that straddle the
               -- crossing.  Clamped: when both projections are close to
               -- zero the ratio is numerically meaningless and would
               -- place the event anywhere at all.
               then let d = rtProj st - proj
                        frac = if abs d < 1e-18 then 0.5
                               else max 0 (min 1 (rtProj st / d))
                        at = t - rtWin st `div` 2 + round frac
                    in go (i + 1) st1 (at : acc)
               else go (i + 1) st1 acc
