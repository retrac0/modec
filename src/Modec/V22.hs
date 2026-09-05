{-# LANGUAGE BangPatterns #-}
-- | ITU-T V.22 / V.22bis data pump: 600 baud on a 1200 Hz (low, calling
-- modem) or 2400 Hz (high, answering modem) carrier, square-root
-- raised-cosine shaping with 75 % roll-off, and the 1 + x^-14 + x^-17
-- self-synchronising scrambler.
--
-- * 1200 bit/s (V.22): dibits as differential quadrant changes
--   (Table 1/V.22).  The V.22bis "01" point of each quadrant is
--   transmitted, as V.22bis §2.5.2.2 requires.
-- * 2400 bit/s (V.22bis): quadbits; the first two bits are the
--   differential quadrant change, the last two select one of four points
--   in the new quadrant (Figure 2/V.22bis).
--
-- Transmitter: symbols on a fractional symbol clock, shaped baseband
-- evaluated per output sample, so any sample rate works.  Modes cover
-- the handshake signals (unscrambled binary 1, the S1 double-dibit
-- pattern, scrambled binary 1) and scrambled data with an idle-mark byte
-- queue.
--
-- Receiver: complex downconversion at the nominal carrier, matched RRC
-- filter, Gardner symbol timing recovery with cubic interpolation, and
-- then two decision paths on the same samples:
--
-- * differential phase steps (no carrier lock needed; used for the
--   1200 bit/s data and for the handshake signal detectors), and
-- * a coherent path: automatic gain, decision-directed carrier phase
--   and frequency tracking, a T/2-spaced complex LMS equaliser, and
--   16-way decisions in a frame rotated into the current quadrant.  The
--   quadrant coding is differential, so the four-fold phase ambiguity of
--   the carrier lock does not matter.
module Modec.V22
  ( V22Channel (..)
  , carrierOf
  , Rate (..)
  , TxMode (..)
  , V22TxState
  , v22TxInit
  , v22TxBlock
  , V22RxState
  , v22RxInit
  , v22RxInitWith
  , v22RxBlock
  , v22RxSetRate
  , v22RxSetCoherentGains
  , rxSpsEstimate
  , rxEvmEstimate
  , rxOnes2400Run
  , rxRateOf
  , scrambleBit
  , descrambleBit
  , dibitToStep
  , stepToDibit
  , RxOut (..)
  , v22Modulate
  , v22ModulateAt
  , v22Demodulate
  , v22DemodulateAt
  , v22DemodulateWith
  , v22RxRun
  , rrcTaps
  , txBitsOf
  , withBits
  ) where

import qualified Data.Vector.Storable as VS
import Data.Bits (shiftL, testBit, (.&.))
import Data.Word (Word8)

import Modec.DSP
import Modec.FSK (Framing, frameBits)

data V22Channel = LowChannel | HighChannel deriving (Eq, Show)

carrierOf :: V22Channel -> Double
carrierOf LowChannel = 1200
carrierOf HighChannel = 2400

data Rate = R1200 | R2400 deriving (Eq, Show)

baud :: Double
baud = 600

rollOff :: Double
rollOff = 0.75

-- | Root-raised-cosine impulse response, time in symbol periods.
rrc :: Double -> Double
rrc t
  | abs t < 1e-9 = 1 - b + 4 * b / pi
  | abs (abs t - 1 / (4 * b)) < 1e-9 =
      b / sqrt 2 * ((1 + 2 / pi) * sin (pi / (4 * b)) + (1 - 2 / pi) * cos (pi / (4 * b)))
  | otherwise =
      (sin (pi * t * (1 - b)) + 4 * b * t * cos (pi * t * (1 + b))) / (pi * t * (1 - (4 * b * t) ^ (2 :: Int)))
  where b = rollOff

-- | Pulse span on each side, in symbols.
pulseSpan :: Double
pulseSpan = 6

-- | Sampled RRC kernel for the matched filter, unit energy.
rrcTaps :: Double -> VS.Vector Double
rrcTaps fs = VS.map (/ norm) raw
  where
    sps = fs / baud
    half = round (pulseSpan * sps) :: Int
    raw = VS.generate (2 * half + 1) (\i -> rrc (fromIntegral (i - half) / sps))
    norm = sqrt (VS.sum (VS.map (\v -> v * v) raw))

-- Scrambler / descrambler: 17-bit history of the line (scrambled) bits.
scrambleBit :: Int -> Bool -> (Int, Bool)
scrambleBit reg d =
  let s = d /= (testBit reg 13 /= testBit reg 16)   -- x^-14 and x^-17
  in (((reg `shiftL` 1) .&. 0x1FFFF) + (if s then 1 else 0), s)

descrambleBit :: Int -> Bool -> (Int, Bool)
descrambleBit reg s =
  let d = s /= (testBit reg 13 /= testBit reg 16)
  in (((reg `shiftL` 1) .&. 0x1FFFF) + (if s then 1 else 0), d)

-- | Dibit (first bit, second bit) to phase change in quadrants
-- (Table 1/V.22): 00 = +90, 01 = 0, 11 = +270, 10 = +180.
dibitToStep :: Bool -> Bool -> Int
dibitToStep False False = 1
dibitToStep False True = 0
dibitToStep True True = 3
dibitToStep True False = 2

stepToDibit :: Int -> (Bool, Bool)
stepToDibit 1 = (False, False)
stepToDibit 0 = (False, True)
stepToDibit 3 = (True, True)
stepToDibit _ = (True, False)

-- | Constellation point (Figure 2/V.22bis) for a quadrant (0 = first)
-- and the last two quadbit bits, in grid units (odd integers).  In the
-- first quadrant 00 = (1,1), 01 = (3,1), 10 = (1,3), 11 = (3,3); the
-- other quadrants are that pattern rotated by 90 degrees per quadrant.
gridPoint :: Int -> Bool -> Bool -> (Double, Double)
gridPoint q b3 b4 = rotate (q `mod` 4) (if b4 then 3 else 1, if b3 then 3 else 1)
  where
    rotate 0 (x, y) = (x, y)
    rotate 1 (x, y) = (-y, x)
    rotate 2 (x, y) = (-x, -y)
    rotate _ (x, y) = (y, -x)

-- | Grid units to unit average power (the 16 points and the four 1200
-- bit/s points both average |s|^2 = 10).
gridScale :: Double
gridScale = 1 / sqrt 10

data TxMode
  = TxU11               -- ^ unscrambled binary 1
  | TxS1                -- ^ unscrambled repetitive double dibit 00 and 11
  | TxScrambledOnes
  | TxScrambledData     -- ^ queued bytes with start/stop framing, idle mark
  deriving (Eq, Show)

data V22TxState = V22TxState
  { txSymClock :: !Double            -- ^ sample index (relative to the block) of the next symbol centre
  , txSymT0    :: !Double            -- ^ centre of the first stored symbol
  , txSymbols  :: [(Double, Double)] -- ^ stored symbols (unit-power complex), oldest first, sps apart from txSymT0
  , txQuadrant :: !Int
  , txScr      :: !Int
  , txS1Toggle :: !Bool
  , txBits     :: [Bool]
  , txQueue    :: [Word8]
  , txCarrier  :: !Double
  , txGuard    :: !Double
  }

v22TxInit :: V22TxState
v22TxInit = V22TxState 0 0 [] 0 0 False [] [] 0 0

-- | Generate @n@ samples.  @guard@ adds the 1800 Hz guard tone at -6 dB
-- (high channel option).
v22TxBlock :: Double -> V22Channel -> Framing -> Double -> Bool -> Rate -> TxMode -> [Word8] -> Int -> V22TxState -> (V22TxState, Signal)
v22TxBlock fs ch fr amp guard rate mode newBytes n st0 = (st', sig)
  where
    sps = fs / baud
    fc = carrierOf ch
    wc = 2 * pi * fc / fs
    wg = 2 * pi * 1800 / fs
    st1 = st0 { txQueue = txQueue st0 ++ newBytes }
    stFilled = fill st1
    fill st
      | txSymClock st <= fromIntegral n + pulseSpan * sps = fill (emit st)
      | otherwise = st
    emit st =
      let (b1, b2, stA) = case mode of
            TxS1 -> (txS1Toggle st, txS1Toggle st, st { txS1Toggle = not (txS1Toggle st) })
            _ -> let (x, s1) = nextBit st; (y, s2) = nextBit s1 in (x, y, s2)
          q = (txQuadrant stA + dibitToStep b1 b2) `mod` 4
          (b3, b4, stB) = case (rate, mode) of
            (R2400, TxScrambledOnes) -> let (x, s1) = nextBit stA; (y, s2) = nextBit s1 in (x, y, s2)
            (R2400, TxScrambledData) -> let (x, s1) = nextBit stA; (y, s2) = nextBit s1 in (x, y, s2)
            _ -> (False, True, stA)     -- 1200 bit/s and handshake signals use the "01" points
          (gx, gy) = gridPoint q b3 b4
          t0 = if null (txSymbols st) then txSymClock st else txSymT0 st
      in stB { txSymClock = txSymClock st + sps, txQuadrant = q, txSymT0 = t0
             , txSymbols = txSymbols stB ++ [(gx * gridScale, gy * gridScale)] }
    nextBit st = case mode of
      TxU11 -> (True, st)
      TxS1 -> (True, st)
      TxScrambledOnes -> scr True st
      TxScrambledData ->
        case txBits st of
          (b : bs) -> scr b st { txBits = bs }
          [] -> case txQueue st of
            (byte : q) -> case frameBits fr [byte] of
              (b : bs) -> scr b st { txBits = bs, txQueue = q }
              [] -> scr True st { txQueue = q }
            [] -> scr True st
    scr b st = let (reg, s) = scrambleBit (txScr st) b in (s, st { txScr = reg })
    symsRe = VS.fromList (map fst (txSymbols stFilled))
    symsIm = VS.fromList (map snd (txSymbols stFilled))
    nSyms = VS.length symsRe
    t0s = txSymT0 stFilled
    sig = VS.generate n $ \i ->
      let t = fromIntegral i
          kLo = max 0 (ceiling ((t - pulseSpan * sps - t0s) / sps))
          kHi = min (nSyms - 1) (floor ((t + pulseSpan * sps - t0s) / sps))
          accum !k !a !b
            | k > kHi = (a, b)
            | otherwise =
                let p = rrc ((t - (t0s + fromIntegral k * sps)) / sps)
                in accum (k + 1) (a + p * VS.unsafeIndex symsRe k) (b + p * VS.unsafeIndex symsIm k)
          (re, im) = accum kLo 0 0
          th = txCarrier stFilled + wc * t
          g = if guard && ch == HighChannel then 0.5 * sin (txGuard stFilled + wg * t) else 0
      in amp * (re * cos th - im * sin th + g)
    wrap p = p - 2 * pi * fromIntegral (floor (p / (2 * pi)) :: Int)
    dropN = max 0 (floor ((fromIntegral n - (pulseSpan + 1) * sps - t0s) / sps)) :: Int
    st' = stFilled
      { txSymClock = txSymClock stFilled - fromIntegral n
      , txSymT0 = t0s + fromIntegral dropN * sps - fromIntegral n
      , txSymbols = drop dropN (txSymbols stFilled)
      , txCarrier = wrap (txCarrier stFilled + wc * fromIntegral n)
      , txGuard = wrap (txGuard stFilled + wg * fromIntegral n)
      }

-- | Pending data bits of a transmitter (for experiments and tests).
txBitsOf :: V22TxState -> [Bool]
txBitsOf = txBits

-- | Append data bits to a transmitter's pending bits.
withBits :: V22TxState -> [Bool] -> V22TxState
withBits st bs = st { txBits = txBits st ++ bs }

-- | Number of T/2-spaced equaliser taps.
eqTaps :: Int
eqTaps = 15

data V22RxState = V22RxState
  { rxN        :: !Int               -- ^ global index of the next input sample (for the mixer phase)
  , rxHistRe   :: !Signal
  , rxHistIm   :: !Signal
  , rxPrevRe   :: !Signal            -- ^ last few matched-filter outputs carried across blocks
  , rxPrevIm   :: !Signal
  , rxTau      :: !Double            -- ^ next symbol sampling position in the carried+block coordinates
  , rxSps      :: !Double
  , rxPrevSym  :: !(Double, Double)  -- ^ previous raw on-time sample (differential path)
  , rxPower    :: !Double            -- ^ tracked raw symbol power (timing normalisation and AGC)
  , rxDescr    :: !Int
  , rxOnesRun  :: !Int
  , rxU11Run   :: !Int
  , rxS1Run    :: !Int
  , rxLastStep :: !Int
  , rxKp       :: !Double
  , rxKi       :: !Double
  , rxClamp    :: !Double
    -- coherent path
  , rxRate     :: !Rate
  , rxTheta    :: !Double            -- ^ carrier phase estimate (radians)
  , rxFreq     :: !Double            -- ^ carrier frequency estimate (radians per symbol)
  , rxEqRe     :: !Signal            -- ^ equaliser taps
  , rxEqIm     :: !Signal
  , rxLineRe   :: !Signal            -- ^ equaliser delay line (T/2 spaced, newest first)
  , rxLineIm   :: !Signal
  , rxQuadrant :: !Int               -- ^ quadrant of the previous coherent decision
  , rxEvm      :: !Double            -- ^ tracked decision error power (coherent path quality)
  , rxOnes2400 :: !Int               -- ^ consecutive descrambled ones decided 16-way
  , rxThKp     :: !Double            -- ^ carrier loop proportional gain
  , rxThKi     :: !Double            -- ^ carrier loop integral gain
  , rxEqMu     :: !Double            -- ^ equaliser step size
  , rxConstRun :: !Int               -- ^ consecutive identical phase steps (a pure tone or unscrambled ones)
  , rxBadEvm   :: !Int               -- ^ consecutive symbols with a large decision error
  }

v22RxInit :: Double -> V22RxState
v22RxInit fs = v22RxInitWith fs 0 (0.16, 0.002, 2)

-- | Receiver with an initial timing offset (samples) and loop
-- parameters (proportional gain, integral gain, error clamp); for
-- experiments.
v22RxInitWith :: Double -> Double -> (Double, Double, Double) -> V22RxState
v22RxInitWith fs tauOff (kp, ki, cl) =
  V22RxState 0 (VS.replicate (taps - 1) 0) (VS.replicate (taps - 1) 0) (VS.replicate carry 0) (VS.replicate carry 0)
             (fromIntegral carry + sps + tauOff) sps (1, 0) 1e-6 0 0 0 0 0 kp ki cl
             R1200 0 0 eq0Re eq0Im (VS.replicate eqTaps 0) (VS.replicate eqTaps 0) 0 1 0 0.15 0.01 0.004 0 0
  where
    taps = VS.length (rrcTaps fs)
    sps = fs / baud
    carry = 4 + ceiling sps
    -- even indices of the delay line are on-time samples; start on the middle one
    eq0Re = VS.generate eqTaps (\i -> if i == 2 * (eqTaps `div` 4) then 1 else 0)
    eq0Im = VS.replicate eqTaps 0

-- | Set the coherent loop gains (carrier proportional, carrier integral,
-- equaliser step); for experiments.
v22RxSetCoherentGains :: (Double, Double, Double) -> V22RxState -> V22RxState
v22RxSetCoherentGains (a, b, c) st = st { rxThKp = a, rxThKi = b, rxEqMu = c }

-- | Fresh coherent state: centre-tap equaliser, zero carrier phase and
-- frequency.  Used when a V.22 signal starts after silence or a tone,
-- and by the decision-error watchdog.
resetCoherent :: V22RxState -> V22RxState
resetCoherent st = st
  { rxTheta = 0, rxFreq = 0, rxEvm = 1, rxBadEvm = 0
  , rxEqRe = VS.generate eqTaps (\i -> if i == 2 * (eqTaps `div` 4) then 1 else 0)
  , rxEqIm = VS.replicate eqTaps 0 }

-- | Switch the decision rate (the handshake does this after S1).
v22RxSetRate :: Rate -> V22RxState -> V22RxState
v22RxSetRate r st = st { rxRate = r, rxOnes2400 = 0 }

-- | Tracked coherent decision error power (for tracing).
rxEvmEstimate :: V22RxState -> Double
rxEvmEstimate = rxEvm

-- | Consecutive 16-way-decided descrambled ones (for tracing).
rxOnes2400Run :: V22RxState -> Int
rxOnes2400Run = rxOnes2400

-- | Current decision rate.
rxRateOf :: V22RxState -> Rate
rxRateOf = rxRate

-- | Current samples-per-symbol estimate of the timing loop.
rxSpsEstimate :: V22RxState -> Double
rxSpsEstimate = rxSps

data RxOut = RxOut
  { roSymbols  :: [(Double, Double)]   -- ^ equalised, derotated symbols in grid units (constellation display)
  , roDibits   :: [Int]                -- ^ differential phase steps 0..3
  , roBits     :: [Bool]               -- ^ descrambled bits (2 per symbol at 1200, 4 at 2400)
  , roEnergy   :: !Double              -- ^ mean baseband power over the block
  , roAngleErr :: !Double              -- ^ mean |phase step error| in degrees (differential path quality)
  , roEvm      :: !Double              -- ^ tracked coherent decision error power, grid units squared
  , roOnesRun  :: !Int
  , roU11Run   :: !Int
  , roS1Run    :: !Int
  , roOnes2400 :: !Int                 -- ^ consecutive 16-way-decided descrambled ones
  }

-- | Four-point cubic (Catmull-Rom) interpolation at fractional index t.
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

-- | Quadrant (0..3) of a point.
quadrantOf :: Double -> Double -> Int
quadrantOf x y
  | x >= 0 && y >= 0 = 0
  | x < 0 && y >= 0 = 1
  | x < 0 = 2
  | otherwise = 3

-- | Nearest odd grid value, clamped to +-3.
sliceOdd :: Double -> Double
sliceOdd v = max (-3) (min 3 (2 * fromIntegral (round ((v - 1) / 2) :: Int) + 1))

-- | Decision on the coherent sample: the decided point and the last two
-- quadbit bits, read in the frame rotated into the first quadrant.
decide16 :: Double -> Double -> ((Double, Double), Bool, Bool)
decide16 x y =
  let px = sliceOdd x
      py = sliceOdd y
      q = quadrantOf px py
      (rx, ry) = case q of
        0 -> (px, py)
        1 -> (py, -px)
        2 -> (-px, -py)
        _ -> (-py, px)
  in ((px, py), ry > 2, rx > 2)

-- | Decision at 1200 bit/s: the "01" point of the quadrant, with sectors
-- centred on those points (18.4 degrees into each quadrant).
decide4 :: Double -> Double -> (Double, Double)
decide4 x y = gridPoint q False True
  where
    ang = atan2 y x - atan2 1 3
    q = (floor ((ang + pi / 4) / (pi / 2)) :: Int) `mod` 4

v22RxBlock :: Double -> V22Channel -> Signal -> V22RxState -> (V22RxState, RxOut)
v22RxBlock fs ch chunk st0 = (st', out)
  where
    n = VS.length chunk
    wc = 2 * pi * carrierOf ch / fs
    n0 = rxN st0
    mixRe = VS.imap (\i v -> v * cos (wc * fromIntegral (n0 + i))) chunk
    mixIm = VS.imap (\i v -> negate v * sin (wc * fromIntegral (n0 + i))) chunk
    hrev = VS.reverse (rrcTaps fs)
    (mfRe, histRe') = firStream hrev (rxHistRe st0) mixRe
    (mfIm, histIm') = firStream hrev (rxHistIm st0) mixIm
    extRe = rxPrevRe st0 VS.++ mfRe
    extIm = rxPrevIm st0 VS.++ mfIm
    len = VS.length extRe
    energy = if n == 0 then 0 else (VS.sum (VS.map (\v -> v * v) mfRe) + VS.sum (VS.map (\v -> v * v) mfIm)) / fromIntegral n
    kp = rxKp st0
    ki = rxKi st0
    -- coherent loop gains (per symbol)
    thetaKp = rxThKp st0
    thetaKi = rxThKi st0
    eqMu = rxEqMu st0
    wrapPi x = x - 2 * pi * fromIntegral (round (x / (2 * pi)) :: Int)
    -- per-symbol loop; accumulates symbols, steps, bits and angle errors
    go st syms dibits bits aerrs
      | floor (rxTau st) + 2 >= len = (st, reverse syms, reverse dibits, reverse bits, aerrs)
      | rxTau st - rxSps st / 2 < 1 = go st { rxTau = rxTau st + rxSps st } syms dibits bits aerrs
      | otherwise =
          let tau = rxTau st
              sps = rxSps st
              yr = cubicAt extRe tau; yi = cubicAt extIm tau
              hr = cubicAt extRe (tau - sps / 2); hi = cubicAt extIm (tau - sps / 2)
              (pr, pim) = rxPrevSym st
              -- Gardner timing error, normalised by signal power, clamped,
              -- and ignored while there is no signal
              pw = 0.95 * rxPower st + 0.05 * (yr * yr + yi * yi)
              eRaw = ((yr - pr) * hr + (yi - pim) * hi) / max 1e-9 pw
              e = if pw < 1e-5 then 0 else max (negate (rxClamp st)) (min (rxClamp st) eRaw)
              sps' = sps - ki * e
              tau' = tau + max (0.5 * sps) (sps' - kp * e)
              -- differential path
              dr = yr * pr + yi * pim
              di = yi * pr - yr * pim
              ang = atan2 di dr
              stepQ = (round (ang / (pi / 2)) :: Int) `mod` 4
              aerr = let d = ang * 180 / pi; qd = fromIntegral (round (d / 90) :: Int) * 90 in abs (d - qd)
              (d1, d2) = stepToDibit stepQ
              -- coherent path: AGC to grid units, derotate, equalise
              agc = if pw < 1e-9 then 0 else sqrt (10 / pw)
              th = rxTheta st
              c = cos th; s = sin th
              rot a b = (agc * (a * c + b * s), agc * (b * c - a * s))
              (zmr, zmi) = rot hr hi
              (zr, zi) = rot yr yi
              lineRe = VS.cons zr (VS.cons zmr (VS.take (eqTaps - 2) (rxLineRe st)))
              lineIm = VS.cons zi (VS.cons zmi (VS.take (eqTaps - 2) (rxLineIm st)))
              ur = VS.sum (VS.zipWith (-) (VS.zipWith (*) (rxEqRe st) lineRe) (VS.zipWith (*) (rxEqIm st) lineIm))
              ui = VS.sum (VS.zipWith (+) (VS.zipWith (*) (rxEqRe st) lineIm) (VS.zipWith (*) (rxEqIm st) lineRe))
              -- decisions
              ((px, py), b3, b4) = case rxRate st of
                R2400 -> decide16 ur ui
                R1200 -> (decide4 ur ui, False, True)
              q = quadrantOf px py
              stepC = (q - rxQuadrant st) `mod` 4
              (c1, c2) = stepToDibit stepC
              -- carrier phase error and equaliser error from the decided point
              phErr = atan2 (ui * px - ur * py) (ur * px + ui * py)
              errR = px - ur; errI = py - ui
              evm = 0.98 * rxEvm st + 0.02 * (errR * errR + errI * errI)
              locked = pw > 1e-5
              -- the differential path measures the carrier offset directly
              -- (signed deviation of the phase step from the nearest quadrant);
              -- during 1200 bit/s training it steers the frequency estimate so
              -- the decision-directed loop only has to track the residual
              devRad = ang - fromIntegral (round (ang / (pi / 2)) :: Int) * (pi / 2)
              freqFf = case rxRate st of
                R1200 | locked && constRun < 16 -> 0.97 * rxFreq st + 0.03 * devRad
                _ -> rxFreq st
              freq' = if locked then freqFf + thetaKi * phErr else rxFreq st
              theta' = if locked then wrapPi (th + freq' + thetaKp * phErr) else th
              -- LMS: w += mu * err * conj(line), normalised by the line power.
              -- Do not adapt on a pure tone or unscrambled ones (constant
              -- phase steps): their autocorrelation is singular and the taps
              -- run away.
              constRun = if stepQ == rxLastStep st then rxConstRun st + 1 else 0
              lineP = max 1e-6 (VS.sum (VS.zipWith (\a b -> a * a + b * b) lineRe lineIm) / fromIntegral eqTaps)
              mu = if locked && evm < 4 && constRun < 16 then eqMu / lineP else 0
              badEvm = if locked && evm > 2 then rxBadEvm st + 1 else 0
              eqRe' = VS.zipWith3 (\w lr li -> w + mu * (errR * lr + errI * li)) (rxEqRe st) lineRe lineIm
              eqIm' = VS.zipWith3 (\w lr li -> w + mu * (errI * lr - errR * li)) (rxEqIm st) lineRe lineIm
              -- bits out, according to the rate
              bitsIn = case rxRate st of
                R1200 -> [d1, d2]
                R2400 -> [c1, c2, b3, b4]
              (reg', descRev) = foldl (\(r, acc) b -> let (r', d) = descrambleBit r b in (r', d : acc)) (rxDescr st, []) bitsIn
              descBits = reverse descRev
              ones = foldl (\acc b -> if b then acc + 1 else 0) (rxOnesRun st) descBits
              ones2400 = case rxRate st of
                R2400 -> ones
                R1200 -> 0
              u11 = if stepQ == 3 then rxU11Run st + 1 else 0
              s1 = if (stepQ == 1 || stepQ == 3) && stepQ /= rxLastStep st && (rxLastStep st == 1 || rxLastStep st == 3)
                     then rxS1Run st + 1 else (if stepQ == 1 || stepQ == 3 then 1 else 0)
              st1 = st { rxTau = tau', rxSps = max (0.9 * (fs / baud)) (min (1.1 * (fs / baud)) sps')
                       , rxPrevSym = (yr, yi), rxPower = pw, rxDescr = reg'
                       , rxOnesRun = ones, rxU11Run = u11, rxS1Run = s1, rxLastStep = stepQ
                       , rxTheta = theta', rxFreq = freq', rxEqRe = eqRe', rxEqIm = eqIm'
                       , rxLineRe = lineRe, rxLineIm = lineIm, rxQuadrant = q, rxEvm = evm
                       , rxOnes2400 = ones2400, rxConstRun = constRun, rxBadEvm = badEvm }
              -- a V.22 signal has just started (unscrambled ones recognised), or the
              -- coherent path has been lost for 50 symbols: start the coherent path over
              st2 = if u11 == 93 || badEvm >= 50 then resetCoherent st1 else st1
          in go st2 ((ur, ui) : syms) (stepQ : dibits) (descRev ++ bits) (aerr : aerrs)
    (stSym, symsOut, dibitsOut, bitsOut, aerrs) = go st0 [] [] [] []
    angleErr = if null aerrs then 0 else sum aerrs / fromIntegral (length aerrs)
    carry = VS.length (rxPrevRe st0)
    keepFrom = max 0 (len - carry)
    st' = stSym
      { rxN = n0 + n
      , rxHistRe = histRe', rxHistIm = histIm'
      , rxPrevRe = VS.drop keepFrom extRe, rxPrevIm = VS.drop keepFrom extIm
      , rxTau = rxTau stSym - fromIntegral keepFrom
      }
    out = RxOut symsOut dibitsOut bitsOut energy angleErr (rxEvm stSym) (rxOnesRun stSym) (rxU11Run stSym) (rxS1Run stSym) (rxOnes2400 stSym)

-- | Offline: scrambled data bits to a signal at 1200 bit/s.
v22Modulate :: Double -> V22Channel -> Double -> [Bool] -> Signal
v22Modulate fs ch = v22ModulateAt fs ch R1200

v22ModulateAt :: Double -> V22Channel -> Rate -> Double -> [Bool] -> Signal
v22ModulateAt fs ch rate amp bits = VS.concat (go v22TxInit bits)
  where
    n = 160
    go st bs
      | null bs && null (txBits st) = flushTail st
      | otherwise =
          let (st', sig) = v22TxBlock fs ch frDummy amp False rate TxScrambledData [] n st { txBits = txBits st ++ take 2000 bs }
          in sig : go st' (drop 2000 bs)
    flushTail st = let (_, sig) = v22TxBlock fs ch frDummy amp False rate TxScrambledOnes [] (round (fs * 0.05)) st in [sig]
    frDummy = error "framing not used"

-- | Offline: descrambled bits from a signal at 1200 bit/s.
v22Demodulate :: Double -> V22Channel -> Signal -> [Bool]
v22Demodulate fs ch = v22DemodulateWith (v22RxInit fs) ch

v22DemodulateAt :: Double -> V22Channel -> Rate -> Signal -> [Bool]
v22DemodulateAt fs ch rate = v22DemodulateWith (v22RxSetRate rate (v22RxInit fs)) ch

v22DemodulateWith :: V22RxState -> V22Channel -> Signal -> [Bool]
v22DemodulateWith st0 ch x = concatMap roBits (v22RxRun 8000 st0 ch x)

-- | Offline: all receiver outputs per 160-sample block.
v22RxRun :: Double -> V22RxState -> V22Channel -> Signal -> [RxOut]
v22RxRun fs st0 ch x = go st0 (chunks x)
  where
    chunks v | VS.null v = [] | otherwise = VS.take 160 v : chunks (VS.drop 160 v)
    go _ [] = []
    go st (c : cs) = let (st', o) = v22RxBlock fs ch c st in o : go st' cs
