{-# LANGUAGE BangPatterns #-}
-- | ITU-T V.22 data pump: 1200 bit/s, 600 baud differential 4-PSK on a
-- 1200 Hz (low, calling modem) or 2400 Hz (high, answering modem)
-- carrier, square-root raised-cosine shaping with 75 % roll-off, and the
-- 1 + x^-14 + x^-17 self-synchronising scrambler.
--
-- Transmitter: symbols are generated on a fractional symbol clock and
-- the shaped baseband is evaluated per output sample from the last
-- twelve symbols, so any sample rate works without an integer
-- samples-per-symbol.  Modes cover the handshake signals (unscrambled
-- binary 1, the S1 double-dibit pattern, scrambled binary 1) and
-- scrambled data with an idle-mark byte queue.
--
-- Receiver: complex downconversion at the nominal carrier, matched RRC
-- filter, Gardner symbol timing recovery with cubic interpolation,
-- differential phase decisions (no carrier loop is needed for DQPSK:
-- the 7 Hz offset the standard allows rotates only 4 degrees per
-- symbol), then descrambling.  Runs of unscrambled ones, S1 and
-- scrambled ones are counted for the handshake.
module Modec.V22
  ( V22Channel (..)
  , carrierOf
  , TxMode (..)
  , V22TxState
  , v22TxInit
  , v22TxBlock
  , V22RxState
  , v22RxInit
  , v22RxInitWith
  , v22RxBlock
  , rxSpsEstimate
  , scrambleBit
  , descrambleBit
  , dibitToStep
  , stepToDibit
  , RxOut (..)
  , v22Modulate
  , v22Demodulate
  , v22DemodulateWith
  , v22RxRun
  , rrcTaps
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

data TxMode
  = TxU11               -- ^ unscrambled binary 1
  | TxS1                -- ^ unscrambled repetitive double dibit 00 and 11
  | TxScrambledOnes
  | TxScrambledData     -- ^ queued bytes with start/stop framing, idle mark
  deriving (Eq, Show)

data V22TxState = V22TxState
  { txSymClock :: !Double            -- ^ sample index (relative to the block) of the next symbol centre
  , txSymT0    :: !Double            -- ^ centre of the first stored symbol
  , txSymbols  :: [Int]              -- ^ quadrants of stored symbols, oldest first, sps apart from txSymT0
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
v22TxBlock :: Double -> V22Channel -> Framing -> Double -> Bool -> TxMode -> [Word8] -> Int -> V22TxState -> (V22TxState, Signal)
v22TxBlock fs ch fr amp guard mode newBytes n st0 = (st', sig)
  where
    sps = fs / baud
    fc = carrierOf ch
    wc = 2 * pi * fc / fs
    wg = 2 * pi * 1800 / fs
    st1 = st0 { txQueue = txQueue st0 ++ newBytes }
    -- make sure symbols exist up to pulseSpan symbols past the block end
    stFilled = fill st1
    fill st
      | txSymClock st <= fromIntegral n + pulseSpan * sps = fill (emit st)
      | otherwise = st
    emit st =
      let (b1, b2, st2') = case mode of
            -- S1 alternates whole dibits 00 and 11
            TxS1 -> (txS1Toggle st, txS1Toggle st, st { txS1Toggle = not (txS1Toggle st) })
            _ -> let (x, s1) = nextBit st; (y, s2) = nextBit s1 in (x, y, s2)
          q = (txQuadrant st2' + dibitToStep b1 b2) `mod` 4
          t0 = if null (txSymbols st) then txSymClock st else txSymT0 st
      in st2' { txSymClock = txSymClock st + sps, txQuadrant = q, txSymT0 = t0, txSymbols = txSymbols st2' ++ [q] }
    nextBit st = case mode of
      TxU11 -> (True, st)
      TxS1 -> (True, st)   -- handled in emit
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
    symsV = VS.fromList (map fromIntegral (txSymbols stFilled)) :: VS.Vector Double
    nSyms = VS.length symsV
    t0s = txSymT0 stFilled
    sig = VS.generate n $ \i ->
      let t = fromIntegral i
          kLo = max 0 (ceiling ((t - pulseSpan * sps - t0s) / sps))
          kHi = min (nSyms - 1) (floor ((t + pulseSpan * sps - t0s) / sps))
          accum !k !a !b
            | k > kHi = (a, b)
            | otherwise =
                let u = (t - (t0s + fromIntegral k * sps)) / sps
                    p = rrc u
                    (cr, ci) = quadrantPoint (round (VS.unsafeIndex symsV k))
                in accum (k + 1) (a + p * cr) (b + p * ci)
          (re, im) = accum kLo 0 0
          th = txCarrier stFilled + wc * t
          g = if guard && ch == HighChannel then 0.5 * sin (txGuard stFilled + wg * t) else 0
      in amp * (re * cos th - im * sin th + g)
    wrap p = p - 2 * pi * fromIntegral (floor (p / (2 * pi)) :: Int)
    -- drop symbols that can no longer reach the next block
    dropN = max 0 (floor ((fromIntegral n - (pulseSpan + 1) * sps - t0s) / sps)) :: Int
    st' = stFilled
      { txSymClock = txSymClock stFilled - fromIntegral n
      , txSymT0 = t0s + fromIntegral dropN * sps - fromIntegral n
      , txSymbols = drop dropN (txSymbols stFilled)
      , txCarrier = wrap (txCarrier stFilled + wc * fromIntegral n)
      , txGuard = wrap (txGuard stFilled + wg * fromIntegral n)
      }

-- | Unit-magnitude constellation point for a quadrant (45, 135, 225, 315 degrees).
quadrantPoint :: Int -> (Double, Double)
quadrantPoint q = case q `mod` 4 of
  0 -> (r, r)
  1 -> (-r, r)
  2 -> (-r, -r)
  _ -> (r, -r)
  where r = sqrt 0.5

data V22RxState = V22RxState
  { rxN        :: !Int               -- ^ global index of the next input sample (for the mixer phase)
  , rxHistRe   :: !Signal
  , rxHistIm   :: !Signal
  , rxPrevRe   :: !Signal            -- ^ last few matched-filter outputs carried across blocks
  , rxPrevIm   :: !Signal
  , rxTau      :: !Double            -- ^ next symbol sampling position in the carried+block coordinates
  , rxSps      :: !Double
  , rxPrevSym  :: !(Double, Double)
  , rxPower    :: !Double
  , rxDescr    :: !Int
  , rxOnesRun  :: !Int               -- ^ consecutive descrambled ones
  , rxU11Run   :: !Int               -- ^ consecutive dibits 11 (unscrambled binary 1)
  , rxS1Run    :: !Int               -- ^ consecutive alternating 00/11 dibits
  , rxLastStep :: !Int
  , rxKp       :: !Double
  , rxKi       :: !Double
  , rxClamp    :: !Double
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
  where
    taps = VS.length (rrcTaps fs)
    sps = fs / baud
    carry = 4 + ceiling sps

-- | Current samples-per-symbol estimate of the timing loop.
rxSpsEstimate :: V22RxState -> Double
rxSpsEstimate = rxSps

data RxOut = RxOut
  { roSymbols  :: [(Double, Double)]   -- ^ symbol samples (for constellation display)
  , roDibits   :: [Int]                -- ^ phase steps 0..3
  , roBits     :: [Bool]               -- ^ descrambled bits, two per symbol
  , roEnergy   :: !Double              -- ^ mean baseband power over the block
  , roAngleErr :: !Double              -- ^ mean |phase step error| in degrees (decision quality)
  , roOnesRun  :: !Int
  , roU11Run   :: !Int
  , roS1Run    :: !Int
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

v22RxBlock :: Double -> V22Channel -> Signal -> V22RxState -> (V22RxState, RxOut)
v22RxBlock fs ch chunk st0 = (st', out)
  where
    n = VS.length chunk
    wc = 2 * pi * carrierOf ch / fs
    n0 = rxN st0
    mixRe = VS.imap (\i v -> v * cos (wc * fromIntegral (n0 + i))) chunk
    mixIm = VS.imap (\i v -> negate v * sin (wc * fromIntegral (n0 + i))) chunk
    hrev = VS.reverse (rrcTaps fs)   -- symmetric anyway
    (mfRe, histRe') = firStream hrev (rxHistRe st0) mixRe
    (mfIm, histIm') = firStream hrev (rxHistIm st0) mixIm
    extRe = rxPrevRe st0 VS.++ mfRe
    extIm = rxPrevIm st0 VS.++ mfIm
    len = VS.length extRe
    energy = if n == 0 then 0 else (VS.sum (VS.map (\v -> v * v) mfRe) + VS.sum (VS.map (\v -> v * v) mfIm)) / fromIntegral n
    -- symbol loop
    kp = rxKp st0
    ki = rxKi st0
    go st syms dibits bits
      | floor (rxTau st) + 2 >= len = (st, reverse syms, reverse dibits, reverse bits)
      | rxTau st - rxSps st / 2 < 1 = go st { rxTau = rxTau st + rxSps st } syms dibits bits
      | otherwise =
          let tau = rxTau st
              sps = rxSps st
              yr = cubicAt extRe tau; yi = cubicAt extIm tau
              hr = cubicAt extRe (tau - sps / 2); hi = cubicAt extIm (tau - sps / 2)
              (pr, pim) = rxPrevSym st
              -- Gardner timing error, normalised by signal power, clamped,
              -- and ignored while there is no signal (otherwise silence
              -- produces huge corrections and the loop runs away)
              pw = 0.95 * rxPower st + 0.05 * (yr * yr + yi * yi)
              eRaw = ((yr - pr) * hr + (yi - pim) * hi) / max 1e-9 pw
              e = if pw < 1e-5 then 0 else max (negate (rxClamp st)) (min (rxClamp st) eRaw)
              -- positive e means the sample point is late (found empirically
              -- against the modulator: the loop only locks with this sign)
              sps' = sps - ki * e
              tau' = tau + max (0.5 * sps) (sps' - kp * e)
              -- differential phase step in quadrants
              dr = yr * pr + yi * pim
              di = yi * pr - yr * pim
              ang = atan2 di dr
              stepQ = (round (ang / (pi / 2)) :: Int) `mod` 4
              (b1, b2) = stepToDibit stepQ
              (reg1, d1) = descrambleBit (rxDescr st) b1
              (reg2, d2) = descrambleBit reg1 b2
              ones = if d1 && d2 then rxOnesRun st + 2 else if d2 then 1 else 0
              u11 = if stepQ == 3 then rxU11Run st + 1 else 0
              s1 = if (stepQ == 1 || stepQ == 3) && stepQ /= rxLastStep st && (rxLastStep st == 1 || rxLastStep st == 3)
                     then rxS1Run st + 1 else (if stepQ == 1 || stepQ == 3 then 1 else 0)
              st1 = st { rxTau = tau', rxSps = max (0.9 * (fs / baud)) (min (1.1 * (fs / baud)) sps')
                       , rxPrevSym = (yr, yi), rxPower = pw, rxDescr = reg2
                       , rxOnesRun = ones, rxU11Run = u11, rxS1Run = s1, rxLastStep = stepQ }
          in go st1 ((yr, yi) : syms) (stepQ : dibits) (d2 : d1 : bits)
    (stSym, symsOut, dibitsOut, bitsOut) = go st0 [] [] []
    angleErr =
      let steps = zip symsOut (rxPrevSym st0 : symsOut)
          errs = [ let d = atan2 (yi * pr - yr * pim) (yr * pr + yi * pim) * 180 / pi
                       q = fromIntegral (round (d / 90) :: Int) * 90
                   in abs (d - q)
                 | ((yr, yi), (pr, pim)) <- steps ]
      in if null errs then 0 else sum errs / fromIntegral (length errs)
    carry = VS.length (rxPrevRe st0)
    keepFrom = max 0 (len - carry)
    st' = stSym
      { rxN = n0 + n
      , rxHistRe = histRe', rxHistIm = histIm'
      , rxPrevRe = VS.drop keepFrom extRe, rxPrevIm = VS.drop keepFrom extIm
      , rxTau = rxTau stSym - fromIntegral keepFrom
      }
    out = RxOut symsOut dibitsOut bitsOut energy angleErr (rxOnesRun stSym) (rxU11Run stSym) (rxS1Run stSym)

-- | Offline: scrambled data bits to a signal (idle-free, exactly the bits given).
v22Modulate :: Double -> V22Channel -> Double -> [Bool] -> Signal
v22Modulate fs ch amp bits = VS.concat (go v22TxInit bits)
  where
    n = 160
    go st bs
      | null bs && null (txBits st) = flushTail st
      | otherwise =
          let (st', sig) = v22TxBlock fs ch (frDummy) amp False TxScrambledData [] n st { txBits = txBits st ++ take 2000 bs }
          in sig : go st' (drop 2000 bs)
    flushTail st = let (_, sig) = v22TxBlock fs ch frDummy amp False TxScrambledOnes [] (round (fs * 0.05)) st in [sig]
    frDummy = error "framing not used"

-- | Offline: descrambled bits from a signal.
v22Demodulate :: Double -> V22Channel -> Signal -> [Bool]
v22Demodulate fs ch = v22DemodulateWith (v22RxInit fs) ch

v22DemodulateWith :: V22RxState -> V22Channel -> Signal -> [Bool]
v22DemodulateWith st0 ch x = concat (go st0 (chunks x))
  where
    fs = 8000 :: Double
    _ = fs
    chunks v | VS.null v = [] | otherwise = VS.take 160 v : chunks (VS.drop 160 v)
    go _ [] = []
    go st (c : cs) = let (st', o) = v22RxBlock 8000 ch c st in roBits o : go st' cs

-- | Offline: all receiver outputs per 160-sample block.
v22RxRun :: Double -> V22RxState -> V22Channel -> Signal -> [RxOut]
v22RxRun fs st0 ch x = go st0 (chunks x)
  where
    chunks v | VS.null v = [] | otherwise = VS.take 160 v : chunks (VS.drop 160 v)
    go _ [] = []
    go st (c : cs) = let (st', o) = v22RxBlock fs ch c st in o : go st' cs
