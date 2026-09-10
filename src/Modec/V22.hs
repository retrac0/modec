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
-- Receiver: "Modec.QAM", wired for 600 baud and this constellation.
-- Everything from the downconversion to the equaliser is that module --
-- it was a byte-for-byte copy of it -- and what is left here is the two
-- decision paths on the symbols it returns:
--
-- * differential phase steps (no carrier lock needed; used for the
--   1200 bit/s data and for the handshake signal detectors), and
-- * a coherent path: 16-way decisions in a frame rotated into the
--   current quadrant.  The quadrant coding is differential, so the
--   four-fold phase ambiguity of the carrier lock does not matter.
--
-- The differential path is the reason this receiver needed anything of
-- QAM that V.32 did not.  It is not a second reading of the line but a
-- second use of the same one, and three of its consequences act inside
-- the symbol loop rather than after it: the carrier offset it measures
-- directly steers the frequency estimate, a constant phase step stops
-- the equaliser adapting, and 93 of them restart the coherent path
-- mid-block.
module Modec.V22
  ( TxMode (..)
  , V22TxState
  , v22TxInit
  , v22TxBlock
  , V22RxState
  , v22RxInit
  , v22RxInitWith
  , v22RxBlock
  , v22Receiver
  , v22ReceiverFrom
  , v22RxSetRate
  , v22RxSetCoherentGains
  , rxSpsEstimate
  , rxEvmEstimate
  , decisionMargin
  , rxOnes2400Run
  , rxRateOf
  , scrambleBit
  , descrambleBit
  , dibitToStep
  , stepToDibit
  , RxOut (..)
  , V22Report (..)
  , v22Modulate
  , v22ModulateAt
  , v22Demodulate
  , v22DemodulateAt
  , v22DemodulateWith
  , v22RxRun
  , rrcTaps
  , txBitsOf
  , v22TxPending
  , v22TxQueued
  , withBits
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.DSP
import Modec.FSK (frameBits)
import Modec.Link
import Modec.QAM
import Modec.Standards (Framing)
import Modec.Hdlc (hdlcFlagBits)
import Modec.Scrambler
import Modec.Stream

baud :: Double
baud = 600

rollOff :: Double
rollOff = 0.75

-- | Pulse span on each side, in symbols.
pulseSpan :: Double
pulseSpan = 6

-- | Sampled RRC kernel for the matched filter, unit energy.
rrcTaps :: Double -> VS.Vector Double
rrcTaps fs = rrcKernel fs baud rollOff pulseSpan

-- | The V.22 scrambler polynomial (§6.2), 1 + x^-14 + x^-17.  The
-- register it runs on is a 17-bit history of the line (scrambled) bits.
v22Lfsr :: Lfsr
v22Lfsr = lfsr 14 17

scrambleBit :: Int -> Bool -> (Int, Bool)
scrambleBit = scramble v22Lfsr

descrambleBit :: Int -> Bool -> (Int, Bool)
descrambleBit = descramble v22Lfsr

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
  | TxSyncData          -- ^ raw synchronous bits, idling on HDLC flags (MNP framing mode 3)
  deriving (Eq, Show)

-- | The V.22 transmitter: a QAM transmitter, plus the coder that says
-- what the symbols are.
--
-- The pulse shaping, the symbol clock and the carrier were a copy of
-- "Modec.QAM"'s, down to the expression that sums the pulse tails; what
-- is here is Table 1's quadrant coding, the scrambler and the two
-- queues feeding it.
data V22TxState = V22TxState
  { txQam      :: !QamTxState
  , txQuadrant :: !Int
  , txScr      :: !Int
  , txS1Toggle :: !Bool
  , txBits     :: [Bool]
  , txQueue    :: [Word8]
  }

v22TxInit :: V22TxState
v22TxInit = V22TxState qamTxInit 0 0 False [] []

-- | Octets still waiting to go on the line: queued bytes plus whatever
-- part of the current character has not been shifted out yet.  A protocol
-- layer above uses this to pace itself, so that a retransmission timer
-- measures the far end's silence rather than our own backlog.
v22TxPending :: V22TxState -> Int
v22TxPending st = length (txQueue st) + (length (txBits st) + 7) `div` 8

-- | Only the bytes still waiting to be framed, not the bits of the
-- character already being shifted out.  The synchronous mode keeps its
-- own frame bits in the same place as those character bits, so a caller
-- asking "has the start-stop queue drained" has to ask for this one.
v22TxQueued :: V22TxState -> Int
v22TxQueued = length . txQueue

-- | Generate @n@ samples.  @guard@ adds the 1800 Hz guard tone at -6 dB
-- (high channel option).
v22TxBlock :: Double -> V22Channel -> Framing -> Double -> Bool -> Rate -> TxMode -> [Word8] -> Int -> V22TxState -> (V22TxState, Signal)
v22TxBlock fs ch fr amp guard rate mode newBytes n st0 = (st', sig)
  where
    p = (v22Params fs ch)
      { qpGuard = if guard && ch == HighChannel then Just (1800, 0.5) else Nothing }
    -- Exactly the symbols this block has room for, and no more: the
    -- coder must not be run speculatively -- every symbol it makes
    -- advances the scrambler and the quadrant, and one made and not
    -- sent is one the far end never sees the effect of.
    want = qamTxSymbolsFor p n (txQam st0)
    (stC, pts) = coded want (st0 { txQueue = txQueue st0 ++ newBytes }) []
    coded 0 st acc = (st, reverse acc)
    coded k st acc = let (st1, pt) = emit st in coded (k - 1 :: Int) st1 (pt : acc)
    (qam', sig, _) = qamTxBlock p amp n pts (txQam stC)
    st' = stC { txQam = qam' }

    -- One symbol: a differential quadrant change from the first dibit,
    -- and at 2400 bit/s a point within that quadrant from the second.
    emit st =
      let (b1, b2, stA) = case mode of
            TxS1 -> (txS1Toggle st, txS1Toggle st, st { txS1Toggle = not (txS1Toggle st) })
            _ -> let (x, s1) = nextBit st; (y, s2) = nextBit s1 in (x, y, s2)
          q = (txQuadrant stA + dibitToStep b1 b2) `mod` 4
          (b3, b4, stB) = case (rate, mode) of
            (R2400, TxScrambledOnes) -> let (x, s1) = nextBit stA; (y, s2) = nextBit s1 in (x, y, s2)
            (R2400, TxScrambledData) -> let (x, s1) = nextBit stA; (y, s2) = nextBit s1 in (x, y, s2)
            -- synchronous data is data: at 2400 bit/s it takes the second
            -- dibit and the full sixteen-point constellation like any
            -- other, and leaving it out of this list transmits half the
            -- bits on the 1200 bit/s points while the far end decodes four
            -- to the symbol
            (R2400, TxSyncData) -> let (x, s1) = nextBit stA; (y, s2) = nextBit s1 in (x, y, s2)
            _ -> (False, True, stA)     -- 1200 bit/s and handshake signals use the "01" points
          (gx, gy) = gridPoint q b3 b4
      in (stB { txQuadrant = q }, (gx * gridScale, gy * gridScale))
    nextBit st = case mode of
      TxU11 -> (True, st)
      -- Synchronous: the bits arrive already framed from the protocol
      -- layer, and the interframe fill is the flag, as ISO 3309 requires.
      -- The scrambler still runs underneath: a bare stream of flags is a
      -- strong periodic pattern that the far end's timing recovery would
      -- not enjoy.
      TxSyncData -> case txBits st of
        (b : bs) -> scr b st { txBits = bs }
        [] -> case hdlcFlagBits of
          (b : bs) -> scr b st { txBits = bs }
          [] -> scr True st
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

-- | Pending data bits of a transmitter (for experiments and tests).
txBitsOf :: V22TxState -> [Bool]
txBitsOf = txBits

-- | Append data bits to a transmitter's pending bits.
withBits :: V22TxState -> [Bool] -> V22TxState
withBits st bs = st { txBits = txBits st ++ bs }

-- | Number of T/2-spaced equaliser taps.
eqTaps :: Int
eqTaps = 15

-- | The V.22 receiver: a QAM receiver, plus what only V.22 counts.
--
-- Everything from the matched filter to the equaliser is "Modec.QAM" --
-- the AGC, Gardner timing, the decision-directed carrier loop and the
-- T\/2 LMS were byte-identical between the two pumps.  What is left here
-- is what V.22 alone has: which rate it is deciding at, the
-- descrambler, and the run lengths the handshake's timings are measured
-- against.
data V22RxState = V22RxState
  { rxQam      :: !QamRxState
  , rxRate     :: !Rate
  , rxTune     :: !V22Tune           -- ^ loop gains, which a caller may change mid-call
  , rxDescr    :: !Int
  , rxQuadrant :: !Int               -- ^ quadrant of the previous coherent decision
  , rxOnesRun  :: !Int
  , rxU11Run   :: !Int
  , rxS1Run    :: !Int
  , rxOnes2400 :: !Int               -- ^ consecutive descrambled ones decided 16-way
  , rxLastStep :: !Int
  }

-- | The loop gains, which 'v22RxSetCoherentGains' and 'v22RxInitWith'
-- can change during a call.  "Modec.QAM" takes its configuration per
-- block rather than storing it, so a receiver whose gains move rebuilds
-- one each block and a receiver whose gains are fixed builds it once.
data V22Tune = V22Tune
  { tuKp    :: !Double
  , tuKi    :: !Double
  , tuClamp :: !Double
  , tuThKp  :: !Double
  , tuThKi  :: !Double
  , tuEqMu  :: !Double
  } deriving (Eq, Show)

defaultTune :: V22Tune
defaultTune = V22Tune 0.16 0.002 2 0.15 0.01 0.004

-- | The line, as "Modec.QAM" describes one.
v22Params :: Double -> V22Channel -> QamParams
v22Params fs ch = QamParams
  { qpFs = fs, qpBaud = baud, qpCarrier = carrierOf ch
  , qpRollOff = rollOff, qpSpan = pulseSpan }

-- | V.22's receiver as a QAM configuration.
--
-- Every field that differs from 'defaultRxCfg' is a measured V.22
-- number that used to be written into the loop or carried in the state
-- record.  The ones worth naming:
--
-- * @qrPower = 10@ -- V.22 works in grid units, where the sixteen
--   points sit on the odd lattice and average a squared magnitude of
--   10.  V.32 pre-scales its constellation to unit power instead.  Both
--   are exact; each matches the figure in its own Recommendation.
-- * @qrAgcRate = qrAgcSettled@ -- V.22 picks the gain's speed by rate
--   rather than by elapsed symbols, so both are the same number and
--   'agcSettleSyms' cannot bite.  No acquisition problem comes with the
--   slow rate at 2400, because the rate only becomes 'R2400' after S1,
--   by which point the estimate has spent the whole handshake
--   converging on a constant-amplitude signal.
-- * @qrTrackAt = 0@, with @qrLockAt@ and @qrLoopGate@ infinite -- V.22
--   has one timing bandwidth and no lock gate.
-- * @qrRestartOn@ -- the 93rd consecutive symbol of the answerer's
--   unscrambled ones restarts the coherent path, mid-block, because it
--   has to take effect for the rest of that block.  Note the
--   off-by-one: V.22 counted the current symbol, so its 93 is 92
--   further repeats.
v22RxCfg :: Rate -> V22Tune -> QamRxCfg
v22RxCfg rate tu = (defaultRxCfg (sliceGrid rate) pointOfIndex)
  { qrKp = tuKp tu, qrKi = tuKi tu
  , qrKpTrack = tuKp tu, qrKiTrack = tuKi tu, qrTrackAt = 0
  , qrClamp = tuClamp tu
  , qrThKp = tuThKp tu, qrThKi = tuThKi tu
  , qrEqMu = tuEqMu tu, qrEqTaps = eqTaps
  , qrAgcRate = agc, qrAgcSettled = agc
  , qrLockAt = 1 / 0, qrLoopGate = 1 / 0
  , qrEvmFreeze = 4, qrEvmGiveUp = 50, qrEvmBad = Just 2
  , qrAdapt = True, qrTrack = True, qrPower = 10
  , qrSteerAt = 1, qrAdaptAt = 1, qrAdaptRun = 16
  , qrFreqFf = ff, qrFreqFfRun = 16
  , qrRestartOn = \t -> qtStep t == 3 && qtStepRun t == 92
  , qrResetLine = False
  }
  where
    agc = case rate of { R1200 -> 0.05; R2400 -> 0.005 }
    -- Only while training at 1200: the differential path sees the
    -- carrier offset directly and pulls the estimate in, so the
    -- decision-directed loop has only the residual to track.
    ff = case rate of { R1200 -> 0.03; R2400 -> 0 }

-- | The sixteen odd-grid points, indexed.  Both rates decide into this
-- space: the four points used at 1200 bit\/s are the \"01\" point of each
-- quadrant, which are four of these sixteen.
pointOfIndex :: Int -> (Double, Double)
pointOfIndex i = (coord (i `div` 4), coord (i `mod` 4))
  where coord k = 2 * fromIntegral k - 3

indexOfPoint :: (Double, Double) -> Int
indexOfPoint (x, y) = slot x * 4 + slot y
  where slot v = round ((v + 3) / 2) :: Int

-- | The slicer, at the rate being decided.
sliceGrid :: Rate -> (Double, Double) -> Int
sliceGrid R2400 (x, y) = indexOfPoint (sliceOdd x, sliceOdd y)
sliceGrid R1200 (x, y) = indexOfPoint (decide4 x y)

-- | The last two quadbit bits of a decided point, read in the frame
-- rotated into the first quadrant.  At 1200 bit\/s this is (False, True)
-- for all four points, which is what the \"01\" in their name means.
bitsOfPoint :: (Double, Double) -> (Bool, Bool)
bitsOfPoint (px, py) =
  let (rx, ry) = case quadrantOf px py of
        0 -> (px, py)
        1 -> (py, -px)
        2 -> (-px, -py)
        _ -> (-py, px)
  in (ry > 2, rx > 2)

v22RxInit :: Double -> V22RxState
v22RxInit fs = v22RxInitWith fs 0 (0.16, 0.002, 2)

-- | Receiver with an initial timing offset (samples) and loop
-- parameters (proportional gain, integral gain, error clamp); for
-- experiments.
v22RxInitWith :: Double -> Double -> (Double, Double, Double) -> V22RxState
v22RxInitWith fs tauOff (kp, ki, cl) = V22RxState
  { rxQam = qamRxInitWith (v22Params fs LowChannel) (v22RxCfg R1200 tune) seed
  , rxRate = R1200, rxTune = tune
  , rxDescr = 0, rxQuadrant = 0
  , rxOnesRun = 0, rxU11Run = 0, rxS1Run = 0, rxOnes2400 = 0, rxLastStep = 0
  }
  where
    tune = defaultTune { tuKp = kp, tuKi = ki, tuClamp = cl }
    -- Where V.22 starts, which is not where V.32 does: a symbol later,
    -- with the previous symbol at (1, 0) and the error estimate at 1.
    -- The first Gardner error and the first differential step both
    -- follow from those, so they are part of the receiver's behaviour
    -- rather than an arbitrary zero.
    seed = QamRxSeed { srTau0 = fs / baud + tauOff, srPower0 = 1e-6
                     , srEvm0 = 1, srPrev0 = (1, 0) }

-- | Set the coherent loop gains (carrier proportional, carrier integral,
-- equaliser step); for experiments.
v22RxSetCoherentGains :: (Double, Double, Double) -> V22RxState -> V22RxState
v22RxSetCoherentGains (a, b, c) st =
  st { rxTune = (rxTune st) { tuThKp = a, tuThKi = b, tuEqMu = c } }

-- | Switch the decision rate (the handshake does this after S1).
v22RxSetRate :: Rate -> V22RxState -> V22RxState
v22RxSetRate r st = st { rxRate = r, rxOnes2400 = 0 }

-- | Tracked coherent decision error power (for tracing).
rxEvmEstimate :: V22RxState -> Double
rxEvmEstimate = qamRxEvm . rxQam

-- | Half the distance to the nearest wrong answer, in the grid units the
-- AGC normalises to and 'rxEvmEstimate' is squared in.
--
-- The two rates decide against different things and the same error means
-- different things to them.  At 2400 the sixteen points sit on the odd
-- grid two apart, so a symbol is wrong once it has moved 1.0.  At 1200
-- only the four "01" points are transmitted, at a radius of sqrt 10 and
-- a quarter turn apart, which puts the nearest wrong answer 2 * sqrt 5
-- away and the boundary at sqrt 5.
decisionMargin :: Rate -> Double
decisionMargin R2400 = 1.0
decisionMargin R1200 = sqrt 5

-- | Consecutive 16-way-decided descrambled ones (for tracing).
rxOnes2400Run :: V22RxState -> Int
rxOnes2400Run = rxOnes2400

-- | Current decision rate.
rxRateOf :: V22RxState -> Rate
rxRateOf = rxRate

-- | Current samples-per-symbol estimate of the timing loop.
rxSpsEstimate :: V22RxState -> Double
rxSpsEstimate = qamRxSps . rxQam

-- | What a V.22 receiver listening to the remote channel currently sees,
-- as the handshake needs it: the run lengths its timings are measured
-- against, and how cleanly the phase steps land.
--
-- A projection of 'RxOut', built once per audio block.  It lives here
-- rather than with the handshake because it is the receiver's report --
-- the handshake is one reader of it, not its owner.
data V22Report = V22Report
  { vrEnergy   :: !Double
  , vrAngleErr :: !Double   -- ^ mean phase-step error in degrees
  , vrU11Run   :: !Int      -- ^ consecutive symbols of unscrambled ones
  , vrOnesRun  :: !Int      -- ^ consecutive descrambled ones
  , vrZerosRun :: !Int      -- ^ consecutive descrambled zeros
  , vrS1Run    :: !Int      -- ^ consecutive symbols of the S1 double-dibit pattern
  , vrOnes2400 :: !Int      -- ^ consecutive descrambled ones decided 16-way
  } deriving (Show)

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

-- | Decision at 1200 bit/s: the "01" point of the quadrant, with sectors
-- centred on those points (18.4 degrees into each quadrant).
decide4 :: Double -> Double -> (Double, Double)
decide4 x y = gridPoint q False True
  where
    ang = atan2 y x - atan2 1 3
    q = (floor ((ang + pi / 4) / (pi / 2)) :: Int) `mod` 4

-- | The receiver as a stream stage, so it composes with the rest of the
-- chain and does not care how the audio is cut up.
v22Receiver :: Double -> V22Channel -> Stage Signal RxOut
v22Receiver fs ch = v22ReceiverFrom fs ch (v22RxInit fs)

-- | 'v22Receiver' resuming from a receiver that has already run, or one
-- set to a rate other than the 1200 bit\/s it starts at.
v22ReceiverFrom :: Double -> V22Channel -> V22RxState -> Stage Signal RxOut
v22ReceiverFrom fs ch st0 = Stage st0 (\st chunk -> v22RxBlock fs ch chunk st)

v22RxBlock :: Double -> V22Channel -> Signal -> V22RxState -> (V22RxState, RxOut)
v22RxBlock fs ch chunk st0 = (st', out)
  where
    cfg = v22RxCfg (rxRate st0) (rxTune st0)
    (qam', syms) = qamRxBlock (v22Params fs ch) cfg chunk (rxQam st0)

    -- The differential path, folded over the symbols in the order they
    -- were decided.  It is recomputed here rather than read off 'qsStep'
    -- and 'qsDev' so that the angle -- and the mean error in degrees
    -- that the handshake reads -- is the same expression it always was,
    -- to the last bit.  The loop keeps its own copy because three things
    -- inside it act on the difference for the next symbol.
    steps = differential (qamRxPrevSym (rxQam st0)) syms
    differential _ [] = []
    differential (pr, pim) (sy : rest) =
      let (yr, yi) = qsRaw sy
          dr = yr * pr + yi * pim
          di = yi * pr - yr * pim
          ang = atan2 di dr
          stepQ = (round (ang / (pi / 2)) :: Int) `mod` 4
          aerr = let d = ang * 180 / pi
                     qd = fromIntegral (round (d / 90) :: Int) * 90
                 in abs (d - qd)
      in (stepQ, aerr) : differential (yr, yi) rest

    -- Everything V.22 counts, in one pass: the descrambler and the four
    -- run lengths the handshake's timings are measured against.  These
    -- were inside the symbol loop, where they had no business being --
    -- they read the decisions and never feed them.
    tally = foldl one (rxDescr st0, rxQuadrant st0, rxLastStep st0,
                       rxOnesRun st0, rxU11Run st0, rxS1Run st0, rxOnes2400 st0,
                       [], [], []) (zip syms steps)
    one (reg, q0, lastStep, ones0, u110, s10, _, dbs, bits, aerrs) (sy, (stepQ, aerr)) =
      let point = pointOfIndex (qsIndex sy)
          (b3, b4) = bitsOfPoint point
          q = quadrantOf (fst point) (snd point)
          bitsIn = case rxRate st0 of
            R1200 -> let (d1, d2) = stepToDibit stepQ in [d1, d2]
            R2400 -> let (c1, c2) = stepToDibit ((q - q0) `mod` 4) in [c1, c2, b3, b4]
          (reg', descBits) = descrambleRun v22Lfsr reg bitsIn
          ones = foldl (\acc b -> if b then acc + 1 else 0) ones0 descBits
          u11 = if stepQ == 3 then u110 + 1 else 0
          s1 | (stepQ == 1 || stepQ == 3) && stepQ /= lastStep
               && (lastStep == 1 || lastStep == 3) = s10 + 1
             | stepQ == 1 || stepQ == 3 = 1
             | otherwise = 0
          ones2400 = case rxRate st0 of { R2400 -> ones; R1200 -> 0 }
      in (reg', q, stepQ, ones, u11, s1, ones2400,
          stepQ : dbs, reverse descBits ++ bits, aerr : aerrs)
    (descr', quad', lastStep', ones', u11', s1', ones2400', dibitsR, bitsR, aerrsR) = tally

    -- Summed newest-first, which is the order the loop accumulated them
    -- in and never reversed.  Floating-point addition is not
    -- associative, so this is not a matter of taste: summing the other
    -- way changes the mean in the last bits, and the handshake compares
    -- it against 8 degrees.
    angleErr = if null aerrsR then 0 else sum aerrsR / fromIntegral (length aerrsR)

    st' = st0
      { rxQam = qam', rxDescr = descr', rxQuadrant = quad', rxLastStep = lastStep'
      , rxOnesRun = ones', rxU11Run = u11', rxS1Run = s1', rxOnes2400 = ones2400' }
    out = RxOut (map qsPoint syms) (reverse dibitsR) (reverse bitsR)
                (qamRxEnergy qam') angleErr (qamRxEvm qam')
                ones' u11' s1' ones2400'

-- | Offline: scrambled data bits to a signal at 1200 bit/s.
v22Modulate :: Double -> V22Channel -> Double -> [Bool] -> Signal
v22Modulate fs ch = v22ModulateAt fs ch R1200

v22ModulateAt :: Double -> V22Channel -> Rate -> Double -> [Bool] -> Signal
v22ModulateAt fs ch rate amp bits = VS.concat (go v22TxInit bits)
  where
    n = blockOf fs
    go st bs
      | null bs && null (txBits st) = flushTail st
      | otherwise =
          let (st', sig) = v22TxBlock fs ch frDummy amp False rate TxScrambledData [] n st { txBits = txBits st ++ take 2000 bs }
          in sig : go st' (drop 2000 bs)
    flushTail st = let (_, sig) = v22TxBlock fs ch frDummy amp False rate TxScrambledOnes [] (round (fs * 0.05)) st in [sig]
    frDummy = error "framing not used"

-- | Offline: descrambled bits from a signal at 1200 bit/s.
v22Demodulate :: Double -> V22Channel -> Signal -> [Bool]
v22Demodulate fs ch = v22DemodulateWith fs (v22RxInit fs) ch

v22DemodulateAt :: Double -> V22Channel -> Rate -> Signal -> [Bool]
v22DemodulateAt fs ch rate = v22DemodulateWith fs (v22RxSetRate rate (v22RxInit fs)) ch

v22DemodulateWith :: Double -> V22RxState -> V22Channel -> Signal -> [Bool]
v22DemodulateWith fs st0 ch x = concatMap roBits (v22RxRun fs st0 ch x)

-- | Offline: all receiver outputs per 20 ms block.
v22RxRun :: Double -> V22RxState -> V22Channel -> Signal -> [RxOut]
v22RxRun fs st0 ch x = runStage (v22ReceiverFrom fs ch st0) (chunksOf (blockOf fs) x)
