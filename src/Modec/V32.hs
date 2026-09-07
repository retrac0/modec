{-# LANGUAGE BangPatterns #-}
-- | ITU-T V.32 (11/1988): the coding layer, and nothing else.
--
-- Everything here is pure and free of any sample rate: bits in, symbols
-- out, and back.  The passband machinery that carries these symbols at
-- 2400 baud on an 1800 Hz carrier lives in "Modec.QAM", and the two are
-- kept apart deliberately -- every constant in this module can be
-- checked against the Recommendation by reading it, with no DSP in the
-- way, and the Recommendation supplies enough test vectors to pin the
-- whole module down (see the @v32Tests@ group).
--
-- The three alternatives, all at 2400 baud:
--
-- * 4800 bit/s: dibits differentially encoded by Table 1\/V.32 onto the
--   four states A, B, C and D.
-- * 9600 bit/s non-redundant: quadbits, the first two differentially
--   encoded by Table 1 into the quadrant, the last two selecting one of
--   four points in it (Figure 1\/V.32).
-- * 9600 bit/s trellis coded: quadbits, the first two differentially
--   encoded by /Table 2/ -- a different table -- then fed to a
--   systematic convolutional encoder whose redundant fifth bit selects
--   between two halves of a 32-point cross (Figures 2 and 3\/V.32).
--
-- §1 e) requires every modem offering 9600 bit\/s to be able to
-- interwork using the 16-state alternative, so the non-redundant path is
-- not an optional extra: it is the one that always has to work.
module Modec.V32
  ( -- * Rates
    V32Rate (..)
  , rateBitsPerSymbol
  , rateBitRate
    -- * Scrambler (§4)
  , Direction (..)
  , Scrambler
  , scramblerInit
  , scrambleBit
  , descrambleBit
  , scrambleRun
  , descrambleRun
    -- * Signal states and constellations (Figures 1 and 3, Table 3)
  , Point
  , TrainState (..)
  , trainStates
  , statePoint
  , stateOfDibit
  , constellation
  , slicePoint
  , subsetPoint
  , gridScale
    -- * Differential encoding (Tables 1 and 2)
  , diffEncode1
  , diffDecode1
  , diffEncode2
  , diffDecode2
    -- * Trellis coding (Figure 2)
  , ConvState (..)
  , convInit
  , convStep
  , viterbiDecode
    -- * Start-up signals (§5.2, §5.3)
  , trnBits
  , trnStates
  , RateSeq (..)
  , noRates
  , rateSeqBits
  , eSeqBits
  , decodeRateSeq
  , decodeESeq
  , rateSeqCleardown
  , bestCommonRate
  ) where

import Data.Bits (testBit, (.&.), (.|.))
import Data.List (foldl')

import Modec.Scrambler (Lfsr, lfsr, scramble, descramble)
import qualified Modec.Scrambler as Scr

-- | The rates this implementation offers.  2400 bit\/s is "for further
-- study" in §2.4.3 and does not exist in any real modem, so it is not
-- here; the rate signal can still advertise it as unavailable.
data V32Rate
  = V32R4800    -- ^ 4800 bit\/s, four states, no trellis
  | V32R9600    -- ^ 9600 bit\/s, 16-point non-redundant (§2.4.1.1)
  | V32R9600T   -- ^ 9600 bit\/s, 32-point trellis coded (§2.4.1.2)
  deriving (Eq, Show, Enum, Bounded)

-- | Data bits carried per symbol.  The trellis alternative carries the
-- same four; its fifth bit is redundant.
rateBitsPerSymbol :: V32Rate -> Int
rateBitsPerSymbol V32R4800 = 2
rateBitsPerSymbol V32R9600 = 4
rateBitsPerSymbol V32R9600T = 4

rateBitRate :: V32Rate -> Int
rateBitRate r = 2400 * rateBitsPerSymbol r

-- | Which end of the call we are.  §4.1.1: the calling station scrambles
-- with GPC and descrambles with GPA, the answering station the other way
-- round.  Giving each direction its own polynomial is not only about
-- whitening -- it is what stops an echo canceller from mistaking our own
-- returning signal for the far end's.
data Direction = Calling | Answering deriving (Eq, Show)

-- | 23-bit history of the line (scrambled) bits, newest in bit 0.
newtype Scrambler = Scrambler Int deriving (Eq, Show)

scramblerInit :: Scrambler
scramblerInit = Scrambler 0

-- | The generating polynomial of each direction (§4): GPC =
-- 1 + x^-18 + x^-23 for the calling modem, GPA = 1 + x^-5 + x^-23 for
-- the answering one.
scrPoly :: Direction -> Lfsr
scrPoly Calling = lfsr 18 23
scrPoly Answering = lfsr 5 23

-- | Scramble one bit: the line bit is the data bit plus the two tapped
-- line bits, and the register then remembers it.
scrambleBit :: Direction -> Scrambler -> Bool -> (Scrambler, Bool)
scrambleBit dir (Scrambler reg) d = wrapScr (scramble (scrPoly dir) reg d)

-- | Descramble one bit.  The register takes the same line bits as the
-- far scrambler did, which is what makes it self-synchronising.
descrambleBit :: Direction -> Scrambler -> Bool -> (Scrambler, Bool)
descrambleBit dir (Scrambler reg) line = wrapScr (descramble (scrPoly dir) reg line)

-- | Scramble a run of bits, in order.
scrambleRun :: Direction -> Scrambler -> [Bool] -> (Scrambler, [Bool])
scrambleRun dir (Scrambler reg) bs = wrapScr (Scr.scrambleRun (scrPoly dir) reg bs)

-- | Descramble a run of bits, in order.  The far end scrambles with the
-- polynomial of /its/ direction, so a receiver passes the other one.
descrambleRun :: Direction -> Scrambler -> [Bool] -> (Scrambler, [Bool])
descrambleRun dir (Scrambler reg) bs = wrapScr (Scr.descrambleRun (scrPoly dir) reg bs)

wrapScr :: (Int, a) -> (Scrambler, a)
wrapScr (reg, x) = (Scrambler reg, x)

-- | A constellation point, in the Recommendation's integer grid units
-- scaled by 'gridScale'.
type Point = (Double, Double)

-- | Both signal structures have a mean square power of 10 in grid units,
-- and so do the four training states, so one scale factor normalises
-- every signal this modem sends to unit mean power.  That the training
-- states share it is not a coincidence: A, B, C and D are the four
-- points of power 10, which is why training and data go to line at the
-- same level and an echo canceller trained on one is right for the other.
gridScale :: Double
gridScale = 1 / sqrt 10

-- | The four states of Figure 1\/V.32, used at 4800 bit\/s and, at every
-- rate, for the start-up signals.
data TrainState = StA | StB | StC | StD deriving (Eq, Show, Enum, Bounded)

trainStates :: [TrainState]
trainStates = [StA, StB, StC, StD]

-- | Figure 1\/V.32: A, B, C and D are the "01" point of each quadrant --
-- the circled points, at 198.43, 288.43, 18.43 and 108.43 degrees.  Each
-- is 90 degrees from the next, so A and C are exactly antipodal: that is
-- what makes the AA-to-CC transition a phase reversal, and what puts the
-- answering modem's alternating AC signal at 1800 +/- 1200 Hz, the
-- 600 Hz and 3000 Hz the calling modem listens for.
statePoint :: TrainState -> Point
statePoint s = scalePoint $ case s of
  StA -> (-3, -1)
  StB -> (1, -3)
  StC -> (3, 1)
  StD -> (-1, 3)

-- | The state named by a differentially encoded dibit (Y1, Y2), per the
-- signal-state column of Table 1\/V.32.
stateOfDibit :: (Bool, Bool) -> TrainState
stateOfDibit (False, False) = StA
stateOfDibit (False, True) = StB
stateOfDibit (True, True) = StC
stateOfDibit (True, False) = StD

scalePoint :: (Double, Double) -> Point
scalePoint (x, y) = (x * gridScale, y * gridScale)

-- | Table 3\/V.32, non-redundant column, indexed by Y1 Y2 Q3 Q4 with Y1
-- most significant.  Y1 Y2 is the quadrant and Q3 Q4 the point within
-- it; the point within a quadrant is unchanged by a 90 degree rotation,
-- which is what lets the differential quadrant coding of Table 1 absorb
-- the receiver's four-fold phase ambiguity on its own.
points16 :: [(Double, Double)]
points16 =
  [ (-1, -1), (-3, -1), (-1, -3), (-3, -3)
  , ( 1, -1), ( 1, -3), ( 3, -1), ( 3, -3)
  , (-1,  1), (-1,  3), (-3,  1), (-3,  3)
  , ( 1,  1), ( 3,  1), ( 1,  3), ( 3,  3) ]

-- | Table 3\/V.32, trellis column, indexed by Y0 Y1 Y2 Q3 Q4 with Y0
-- most significant.  A 32-point cross on the checkerboard lattice: the
-- Y0 = 0 half has even real and odd imaginary parts, the Y0 = 1 half the
-- other way round, so the redundant bit separates two subsets whose own
-- minimum distance is larger than the whole set's.  That gap is the
-- coding gain; the Viterbi decoder exists to collect it.
points32 :: [(Double, Double)]
points32 =
  [ (-4,  1), ( 0, -3), ( 0,  1), ( 4,  1)
  , ( 4, -1), ( 0,  3), ( 0, -1), (-4, -1)
  , (-2,  3), (-2, -1), ( 2,  3), ( 2, -1)
  , ( 2, -3), ( 2,  1), (-2, -3), (-2,  1)
  , (-3, -2), ( 1, -2), (-3,  2), ( 1,  2)
  , ( 3,  2), (-1,  2), ( 3, -2), (-1, -2)
  , ( 1,  4), (-3,  0), ( 1,  0), ( 1, -4)
  , (-1, -4), ( 3,  0), (-1,  0), (-1,  4) ]

-- | The transmitted point for a coded symbol index: 2 bits at 4800
-- (a state), 4 bits at 9600 non-redundant (Y1 Y2 Q3 Q4), 5 bits at 9600
-- trellis (Y0 Y1 Y2 Q3 Q4).
constellation :: V32Rate -> Int -> Point
constellation V32R4800 i = statePoint (toEnum (i .&. 3))
constellation V32R9600 i = scalePoint (points16 !! (i .&. 15))
constellation V32R9600T i = scalePoint (points32 !! (i .&. 31))

-- | Nearest constellation point, as its index.  This is an immediate
-- decision with no memory: at 4800 and 9600 non-redundant it is the
-- decision, and on the trellis alternative it is what the carrier and
-- timing loops use, because they cannot wait for the Viterbi decoder's
-- traceback without going unstable.
slicePoint :: V32Rate -> Point -> Int
slicePoint r p = snd (minimum [ (dist2 p (constellation r i), i) | i <- [0 .. n - 1] ])
  where
    n = case r of
      V32R4800 -> 4
      V32R9600 -> 16
      V32R9600T -> 32

dist2 :: Point -> Point -> Double
dist2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)

-- | The nearest point of the trellis subset named by Y0 Y1 Y2, with its
-- squared distance and the uncoded bits that chose it.  The Viterbi
-- decoder's branch metric.
subsetPoint :: Point -> (Bool, Bool, Bool) -> (Double, (Bool, Bool))
subsetPoint p (y0, y1, y2) = minimum
  [ (dist2 p (constellation V32R9600T i), (q3, q4))
  | q3 <- [False, True], q4 <- [False, True]
  , let i = bitsToInt [y0, y1, y2, q3, q4] ]

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' (\acc b -> acc * 2 + (if b then 1 else 0)) 0

-- | Table 1\/V.32: differential quadrant coding, used at 4800 bit\/s,
-- for the non-redundant 9600 alternative, and for the rate sequences at
-- every rate.  The input dibit names a quadrant change of 90, 0, 180 or
-- 270 degrees.
diffEncode1 :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
diffEncode1 (q1, q2) prev = rotate1 (turnOf (q1, q2)) prev

-- | Recover the input dibit from this symbol's and the previous
-- symbol's quadrants.
diffDecode1 :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
diffDecode1 cur prev = turnBack (quadIndex cur - quadIndex prev)

-- | Quadrant changes, in quarter turns: Table 1's second column.
turnOf :: (Bool, Bool) -> Int
turnOf (False, False) = 1
turnOf (False, True) = 0
turnOf (True, False) = 2
turnOf (True, True) = 3

turnBack :: Int -> (Bool, Bool)
turnBack k = case k `mod` 4 of
  1 -> (False, False)
  0 -> (False, True)
  2 -> (True, False)
  _ -> (True, True)

-- | Quadrants in increasing phase: A, B, C, D are 198.43, 288.43, 18.43
-- and 108.43 degrees, so the cycle is C, D, A, B.
quadIndex :: (Bool, Bool) -> Int
quadIndex yy = case stateOfDibit yy of
  StC -> 0
  StD -> 1
  StA -> 2
  StB -> 3

quadOfIndex :: Int -> (Bool, Bool)
quadOfIndex i = case i `mod` 4 of
  0 -> (True, True)     -- C
  1 -> (True, False)    -- D
  2 -> (False, False)   -- A
  _ -> (False, True)    -- B

rotate1 :: Int -> (Bool, Bool) -> (Bool, Bool)
rotate1 k prev = quadOfIndex (quadIndex prev + k)

-- | Table 2\/V.32: the differential encoding used with the trellis
-- alternative, and deliberately /not/ Table 1.  It is a bare exclusive
-- or on each bit, not a rotation of the quadrant, because here the
-- rotational invariance is provided by the convolutional encoder.
diffEncode2 :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
diffEncode2 (q1, q2) (p1, p2) = (y1, y2)
  where
    y1 = q1 /= p1
    y2 = if q1 then q2 /= (p1 /= p2) else q2 /= p2

diffDecode2 :: (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
diffDecode2 (y1, y2) (p1, p2) = (q1, q2)
  where
    q1 = y1 /= p1
    q2 = if q1 then y2 /= (p1 /= p2) else y2 /= p2

-- | The three delay elements of Figure 2\/V.32, as bits 2, 1 and 0.
newtype ConvState = ConvState Int deriving (Eq, Ord, Show)

convInit :: ConvState
convInit = ConvState 0

-- | One step of the systematic convolutional encoder of Figure 2\/V.32.
-- Y0 is the current content of the last delay element; the two AND gates
-- are what make the code non-linear, and what make it invariant to the
-- 90 degree rotations the receiver's carrier loop cannot resolve.
convStep :: ConvState -> (Bool, Bool) -> (ConvState, Bool)
convStep (ConvState st) (y1, y2) = (ConvState st', y0)
  where
    a = testBit st 2
    b = testBit st 1
    c = testBit st 0
    y0 = c
    s = b /= y2                       -- the adder between the second and third delay
    and1 = c && s
    and2 = y1 && c
    a' = c                            -- Y0 feeds back to the first delay
    b' = ((a /= (y1 /= y2)) /= and1)
    c' = s /= and2
    st' = (if a' then 4 else 0) .|. (if b' then 2 else 0) .|. (if c' then 1 else 0)

-- | Viterbi decoder over the 8-state trellis, with @depth@ symbols of
-- traceback.  Returns (Y1, Y2, Q3, Q4) per symbol -- still differentially
-- encoded, so Table 2 undoes the rotation afterwards.
--
-- Emission is delayed by @depth@ symbols; the tail is flushed from the
-- best surviving path at the end.
viterbiDecode :: Int -> [Point] -> [(Bool, Bool, Bool, Bool)]
viterbiDecode depth = go start (0 :: Int)
  where
    -- §5.4: the encoder's delay elements start at zero, so the decoder
    -- knows the initial state and need not consider the other seven.
    start = [ (if s == 0 then 0 else 1e30, []) | s <- [0 .. 7 :: Int] ]

    -- Survivor histories are newest first and hold depth + 1 symbols:
    -- the oldest is the one just emitted, kept only so the flush at the
    -- end knows not to emit it twice.
    go sts n [] =
      let hist = snd (bestOf sts)
      in reverse (if n > 0 then init hist else hist)
    go sts n (p : ps) =
      let sts' = step sts p
          hist = snd (bestOf sts')
      in if length hist > depth
           then last hist : go sts' (n + 1) ps
           else go sts' n ps

    bestOf sts = snd (minimum [ (m, (m, h)) | (m, h) <- sts ])

    step sts p =
      [ pick s' | s' <- [0 .. 7] ]
      where
        cands =
          [ (s', (m + bm, take (depth + 1) ((y1, y2, q3, q4) : hist)))
          | (s, (m, hist)) <- zip [0 ..] sts
          , (y1, y2) <- [(False, False), (False, True), (True, False), (True, True)]
          , let (ConvState s', y0) = convStep (ConvState s) (y1, y2)
          , let (bm, (q3, q4)) = subsetPoint p (y0, y1, y2) ]
        pick s' = minimum [ c | (t, c) <- cands, t == s' ]

-- | Segment 3 of the receiver conditioning signal (§5.2.3): binary ones
-- scrambled at 4800 bit\/s from an all-zero register, with the
-- differential encoding disabled.
trnBits :: Direction -> Int -> [Bool]
trnBits dir n = go scramblerInit n
  where
    go _ 0 = []
    go sc k = let (sc', b) = scrambleBit dir sc True in b : go sc' (k - 1)

-- | The states TRN puts on the line.  For the first 256 symbols only the
-- first bit of each dibit counts, and it chooses between A and C; after
-- that Table 5\/V.32 maps the whole dibit to a state.  Getting that
-- switch-over wrong is invisible in the spectrum and fatal to the far
-- end's equaliser, so the Recommendation's own opening strings are worth
-- keeping as a test.
trnStates :: Direction -> Int -> [TrainState]
trnStates dir n = zipWith pick [0 :: Int ..] (dibits (trnBits dir (2 * n)))
  where
    dibits (a : b : rest) = (a, b) : dibits rest
    dibits _ = []
    pick i (a, b)
      | i < 256 = if a then StC else StA
      | otherwise = table5 (a, b)

-- | Table 5\/V.32.
table5 :: (Bool, Bool) -> TrainState
table5 (False, False) = StA
table5 (False, True) = StB
table5 (True, True) = StC
table5 (True, False) = StD

-- | The capabilities a rate signal carries (Table 6\/V.32).
data RateSeq = RateSeq
  { rsCan2400 :: !Bool
  , rsCan4800 :: !Bool
  , rsCan9600 :: !Bool
  , rsTrellis :: !Bool   -- ^ trellis coding available at the highest rate indicated
  } deriving (Eq, Show)

-- | All rates refused: Table 6's call for a GSTN cleardown.
noRates :: RateSeq
noRates = RateSeq False False False False

rateSeqCleardown :: RateSeq -> Bool
rateSeqCleardown r = not (rsCan2400 r || rsCan4800 r || rsCan9600 r)

-- | Table 6\/V.32: the 16 bits of a rate sequence, B0 first.  B0-B3, B7,
-- B11 and B15 are there to synchronise on, B4-B6 are the rates we can
-- receive, B8 offers trellis coding and B9-B14 = 001000 says there are
-- no special operational modes.
rateSeqBits :: RateSeq -> [Bool]
rateSeqBits r =
  [ False, False, False, False
  , rsCan2400 r, rsCan4800 r, rsCan9600 r, True
  , rsTrellis r, False, False, True
  , False, False, False, True ]

-- | Table 7\/V.32: signal E, which ends a rate signal and names the rate
-- and coding of the scrambled ones that follow it.  B0-B3 are ones
-- rather than zeros, which is what tells it from a rate sequence.
eSeqBits :: RateSeq -> [Bool]
eSeqBits r =
  [ True, True, True, True
  , rsCan2400 r, rsCan4800 r, rsCan9600 r, True
  , rsTrellis r, False, False, True
  , False, False, False, True ]

decodeRateSeq :: [Bool] -> Maybe RateSeq
decodeRateSeq = decodeSeq False

decodeESeq :: [Bool] -> Maybe RateSeq
decodeESeq = decodeSeq True

-- | §5.3.1: a sequence is only a rate signal if the synchronising bits
-- are right, which is the whole of the protection it has.
decodeSeq :: Bool -> [Bool] -> Maybe RateSeq
decodeSeq lead bs = case bs of
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15]
    | [b0, b1, b2, b3] == replicate 4 lead
    , b7, b11, b15
    , not b9, not b10, not b12, not b13, not b14 ->
        Just (RateSeq b4 b5 b6 b8)
  _ -> Nothing

-- | The best rate both ends can run, given what the far end offered and
-- what we can do.  §5.4.1: a reply must exclude anything absent from the
-- signal it answers.
bestCommonRate :: RateSeq -> RateSeq -> Maybe V32Rate
bestCommonRate ours theirs
  | rsCan9600 ours && rsCan9600 theirs =
      Just (if rsTrellis ours && rsTrellis theirs then V32R9600T else V32R9600)
  | rsCan4800 ours && rsCan4800 theirs = Just V32R4800
  | otherwise = Nothing
