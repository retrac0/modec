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
  , rateTrellis
  , rateUncoded
  , allV32Rates
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
  , dibitOfState
  , bitsToInt
  , constellation
  , slicePoint
  , rateMargin
  , subsetPoint
  , gridScale
    -- * Differential encoding (Tables 1 and 2)
  , diffEncode1
  , diffDecode1
  , dibitOfTurn
  , turnOfDibit
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
  , allRates
  , defaultRates
  , v32Rates
  , v32bisRates
  , rateSeqV32bis
  , rateSeqBits
  , eSeqBits
  , decodeRateSeq
  , decodeESeq
  , rateSeqCleardown
  , bestCommonRate
  , ratesBelow
  , chosenRate
  ) where

import Data.Maybe (listToMaybe)
import Data.Bits (testBit, (.|.))
import Control.Monad (replicateM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as VU

import Modec.Scrambler (Lfsr, lfsr, scramble, descramble)
import qualified Modec.Scrambler as Scr

-- | The rates this implementation offers.  2400 bit\/s is "for further
-- study" in §2.4.3 and does not exist in any real modem, so it is not
-- here; the rate signal can still advertise it as unavailable.
data V32Rate
  = V32R4800    -- ^ 4800 bit\/s, four states, no trellis (V.32 §2.4.2)
  | V32R7200    -- ^ 7200 bit\/s, 16-point trellis coded (V.32bis §2.3.4)
  | V32R9600    -- ^ 9600 bit\/s, 16-point non-redundant (V.32 §2.4.1.1)
  | V32R9600T   -- ^ 9600 bit\/s, 32-point trellis coded (V.32 §2.4.1.2)
  | V32R12000   -- ^ 12000 bit\/s, 64-point trellis coded (V.32bis §2.3.2)
  | V32R14400   -- ^ 14400 bit\/s, 128-point trellis coded (V.32bis §2.3.1)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The V.32bis rates, best first.  4800 and 9600 are V.32's and are
-- reached by a V.32bis modem talking to a V.32 one; Table 5\/V.32bis
-- Note 1 says as much, by making the bits for those two rates
-- permanently set.
allV32Rates :: [V32Rate]
allV32Rates = [V32R14400, V32R12000, V32R9600T, V32R9600, V32R7200, V32R4800]

-- | Data bits carried per symbol.  A trellis rate's redundant bit is not
-- among them: it is the constellation that grows, not the payload.
rateBitsPerSymbol :: V32Rate -> Int
rateBitsPerSymbol V32R4800 = 2
rateBitsPerSymbol V32R7200 = 3
rateBitsPerSymbol V32R9600 = 4
rateBitsPerSymbol V32R9600T = 4
rateBitsPerSymbol V32R12000 = 5
rateBitsPerSymbol V32R14400 = 6

-- | Whether this rate runs through the convolutional encoder.
rateTrellis :: V32Rate -> Bool
rateTrellis r = r `elem` [V32R7200, V32R9600T, V32R12000, V32R14400]

-- | Bits that bypass the coding entirely (Q3 onwards), choosing between
-- the points of one trellis subset.
rateUncoded :: V32Rate -> Int
rateUncoded r
  | rateTrellis r = rateBitsPerSymbol r - 2
  | otherwise = 0

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

-- | And back.  The start-up reads states off the line as often as it
-- puts them on it.
dibitOfState :: TrainState -> (Bool, Bool)
dibitOfState StA = (False, False)
dibitOfState StB = (False, True)
dibitOfState StC = (True, True)
dibitOfState StD = (True, False)

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
-- | Figure 2-4\/V.32bis, 7200 bit\/s, indexed by Y0 Y1 Y2 Q3.  A square
-- of 16 points at odd coordinates -- not the checkerboard the 9600 and
-- 14400 sets use, and worth noticing: the lattice is a property of the
-- rate, not of V.32.
points16at7200 :: [(Double, Double)]
points16at7200 =
  [ ( 3, -3), (-1,  1), (-3,  3), ( 1, -1)
  , ( 3,  1), (-1, -3), (-3, -1), ( 1,  3)
  , (-1,  3), ( 3, -1), ( 1, -3), (-3,  1)
  , (-3, -3), ( 1,  1), ( 3,  3), (-1, -1)
  ]
-- | Figure 2-2\/V.32bis, 12000 bit\/s, indexed by Y0 Y1 Y2 Q3 Q4 Q5.
-- An 8 by 8 square at odd coordinates.
points64 :: [(Double, Double)]
points64 =
  [ ( 7,  1), ( 3,  5), ( 7, -7), (-5,  5)
  , ( 3, -3), (-1,  1), (-1, -7), (-5, -3)
  , (-7, -1), (-3, -5), (-7,  7), ( 5, -5)
  , (-3,  3), ( 1, -1), ( 1,  7), ( 5,  3)
  , (-1,  5), (-5,  1), ( 7,  5), (-5, -7)
  , ( 3,  1), (-1, -3), ( 7, -3), ( 3, -7)
  , ( 1, -5), ( 5, -1), (-7, -5), ( 5,  7)
  , (-3, -1), ( 1,  3), (-7,  3), (-3,  7)
  , (-5, -1), (-1, -5), (-5,  7), ( 7, -5)
  , (-1,  3), ( 3, -1), ( 3,  7), ( 7,  3)
  , ( 5,  1), ( 1,  5), ( 5, -7), (-7,  5)
  , ( 1, -3), (-3,  1), (-3, -7), (-7, -3)
  , ( 1, -7), ( 5, -3), (-7, -7), ( 5,  5)
  , (-3, -3), ( 1,  1), (-7,  1), (-3,  5)
  , (-1,  7), (-5,  3), ( 7,  7), (-5, -5)
  , ( 3,  3), (-1, -1), ( 7, -1), ( 3, -5)
  ]
-- | Figure 2-1\/V.32bis, 14400 bit\/s, indexed by Y0 Y1 Y2 Q3 Q4 Q5 Q6.
-- A 128-point cross on the same checkerboard as the 32-point set: even
-- real part with odd imaginary, or the other way about.  The top and
-- bottom rows hold two points rather than three, at plus and minus two
-- -- the notch is in the Recommendation's figure, and the lattice
-- requires it, since at an odd imaginary part the real part must be even.
points128 :: [(Double, Double)]
points128 =
  [ (-8, -3), ( 8, -3), ( 4, -3), ( 4, -7)
  , (-4, -3), (-4, -7), ( 0, -3), ( 0, -7)
  , (-8,  1), ( 8,  1), ( 4,  1), ( 4,  5)
  , (-4,  1), (-4,  5), ( 0,  1), ( 0,  5)
  , ( 8,  3), (-8,  3), (-4,  3), (-4,  7)
  , ( 4,  3), ( 4,  7), ( 0,  3), ( 0,  7)
  , ( 8, -1), (-8, -1), (-4, -1), (-4, -5)
  , ( 4, -1), ( 4, -5), ( 0, -1), ( 0, -5)
  , ( 2, -9), ( 2,  7), ( 2,  3), ( 6,  3)
  , ( 2, -5), ( 6, -5), ( 2, -1), ( 6, -1)
  , (-2, -9), (-2,  7), (-2,  3), (-6,  3)
  , (-2, -5), (-6, -5), (-2, -1), (-6, -1)
  , (-2,  9), (-2, -7), (-2, -3), (-6, -3)
  , (-2,  5), (-6,  5), (-2,  1), (-6,  1)
  , ( 2,  9), ( 2, -7), ( 2, -3), ( 6, -3)
  , ( 2,  5), ( 6,  5), ( 2,  1), ( 6,  1)
  , ( 9,  2), (-7,  2), (-3,  2), (-3,  6)
  , ( 5,  2), ( 5,  6), ( 1,  2), ( 1,  6)
  , ( 9, -2), (-7, -2), (-3, -2), (-3, -6)
  , ( 5, -2), ( 5, -6), ( 1, -2), ( 1, -6)
  , (-9, -2), ( 7, -2), ( 3, -2), ( 3, -6)
  , (-5, -2), (-5, -6), (-1, -2), (-1, -6)
  , (-9,  2), ( 7,  2), ( 3,  2), ( 3,  6)
  , (-5,  2), (-5,  6), (-1,  2), (-1,  6)
  , (-3,  8), (-3, -8), (-3, -4), (-7, -4)
  , (-3,  4), (-7,  4), (-3,  0), (-7,  0)
  , ( 1,  8), ( 1, -8), ( 1, -4), ( 5, -4)
  , ( 1,  4), ( 5,  4), ( 1,  0), ( 5,  0)
  , ( 3, -8), ( 3,  8), ( 3,  4), ( 7,  4)
  , ( 3, -4), ( 7, -4), ( 3,  0), ( 7,  0)
  , (-1, -8), (-1,  8), (-1,  4), (-5,  4)
  , (-1, -4), (-5, -4), (-1,  0), (-5,  0)
  ]
-- | Each rate's points, scaled so the set has unit mean power.
--
-- The scaling is per rate because the Recommendations draw each
-- constellation on whatever integer grid suits it -- mean square 10 for
-- the V.32 sets and the 7200 one, 42 for 12000, 41 for 14400 -- while a
-- modem transmits at one level whatever rate it is running.  Normalising
-- each set to unit mean power is what makes that true, and it leaves the
-- four training states at unit power too, which they must be: they go on
-- the line before either end knows what the rate will be.
pointsFor :: V32Rate -> VU.Vector (Double, Double)
pointsFor r = case r of
  V32R4800 -> pts4800
  V32R7200 -> pts7200
  V32R9600 -> pts9600
  V32R9600T -> pts9600T
  V32R12000 -> pts12000
  V32R14400 -> pts14400

pts4800, pts7200, pts9600, pts9600T, pts12000, pts14400 :: VU.Vector (Double, Double)
-- indexed by the differentially encoded dibit Y1 Y2, which is not the
-- order the states are declared in: Table 1's signal-state column has
-- 10 as D and 11 as C
pts4800 = VU.fromList (map (statePoint . stateOfDibit)
            [ (False, False), (False, True), (True, False), (True, True) ])
pts7200 = normalisePoints points16at7200
pts9600 = normalisePoints points16
pts9600T = normalisePoints points32
pts12000 = normalisePoints points64
pts14400 = normalisePoints points128

normalisePoints :: [(Double, Double)] -> VU.Vector (Double, Double)
normalisePoints ps = VU.fromList [ (x * k, y * k) | (x, y) <- ps ]
  where
    mean = sum [ x * x + y * y | (x, y) <- ps ] / fromIntegral (length ps)
    k = 1 / sqrt mean

-- | The transmitted point for a coded symbol index: the coded bits most
-- significant, so Y0 (where there is one) then Y1 Y2 then Q3 onwards.
constellation :: V32Rate -> Int -> Point
constellation r i = pointsFor r VU.! (i `mod` VU.length (pointsFor r))

-- | Nearest constellation point, as its index.  This is an immediate
-- decision with no memory: at 4800 and 9600 non-redundant it is the
-- decision, and on a trellis alternative it is what the carrier and
-- timing loops use, because they cannot wait for a traceback without
-- going unstable.
-- | Half the distance to the nearest wrong answer, for the rate's own
-- constellation.
--
-- This is the unit a decision error means anything in.  A tenth of the
-- signal is a comfortable error at 4800, where the four points are 0.71
-- apart from the decision boundary, and it is past the boundary
-- altogether at 14400, where they are 0.11.  Anything that compares a
-- decision error against a fixed number is really comparing it against
-- six different things depending on the rate.
rateMargin :: V32Rate -> Double
rateMargin r = 0.5 * sqrt (minimum [ dist2 (ps VU.! i) (ps VU.! j)
                                   | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ])
  where
    ps = pointsFor r
    n = VU.length ps

slicePoint :: V32Rate -> Point -> Int
slicePoint r p = snd (minimum [ (dist2 p (ps VU.! i), i) | i <- [0 .. VU.length ps - 1] ])
  where ps = pointsFor r

dist2 :: Point -> Point -> Double
dist2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)

-- | The nearest point of the trellis subset named by Y0 Y1 Y2, with its
-- squared distance and the uncoded bits that chose it: the Viterbi
-- decoder's branch metric.  How many uncoded bits there are is the only
-- thing that changes between 7200 and 14400.
subsetPoint :: V32Rate -> Point -> (Bool, Bool, Bool) -> (Double, [Bool])
subsetPoint r p (y0, y1, y2) = minimum
  [ (dist2 p (constellation r (bitsToInt ([y0, y1, y2] ++ q))), q)
  | q <- replicateM (rateUncoded r) [False, True] ]

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

-- | The input dibit that asks for this many quarter turns, and back.
-- Table 1 is a table of quadrant /changes/, so a receiver that has
-- measured the change has already done the work.
dibitOfTurn :: Int -> (Bool, Bool)
dibitOfTurn = turnBack

turnOfDibit :: (Bool, Bool) -> Int
turnOfDibit = turnOf

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
-- traceback.  Returns, per symbol, the differentially encoded Y1 and Y2
-- and whatever uncoded bits the rate carries above them -- so Table 2
-- still has to undo the rotation afterwards.
--
-- Emission is delayed by @depth@ symbols; the tail is flushed from the
-- best surviving path at the end.
viterbiDecode :: V32Rate -> Int -> [Point] -> [(Bool, Bool, [Bool])]
viterbiDecode rate depth = go start (0 :: Int)
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
          [ (s', (m + bm, take (depth + 1) ((y1, y2, q) : hist)))
          | (s, (m, hist)) <- zip [0 ..] sts
          , (y1, y2) <- [(False, False), (False, True), (True, False), (True, True)]
          , let (ConvState s', y0) = convStep (ConvState s) (y1, y2)
          , let (bm, q) = subsetPoint rate p (y0, y1, y2) ]
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

-- | The capabilities a rate signal carries.
--
-- One 16-bit sequence serves both Recommendations, which is the whole
-- trick of V.32bis interworking: Table 6/V.32 defines B4, B5, B6 and B8,
-- and Table 5/V.32 bis keeps all four and adds B9, B10 and B12 out of
-- bits V.32 had reserved.  A V.32 modem reading a V.32bis signal sees
-- the rates it knows and bits it was told to ignore.
data RateSeq = RateSeq
  { rsCan2400  :: !Bool   -- ^ B4; V.32bis fixes this at 1 (Table 5, Note 1)
  , rsCan4800  :: !Bool   -- ^ B5
  , rsCan9600  :: !Bool   -- ^ B6
  , rsTrellis  :: !Bool   -- ^ B8; V.32bis fixes this at 1 as well
  , rsCan7200  :: !Bool   -- ^ B9, V.32bis only
  , rsCan12000 :: !Bool   -- ^ B10, V.32bis only
  , rsCan14400 :: !Bool   -- ^ B12, V.32bis only
  } deriving (Eq, Show)

-- | All rates refused: Table 6's call for a GSTN cleardown.
noRates :: RateSeq
noRates = RateSeq False False False False False False False

-- | Every rate the two Recommendations define.
allRates :: RateSeq
allRates = RateSeq True True True True True True True

-- | What this modem offers by default: V.32's 4800 and both 9600s.
--
-- All three V.32bis rates are implemented and negotiate correctly, and
-- the data pump carries every one of them through a telephone channel
-- once it has the training the start-up provides -- that is what the
-- impairment tests measure, down to 25 dB at 12000 and 14400.  What none
-- of them yet survives is a whole call: 7200 delivers the text with a
-- dozen bytes of rubbish in front of it, 12000 manages one direction of
-- two, and 14400 neither.  Offering a rate that then damages the session
-- is worse than not offering it, so they are opt-in through
-- 'Modec.Modem.mcV32Rates' until that is fixed.
--
-- For 14400 at least the ceiling is the receiver's own noise floor
-- rather than the line's: cubic interpolation at 3.3 samples per symbol
-- and a root raised cosine cut at 12 symbols leave about 25 dB of
-- implementation signal to noise, which is enough for 32 points and not
-- for 128.  Raising it means a better interpolator, not a better
-- channel.  'allRates' offers the lot, for measuring exactly that.
defaultRates :: RateSeq
defaultRates = v32Rates

-- | What a V.32 call offers: 4800, and 9600 with the trellis and
-- without.  B4 stays clear, which is how Note 1 has a modem say it is
-- not speaking V.32bis; B8 is set because V.32 has a trellis of its own.
v32Rates :: RateSeq
v32Rates = noRates
  { rsTrellis = True, rsCan4800 = True, rsCan9600 = True }

-- | What a V.32bis call offers: V.32's rates, 7200, and the B4 that
-- announces the Recommendation.
--
-- 7200 is in now that the receiver acquires the data constellation
-- instead of trying to track it: a call at 7200 delivers both
-- directions exactly, head of the session included.  12000 is in too:
-- dialled at a real board it connects, negotiates MNP class 4 and holds
-- a session at a decision error of 0.003 to 0.007, which is what took
-- it out of the "works down a pair of pipes" category.  14400 is not.
-- It trains and connects and then cannot hold the line -- on that same
-- board the receiver gave up four seconds in -- and in loopback the
-- receiver's decision error settles at around half the distance to the
-- wrong answer -- 39 to 49 % of it with no channel in the way -- and
-- the gate that keeps noise off the terminal keeps the data off with
-- it.  The offline pump reaches both rates error-free through a
-- telephone band at 25 dB, so whatever is missing is in the start-up,
-- the handover or the canceller and not in the modulation.  For 14400 the ceiling is the receiver's own noise floor
-- rather than the line's: cubic interpolation at 3.3 samples per symbol
-- and a root raised cosine cut at 12 symbols leave about 25 dB of
-- implementation signal to noise, enough for 32 points and not for 128.
-- Raising it means a better interpolator, not a better channel.  Ask
-- for either by name and you get it; offering one that then damages the
-- session is worse than not offering it.
v32bisRates :: RateSeq
v32bisRates = v32Rates { rsCan2400 = True, rsCan7200 = True, rsCan12000 = True }

rateSeqCleardown :: RateSeq -> Bool
rateSeqCleardown r = not (or [ rsCan2400 r, rsCan4800 r, rsCan9600 r
                             , rsCan7200 r, rsCan12000 r, rsCan14400 r ])

-- | Whether the far end is speaking V.32bis at all.  Table 5/V.32 bis
-- Note 1: with B4 or B8 clear in a signal sent or received, interworking
-- proceeds only under V.32.  Those two bits are therefore how a V.32bis
-- modem announces itself, and a plain V.32 modem cannot say it by
-- accident -- B4 means "can receive 2400 bit/s" to V.32, a rate 2.4.3
-- leaves for further study and no modem implements.
rateSeqV32bis :: RateSeq -> Bool
rateSeqV32bis r = rsCan2400 r && rsTrellis r

-- | Table 6/V.32 and Table 5/V.32 bis: the 16 bits of a rate sequence,
-- B0 first.  B0-B3, B7, B11 and B15 are there to synchronise on.
rateSeqBits :: RateSeq -> [Bool]
rateSeqBits = seqBits False

-- | Table 7/V.32: signal E, which ends a rate signal and names the rate
-- and coding of the scrambled ones that follow it.  B0-B3 are ones
-- rather than zeros, which is what tells it from a rate sequence.
eSeqBits :: RateSeq -> [Bool]
eSeqBits = seqBits True

decodeRateSeq :: [Bool] -> Maybe RateSeq
decodeRateSeq = decodeSeq False

decodeESeq :: [Bool] -> Maybe RateSeq
decodeESeq = decodeSeq True

-- | §5.3.1: a sequence is only a rate signal if the synchronising bits
-- are right, which is the whole of the protection it has.  B13 and B14
-- are read but not checked: Table 5 Note 2 reserves them and says to
-- ignore them on reception, so a modem that insisted on their value
-- would refuse a conformant signal from a later modem.
-- | The sixteen bits of a rate sequence or of E, which differ only in
-- the four that lead them: R1/R2/R3 open with four zeros and E with four
-- ones.  'decodeSeq' takes the same argument for the same reason.
seqBits :: Bool -> RateSeq -> [Bool]
seqBits lead r =
  replicate 4 lead ++
  [ rsCan2400 r, rsCan4800 r, rsCan9600 r, True
  , rsTrellis r, rsCan7200 r, rsCan12000 r, True
  , rsCan14400 r, False, False, True ]

decodeSeq :: Bool -> [Bool] -> Maybe RateSeq
decodeSeq lead bs = case bs of
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, _, _, b15]
    | [b0, b1, b2, b3] == replicate 4 lead
    , b7, b11, b15 ->
        Just (RateSeq b4 b5 b6 b8 b9 b10 b12)
  _ -> Nothing

-- | The same offer with everything at or above a rate's speed taken out.
--
-- What makes a rate change a rate change: a modem that retrains without
-- narrowing its offer negotiates its way straight back to the rate that
-- had just stopped working.
--
-- By bit rate and not by the order of 'allV32Rates', which sorts by
-- what to prefer rather than by what will survive.  The two disagree in
-- one place and it matters: the step below 9600 trellis in that list is
-- plain 9600, which carries the same 9600 bit\/s with the trellis
-- thrown away.  Falling back to it makes the link worse at the same
-- speed, which is the one thing a fallback must not do.  9600
-- non-trellis is in the Recommendation so that a modem without the
-- trellis can be interworked with; it is not a rung on this ladder.
ratesBelow :: V32Rate -> RateSeq -> RateSeq
ratesBelow r offer =
  foldl clear offer [ x | x <- allV32Rates, rateBitRate x >= rateBitRate r ]
  where
    clear c x = case x of
      V32R14400 -> c { rsCan14400 = False }
      V32R12000 -> c { rsCan12000 = False }
      V32R9600T -> c { rsTrellis = False }
      V32R9600 -> c { rsCan9600 = False }
      V32R7200 -> c { rsCan7200 = False }
      V32R4800 -> c { rsCan4800 = False }

-- | The best rate both ends can run, given what the far end offered and
-- what we can do.  §5.4.1: a reply must exclude anything absent from the
-- signal it answers.
--
-- The V.32bis rates are only on the table if both signals claim V.32bis;
-- otherwise this is a V.32 call and 9600 is the ceiling, which is Note 1
-- of Table 5 doing its work.
bestCommonRate :: RateSeq -> RateSeq -> Maybe V32Rate
bestCommonRate ours theirs = listToMaybe (filter usable allV32Rates)
  where
    bis = rateSeqV32bis ours && rateSeqV32bis theirs
    usable r = has ours r && has theirs r
               && (bis || r `elem` [V32R9600T, V32R9600, V32R4800])
    has c r = case r of
      V32R14400 -> rsCan14400 c
      V32R12000 -> rsCan12000 c
      V32R9600T -> rsCan9600 c && rsTrellis c
      V32R9600 -> rsCan9600 c
      V32R7200 -> rsCan7200 c
      V32R4800 -> rsCan4800 c

-- | The rate signal that offers exactly one rate.  B4 and B8 stay set on
-- the V.32bis rates so the far end can still tell which Recommendation
-- it is talking to.
chosenRate :: V32Rate -> RateSeq
chosenRate r = case r of
  V32R4800 -> base { rsCan4800 = True }
  V32R7200 -> bis { rsCan7200 = True }
  V32R9600 -> base { rsCan9600 = True }
  V32R9600T -> base { rsCan9600 = True, rsTrellis = True }
  V32R12000 -> bis { rsCan12000 = True }
  V32R14400 -> bis { rsCan14400 = True }
  where
    base = noRates
    bis = noRates { rsCan2400 = True, rsTrellis = True }
