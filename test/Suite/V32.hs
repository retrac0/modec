-- | V.32 and V.32bis: the coding layer, the data pump, the start-up of
-- Figure 4, and the echo canceller that makes it possible.
module Suite.V32 (table1, table2, bitPair, pairBits, Quad, quadsFrom, enc16, dec16, enc32, dec32, rot90, v32Tests, v32PumpTests, modulateStates2, modulateStates, v32SignalTests, echoPath, runEcho, echoTests, v32StartDuplex, v32PumpDuplex, v32CallEvm, v32StartTests) where

import Control.Monad (forM_, replicateM)
import Data.List (nub)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Channel
import Modec.DSP
import Modec.Modem
import Modec.V32
import Modec.QAM
import Modec.V32Pump
import Modec.V32Start
import Modec.Echo
import qualified Modec.V32 as V32
import Data.Bits (testBit)

-- Table 1/V.32, transcribed from the Recommendation rather than derived:
-- inputs Q1 Q2, previous Y1 Y2, resulting Y1 Y2.
table1 :: [((Bool, Bool), (Bool, Bool), (Bool, Bool))]
table1 = [ (q, p, o) | (qi, ps) <- zip [0 :: Int ..] rows, (pi_, oi) <- zip [0 :: Int ..] ps
         , let q = bitPair qi, let p = bitPair pi_, let o = bitPair oi ]
  where
    -- rows are Q1Q2 = 00, 01, 10, 11; within a row, previous = 00, 01, 10, 11
    rows = [ [1, 3, 0, 2]      -- +90 degrees
           , [0, 1, 2, 3]      --   0
           , [3, 2, 1, 0]      -- +180
           , [2, 0, 3, 1] ]    -- +270

-- Table 2/V.32, likewise: the trellis alternative's differential
-- encoding, which is a different table and must stay one.
table2 :: [((Bool, Bool), (Bool, Bool), (Bool, Bool))]
table2 = [ (q, p, o) | (qi, ps) <- zip [0 :: Int ..] rows, (pi_, oi) <- zip [0 :: Int ..] ps
         , let q = bitPair qi, let p = bitPair pi_, let o = bitPair oi ]
  where
    rows = [ [0, 1, 2, 3]
           , [1, 0, 3, 2]
           , [2, 3, 1, 0]
           , [3, 2, 0, 1] ]

bitPair :: Int -> (Bool, Bool)
bitPair i = (testBit i (1 :: Int), testBit i (0 :: Int))

pairBits :: (Bool, Bool) -> Int
pairBits (a, b) = (if a then 2 else 0) + (if b then 1 else 0)

-- One symbol's worth of data at 9600: Q1 Q2 Q3 Q4.
type Quad = (Bool, Bool, Bool, Bool)

quadsFrom :: Int -> [Quad]
quadsFrom n = [ (b 0, b 1, b 2, b 3) | i <- [0 .. n - 1]
              , let b k = odd ((i * 7919 + 13) `div` (3 ^ (k :: Int)) + i `div` 7) ]

-- The 9600 bit/s non-redundant chain, end to end.
enc16 :: [Quad] -> [Point]
enc16 = go (False, False)
  where
    go _ [] = []
    go prev ((q1, q2, q3, q4) : rest) =
      let y@(y1, y2) = diffEncode1 (q1, q2) prev
      in constellation V32R9600 (pairBits (y1, y2) * 4 + pairBits (q3, q4)) : go y rest

dec16 :: [Point] -> [Quad]
dec16 pts = go (False, False) pts
  where
    go _ [] = []
    go prev (p : rest) =
      let i = slicePoint V32R9600 p
          y = bitPair (i `div` 4)
          (q3, q4) = bitPair (i `mod` 4)
          (q1, q2) = diffDecode1 y prev
      in (q1, q2, q3, q4) : go y rest

-- The 9600 bit/s trellis chain, end to end.
enc32 :: [Quad] -> [Point]
enc32 = go (False, False) convInit
  where
    go _ _ [] = []
    go prev cs ((q1, q2, q3, q4) : rest) =
      let y@(y1, y2) = diffEncode2 (q1, q2) prev
          (cs', y0) = convStep cs y
          i = pairBits (y0, y1) * 8 + pairBits (y2, q3) * 2 + (if q4 then 1 else 0)
      in constellation V32R9600T i : go y cs' rest

dec32 :: Int -> [Point] -> [Quad]
dec32 depth pts = go (False, False) (viterbiDecode V32R9600T depth pts)
  where
    go _ [] = []
    go prev ((y1, y2, [q3, q4]) : rest) =
      let (q1, q2) = diffDecode2 (y1, y2) prev
      in (q1, q2, q3, q4) : go (y1, y2) rest
    go _ _ = []

rot90 :: Point -> Point
rot90 (x, y) = (negate y, x)

v32Tests :: TestTree
v32Tests = testGroup "V.32 coding layer"
  [ testCase "both scramblers reproduce the TRN openings of 5.2.3" $ do
      -- The Recommendation prints the first 30 scrambled bits and the
      -- states they become, for each direction.  One assertion pins both
      -- polynomials, the all-zero register, the dibit ordering and the
      -- A/C convention of the first 256 symbols.
      let showBits = concatMap (\b -> if b then "1" else "0")
          showStates = map (\st -> case st of StA -> 'A'; StB -> 'B'; StC -> 'C'; StD -> 'D')
      assertEqual "GPC bits" "111111111111111111000001111111" (showBits (trnBits Calling 30))
      assertEqual "GPA bits" "111110000011111000001110011111" (showBits (trnBits Answering 30))
      assertEqual "call mode states" "CCCCCCCCCAAACCC" (showStates (trnStates Calling 15))
      assertEqual "answer mode states" "CCCAACCCAACCACC" (showStates (trnStates Answering 15))

  , testCase "a scrambler and its descrambler are inverse" $
      forM_ [Calling, Answering] $ \d -> do
        let bits = prbs (11, 9) 500
            line = snd (foldl (\(sc, acc) b -> let (sc', o) = V32.scrambleBit d sc b in (sc', acc ++ [o])) (scramblerInit, []) bits)
            back = snd (foldl (\(sc, acc) b -> let (sc', o) = V32.descrambleBit d sc b in (sc', acc ++ [o])) (scramblerInit, []) line)
        assertEqual (show d) bits back

  , testCase "Table 1 is transcribed correctly and inverts" $
      forM_ table1 $ \(q, p, o) -> do
        assertEqual ("encode " ++ show (q, p)) o (diffEncode1 q p)
        assertEqual ("decode " ++ show (q, p)) q (diffDecode1 o p)

  , testCase "Table 2 is transcribed correctly and inverts" $
      forM_ table2 $ \(q, p, o) -> do
        assertEqual ("encode " ++ show (q, p)) o (diffEncode2 q p)
        assertEqual ("decode " ++ show (q, p)) q (diffDecode2 o p)

  , testCase "Table 1 and Table 2 are different tables" $ do
      -- Copying one into the other's path gives a link that trains and
      -- then errors systematically, so assert outright that they differ.
      let differing = [ () | ((q, p, o1), (_, _, o2)) <- zip table1 table2, o1 /= o2 ]
      assertBool "the two differential encodings must not coincide" (length differing >= 8)

  , testCase "the training states are Figure 1's circled points" $ do
      -- A, B, C, D each have power 10 in grid units, the mean power of
      -- both data constellations, so training goes to line at the data
      -- level.  They are 90 degrees apart in the order C D A B.
      forM_ trainStates $ \st -> do
        let (x, y) = statePoint st
        assertBool (show st ++ " power") (abs (x * x + y * y - 1) < 1e-12)
      let ang st = let (x, y) = statePoint st in atan2 y x
          step a b = let d = (ang b - ang a) * 180 / pi in if d < -1 then d + 360 else d
      forM_ [(StC, StD), (StD, StA), (StA, StB)] $ \(a, b) ->
        assertBool (show (a, b) ++ " is a quarter turn") (abs (step a b - 90) < 1e-9)
      let (ax, ay) = statePoint StA
          (cx, cy) = statePoint StC
      assertBool "A and C are antipodal" (abs (ax + cx) < 1e-12 && abs (ay + cy) < 1e-12)

  , testCase "both constellations have unit mean power and slice back" $
      forM_ [(V32R4800, 4), (V32R9600, 16), (V32R9600T, 32)] $ \(r, n) -> do
        let pts = [ constellation r i | i <- [0 .. n - 1] ]
            mp = sum [ x * x + y * y | (x, y) <- pts ] / fromIntegral n
        assertEqual (show r ++ ": all points distinct") n (length (nubPoints pts))
        assertBool (show r ++ " mean power " ++ show mp) (abs (mp - 1) < 1e-12)
        forM_ [0 .. n - 1] $ \i ->
          assertEqual (show r ++ " slices index " ++ show i) i (slicePoint r (constellation r i))

  , testCase "the trellis subsets are further apart than the whole set" $ do
      -- The Y0 bit splits the 32 points into two halves whose own
      -- minimum distance is larger than the set's.  That gap is the
      -- coding gain; if the subset partition is wrong it silently
      -- vanishes and the Viterbi decoder buys nothing.
      let pts = [ (i, constellation V32R9600T i) | i <- [0 .. 31] ]
          d2 (a, b) (c, d) = (a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int)
          whole = minimum [ d2 p q | (i, p) <- pts, (j, q) <- pts, i < j ]
          half h = minimum [ d2 p q | (i, p) <- pts, (j, q) <- pts, i < j
                           , i `div` 16 == h, j `div` 16 == h ]
      assertBool "Y0 = 0 subset" (half (0 :: Int) > whole * 1.9)
      assertBool "Y0 = 1 subset" (half 1 > whole * 1.9)

  , testCase "every V.32bis constellation is a constellation" $
      forM_ allV32Rates $ \r -> do
        let n = 2 ^ (rateBitsPerSymbol r + (if rateTrellis r then 1 else 0))
            pts = [ constellation r i | i <- [0 .. n - 1] ]
            mp = sum [ x * x + y * y | (x, y) <- pts ] / fromIntegral n
        assertEqual (show r ++ ": all points distinct") n (length (nubPoints pts))
        assertBool (show r ++ ": mean power " ++ show mp) (abs (mp - 1) < 1e-12)
        forM_ [0 .. n - 1] $ \i ->
          assertEqual (show r ++ ": slices index " ++ show i) i (slicePoint r (constellation r i))

  , testCase "every V.32bis rate ignores a quarter turn of the line" $
      -- The check that actually pins a transcription.  These tables were
      -- read off scanned figures; a single mislabelled point breaks the
      -- rotational invariance that the differential coding and the
      -- non-linear convolutional encoder exist to provide, and nothing
      -- else in the module would notice.
      forM_ allV32Rates $ \r -> do
        let payload = prbs (11, 9) 1600
            enc = snd (encodeSymbols Calling r payload txCoderInit)
            dec ps = snd (decodeQuads Answering r (codedQuads r
                       [ QamSym p (slicePoint r p) 0 p | p <- ps ]) rxCoderInit)
            skip = if rateTrellis r then 200 else 40
        forM_ [0, 1, 2, 3] $ \k -> do
          let turned = iterate (map rot90) enc !! k
          sameBits (show r ++ ", " ++ show k ++ " quarter turns")
            (drop skip payload) (drop skip (dec turned))

  , testCase "every trellis rate has the free distance it is supposed to" $
      -- The decisive check on a transcribed constellation.  Two things
      -- bound how far apart two distinct transmitted sequences can be:
      -- the distance between the points sharing a branch (parallel
      -- transitions), and the distance two paths accumulate between
      -- diverging and remerging.  A mislabelled point moves one or the
      -- other and nothing else in the module notices.
      forM_ [ (V32R9600T, 4.0), (V32R7200, 3.0), (V32R12000, 3.0), (V32R14400, 3.0) ] $
        \(r, wantGain) -> do
          let sq (a, b) (c, d) = (a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int)
              n = 2 ^ (rateBitsPerSymbol r + 1)
              pts = [ constellation r i | i <- [0 .. n - 1] ]
              whole = minimum [ sq p q | (i, p) <- zip [0 :: Int ..] pts
                              , (j, q) <- zip [0 :: Int ..] pts, i < j ]
              subsetPts y0 y1 y2 =
                [ constellation r (bitsToI ([y0, y1, y2] ++ q))
                | q <- replicateM (rateUncoded r) [False, True] ]
              bitsToI = foldl (\a b -> a * 2 + (if b then 1 else 0)) 0
              dibs = [(False, False), (False, True), (True, False), (True, True)]
              stepOf st u = let (ConvState st', y0) = convStep (ConvState st) u in (st', y0)
              branchPts st u = let (_, y0) = stepOf st u in subsetPts y0 (fst u) (snd u)
              interD a u b u' = minimum [ sq p q | p <- branchPts a u, q <- branchPts b u' ]
              parallel = minimum
                [ sq p q | y0 <- [False, True], y1 <- [False, True], y2 <- [False, True]
                , let ps = subsetPts y0 y1 y2
                , (i, p) <- zip [0 :: Int ..] ps, (j, q) <- zip [0 :: Int ..] ps, i < j ]
              minPerKey xs = [ (k, minimum [ v | (k', v) <- xs, k' == k ]) | k <- nub (map fst xs) ]
              expand (a, b) = [ ((fst (stepOf a u), fst (stepOf b u')), interD a u b u')
                              | u <- dibs, u' <- dibs ]
              seeds = [ ((fst (stepOf st u), fst (stepOf st u')), interD st u st u')
                      | st <- [0 .. 7 :: Int], u <- dibs, u' <- dibs, u /= u' ]
              walk best frontier k
                | k <= (0 :: Int) || null frontier = best
                | otherwise =
                    let nxt = minPerKey [ ((x, y), c + d) | ((a, b), c) <- frontier
                                        , ((x, y), d) <- expand (a, b) ]
                        best' = minimum (best : [ v | ((x, y), v) <- nxt, x == y ])
                    in walk best' [ (kk, v) | (kk@(x, y), v) <- nxt, x /= y, v < best' ] (k - 1)
              merged0 = minimum ([ c | ((x, y), c) <- seeds, x == y ] ++ [1e9])
              dfree = walk merged0 (minPerKey [ (k, c) | (k, c) <- seeds, fst k /= snd k ]) 25
              eff = min parallel dfree
              gain = 10 * logBase 10 (eff / whole)
          assertBool (show r ++ ": parallel " ++ show parallel ++ ", free " ++ show dfree
                      ++ ", whole " ++ show whole ++ ", gain over an uncoded set of the "
                      ++ "same size " ++ show gain ++ " dB")
            (gain >= wantGain)

  , testCase "the trellis decoder is right where the plain slicer is wrong" $ do
      -- And the gain shows up in practice: at a noise level that costs
      -- the 16-point slicer a dozen symbols, the Viterbi decoder loses
      -- none.  Both constellations have unit mean power, so the same
      -- sigma is the same channel.
      let qs = quadsFrom 3000
          sigma = 0.10
          noisy sd ps = zipWith3 (\(x, y) a b -> (x + a, y + b)) ps
            (VS.toList (gaussianNoise sd (length ps) sigma))
            (VS.toList (gaussianNoise (sd + 7) (length ps) sigma))
          wrong a b = length [ () | (x, y) <- zip a b, x /= y ]
          plain = wrong (drop 30 qs) (drop 30 (dec16 (noisy 1 (enc16 qs))))
          coded = wrong (drop 30 qs) (drop 30 (dec32 16 (noisy 1 (enc32 qs))))
      assertBool ("the plain slicer should be making errors here, made " ++ show plain)
        (plain >= 8)
      assertEqual "the trellis decoder should make none" 0 coded

  , testCase "rate sequences survive Table 6 and Table 7 and reject noise" $ do
      let seqs = [ RateSeq a b c t x y z
                 | a <- [False, True], b <- [False, True], c <- [False, True]
                 , t <- [False, True], (x, y, z) <- [ (False, False, False)
                                                    , (True, False, True)
                                                    , (True, True, True) ] ]
      forM_ seqs $ \r -> do
        assertEqual "R round trip" (Just r) (decodeRateSeq (rateSeqBits r))
        assertEqual "E round trip" (Just r) (decodeESeq (eSeqBits r))
        -- E and a rate sequence differ only in B0-B3, and each decoder
        -- must refuse the other's leader.
        assertEqual "R is not an E" Nothing (decodeESeq (rateSeqBits r))
        assertEqual "E is not an R" Nothing (decodeRateSeq (eSeqBits r))
      let good = rateSeqBits (RateSeq False True True True False False False)
      forM_ [0, 1, 2, 3, 7, 11, 15] $ \i ->
        assertEqual ("a flipped sync bit " ++ show i ++ " is refused")
          Nothing (decodeRateSeq (flipAt i good))
      assertBool "all rates off is a cleardown" (rateSeqCleardown noRates)

  , testCase "the rate both ends can run is the best they share" $ do
      let v32 a b c t = RateSeq a b c t False False False
          full = v32 False True True True
          noTcm = v32 False True True False
          slow = v32 False True False False
      assertEqual "both trellis" (Just V32R9600T) (bestCommonRate full full)
      assertEqual "one without trellis" (Just V32R9600) (bestCommonRate full noTcm)
      assertEqual "one without 9600" (Just V32R4800) (bestCommonRate full slow)
      assertEqual "nothing in common" Nothing
        (bestCommonRate slow (v32 False False False False))
      -- and the V.32bis half: both ends must claim it (B4 and B8) before
      -- any rate above 9600 is on the table at all
      assertEqual "two V.32bis modems" (Just V32R14400) (bestCommonRate allRates allRates)
      assertEqual "V.32bis meeting V.32" (Just V32R9600T) (bestCommonRate allRates full)
      assertEqual "V.32bis, far end has no 14400" (Just V32R12000)
        (bestCommonRate allRates allRates { rsCan14400 = False })
      assertEqual "V.32bis down to 7200" (Just V32R7200)
        (bestCommonRate allRates (RateSeq True False False True True False False))

  , testCase "9600 non-redundant carries data through a clean channel" $ do
      let qs = quadsFrom 400
      sameQuads "non-redundant" (drop 1 qs) (drop 1 (dec16 (enc16 qs)))

  , testCase "9600 trellis carries data through a clean channel" $ do
      let qs = quadsFrom 400
      sameQuads "trellis" (drop 1 qs) (drop 1 (dec32 16 (enc32 qs)))

  , testCase "both 9600 alternatives ignore a quarter turn of the line" $ do
      -- The receiver's carrier loop locks with a four-fold phase
      -- ambiguity it cannot resolve on its own.  Table 1 removes it for
      -- the non-redundant alternative; for the trellis one it is the
      -- non-linear convolutional encoder that has to, which makes this
      -- the test that the wiring of Figure 2 was traced correctly.
      let qs = quadsFrom 400
          turns k = iterate (map rot90) (enc16 qs) !! k
          turnsT k = iterate (map rot90) (enc32 qs) !! k
      forM_ [0, 1, 2, 3] $ \k -> do
        sameQuads ("non-redundant, " ++ show k ++ " quarter turns")
          (drop 1 qs) (drop 1 (dec16 (turns k)))
        sameQuads ("trellis, " ++ show k ++ " quarter turns")
          (drop 20 qs) (drop 20 (dec32 16 (turnsT k)))
  ]
  where
    nubPoints [] = []
    nubPoints (x : xs) = x : nubPoints (filter (/= x) xs)
    flipAt i bs = [ if j == i then not b else b | (j, b) <- zip [0 :: Int ..] bs ]
    sameBits what want got =
      case [ i | (i, a, b) <- zip3 [0 :: Int ..] want got, a /= b ] of
        [] -> assertBool (what ++ ": nothing decoded") (length got >= length want - 8)
        (i : _) -> assertFailure (what ++ ": bit " ++ show i ++ " of "
                     ++ show (length want) ++ " differs ("
                     ++ show (length [ () | (a, b) <- zip want got, a /= b ]) ++ " wrong)")
    -- These lists are hundreds of symbols long; report where they first
    -- differ rather than printing both.
    sameQuads what want got = do
      assertEqual (what ++ ": length") (length want) (length got)
      case [ (i, a, b) | (i, a, b) <- zip3 [0 :: Int ..] want got, a /= b ] of
        [] -> return ()
        ((i, a, b) : _) ->
          assertFailure (what ++ ": symbol " ++ show i ++ " is " ++ show b
                         ++ ", expected " ++ show a ++ " ("
                         ++ show (length [ () | (x, y) <- zip want got, x /= y ])
                         ++ " of " ++ show (length want) ++ " wrong)")

-- The V.32 pump on a line, at each of its three rates.
--
-- Every case sends the receiver conditioning signal of 5.2 before the
-- data, because that is what a V.32 modem does and what its receiver is
-- entitled to expect: 256 symbols of S, 16 of S-bar and then TRN, whose
-- stated purpose is training the far equaliser.  Judging a cold
-- receiver on data it was handed with no training measures something
-- the Recommendation never asks for -- and, tried, it fails on
-- impairments it handles comfortably once trained.
v32PumpTests :: TestTree
v32PumpTests = testGroup "V.32 data pump"
  [ testCase (rateName r ++ ": " ++ nm) $ do
      let (clean, preSyms) = v32ModulateTrained fs Calling r 0.5 trn payload
          sig = applyChannel fs ch clean
          got = v32DemodulateTrained fs Answering r preSyms sig
          errs = minimum [ length (filter id (zipWith (/=) (drop 200 payload) (drop (200 + o) got)))
                         | o <- [0 .. 300] ]
      assertEqual "bit errors after training" 0 errs
  | (r, conds) <- [ (V32R4800, slow), (V32R7200, fastCoded), (V32R9600, fastPlain)
                  , (V32R9600T, fastCoded), (V32R12000, top), (V32R14400, top) ]
  , (nm, ch) <- conds ]
  where
    fs = 8000
    trn = 1400
    payload = prbs (11, 9) 4000
    rateName r = case r of
      V32R4800 -> "4800"
      V32R7200 -> "7200"
      V32R9600 -> "9600"
      V32R9600T -> "9600 trellis"
      V32R12000 -> "12000"
      V32R14400 -> "14400"
    tel s = telephoneChannel s
    -- Conditions every rate must survive.  +/- 7 Hz is the frequency
    -- offset 2.1/V.32 obliges the receiver to work through.
    common =
      [ ("clean", idealChannel)
      , ("telephone band", idealChannel { chBandpass = Just (300, 3400) })
      , ("SNR 30 dB", tel 30)
      , ("SNR 25 dB", tel 25)
      , ("carrier offset +7 Hz", (tel 25) { chFreqOffsetHz = 7 })
      , ("carrier offset -7 Hz", (tel 25) { chFreqOffsetHz = -7 })
      , ("clock +0.3 %", (tel 25) { chRateOffset = 0.003 })
      , ("clock -0.3 %", (tel 25) { chRateOffset = -0.003 })
      ]
    jitter = ("jitter", (tel 25) { chJitter = SineJitter 3 2 })
    delay1 = ("delay distortion 1 ms", (tel 25) { chDelayDist = 1 })
    fastClock = ("clock +0.5 %", (tel 25) { chRateOffset = 0.005 })
    -- 4800 bit/s uses the same four points as the training signal and is
    -- as robust as the rest of this modem: it survives everything the
    -- channel simulator offers, in-band echo included.
    slow = common ++
      [ ("SNR 20 dB", tel 20), ("SNR 15 dB", tel 15), ("SNR 12 dB", tel 12)
      , jitter, delay1, fastClock
      , ("delay distortion 3 ms", (tel 25) { chDelayDist = 3 })
      , ("echo -12 dB at 5 ms", (tel 25) { chEcho = Just (0.005, fromDb (-12)) })
      ]
    -- 9600 non-redundant reaches 18 dB; the trellis alternative reaches
    -- 16, which is the coding gain of 4.2 showing up on a line rather
    -- than in a distance calculation.  The trellis decoder pays for it
    -- in sensitivity to timing jitter, which moves the phase under a
    -- decoder that judges a sequence rather than a symbol.
    fast = common ++ [ ("SNR 20 dB", tel 20), ("SNR 18 dB", tel 18), delay1, fastClock ]
    fastPlain = fast ++ [ jitter ]
    fastCoded = fast ++ [ ("SNR 16 dB", tel 16) ]
    -- 12000 and 14400 pack 64 and 128 points into the same band, so they
    -- want a quieter line than anything else here does
    top = common ++ [ delay1, fastClock ]

-- | An echo path with several taps at fractional delays -- what a
-- hybrid actually returns.  Modec.Channel's chEcho is a single real tap
-- at a whole number of samples, which a linear FIR cancels exactly; a
-- canceller measured against that reports a number it will not repeat on
-- a telephone line.
echoPath :: [(Double, Double)] -> Signal -> Signal
echoPath taps x = VS.generate (VS.length x) $ \i ->
  sum [ g * sampleAt x (fromIntegral i - d) | (d, g) <- taps ]

-- Run the canceller the way Modec.Modem will: cancel the received block
-- first, then remember the block we transmitted.  A modem produces its
-- transmit audio only after consuming the receive block, so the
-- reference is always a block behind, and the test has to honour that or
-- it is measuring a canceller that could not exist.
runEcho :: EchoConfig -> Int -> Signal -> Signal -> (Signal, EchoState)
runEcho cfg blk tx rx = go 0 (echoInit cfg) []
  where
    n = VS.length rx
    go i st acc
      | i >= n = (VS.concat (reverse acc), st)
      | otherwise =
          let take_ = min blk (n - i)
              (st1, clean) = echoBlock cfg True (VS.slice i take_ rx) st
              st2 = echoPush cfg (VS.slice i take_ tx) st1
          in go (i + take_) st2 (clean : acc)

echoTests :: TestTree
echoTests = testGroup "echo cancellation"
  [ testCase "a dispersive hybrid return is cancelled by 30 dB" $ do
      let tx = gaussianNoise 5 24000 0.3
          rx = echoPath [(200, 0.20), (203.5, 0.10), (209.2, 0.04)] tx
          cfg = defaultEchoConfig
          (out, st) = runEcho cfg 160 tx rx
          tailOf v = VS.drop (VS.length v - 6000) v
          p v = VS.sum (VS.map (\a -> a * a) v) / fromIntegral (VS.length v)
          erle = 10 * logBase 10 (p (tailOf rx) / p (tailOf out))
      assertBool ("converged ERLE " ++ show erle ++ " dB") (erle > 30)
      assertBool ("tracked ERLE " ++ show (echoErle st) ++ " dB") (echoErle st > 25)

  , testCase "it does not move the taps while the far end is talking" $ do
      -- With both ends transmitting, the far end's signal lands in the
      -- error term and drives the filter away from the echo path.  The
      -- start-up of Figure 4/V.32 is half duplex so this never has to be
      -- guessed at, and the canceller simply refuses to adapt unless it
      -- is told the line is ours.
      let tx = gaussianNoise 5 16000 0.3
          far = gaussianNoise 99 16000 0.3
          rx = VS.zipWith (+) (echoPath [(200, 0.2), (203.5, 0.1)] tx) far
          cfg = defaultEchoConfig
          frozen = echoInit cfg
          (_, stNo) = foldl (\(i, st) _ ->
              let sl = VS.slice i 160 rx
                  (st1, _) = echoBlock cfg False sl st
              in (i + 160, echoPush cfg (VS.slice i 160 tx) st1))
            (0, frozen) [1 .. 90 :: Int]
      assertEqual "a frozen canceller subtracts nothing"
        0 (round (1e9 * echoErle stNo) :: Int)

  , testCase "the answer does not depend on how the audio is cut up" $ do
      let tx = gaussianNoise 5 12000 0.3
          rx = echoPath [(200, 0.2), (203.5, 0.1)] tx
          cfg = defaultEchoConfig
          (a, _) = runEcho cfg 160 tx rx
          (b, _) = runEcho cfg 80 tx rx
          worst = VS.maximum (VS.map abs (VS.zipWith (-) a b))
      assertBool ("largest difference " ++ show worst) (worst < 1e-12)

  , testCase "a canceller with nothing to cancel does nothing at all" $ do
      -- On a line with no echo -- a four-wire VoIP leg, or two modems
      -- wired together -- an adapting filter can only add its own
      -- wandering, and at a step size that converges quickly that is
      -- enough to take 9600 bit/s apart.  It was, too: two modems over a
      -- pair of pipes connected and then talked nonsense at each other.
      let tx = gaussianNoise 5 12000 0.3
          rx = gaussianNoise 42 12000 0.2
          (out, _) = runEcho defaultEchoConfig 160 tx rx
      assertEqual "the received signal is handed on untouched"
        (VS.toList rx) (VS.toList out)

  , testCase "echoSetFar moves the delay the filter actually reads" $ do
      -- 'esDelay' is the bulk delay in force and 'echoSetFar' is the only
      -- thing that moves it.  The filter used to take its offsets from
      -- 'ecDelay' in the config instead, so echoSetFar dropped the taps
      -- and retargeted nothing -- a trap for whoever called it next.
      -- Nothing in the modem calls it today; this is what keeps it
      -- honest for when something does.
      let far = 900
          tx = gaussianNoise 7 24000 0.3
          rx = echoPath [(fromIntegral far, 0.25)] tx
          -- a filter aimed at the default 160 cannot see an echo at 900,
          -- since 256 taps only reach 415
          (_, stNear) = runEcho defaultEchoConfig 160 tx rx
          aimed cfg blk = go 0 (echoSetFar far (echoInit cfg)) []
            where
              n = VS.length rx
              go i st acc
                | i >= n = (VS.concat (reverse acc), st)
                | otherwise =
                    let take_ = min blk (n - i)
                        (st1, clean) = echoBlock cfg True (VS.slice i take_ rx) st
                        st2 = echoPush cfg (VS.slice i take_ tx) st1
                    in go (i + take_) st2 (clean : acc)
          (_, stFar) = aimed defaultEchoConfig 160
      assertBool ("aimed at 160 it finds nothing: " ++ show (echoErle stNear))
        (echoErle stNear < 3)
      assertBool ("aimed at 400 it cancels: " ++ show (echoErle stFar))
        (echoErle stFar > 20)
  ]

-- The start-up signals, as they actually go on the line.
v32SignalTests :: TestTree
v32SignalTests = testGroup "V.32 start-up signals"
  [ testCase "AA and AC land where the Recommendation says to listen" $ do
      -- The calling modem's steady state A is a tone at the carrier;
      -- the answering modem's alternating A and C is that carrier
      -- switched 180 degrees every symbol, which puts its energy at
      -- 1800 +/- 1200 Hz.  Those are the 600 and 3000 Hz that 5.4.1 has
      -- the calling modem listen for, and they fall out of the
      -- constellation rather than being stated anywhere.
      let aa = modulateStates 600 [StA]
          ac = modulateStates 600 [StA, StC]
          at f x = goertzel 8000 f (VS.drop 800 x)
      assertBool "AA is a tone at 1800" (at 1800 aa > 20 * at 600 aa && at 1800 aa > 20 * at 3000 aa)
      assertBool "AC has no energy at 1800" (at 1800 ac < 0.05 * at 600 ac)
      assertBool "AC sits at 600 and 3000" (at 600 ac > 10 * at 1800 ac && at 3000 ac > 10 * at 1800 ac)

  , testCase "a phase reversal is found to the sample" $ do
      -- 5.4.1 fixes the turnaround from hearing a reversal to sending
      -- one at 64 +/- 2 symbol periods: 26.67 +/- 0.83 ms, or +/- 6.7
      -- samples at 8 kHz.  A 20 ms handshake tick cannot express that,
      -- so the receiver has to timestamp the event itself.
      forM_ [ ("AA to CC at 1800 Hz", 1800, [StA], [StC])
            , ("AC to CA at 600 Hz", 600, [StA, StC], [StC, StA])
            , ("AC to CA at 3000 Hz", 3000, [StA, StC], [StC, StA]) ] $
        \(nm, f, before, after) -> do
          let n = 400
              sig = modulateStates2 n before n after
              want = round (fromIntegral n * 8000 / 2400 :: Double) :: Int
              (_, revs) = revBlock sig (revInit 8000 f)
              near = [ r | r <- revs, abs (r - want) < 40 ]
          assertBool (nm ++ ": found " ++ show revs ++ ", wanted near " ++ show want)
            (length near == 1)
          let got = head near
          assertBool (nm ++ ": off by " ++ show (got - want) ++ " samples")
            (abs (got - want) <= 7)
  ]

-- A run of one state, then a run of another, through the real pump.
modulateStates2 :: Int -> [TrainState] -> Int -> [TrainState] -> Signal
modulateStates2 n1 a n2 b =
  modulatePointsFor (take n1 (cycle (map statePoint a)) ++ take n2 (cycle (map statePoint b)))

modulateStates :: Int -> [TrainState] -> Signal
modulateStates n a = modulatePointsFor (take n (cycle (map statePoint a)))

-- Two V.32 modems talking to each other through a noisy, attenuated
-- line, one block of transport delay in each direction -- the same
-- arrangement modemDuplex uses for the other modes.  The round trip that
-- the start-up measures for itself is that delay, so a test can check
-- the modem's own answer against a number it knows.
v32StartDuplex :: Double -> Double -> Int -> (V32Status, V32Status, [(Double, (V32Phase, V32Phase))], Maybe Int, Maybe Int)
v32StartDuplex snr maxT blk = go 0 o0 a0 quiet quiet V32Busy V32Busy []
  where
    fs = 8000
    quiet = VS.replicate blk 0
    offer = allRates
    o0 = v32StartInit fs Calling offer
    a0 = v32StartInit fs Answering offer
    impair k t x = addNoise (k * 100003 + t) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.7) x)
    go t so sa fromA fromO stO stA trace
      | fromIntegral t * fromIntegral blk / fs > maxT = (stO, stA, reverse trace, v32RoundTrip so, v32RoundTrip sa)
      | otherwise =
          let (so', audO, s1) = v32StartStep so (impair 1 t fromA)
              (sa', audA, s2) = v32StartStep sa (impair 2 t fromO)
              secs = fromIntegral t * fromIntegral blk / fs
              here = (v32Phase so', v32Phase sa')
              trace' = if null trace || snd (head trace) /= here
                         then (secs, here) : trace else trace
          in case (s1, s2) of
               (V32Busy, V32Busy) -> go (t + 1) so' sa' audA audO s1 s2 trace'
               _ | done s1 && done s2 -> (s1, s2, reverse trace', v32RoundTrip so', v32RoundTrip sa')
                 | otherwise -> go (t + 1) so' sa' audA audO s1 s2 trace'
    done V32Busy = False
    done _ = True

-- Two V.32 data pumps cross-connected, with the training the start-up
-- would have given them.
v32PumpDuplex :: V32Rate -> Int -> [Bool] -> ([Bool], Double)
v32PumpDuplex r blocks payload = go 0 (v32DataInit fs r) (v32DataInit fs r) quiet payload []
  where
    fs = 8000
    quiet = VS.replicate 160 0
    per = rateBitsPerSymbol r * 48
    go i po pa fromA bits acc
      | i >= blocks = (concat (reverse acc), v32DataEvm po)
      | otherwise =
          let (po1, got) = v32DataRx fs Calling r po fromA
              (pa1, _) = v32DataRx fs Answering r pa quiet
              (pa2, audA) = v32DataTx fs Answering r 0.5 160 (take per bits) pa1
              (po2, _) = v32DataTx fs Calling r 0.5 160 [] po1
          in go (i + 1) po2 pa2 audA (drop per bits) (got : acc)

-- The calling modem's V.32 decision error at the end of a call.
v32CallEvm :: ModemConfig -> ModemConfig -> Double -> Maybe Double
v32CallEvm cfgO cfgA maxT = go 0 (modemInit cfgO) (modemInit cfgA) quiet quiet Nothing
  where
    blk = 160; fs = 8000
    quiet = VS.replicate blk 0
    impair k t x = addNoise (k * 100003 + t) (0.05 * 0.707 / fromDb 30) (VS.map (* 0.1) x)
    go t so sa fromA fromO best
      | fromIntegral t * fromIntegral blk / fs > maxT = best
      | otherwise =
          let (so', audO, _, _) = modemStep cfgO so (impair 1 t fromA) []
              (sa', audA, _, _) = modemStep cfgA sa (impair 2 t fromO) []
          in go (t + 1) so' sa' audA audO (case modemV32Evm so' of
                                             Just e -> Just (abs e)
                                             Nothing -> best)

v32StartTests :: TestTree
v32StartTests = testGroup "V.32 start-up per Figure 4"
  [ testCase "the data pump carries bits between two of itself" $ do
      -- First, the block receiver against the offline modulator, which
      -- the pump tests already trust.  These start the receiver cold, so
      -- they stop at 9600: above that a receiver needs the training the
      -- start-up gives it, which is what the impairment tests use and
      -- what the whole-modem test exercises.  What is being checked here
      -- is the block plumbing, and that is the same at every rate.
      forM_ [V32R4800, V32R9600, V32R9600T] $ \r -> do
        let payload = prbs (11, 9) 6000
            sig = v32Modulate 8000 Answering r 0.5 payload
            step (st, acc) blk = let (st', bs) = v32DataRx 8000 Calling r st blk
                                 in (st', acc ++ bs)
            (_, got) = foldl step (v32DataInit 8000 r, []) (chunksOf 160 sig)
            best = minimum [ (length (filter id (zipWith (/=) (drop 500 payload) (drop o got))), o)
                           | o <- [0 .. 800] ]
        assertBool ("block receiver, " ++ show r ++ ": " ++ show (length got) ++ " bits, best " ++ show best)
          (fst best == 0)
      -- then the block transmitter, demodulated offline
      forM_ [V32R4800, V32R9600, V32R9600T] $ \r -> do
        let payload = prbs (11, 9) 6000
            step (st, acc) chunk =
              let (st', a) = v32DataTx 8000 Answering r 0.5 160 chunk st
              in (st', acc ++ [a])
            perBlock = rateBitsPerSymbol r * 48
            chunks = takeWhile (not . null) (map (\i -> take perBlock (drop (i * perBlock) payload)) [0 .. 60])
            (_, blocks) = foldl step (v32DataInit 8000 r, []) chunks
            got = v32Demodulate 8000 Calling r (VS.concat blocks)
            best = minimum [ (length (filter id (zipWith (/=) (drop 500 payload) (drop o got))), o)
                           | o <- [0 .. 800] ]
        assertBool ("block transmitter, " ++ show r ++ ": " ++ show (length got) ++ " bits, best " ++ show best)
          (fst best == 0)
      forM_ [V32R4800, V32R7200, V32R9600, V32R9600T] $ \r -> do
        let payload = prbs (11, 9) 4000
            (got, evm) = v32PumpDuplex r 90 payload
            at o = length (filter id (zipWith (/=) (drop 500 payload) (drop o got)))
            best = minimum [ (at o, o) | o <- [0 .. 3000] ]
        assertBool (show r ++ ": EVM " ++ show evm ++ ", " ++ show (length got)
                    ++ " bits, best " ++ show best)
          (fst best == 0)
  , testCase "two V.32 modems reach 9600 trellis" $ do
      let (so, sa, trace, nt, mt) = v32StartDuplex 25 20 160
      -- both ends offer everything, so Table 5/V.32 bis has them settle
      -- on the top rate rather than on V.32's ceiling
      assertEqual ("calling side (trace " ++ show trace ++ ")")
        (V32Connected V32R14400) so
      assertEqual "answering side" (V32Connected V32R14400) sa
      -- the harness delays each direction by one block, so the round
      -- trip the modem measures for itself should be about two of them
      case (nt, mt) of
        (Just a, Just b) -> do
          assertBool ("NT " ++ show a ++ " samples") (a > 100 && a < 1200)
          assertBool ("MT " ++ show b ++ " samples") (b > 100 && b < 1200)
        _ -> assertFailure ("round trip not measured: " ++ show (nt, mt))
  ]
