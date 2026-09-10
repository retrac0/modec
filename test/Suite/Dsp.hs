-- | The primitives everything else is built on.
module Suite.Dsp (dspTests, scramblerTests, stageTests, toneFrameTests, wavTests) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM_)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Standards
import Modec.Link
import Modec.Detect
import Modec.Scrambler (lfsr)
import qualified Modec.Scrambler as Scr
import Modec.DSP
import Modec.V22
import Modec.V32
import Modec.QAM
import Modec.V32Pump
import Modec.Stream
import Modec.Wav

wavTests :: TestTree
wavTests = testGroup "wav"
  [ testCase "16-bit mono round trip" $ do
      let x = VS.fromList [0, 0.5, -0.5, 0.999, -1]
          bs = BL.toStrict (encodeWav16Mono 8000 x)
      case decodeWav bs of
        Left e -> assertFailure e
        Right w -> do
          assertEqual "rate" 8000 (wavRate w)
          assertEqual "len" 5 (VS.length (wavSamples w))
          assertBool "values" (VS.all (< 1e-4) (VS.zipWith (\a b -> abs (a - b)) x (wavSamples w)))
  ]

-- The shared primitives V.32 will be built on.  'cubicAt' needs no test
-- of its own: it moved out of Modec.V22 unchanged, and the V.22 pump's
-- exact-zero bit error assertions are a tighter check than anything
-- written here would be.
dspTests :: TestTree
dspTests = testGroup "shared DSP primitives"
  [ testCase "the lifted RRC kernel is the one V.22 has been using" $ do
      -- Modec.V22 no longer has its own kernel: 'rrcTaps' is 'rrcKernel'
      -- at 600 Bd, 0.75 roll-off and a 6 symbol span.  Asserting those
      -- two against each other would now be a tautology, so the
      -- reference here is the formula V.22 used to carry inline, and
      -- what it guards is that the arguments still say what they said.
      let refPulse t
            | abs t < 1e-9 = 1 - b + 4 * b / pi
            | abs (abs t - 1 / (4 * b)) < 1e-9 =
                b / sqrt 2 * ((1 + 2 / pi) * sin (pi / (4 * b)) + (1 - 2 / pi) * cos (pi / (4 * b)))
            | otherwise =
                (sin (pi * t * (1 - b)) + 4 * b * t * cos (pi * t * (1 + b))) / (pi * t * (1 - (4 * b * t) ^ (2 :: Int)))
            where b = 0.75
          sps = 8000 / 600 :: Double
          half = round (6 * sps) :: Int
          raw = [ refPulse (fromIntegral (i - half) / sps) | i <- [0 .. 2 * half] ]
          norm = sqrt (sum (map (\v -> v * v) raw))
      assertEqual "taps" (map (/ norm) raw) (VS.toList (rrcTaps 8000))

  , testCase "chunksOf partitions a signal and loses nothing" $
      forM_ [1, 7, 160, 999, 5000] $ \n -> do
        let x = VS.generate 3000 (\i -> sin (0.01 * fromIntegral i)) :: Signal
            cs = chunksOf n x
        assertEqual ("rejoins at " ++ show n) (VS.toList x) (VS.toList (VS.concat cs))
        assertBool ("no empty block at " ++ show n) (all (not . VS.null) cs)
        assertBool ("full but the last at " ++ show n)
          (all (\c -> VS.length c == n) (if null cs then [] else init cs))

  , testCase "the RRC kernel has unit energy and is symmetric" $
      forM_ [(8000, 600, 0.75, 6), (8000, 2400, 0.25, 8), (48000, 2400, 0.5, 6)] $
        \(fs, bd, ro, sp) -> do
          let k = rrcKernel fs bd ro sp
              e = VS.sum (VS.map (\v -> v * v) k)
          assertBool "odd length" (odd (VS.length k))
          assertBool ("unit energy: " ++ show e) (abs (e - 1) < 1e-9)
          assertBool "symmetric" (VS.toList k == reverse (VS.toList k))

  , testCase "root raised cosine squares up to a Nyquist pulse" $ do
      -- RRC convolved with itself is a raised cosine, which is zero at
      -- every non-zero multiple of the symbol period.  That is the
      -- property the matched filter exists to provide, and it catches a
      -- wrong roll-off or a mis-scaled time axis.
      -- 9600/2400 gives exactly 4 samples per symbol, so the zeros land
      -- on samples and the check is not blunted by where we sample.
      let sps = 4 :: Int
          k = rrcKernel 9600 2400 0.5 10
          n = VS.length k
          rc d = sum [ VS.unsafeIndex k i * VS.unsafeIndex k (i + d)
                     | i <- [0 .. n - 1 - d] ]
          peak = rc 0
      forM_ [1 .. 4 :: Int] $ \m -> do
        let v = abs (rc (m * sps) / peak)
        assertBool ("symbol " ++ show m ++ " leaks " ++ show v) (v < 1e-3)

  , testCase "the singularities of the RRC pulse are their limits" $
      forM_ [0.25, 0.5, 0.75] $ \b -> do
        let near t = rrcPulse b t
            lim t = (rrcPulse b (t - 1e-7) + rrcPulse b (t + 1e-7)) / 2
        assertBool "t = 0" (abs (near 0 - lim 0) < 1e-6)
        assertBool "t = 1/4b" (abs (near (1 / (4 * b)) - lim (1 / (4 * b))) < 1e-6)

  , testCase "O.152 is a maximal-length sequence of 2047 bits" $ do
      let bits = prbs (11, 9) 6141          -- three periods
          period = take 2047 bits
      assertEqual "repeats after 2047" (period ++ period ++ period) bits
      assertBool "no shorter period" $
        and [ take (2047 - k) (drop k bits) /= take (2047 - k) bits
            | k <- [1, 2, 3, 7, 23, 89, 1023] ]

  , testCase "O.152 is balanced and runs no longer than the register" $ do
      let period = prbs (11, 9) 2047
          ones = length (filter id period)
          runs b = maximum (map length (filter (all (== b)) (groupRuns period)))
      -- A maximal-length 11-stage sequence has 2^10 ones and 2^10 - 1
      -- zeros, one run of 11 ones and none of more than 10 zeros.
      assertEqual "ones" 1024 ones
      assertEqual "longest run of ones" 11 (runs True)
      assertEqual "longest run of zeros" 10 (runs False)
  ]
  where
    groupRuns [] = []
    groupRuns (x:xs) = let (a, b) = span (== x) xs in (x : a) : groupRuns b

-- | The shared self-synchronising scrambler.  V.22 and V.32 used to
-- carry a copy each; these pin the behaviour both copies had.
scramblerTests :: TestTree
scramblerTests = testGroup "self-synchronising scrambler"
  [ testCase "V.22's polynomial still scrambles ones the way it did" $
      -- 1 + x^-14 + x^-17 from an all-zero register, which is what the
      -- transmitter sends during scrambled binary 1.  Taken from the
      -- implementation Modec.V22 carried before the lift.
      assertEqual "scrambled ones"
        "111111111111110001111111111100000011111111000111"
        (concatMap (\b -> if b then "1" else "0")
                   (snd (Scr.scrambleRun (lfsr 14 17) 0 (replicate 48 True))))

  , testCase "every polynomial here is its own inverse" $
      forM_ [("V.22", lfsr 14 17), ("V.32 GPC", lfsr 18 23), ("V.32 GPA", lfsr 5 23)] $
        \(name, l) -> do
          let bits = prbs (11, 9) 600
              (_, line) = Scr.scrambleRun l 0 bits
              (_, back) = Scr.descrambleRun l 0 line
          assertEqual name bits back

  , testCase "a descrambler started in the wrong state catches up" $
      -- This is what "self-synchronising" buys and why no framing is
      -- needed underneath it: the register holds line bits, so after as
      -- many bits as it is wide both ends hold the same thing whatever
      -- the receiver started from.  Nothing else here tests it, and a
      -- scrambler that quietly stopped having the property would still
      -- pass every round trip that starts both ends at zero.
      forM_ [(14, 17), (18, 23), (5, 23)] $ \(a, b) -> do
        let l = lfsr a b
            width = max a b
            bits = prbs (11, 9) 400
            (_, line) = Scr.scrambleRun l 0 bits
            (_, back) = Scr.descrambleRun l 0x2AAAA line
        assertEqual ("after " ++ show width ++ " bits, taps " ++ show (a, b))
          (drop width bits) (drop width back)
  ]

-- | The two QAM-family receivers as stream stages.  A receiver that is
-- only correct at one block size is not a streaming receiver, and both
-- of these are driven from PipeWire buffers whose size is not ours to
-- pick.
stageTests :: TestTree
stageTests = testGroup "pump receivers do not depend on the block size"
  [ testCase "V.22 at 1200 bit/s" $ do
      let bits = prbs (11, 9) 2000
          sig = v22Modulate 8000 HighChannel 0.5 bits
          at c = concatMap roBits (runStage (v22Receiver 8000 HighChannel) (chunksOf c sig))
      forM_ [7, 160, 1000, 4096] $ \c ->
        assertEqual ("chunks of " ++ show c) (at 160) (at c)

  , testCase "V.32 at 9600 bit/s" $ do
      let bits = prbs (11, 9) 2000
          sig = v32Modulate 8000 Originate V32R9600 0.5 bits
          p = v32Params 8000
          at c = concatStage (qamReceiver p (v32RxCfg V32R9600)) (chunksOf c sig)
      forM_ [7, 160, 1000, 4096] $ \c ->
        assertEqual ("chunks of " ++ show c) (map qsIndex (at 160)) (map qsIndex (at c))
  ]

-- | A tone frame answers questions about itself.
toneFrameTests :: TestTree
toneFrameTests = testGroup "tone frames carry their own bank"
  [ testCase "a measured frequency reads back, an unmeasured one is zero" $ do
      let fs = 8000
          sig = VS.generate 8000 (\i -> 0.5 * sin (2 * pi * 1650 * fromIntegral i / fs))
          frs = toneFrames fs defaultToneBank sig
          fr = last frs
      assertBool "1650 Hz is loud" (toneAmp fr 1650 > 0.4)
      assertBool "1270 Hz is not" (toneAmp fr 1270 < 0.05)
      -- 2250 Hz is in the diagnostic bank but not the default one, so a
      -- frame from the default bank must report it as absent rather than
      -- reading whatever sits at that index of another bank's list
      assertEqual "unmeasured" 0 (toneAmp fr 2250)
      assertEqual "dominant" (Just 1650) (dominant 3e-3 1.5 fr)

  , testCase "the diagnostic bank measures what the handshake bank leaves out" $ do
      let fs = 8000
          sig = VS.generate 8000 (\i -> 0.5 * sin (2 * pi * 2250 * fromIntegral i / fs))
          amp cfg = toneAmp (last (toneFrames fs cfg sig)) 2250
      assertEqual "default bank has no 2250 Hz" 0 (amp defaultToneBank)
      assertBool "diagnostic bank does" (amp diagnosticToneBank > 0.4)
  ]
