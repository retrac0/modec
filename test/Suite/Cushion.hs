-- | The transmit cushion keeper, against a model of the PipeWire graph
-- that did the damage: capture handed over a quantum at a time, playback
-- drawing a quantum at a time from what the loop wrote, and a capture
-- stream that comes up 32 ms short when a softphone's streams connect.
module Suite.Cushion (cushionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Modec.Cushion

fs :: Double
fs = 8000

-- | The graph, run for this many seconds of its own clock.
--
-- Each quantum of 800 samples, capture delivers 800 (less any loss
-- scheduled then) and the loop reads them as 160-sample blocks, as it
-- does when a burst lands.  The first read of a burst waited for it;
-- the rest were already there, and each comes after the modem has run on
-- the block before, which takes anything up to @procMs@.  The monotonic
-- clock runs @ppm@ fast of the graph's, and each burst lands up to
-- @jitterMs@ late.  The loop writes a block for each block and whatever
-- the keeper asks for besides; playback draws 800 samples a quantum from
-- a buffer that starts with the cushion.
--
-- Returns the lowest the playback buffer got just after a draw -- what
-- is left to cover the loop being late with its next burst, 800 samples
-- when all is well -- the same at the end, and the silence added.
data Run = Run { runMin :: Int, runEnd :: Int, runAdded :: Int }

simulate :: Double -> [(Double, Int)] -> Double -> Double -> Bool -> Run
simulate = simulateWith 12

simulateWith :: Double -> Double -> [(Double, Int)] -> Double -> Double -> Bool -> Run
simulateWith procMs secs losses ppm jitterMs keep = go 0 (cushionInit 0) cushion0 cushion0 0 0
  where
    q = 800 :: Int
    cushion0 = 800
    quanta = round (secs * fs / fromIntegral q) :: Int
    go :: Int -> Cushion -> Int -> Int -> Int -> Int -> Run
    go k cu buf lo added carry
      | k >= quanta = Run lo buf added
      | otherwise =
          let tGraph = fromIntegral (k * q) / fs
              lost = sum [ n | (t, n) <- losses, t >= tGraph, t < tGraph + fromIntegral q / fs ]
              delivered = q - lost + carry
              blocks = delivered `div` 160
              carry' = delivered - blocks * 160
              jit = jitterMs / 1000 * fromIntegral ((k * 7) `mod` 5) / 4
              tArrive = tGraph * (1 + ppm * 1e-6) + jit
              -- the modem's time per block, varying with the load
              proc j = procMs / 1000 * fromIntegral (((k + j) * 13) `mod` 7) / 6
              readAll c j t w
                | j >= blocks = (c, w)
                | otherwise =
                    let (began, returned) = if j == 0 then (tArrive - 0.05, tArrive) else (t, t + 0.00005)
                        (extra, c') = if keep then cushionStep defaultCushionParams fs began returned 160 c else (0, c)
                    in readAll c' (j + 1) (returned + proc j) (w + 160 + extra)
              (cu', written) = readAll cu 0 tArrive 0
              extraAdded = written - blocks * 160
              drawn = buf + written - q
              lo' = min lo drawn
          in go (k + 1) cu' drawn lo' (added + extraAdded) carry'

cushionTests :: TestTree
cushionTests = testGroup "the transmit cushion"
  [ testCase "without the keeper, three 32 ms losses run the playback buffer dry" $ do
      let r = simulate 60 [(10, 256), (25, 256), (40, 256)] 0 5 False
      -- 768 of the 800 are gone, and the last 32 are a partial block the
      -- loop is still waiting to complete
      assertEqual "left after a draw, at the end" 0 (runEnd r)
  , testCase "with it, every loss is written back and nothing runs dry" $ do
      let r = simulate 60 [(10, 256), (25, 256), (40, 256), (50, 256)] 0 5 True
      -- each loss is felt until the window has seen it, and only one at a
      -- time: 256 short, and up to a partial block more held back
      assertBool ("lowest after a draw " ++ show (runMin r)) (runMin r >= 400)
      -- what goes back is the loss and the partial block it left waiting,
      -- which the playback buffer is short of just the same
      assertBool ("silence added " ++ show (runAdded r)) (runAdded r >= 4 * 256 && runAdded r <= 4 * (256 + 160))
      assertBool ("ends at " ++ show (runEnd r)) (runEnd r >= 800 && runEnd r <= 960)
  , testCase "a clean stream is left alone" $ do
      let r = simulate 120 [] 0 5 True
      assertEqual "silence added" 0 (runAdded r)
  , testCase "clock drift is followed, not topped up" $ do
      -- 100 ppm over twenty minutes is 120 ms: more than a step, and none
      -- of it is a loss
      let fast = simulate 1200 [] 100 5 True
          slow = simulate 1200 [] (-100) 5 True
      assertEqual "monotonic fast: silence added" 0 (runAdded fast)
      assertEqual "monotonic slow: silence added" 0 (runAdded slow)
  , testCase "losses are still caught under drift" $ do
      -- following the drift lags it by 100 ppm of the 30 s time constant,
      -- 3 ms, and that much more can go in with each loss: latency, not a hole
      let r = simulate 600 [(100, 256), (300, 256), (500, 256)] 100 5 True
      assertBool ("silence added " ++ show (runAdded r)) (runAdded r >= 3 * 256 && runAdded r <= 3 * (256 + 160 + 24))
      assertBool ("lowest after a draw " ++ show (runMin r)) (runMin r >= 400)
      assertBool ("ends at " ++ show (runEnd r)) (runEnd r >= 800 && runEnd r <= 1040)
  , testCase "capture arriving long is latency, and nothing is taken out" $ do
      let r = simulate 60 [(10, -400)] 0 5 True
      assertEqual "silence added" 0 (runAdded r)
      -- 400 extra: 320 of it written, 80 waiting as a partial block
      assertEqual "the extra stays in the buffer" 1120 (runEnd r)
  , testCase "a burst early and the next short by the same is let pass" $ do
      let r = simulate 60 [(10, -256), (13, 256)] 0 5 True
      assertEqual "silence added" 0 (runAdded r)
  , testCase "the modem's load between reads is not mistaken for a loss" $ do
      -- up to 12 ms a block, a whole burst of them: stamped as read, the
      -- level would sag by a burst's processing time whenever the load rose
      let r = simulateWith 12 300 [] 0 5 True
          busy = simulateWith 20 300 [] 0 5 True
      assertEqual "silence added" 0 (runAdded r)
      assertEqual "silence added, busier" 0 (runAdded busy)
  ]
