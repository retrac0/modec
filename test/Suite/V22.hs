-- | V.22 and V.22bis: 600 Bd PSK and QAM on 1200/2400 Hz.
module Suite.V22 (v22Tests, framerTests) where

import Control.Monad (forM_)
import Data.Bits (testBit)
import Data.List (isSuffixOf)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Link
import Modec.Standards
import Modec.Channel
import Modec.DSP
import Modec.Async
import Modec.V22
import Modec.FSK

-- | V.22 data pump: bits through the channel, ignoring the start-up
-- bits before the descrambler has synchronised.
-- | A character stream with one bit missing, fed a block at a time.
-- The framer must realign -- taking any space as a start bit, it never
-- did, and put out high-bit garbage for the rest of the stream.
framerTests :: TestTree
framerTests = testGroup "start-stop framing"
  [ testCase "the framer realigns after a lost bit" $ do
      let text = map (fromIntegral . fromEnum) (concat (replicate 20 "the quick brown fox jumps over the lazy dog 0123456789\r\n"))
          bitsOf w = False : [ testBit w i | i <- [0 .. 7] ] ++ [True]
          stream = concatMap bitsOf text
          lost = take 1000 stream ++ drop 1001 stream
          feed st [] acc = (st, acc)
          feed st bs acc = let (now, rest) = splitAt 160 bs
                               (st', out) = asyncRxBits st now
                           in feed st' rest (acc ++ out)
          (_, got) = feed (asyncRxInit framing8N1) lost []
          tailOf n = reverse . take n . reverse
      assertEqual "the last forty characters" (tailOf 40 text) (tailOf 40 got)
  , testCase "a valid stream comes through unchanged, block by block" $ do
      let text = map (fromIntegral . fromEnum) "pack my box with five dozen liquor jugs 9876543210\r\n"
          bitsOf w = False : [ testBit w i | i <- [0 .. 7] ] ++ [True]
          stream = replicate 30 True ++ concatMap bitsOf text ++ replicate 30 True
          feed st [] acc = (st, acc)
          feed st bs acc = let (now, rest) = splitAt 37 bs
                               (st', out) = asyncRxBits st now
                           in feed st' rest (acc ++ out)
      assertEqual "bytes" text (snd (feed (asyncRxInit framing8N1) stream []))
  ]

v22Tests :: TestTree
v22Tests = testGroup "V.22 data pump" $
  [ testCase (show ch ++ ": " ++ name) $ do
      let sig = f (v22Modulate 8000 ch 0.5 bits)
          got = v22Demodulate 8000 ch sig
          errs = minimum [ length (filter id (zipWith (/=) (drop 40 bits) (drop (40 + o) got))) | o <- [0 .. 200] ]
      assertEqual "bit errors after sync" 0 errs
  | ch <- [LowChannel, HighChannel]
  , (name, f) <- [ ("clean", id)
                 , ("telephone band, SNR 12 dB", applyChannel 8000 (telephoneChannel 12))
                 , ("frequency offset +7 Hz", applyChannel 8000 idealChannel { chFreqOffsetHz = 7 })
                 , ("frequency offset -7 Hz", applyChannel 8000 idealChannel { chFreqOffsetHz = -7 })
                 , ("clock offset +0.5 %", applyChannel 8000 idealChannel { chRateOffset = 0.005 })
                 , ("clock offset -0.5 %", applyChannel 8000 idealChannel { chRateOffset = -0.005 })
                 , ("sine jitter 3 samples at 2 Hz", applyChannel 8000 idealChannel { chJitter = SineJitter 3 2 })
                 , ("echo 5 ms -12 dB", applyChannel 8000 idealChannel { chEcho = Just (0.005, fromDb (-12)) })
                 , ("delay distortion 2 ms at band edges", applyChannel 8000 idealChannel { chDelayDist = 2 }) ]
  ] ++
  [ testCase "async bytes over V.22 with start/stop framing" $ do
      let payload = map (fromIntegral . fromEnum) "V.22 at 1200 bit/s: the quick brown fox\r\n" ++ [0, 255, 128]
          framed = replicate 40 True ++ frameBits framing8N1 payload ++ replicate 40 True
          got = v22Demodulate 8000 HighChannel (applyChannel 8000 (telephoneChannel 20) (v22Modulate 8000 HighChannel 0.5 framed))
          -- start-up bits (filter delays, descrambler sync) may yield a stray character first
          (_, bytes) = asyncRxBits (asyncRxInit framing8N1) got
      assertBool ("payload is a suffix of " ++ show bytes) (payload `isSuffixOf` bytes)
  , testCase "2400 bit/s after 1200 bit/s training: clean, 15 dB, 3 ms delay distortion, +7 Hz" $ do
      let fs = 8000
          fr = framing8N1
          (stPre, pre) = v22TxBlock fs HighChannel fr 0.5 False R1200 TxScrambledOnes [] 4800 v22TxInit
          dataBlocks st bs
            | null bs && null (txBitsOf st) = [snd (v22TxBlock fs HighChannel fr 0.5 False R2400 TxScrambledOnes [] 400 st)]
            | otherwise = let (st', sig) = v22TxBlock fs HighChannel fr 0.5 False R2400 TxScrambledData [] 160 (withBits st (take 2000 bs)) in sig : dataBlocks st' (drop 2000 bs)
          clean = pre VS.++ VS.concat (dataBlocks stPre bits)
          decode sig =
            let chunks v | VS.null v = [] | otherwise = VS.take 160 v : chunks (VS.drop 160 v)
                run _ _ [] = []
                run i st (c : cs) = let st1 = if i == (30 :: Int) then v22RxSetRate R2400 st else st
                                        (st', o) = v22RxBlock fs HighChannel c st1 in o : run (i + 1) st' cs
            in concatMap roBits (drop 30 (run 0 (v22RxInit fs) (chunks sig)))
          errs sig = minimum [ length (filter id (zipWith (/=) (drop 400 bits) (drop (400 + o) (decode sig)))) | o <- [0 .. 300] ]
      forM_ [ ("clean", id), ("SNR 15", applyChannel fs (telephoneChannel 15))
            , ("delay 3 ms", applyChannel fs idealChannel { chDelayDist = 3 }), ("freq +7", applyChannel fs idealChannel { chFreqOffsetHz = 7 }) ] $ \(name, f) ->
        assertEqual (name ++ " bit errors") 0 (errs (f clean))
  , testCase "unscrambled ones and S1 are recognised" $ do
      let (_, u11) = v22TxBlock 8000 HighChannel framing8N1 0.5 False R1200 TxU11 [] 4000 v22TxInit
          (_, s1) = v22TxBlock 8000 HighChannel framing8N1 0.5 False R1200 TxS1 [] 4000 v22TxInit
          (_, ones) = v22TxBlock 8000 HighChannel framing8N1 0.5 False R1200 TxScrambledOnes [] 4000 v22TxInit
          lastOut sig = last (v22RxRun 8000 (v22RxInit 8000) HighChannel sig)
      assertBool "U11 run" (roU11Run (lastOut u11) > 100)
      assertBool "S1 run" (roS1Run (lastOut s1) > 100)
      assertBool "scrambled ones run" (roOnesRun (lastOut ones) > 200)
  , testCase "unscrambled ones descramble to ones as well" $ do
      -- Why the handshake cannot decide it is hearing scrambled binary 1
      -- from a run of descrambled ones alone: a constant input to the
      -- descrambler is a constant output, so unscrambled binary 1 raises
      -- that run just as high.  What separates them is the carrier --
      -- one phase step repeated against a whitened one -- which is what
      -- the caller checks before it starts its settle timer and gives up
      -- on 2400 bit/s.
      let (_, u11) = v22TxBlock 8000 HighChannel framing8N1 0.5 False R1200 TxU11 [] 8000 v22TxInit
          out = last (v22RxRun 8000 (v22RxInit 8000) HighChannel u11)
      assertBool ("descrambled ones run " ++ show (roOnesRun out)) (roOnesRun out > 324)
      assertBool ("constant-step run " ++ show (roU11Run out)) (roU11Run out > 12)
  ]
  where
    bits = [ odd ((i * 7919 + 13) `div` 3 + i `div` 7) | i <- [1 .. 3000 :: Int] ]
