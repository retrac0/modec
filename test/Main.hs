module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf, sort)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, (</>))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Modec.Channel
import Modec.Detect
import Modec.Handshake
import Modec.DSP
import Modec.Metrics
import Modec.Modem
import Modec.Telnet
import Modec.Async
import Modec.V22
import Modec.Stream
import Modec.FSK
import Modec.Standards
import Modec.Wav

fixtureDir :: FilePath
fixtureDir = "test/fixtures"

-- | Fixture file names encode the standard and channel.
specFromName :: String -> FskSpec
specFromName name
  | "bell103_orig" `prefix` name = bell103Originate
  | "bell103_ans" `prefix` name = bell103Answer
  | "v21_ch1" `prefix` name = v21Channel1
  | "v21_ch2" `prefix` name = v21Channel2
  | otherwise = error ("cannot infer spec from fixture name " ++ name)
  where prefix p s = take (length p) s == p

fixtureTests :: IO TestTree
fixtureTests = do
  files <- sort . filter (".wav" `isSuffixOf`) <$> listDirectory fixtureDir
  return $ testGroup "minimodem fixtures" [ fixtureCase f | f <- files ]
  where
    fixtureCase f = testCase f $ do
      w <- readWav (fixtureDir </> f)
      expected <- B.readFile (fixtureDir </> replaceExtension f "txt")
      let spec = specFromName f
          fs = fromIntegral (wavRate w)
          got = B.pack (demodulate fs spec framing8N1 defaultDemodParams (wavSamples w))
      assertEqual "decoded bytes" (BC.unpack expected) (BC.unpack got)

roundTrip :: Double -> FskSpec -> Double -> Int -> [Word8] -> Bool
roundTrip fs spec noise seed bytes =
  let sig = addNoise seed noise (encodeBytes fs spec framing8N1 0.5 0.1 0.1 bytes)
  in demodulate fs spec framing8N1 defaultDemodParams sig == bytes

newtype Payload = Payload [Word8] deriving Show

instance Arbitrary Payload where
  arbitrary = Payload <$> resize 40 (listOf arbitrary)
  shrink (Payload bs) = map Payload (shrink bs)

propertyTests :: TestTree
propertyTests = testGroup "self round trips"
  [ testProperty "bell103 answer 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 bell103Answer 0 1 bs
  , testProperty "bell103 originate 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 bell103Originate 0 2 bs
  , testProperty "v21 ch2 8 kHz clean" $ \(Payload bs) -> roundTrip 8000 v21Channel2 0 3 bs
  , testProperty "bell103 answer 48 kHz clean" $ \(Payload bs) -> roundTrip 48000 bell103Answer 0 4 bs
  , testProperty "bell103 answer 11025 Hz clean" $ \(Payload bs) -> roundTrip 11025 bell103Answer 0 5 bs
  -- amplitude 0.5 tone vs sigma 0.2 white noise at 8 kHz is about 5 dB SNR in the
  -- full band and Eb/N0 of roughly 16 dB, where non-coherent FSK has a BER
  -- near 1e-9, so this must pass every time.
  , testProperty "bell103 answer 8 kHz noisy" $ \(Payload bs) (Positive seed) -> roundTrip 8000 bell103Answer 0.2 seed bs
  ]

-- | At Eb/N0 of about 11 dB (sigma 0.35 against amplitude 0.5 at 8 kHz) theory
-- gives a BER near 5e-4 for orthogonal non-coherent FSK.  Bell 103 tones are
-- only 200 Hz apart at 300 baud (not orthogonal), and measured BER of the
-- discriminator at ideal timing is about 2e-3, i.e. 2 % of bytes.  The
-- deframer's first-zero-crossing edge estimate adds timing jitter and
-- roughly doubles that (measured about 5 %).  Require under 8 %: this
-- catches a broken discriminator or deframer without being flaky, and
-- should be tightened when the deframer gets a better timing estimator.
errorRateTests :: TestTree
errorRateTests = testGroup "error rate under heavy noise"
  [ testCase "bell103 answer 8 kHz, sigma 0.35" $ do
      let payload = [ fromIntegral ((i * 7919 + 13) `mod` 256) | i <- [1 .. 2000 :: Int] ]
          sig = addNoise 42 0.35 (encodeBytes 8000 bell103Answer framing8N1 0.5 0.1 0.1 payload)
          got = demodulate 8000 bell103Answer framing8N1 defaultDemodParams sig
          d = editDistance payload got
      assertBool ("byte edit distance " ++ show d ++ " of " ++ show (length payload)) (d < 160)
  ]

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

-- | The streaming receiver must give the same bytes however the input is chunked.
chunkTests :: TestTree
chunkTests = testGroup "chunk invariance"
  [ testCase ("bell103_ans_8k_noisy.wav in chunks of " ++ show c) $ do
      w <- readWav (fixtureDir </> "bell103_ans_8k_noisy.wav")
      let fs = fromIntegral (wavRate w)
          x = wavSamples w
          whole = demodulate fs bell103Answer framing8N1 defaultDemodParams x
          chunks = [ VS.slice i (min c (VS.length x - i)) x | i <- [0, c .. VS.length x - 1] ] ++ [flushSilence fs bell103Answer]
          streamed = concatStage (fskReceiver fs bell103Answer framing8N1 defaultDemodParams) chunks
      assertEqual "bytes" whole streamed
  | c <- [7, 160, 1000, 4096] ]

-- | Conditions the Bell 103 receiver must survive without a single error.
channelTests :: TestTree
channelTests = testGroup "channel impairments (must be error free)"
  [ cond "telephone band, SNR 20 dB" base { chSnrDb = Just 20 } id
  , cond "rate offset +2 %" base { chRateOffset = 0.02 } id
  , cond "rate offset -2 %" base { chRateOffset = -0.02 } id
  , cond "frequency offset +7 Hz" base { chFreqOffsetHz = 7 } id
  , cond "frequency offset -7 Hz" base { chFreqOffsetHz = -7 } id
  , cond "sine jitter 3 samples at 2 Hz" base { chJitter = SineJitter 3 2 } id
  , cond "adjacent channel +20 dB" base (mixAt 20 adjacent)
  , cond "level -40 dBFS" base { chGain = fromDb (-40) / 0.5 } id
  , cond "hum 60 Hz" base { chHum = Just (60, 0.3) } id
  , cond "realistic acoustic coupling" base { chSnrDb = Just 25, chRateOffset = 0.005, chFreqOffsetHz = 3, chJitter = SineJitter 2 1 } (mixAt 20 adjacent)
  ]
  where
    fs = 8000
    base = idealChannel { chBandpass = Just (300, 3400) }
    payload = [ fromIntegral ((i * 7919 + 13) `mod` 256) | i <- [1 .. 300 :: Int] ]
    clean = encodeBytes fs bell103Answer framing8N1 0.5 0.2 0.2 payload
    adjacent = encodeBytes fs bell103Originate framing8N1 0.5 0.05 0.2 [ fromIntegral (i * 31) | i <- [1 .. 300 :: Int] ]
    cond name ch pre = testCase name $ do
      let got = demodulate fs bell103Answer framing8N1 defaultDemodParams (applyChannel fs ch (pre clean))
      assertEqual "decoded bytes" payload got

-- | Duplex call simulation: two handshake state machines connected by
-- attenuated, noisy audio in 20 ms blocks.
data Side = Side { sdPhase :: Double, sdBank :: Stage Signal [ToneFrame], sdHs :: HsState, sdTrace :: [(Double, TxCmd)], sdStatus :: HsStatus, sdTx :: TxCmd }

simulateCall :: HsConfig -> HsConfig -> Double -> Double -> (Side, Side)
simulateCall cfgO cfgA snr maxT = go 0 (side cfgO) (side cfgA)
  where
    fs = 8000
    blk = 160 :: Int
    side cfg = Side 0 (toneBank fs (hcBank cfg)) (initialHandshake cfg) [] HsBusy TxSilence
    go t o a
      | t >= maxT = (o, a)
      | connected (sdStatus o) && connected (sdStatus a) && t > stopAfter = (o, a)
      | otherwise =
          let (audioO, o1) = gen o
              (audioA, a1) = gen a
              o2 = recv cfgO t o1 (impair 1 t audioA)
              a2 = recv cfgA t a1 (impair 2 t audioO)
          in go (t + fromIntegral blk / fs) o2 a2
      where stopAfter = maybe maxT (+ 1) (connectTime a)
    connected (HsConnected {}) = True
    connected _ = False
    connectTime s = case [ tm | (tm, TxData _) <- sdTrace s ] of
      [] -> Nothing
      ts -> Just (minimum ts)
    -- 20 dB loss and additive noise, deterministic per block
    -- 20 dB loss, then noise for the requested SNR relative to a 0.5 amplitude tone
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    gen s =
      let f = case sdTx s of
            TxSilence -> 0
            TxTone x -> x
            TxMark sp -> fskMark sp
            TxData sp -> fskMark sp
          w = 2 * pi * f / fs
          sig = VS.generate blk (\i -> if f == 0 then 0 else 0.5 * sin (sdPhase s + w * fromIntegral i))
          ph = sdPhase s + w * fromIntegral blk
      in (sig, s { sdPhase = ph - 2 * pi * fromIntegral (floor (ph / (2 * pi)) :: Int) })
    recv cfg t s audio =
      case sdBank s of
       Stage bst bstep ->
        let (bst', frames) = bstep bst audio
            (hs', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep cfg h fr in (h', acc ++ [o])) (sdHs s, []) frames
            s' = s { sdBank = Stage bst' bstep, sdHs = hs' }
        in case outs of
             [] -> s'
             _ -> let (tx, st) = last outs
                  in s' { sdTx = tx, sdStatus = if st == HsBusy then sdStatus s else st, sdTrace = sdTrace s ++ [ (t, tx) | tx /= sdTx s ] }

-- | Duration of the first run of a given transmit command in a trace.
runLength :: (TxCmd -> Bool) -> [(Double, TxCmd)] -> Maybe Double
runLength p tr = case dropWhile (not . p . snd) tr of
  ((t0, _) : rest) -> case dropWhile (p . snd) rest of
    ((t1, _) : _) -> Just (t1 - t0)
    [] -> Nothing
  [] -> Nothing

handshakeTests :: TestTree
handshakeTests = testGroup "handshake simulation"
  [ call "auto / auto -> V.21" Nothing Nothing V21
  , call "originate V.21 / answer auto" (Just V21) Nothing V21
  , call "originate Bell 103 / answer auto" (Just Bell103) Nothing Bell103
  , call "originate auto / answer Bell 103" Nothing (Just Bell103) Bell103
  , call "originate auto / answer V.21" Nothing (Just V21) V21
  , call "Bell 103 / Bell 103" (Just Bell103) (Just Bell103) Bell103
  , testCase "answerer respects V.25 timing" $ do
      let (_, a) = simulateCall (defaultHsConfig Originate) (defaultHsConfig Answer) 30 20
          tr = sdTrace a
          ansStart = case [ t | (t, TxTone 2100) <- tr ] of { (t : _) -> t; [] -> -1 }
          ansLen = runLength (== TxTone 2100) tr
          gapLen = runLength (== TxSilence) (dropWhile ((/= TxTone 2100) . snd) tr)
      assertBool ("billing delay " ++ show ansStart) (ansStart >= 1.8 && ansStart <= 2.5)
      assertBool ("ANS duration " ++ show ansLen) (maybe False (\d -> d >= 2.6 && d <= 4.0) ansLen)
      assertBool ("gap " ++ show gapLen) (maybe False (\d -> d >= 0.055 && d <= 0.095) gapLen)
  , testCase "no answer -> caller fails after timeout" $ do
      let cfg = (defaultHsConfig Originate) { hcTimeout = 5 }
          (o, _) = simulateCall cfg (defaultHsConfig Answer) { hcBilling = 100 } 30 8
      assertEqual "status" (HsFailed "timeout") (sdStatus o)
  ]
  where
    call name so sa expect = testCase name $ do
      let (o, a) = simulateCall (defaultHsConfig Originate) { hcStandard = so } (defaultHsConfig Answer) { hcStandard = sa } 30 20
      assertEqual "originate" (HsConnected expect (txSpecOf Originate expect) (txSpecOf Answer expect)) (sdStatus o)
      assertEqual "answer" (HsConnected expect (txSpecOf Answer expect) (txSpecOf Originate expect)) (sdStatus a)
    txSpecOf Originate Bell103 = bell103Originate
    txSpecOf Answer Bell103 = bell103Answer
    txSpecOf Originate V21 = v21Channel1
    txSpecOf Answer V21 = v21Channel2

detectTests :: TestTree
detectTests = testGroup "detection" $
  [ testCase ("detectFsk " ++ fskName s) $ do
      let sig = encodeBytes 8000 s framing8N1 0.3 0.1 0.1 [ fromIntegral (i * 37) | i <- [1 .. 60 :: Int] ]
      case detectFsk 8000 (applyChannel 8000 (telephoneChannel 25) sig) of
        ((best, score) : _) -> do
          assertEqual "standard" (fskName s) (fskName best)
          assertBool ("score " ++ show score) (score > 0.4)
        [] -> assertFailure "no candidates"
  | s <- fskStandards ] ++
  [ testCase "toneRuns finds a 3 s answer tone" $ do
      let fs = 8000
          sig = VS.concat [ VS.replicate 8000 0
                          , VS.generate 24000 (\i -> 0.3 * sin (2 * pi * 2100 * fromIntegral i / fs))
                          , VS.replicate 600 0
                          , encodeBytes fs v21Channel2 framing8N1 0.3 0.5 0.1 [65, 66, 67] ]
          runs = toneRuns fs sig
          ans = [ r | r@(ToneRun (Just 2100) _ _) <- runs ]
      assertBool ("runs " ++ show (take 6 runs)) (case ans of
        (ToneRun _ t0 t1 : _) -> t0 > 0.9 && t0 < 1.1 && (t1 - t0) > 2.9 && (t1 - t0) < 3.1
        _ -> False)
  ]

-- | Two complete modems talking through attenuated, noisy audio in
-- 20 ms blocks; text is queued on both sides once connected.
modemDuplex :: ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8] -> Double -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplex cfgO cfgA snr textO textA maxT = go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0) False False [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    go t so sa fromA fromO sentO sentA rxO rxA evO evA
      | t >= maxT = (reverse rxO, reverse rxA, reverse evO, reverse evA)
      | otherwise =
          let queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t fromA) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t fromO) queueA
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO (sentO || not (null queueO)) (sentA || not (null queueA))
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA) (reverse eO ++ evO) (reverse eA ++ evA)

modemTests :: TestTree
modemTests = testGroup "full modem duplex"
  [ testCase "automode call, text both ways at 30 dB" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate Nothing) (defaultModemConfig 8000 Answer Nothing) 30 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V21 _ _ : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V21 _ _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "Bell 103 fixed, no handshake, 20 dB" $ do
      let cfg r = (defaultModemConfig 8000 r (Just Bell103)) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 20 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  ]
  where
    textO = map (fromIntegral . fromEnum) "Hello from the caller, 0123456789 !\r\n"
    textA = map (fromIntegral . fromEnum) "Answerer here; all bytes: \255\0\128 end\r\n"

telnetTests :: TestTree
telnetTests = testGroup "telnet codec"
  [ testCase "negotiation and IAC escapes" $ do
      let (st, _) = telnetHello telnetInit
          input = B.pack ([104, 105, iac, doOpt, optBinary, iac, iac, iac, doOpt, 24, iac, will, optSga, iac, 250, 24, 1, iac, 240, 33])
          (_, payload, reply) = telnetDecode st input
      assertEqual "payload" (B.pack [104, 105, 255, 33]) payload
      -- BINARY and SGA were already offered in the hello, so only the refusal of TERMINAL-TYPE (24) comes back
      assertEqual "reply" (B.pack [iac, wont, 24]) reply
  , testCase "fresh state answers DO/WILL for supported options" $ do
      let (_, _, reply) = telnetDecode telnetInit (B.pack [iac, doOpt, optBinary, iac, will, optBinary])
      assertEqual "reply" (B.pack [iac, will, optBinary, iac, doOpt, optBinary]) reply
  , testCase "encode doubles 0xFF" $
      assertEqual "encoded" (B.pack [1, 255, 255, 2]) (telnetEncode (B.pack [1, 255, 2]))
  ]

-- | V.22 data pump: bits through the channel, ignoring the start-up
-- bits before the descrambler has synchronised.
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
                 , ("echo 5 ms -12 dB", applyChannel 8000 idealChannel { chEcho = Just (0.005, fromDb (-12)) }) ]
  ] ++
  [ testCase "async bytes over V.22 with start/stop framing" $ do
      let payload = map (fromIntegral . fromEnum) "V.22 at 1200 bit/s: the quick brown fox\r\n" ++ [0, 255, 128]
          framed = replicate 40 True ++ frameBits framing8N1 payload ++ replicate 40 True
          got = v22Demodulate 8000 HighChannel (applyChannel 8000 (telephoneChannel 20) (v22Modulate 8000 HighChannel 0.5 framed))
          -- start-up bits (filter delays, descrambler sync) may yield a stray character first
          (_, bytes) = asyncRxBits (asyncRxInit framing8N1) got
      assertBool ("payload is a suffix of " ++ show bytes) (payload `isSuffixOf` bytes)
  , testCase "unscrambled ones and S1 are recognised" $ do
      let (_, u11) = v22TxBlock 8000 HighChannel framing8N1 0.5 False TxU11 [] 4000 v22TxInit
          (_, s1) = v22TxBlock 8000 HighChannel framing8N1 0.5 False TxS1 [] 4000 v22TxInit
          (_, ones) = v22TxBlock 8000 HighChannel framing8N1 0.5 False TxScrambledOnes [] 4000 v22TxInit
          lastOut sig = last (v22RxRun 8000 (v22RxInit 8000) HighChannel sig)
      assertBool "U11 run" (roU11Run (lastOut u11) > 100)
      assertBool "S1 run" (roS1Run (lastOut s1) > 100)
      assertBool "scrambled ones run" (roOnesRun (lastOut ones) > 200)
  ]
  where
    bits = [ odd ((i * 7919 + 13) `div` 3 + i `div` 7) | i <- [1 .. 3000 :: Int] ]

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain (testGroup "modec" [wavTests, fx, chunkTests, propertyTests, errorRateTests, channelTests, detectTests, handshakeTests, modemTests, telnetTests, v22Tests])
