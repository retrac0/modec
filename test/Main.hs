module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM_)
import Data.List (isInfixOf, isSuffixOf, sort)
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
import Modec.Hdlc
import Modec.V8bis
import Modec.Hayes
import Modec.Dtmf
import Modec.Baresip
import Data.Bits (testBit, xor)
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
  , cond "delay distortion 3 ms at band edges" base { chDelayDist = 3 } id
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
            TxV22 ch _ _ -> if ch == HighChannel then 2250 else 1050   -- unscrambled ones, as a tone
            TxDual f1 _ _ -> f1
            TxBits sp _ -> fskMark sp
          w = 2 * pi * f / fs
          sig = VS.generate blk (\i -> if f == 0 then 0 else 0.5 * sin (sdPhase s + w * fromIntegral i))
          ph = sdPhase s + w * fromIntegral blk
      in (sig, s { sdPhase = ph - 2 * pi * fromIntegral (floor (ph / (2 * pi)) :: Int) })
    recv cfg t s audio =
      case sdBank s of
       Stage bst bstep ->
        let (bst', frames) = bstep bst audio
            (hs', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep cfg h fr Nothing [] in (h', acc ++ [(hoTx o, hoStatus o)])) (sdHs s, []) frames
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
  -- without a V.22 receiver the caller takes the answerer's V.22 probe (2250 Hz) for a Bell answer tone
  [ call "auto / auto (tones only) -> Bell 103" Nothing Nothing Bell103
  , call "originate V.21 / answer auto" (Just V21) Nothing V21
  , call "originate Bell 103 / answer auto" (Just Bell103) Nothing Bell103
  , call "originate auto / answer Bell 103" Nothing (Just Bell103) Bell103
  , call "originate auto / answer V.21" Nothing (Just V21) V21
  , call "Bell 103 / Bell 103" (Just Bell103) (Just Bell103) Bell103
  , testCase "answerer respects V.25 timing" $ do
      let (_, a) = simulateCall (defaultHsConfig Originate) { hcV8bis = False } (defaultHsConfig Answer) { hcV8bis = False } 30 20
          tr = sdTrace a
          ansStart = case [ t | (t, TxTone 2100) <- tr ] of { (t : _) -> t; [] -> -1 }
          ansLen = runLength (== TxTone 2100) tr
          gapLen = runLength (== TxSilence) (dropWhile ((/= TxTone 2100) . snd) tr)
      assertBool ("billing delay " ++ show ansStart) (ansStart >= 1.8 && ansStart <= 2.5)
      assertBool ("ANS duration " ++ show ansLen) (maybe False (\d -> d >= 2.6 && d <= 4.0) ansLen)
      assertBool ("gap " ++ show gapLen) (maybe False (\d -> d >= 0.055 && d <= 0.095) gapLen)
  , testCase "no answer -> caller fails after timeout" $ do
      let cfg = (defaultHsConfig Originate) { hcTimeout = 5, hcV8bis = False }
          (o, _) = simulateCall cfg (defaultHsConfig Answer) { hcBilling = 100, hcV8bis = False } 30 8
      assertEqual "status" (HsFailed "timeout") (sdStatus o)
  ]
  where
    call name so sa expect = testCase name $ do
      let (o, a) = simulateCall (defaultHsConfig Originate) { hcStandard = so, hcV8bis = False } (defaultHsConfig Answer) { hcStandard = sa, hcV8bis = False } 30 20
      assertEqual "originate" (HsConnected expect (linkFor Originate expect)) (sdStatus o)
      assertEqual "answer" (HsConnected expect (linkFor Answer expect)) (sdStatus a)

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
  [ testCase "automode call with V.8bis -> V.22bis 2400 bit/s, roles reversed, text both ways" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate Nothing) (defaultModemConfig 8000 Answer Nothing) 30 textO textA 18
      -- the station that received MS (the caller) becomes the answering modem on the high channel
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link HighChannel LowChannel R2400) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V22 (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "automode call without V.8bis -> V.22bis at 2400 bit/s" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (noV8 (defaultModemConfig 8000 Originate Nothing)) (noV8 (defaultModemConfig 8000 Answer Nothing)) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V22 (V22Link HighChannel LowChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.8bis answerer, caller without V.8bis -> classic start-up" $ do
      let (rxO, rxA, evO, _) = modemDuplex (noV8 (defaultModemConfig 8000 Originate Nothing)) (defaultModemConfig 8000 Answer Nothing) 30 textO textA 20
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22-only caller (no S1) -> 1200 bit/s" $ do
      let noS1 = (defaultModemConfig 8000 Originate (Just V22)) { mcHandshake = ((defaultHsConfig Originate) { hcStandard = Just V22, hcAllow2400 = False }) }
          (rxO, rxA, evO, _) = modemDuplex noS1 (defaultModemConfig 8000 Answer Nothing) 30 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link _ _ R1200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "originate fixed V.21, answer automode -> V.21" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate (Just V21)) (defaultModemConfig 8000 Answer Nothing) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V21 _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 fixed both sides, 20 dB -> 2400 bit/s" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate (Just V22)) (defaultModemConfig 8000 Answer (Just V22)) 20 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link _ _ R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 no handshake, 15 dB" $ do
      let cfg r = (defaultModemConfig 8000 r (Just V22)) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 15 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  , testCase "Bell 103 fixed, no handshake, 20 dB" $ do
      let cfg r = (defaultModemConfig 8000 r (Just Bell103)) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 20 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  ]
  where
    noV8 c = c { mcHandshake = (mcHandshake c) { hcV8bis = False } }
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
  ]
  where
    bits = [ odd ((i * 7919 + 13) `div` 3 + i `div` 7) | i <- [1 .. 3000 :: Int] ]

hdlcTests :: TestTree
hdlcTests = testGroup "HDLC and V.8bis messages"
  [ testCase "FCS residual after a good frame" $ do
      let info = [0x22, 0x80, 0x80, 0x80, 0x81, 0x09, 0x00, 0xCE] :: [Word8]
          bits = concatMap (\o -> [ testBit o i | i <- [0 .. 7] ]) info
          f = fcs16 bits
          fcsBits = [ testBit f i | i <- [15, 14 .. 0] ]
      -- fcs16 complements the register, so a good frame leaves the complemented residual
      assertEqual "residual" (fcsResidual `xor` 0xFFFF) (fcs16 (bits ++ fcsBits))
  , testCase "CRC-16/X-25 check value" $ do
      -- octets of "123456789" fed bit 1 first give the well-known X.25 FCS 0x906E
      -- when the transmitted bits are read back as two octets, low octet first, bit 1 first
      let bits = concatMap (\o -> [ testBit o i | i <- [0 .. 7] ]) (map (fromIntegral . fromEnum) "123456789" :: [Word8])
          f = fcs16 bits
          fcsBits = [ testBit f i | i <- [15, 14 .. 0] ]
          lowFirst = bitsToWord (take 8 fcsBits) + 256 * bitsToWord (drop 8 fcsBits)
      assertEqual "0x906E" (0x906E :: Int) lowFirst
  , testCase "frame round trip with stuffing" $ do
      let info = [0x22, 0xFF, 0xFF, 0x7E, 0x80, 0x81, 0x09, 0x00, 0xCE, 0x00] :: [Word8]
          line = replicate 30 True ++ hdlcFrameBits 3 2 info ++ replicate 20 True ++ hdlcFrameBits 2 1 [0x24] ++ replicate 10 True
          (_, frames) = hdlcRxBits hdlcRxInit line
      assertEqual "frames" [info, [0x24]] frames
  , testCase "corrupted frame is dropped" $ do
      let line = hdlcFrameBits 2 1 [1, 2, 3, 4]
          bad = take 20 line ++ [not (line !! 20)] ++ drop 21 line
          (_, frames) = hdlcRxBits hdlcRxInit (replicate 8 True ++ bad ++ replicate 8 True)
      assertEqual "frames" [] frames
  , testCase "V.8bis CL, MS and ACK encode/decode" $ do
      assertEqual "CL" (CL [ModeV21, ModeV22, ModeV22bis]) (decodeMessage (encodeMessage (CL [ModeV21, ModeV22, ModeV22bis])))
      assertEqual "MS" (MS ModeV22bis) (decodeMessage (encodeMessage (MS ModeV22bis)))
      assertEqual "ACK(1)" (Ack 1) (decodeMessage (encodeMessage (Ack 1)))
      assertEqual "NAK(3)" (Nak 3) (decodeMessage (encodeMessage (Nak 3)))
      assertEqual "CL octets" [0x22, 0x80, 0x80, 0x80, 0x81, 0x09, 0x00, 0xCE] (encodeMessage (CL [ModeV21, ModeV22, ModeV22bis]))
  , testCase "V.8bis message over V.21 through the line" $ do
      let fs = 8000
          info = encodeMessage (CL [ModeV22bis, ModeV22])
          bits = replicate 30 True ++ hdlcFrameBits 3 2 info ++ replicate 30 True
          sig = applyChannel fs (telephoneChannel 20) (txFilter fs v21Channel2 (VS.map (* 0.5) (modulateBits fs v21Channel2 bits)))
          rxBits = concatStage (fskDiscriminator fs v21Channel2 defaultDemodParams >>> fskSyncBits fs v21Channel2 defaultDemodParams) [sig, flushSilence fs v21Channel2]
          (_, frames) = hdlcRxBits hdlcRxInit rxBits
      assertEqual "frames" [info] frames
  ]
  where
    bitsToWord bs = sum [ if b then 2 ^ i else 0 | (i, b) <- zip [0 .. 7 :: Int] bs ] :: Int

hayesTests :: TestTree
hayesTests = testGroup "Hayes AT interpreter"
  [ testCase "AT, ATE0, ATI, S0" $ do
      let (s1, b1, _, a1) = hayesInput 0 hayesInit (BC.pack "AT\r")
      assertEqual "OK" "AT\r\r\nOK\r\n" (BC.unpack b1)
      assertEqual "no actions" [] a1
      let (s2, b2, _, _) = hayesInput 0.1 s1 (BC.pack "ATE0\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b2)
      let (s3, b3, _, _) = hayesInput 0.2 s2 (BC.pack "ATS0=2\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b3)
      assertBool "auto-answer set" (hayesAutoAnswer s3)
      let (_, b4, _, _) = hayesInput 0.3 s3 (BC.pack "ATS0?\r")
      assertBool "002" ("002" `isInfixOf` BC.unpack b4)
      let (_, b5, _, _) = hayesInput 0.4 s3 (BC.pack "ATY\r")
      assertBool "ERROR" ("ERROR" `isInfixOf` BC.unpack b5)
  , testCase "ATDT dials, CONNECT goes online, data passes, +++ escapes, ATH hangs up" $ do
      let (s1, _, _, a1) = hayesInput 0 hayesInit (BC.pack "atdt 555-1234\r")
      assertEqual "dial" [ActDial "T555-1234"] a1
      let (s2, b2) = hayesEvent s1 (EvConnect 2400)
      assertBool "CONNECT 2400" ("CONNECT 2400" `isInfixOf` BC.unpack b2)
      assertBool "online" (hayesOnline s2)
      let (s3, _, fwd3, _) = hayesInput 1 s2 (BC.pack "hello")
      assertEqual "data forwarded" "hello" (BC.unpack fwd3)
      -- escape needs a second of silence before and after
      let (s4, _, fwd4, _) = hayesInput 2.5 s3 (BC.pack "+++")
      assertEqual "pluses withheld" "" (BC.unpack fwd4)
      let (s5, b5) = hayesTick 3.6 s4
      assertBool "OK after escape" ("OK" `isInfixOf` BC.unpack b5)
      assertBool "command mode" (not (hayesOnline s5))
      let (s6, _, _, a6) = hayesInput 4 s5 (BC.pack "ATO\r")
      assertEqual "online again" [ActOnline] a6
      assertBool "online" (hayesOnline s6)
      let (s7, _, fwd7, _) = hayesInput 5 s6 (BC.pack "+x")
      assertEqual "lone plus is data" "+x" (BC.unpack fwd7)
      let (s8, _, _, _) = hayesInput 7 s7 (BC.pack "+++")
          (s9, _) = hayesTick 8.1 s8
          (_, b10, _, a10) = hayesInput 8.2 s9 (BC.pack "ATH\r")
      assertEqual "hangup" [ActHangup] a10
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b10)
  , testCase "DTMF pairs and dial signal length" $ do
      assertEqual "5" (Just (770, 1336)) (dtmfPair '5')
      assertEqual "#" (Just (941, 1477)) (dtmfPair '#')
      assertEqual "length" (round (8000 * (3 * 0.16 + 1)) :: Int) (VS.length (dtmfDialSignal 8000 0.3 "1,23"))
  ]

baresipTests :: TestTree
baresipTests = testGroup "baresip control protocol and SIP line"
  [ testCase "netstrings" $ do
      assertEqual "encode" "5:hello," (BC.unpack (netstringEncode (BC.pack "hello")))
      let (msgs, rest) = netstringDecode (BC.pack "2:ab,3:cde,7:incompl")
      assertEqual "decoded" ["ab", "cde"] (map BC.unpack msgs)
      assertEqual "rest" "7:incompl" (BC.unpack rest)
  , testCase "JSON parse and encode" $ do
      let ev = BC.pack "{\"event\":true,\"class\":\"call\",\"type\":\"CALL_CLOSED\",\"param\":\"Connection reset by peer\",\"peeruri\":\"sip:bob@biloxi.com\",\"n\":-1.5}"
      assertEqual "event" (Just (BsEvent "call" "CALL_CLOSED" "Connection reset by peer" [("peeruri", "sip:bob@biloxi.com")])) (decodeBsMessage ev)
      assertEqual "response" (Just (BsResponse True "" "t1")) (decodeBsMessage (BC.pack "{\"response\":true,\"ok\":true,\"data\":\"\",\"token\":\"t1\"}"))
      assertEqual "command" "53:{\"command\":\"dial\",\"params\":\"sip:1@x.org\",\"token\":\"a\"}," (BC.unpack (commandJson "dial" "sip:1@x.org" "a"))
      assertEqual "roundtrip" (Just (JObj [("a", JStr "x\"y"), ("b", JArr [JNum 1, JBool False, JNull])])) (jsonParse (jsonEncode (JObj [("a", JStr "x\"y"), ("b", JArr [JNum 1, JBool False, JNull])])))
  , testCase "SIP line: dial, established, closed" $ do
      let l0 = sipLineInit "sip.example.org"
          (l1, a1) = sipLineHayes l0 (ActDial "T555-1234")
      assertEqual "dial" [SipCommand "dial" "sip:5551234@sip.example.org"] a1
      let (l2, a2) = sipLineEvent 1 l1 (BsEvent "call" "CALL_ESTABLISHED" "" [])
      assertEqual "start as caller" [SipStartModem Originate] a2
      assertEqual "in call" (Just Originate) (sipLineInCall l2)
      let (l3, a3) = sipLineEvent 2 l2 (BsEvent "call" "CALL_CLOSED" "Connection reset by peer" [])
      assertEqual "closed" [SipStopModem, SipToDte EvNoCarrier] a3
      assertEqual "idle" Nothing (sipLineInCall l3)
  , testCase "SIP line: incoming, ring repeats, answer, hang up" $ do
      let l0 = sipLineInit "sip.example.org"
          (l1, a1) = sipLineEvent 0 l0 (BsEvent "call" "CALL_INCOMING" "" [("peeruri", "sip:bbs@example.org")])
      assertEqual "ring" [SipToDte EvRing] a1
      assertEqual "no ring yet" [] (snd (sipLineTick 1 l1))
      assertEqual "ring again" [SipToDte EvRing] (snd (sipLineTick 2.1 l1))
      let (l2, a2) = sipLineHayes l1 ActAnswer
      assertEqual "accept" [SipCommand "accept" ""] a2
      let (l3, a3) = sipLineEvent 3 l2 (BsEvent "call" "CALL_ESTABLISHED" "" [])
      assertEqual "start as answerer" [SipStartModem Answer] a3
      let (_, a4) = sipLineHayes l3 ActHangup
      assertEqual "hangup" [SipStopModem, SipCommand "hangup" ""] a4
  , testCase "SIP line: full URI and no answer" $ do
      let (l1, a1) = sipLineHayes (sipLineInit "d") (ActDial "sip:bbs@example.org")
      assertEqual "uri passes" [SipCommand "dial" "sip:bbs@example.org"] a1
      let (_, a2) = sipLineEvent 5 l1 (BsEvent "call" "CALL_CLOSED" "Busy" [])
      assertEqual "no answer" [SipToDte EvNoAnswer] a2
  ]

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain (testGroup "modec" [wavTests, fx, chunkTests, propertyTests, errorRateTests, channelTests, detectTests, handshakeTests, modemTests, telnetTests, v22Tests, hdlcTests, hayesTests, baresipTests])
