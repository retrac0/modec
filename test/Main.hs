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
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

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
import Modec.MnpFrame
import Modec.Mnp
import Modec.V8
import Modec.V8bis
import Modec.Hayes
import Modec.Dtmf
import Modec.Baresip
import Modec.Json
import Modec.Pipewire
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
  -- The prefilter has 2*round(fs/25)+1 taps, so its length grows with the
  -- sample rate and so does the number of samples to push through it:
  -- the front end costs O(fs^2), and 48 kHz is 35 times the work of
  -- 8 kHz.  These two cases exist to show the demodulator is
  -- rate-independent, which a handful of payloads settles; the payload
  -- space itself is covered at 8 kHz above, at a thirty-fifth of the
  -- price.  Left at the default this one test was 73% of the suite.
  , testProperty "bell103 answer 48 kHz clean" $ withMaxSuccess 8 $
      \(Payload bs) -> roundTrip 48000 bell103Answer 0 4 bs
  , testProperty "bell103 answer 11025 Hz clean" $ withMaxSuccess 25 $
      \(Payload bs) -> roundTrip 11025 bell103Answer 0 5 bs
  -- Amplitude 0.5 against sigma 0.2 of white noise at 8 kHz is about 5 dB
  -- SNR in the full band and an Eb/N0 of roughly 16 dB.  An ideal
  -- non-coherent FSK detector would have a bit error rate near 1e-9
  -- there; this receiver measures 2 to 3 dB worse than ideal, which is an
  -- ordinary implementation loss for one that also has to find the bit
  -- clock and track a threshold.  Measured over 12000 characters at this
  -- level it makes no errors, so the property holds -- but the margin is
  -- a few dB, not the nine orders of magnitude the ideal figure suggests,
  -- and tightening the noise a little will start to break it.
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
            (hs', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep cfg h fr Nothing noHsIn in (h', acc ++ [(hoTx o, hoStatus o)])) (sdHs s, []) frames
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
  -- the tone-only simulation cannot carry V.22, so these cases enable the
  -- FSK modes only; the answerer probes V.21 first, so V.21 wins
  [ call "FSK modes both ends -> V.21" fsk fsk V21
  , call "originate V.21 / answer auto" [V21] allStandards V21
  , call "originate Bell 103 / answer auto" [Bell103] allStandards Bell103
  , call "originate auto / answer Bell 103" allStandards [Bell103] Bell103
  , call "originate auto / answer V.21" allStandards [V21] V21
  , call "Bell 103 / Bell 103" [Bell103] [Bell103] Bell103
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
    fsk = [V21, Bell103]
    call name so sa expect = testCase name $ do
      let (o, a) = simulateCall (defaultHsConfig Originate) { hcModes = so, hcV8bis = False } (defaultHsConfig Answer) { hcModes = sa, hcV8bis = False } 30 20
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
modemDuplex cfgO cfgA snr textO textA maxT = modemDuplexCut cfgO cfgA snr textO textA maxT (maxT * 2)

-- | As 'modemDuplex', but the answerer's audio stops at @cutAt@, the way
-- a far end that hangs up stops.
modemDuplexCut :: ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8] -> Double -> Double -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexCut cfgO cfgA snr textO textA maxT cutAt = go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0) False False [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    go t so sa fromA fromO sentO sentA rxO rxA evO evA
      | t >= maxT = (reverse rxO, reverse rxA, reverse evO, reverse evA)
      | otherwise =
          let queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              heard = if t >= cutAt then VS.replicate (VS.length fromA) 0 else fromA
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t heard) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t fromO) queueA
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO (sentO || not (null queueO)) (sentA || not (null queueA))
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA) (reverse eO ++ evO) (reverse eA ++ evA)

-- | Like 'modemDuplex', but each side is offered @perBlock@ bytes of its
-- text on every block once connected, rather than the whole of it in one
-- burst.  A protocol layer needs a continuous stream to exercise its
-- window, and a burst larger than the window would simply sit in a buffer.
modemDuplexStream :: ModemConfig -> ModemConfig -> Double -> Int
                  -> [Word8] -> [Word8] -> Double
                  -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexStream cfgO cfgA snr perBlock textO textA maxT =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0)
     textO textA [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    go t so sa fromA fromO bufO bufA rxO rxA evO evA
      | t >= maxT = (reverse rxO, reverse rxA, reverse evO, reverse evA)
      | otherwise =
          let (queueO, bufO') = if modemConnected so then splitAt perBlock bufO else ([], bufO)
              (queueA, bufA') = if modemConnected sa then splitAt perBlock bufA else ([], bufA)
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t fromA) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t fromO) queueA
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO bufO' bufA'
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA)
                (reverse eO ++ evO) (reverse eA ++ evA)

mnpModemTests :: TestTree
mnpModemTests = testGroup "MNP over the data pump"
  [ testCase "exactly one end of a link starts the protocol" $ do
      -- V.8bis reverses the roles after negotiation, so this reads the
      -- established link rather than the configured role.  Two initiators
      -- or two responders would both be wrong.
      forM_ [ (V22Link LowChannel HighChannel R1200, V22Link HighChannel LowChannel R1200)
            , (V22Link HighChannel LowChannel R2400, V22Link LowChannel HighChannel R2400)
            , (FskLink v21Channel1 v21Channel2, FskLink v21Channel2 v21Channel1)
            , (FskLink bell103Originate bell103Answer, FskLink bell103Answer bell103Originate)
            ] $ \(a, b) ->
        assertBool ("roles for " ++ show (a, b))
          (mnpRoleOf a /= mnpRoleOf b)
  , testCase "only the V.22 links can carry synchronous framing" $ do
      assertBool "V.22" (linkSyncable (V22Link LowChannel HighChannel R1200))
      assertBool "FSK" (not (linkSyncable (FskLink v21Channel1 v21Channel2)))
      assertEqual "1200" 1200 (linkBitRate (V22Link LowChannel HighChannel R1200))
      assertEqual "2400" 2400 (linkBitRate (V22Link LowChannel HighChannel R2400))
      assertEqual "300" 300 (linkBitRate (FskLink v21Channel1 v21Channel2))
  , testCase "MNP corrects a link that is corrupting bytes" $ do
      -- the same channel and the same seeds, run twice: once bare, once
      -- with the protocol.  The bare run is the premise, so the test
      -- cannot quietly pass on a link that was clean all along.
      let payload = map (fromIntegral . (`mod` 251)) [1 .. 200 :: Int]
          mnp = Just ((defaultMnpConfig 1200 False) { mnClass = 2, mnN401 = 16, mnK = 4 })
          cfg m r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True, mcMnp = m }
          snr = 5
          (rawO, rawA, _, _) =
            modemDuplexStream (cfg Nothing Originate) (cfg Nothing Answer) snr 1 payload payload 12
          (mnpO, mnpA, evO, evA) =
            modemDuplexStream (cfg mnp Originate) (cfg mnp Answer) snr 1 payload payload 18
      assertBool ("the bare link must be damaging bytes: "
                  ++ show (byteErrorRate payload rawO, byteErrorRate payload rawA))
        (byteErrorRate payload rawO > 0.01 && byteErrorRate payload rawA > 0.01)
      assertBool ("originate saw the link come up: " ++ show evO) (any isMnpUp evO)
      assertBool ("answer saw the link come up: " ++ show evA) (any isMnpUp evA)
      assertEqual "answer to originate, corrected" payload mnpO
      assertEqual "originate to answer, corrected" payload mnpA
  , testCase "a full automode call carries the protocol through the role reversal" $ do
      let text = map (fromIntegral . fromEnum) "The quick brown fox, 0123456789\r\n"
          mnp = Just ((defaultMnpConfig 2400 False) { mnClass = 2, mnN401 = 16, mnK = 4 })
          cfg r = (defaultModemConfig 8000 r allStandards) { mcMnp = mnp }
          (rxO, rxA, evO, evA) = modemDuplexStream (cfg Originate) (cfg Answer) 30 1 text text 30
      assertBool ("originate events " ++ show evO) (any isMnpUp evO)
      assertBool ("answer events " ++ show evA) (any isMnpUp evA)
      assertEqual "answer to originate" text rxO
      assertEqual "originate to answer" text rxA
  , testCase "synchronous framing costs fewer line bits, and delivers sooner" $ do
      -- The encoding first, exactly: dropping the start and stop bits is
      -- worth a fifth on a large frame, and more on a small one, because
      -- HDLC's two flags and check sequence are cheaper than the four
      -- octets of lead-in and the four of trailer that mode 2 spends.
      let frame n = encodeFrame True (FrLT 7 (replicate n 65))
          bitsOctet n = length (mode2Encode (frame n)) * 10   -- ten bits to the character
          bitsSync n = length (mode3Encode (frame n))
      assertEqual "a 256-octet frame, mode 2" 2670 (bitsOctet 256)
      assertEqual "a 256-octet frame, mode 3" 2104 (bitsSync 256)
      assertBool "at least a fifth cheaper on a full frame"
        (fromIntegral (bitsSync 256) <= 0.8 * (fromIntegral (bitsOctet 256) :: Double))
      assertBool "cheaper still on a short one"
        (fromIntegral (bitsSync 16) <= 0.7 * (fromIntegral (bitsOctet 16) :: Double))
      -- and then on the line.  The terminal offers more than the link can
      -- carry, so the link is what limits the run rather than the source;
      -- at one byte a block it would be the source, and the two framings
      -- would finish together and prove nothing.
      let payload = map (fromIntegral . (`mod` 251)) [1 .. 400 :: Int]
          mnp sync = Just ((defaultMnpConfig 1200 sync) { mnClass = 4, mnN401 = 16, mnK = 4 })
          cfg m r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True, mcMnp = m }
          got sync = let (o, _, _, _) = modemDuplexStream
                           (cfg (mnp sync) Originate) (cfg (mnp sync) Answer) 20 20 payload payload 6
                     in o
          octet = got False
          sync' = got True
      assertEqual "bit framing has delivered all of it by six seconds" payload sync'
      assertBool ("octet framing is still going: " ++ show (length octet))
        (length octet < length payload)
      assertBool ("and is well behind: octet " ++ show (length octet)
                  ++ " against bit " ++ show (length sync'))
        (5 * length sync' >= 6 * length octet)
  , testCase "synchronous framing at 2400 bit/s uses the whole constellation" $ do
      -- Every other test here runs the pump at 1200 bit/s, where a symbol
      -- carries one dibit.  At 2400 it carries two, and a transmit mode
      -- left out of the list that fetches the second one sends half the
      -- bits on the 1200 bit/s points while the far end decodes four to
      -- the symbol.  That is invisible at 1200 and fatal at 2400.
      let payload = map (fromIntegral . (`mod` 251)) [1 .. 200 :: Int]
          mnp = Just ((defaultMnpConfig 2400 True) { mnClass = 4, mnN401 = 32, mnK = 4 })
          cfg r = (defaultModemConfig 8000 r [V22bis])
                    { mcNoHandshake = True, mcMnp = mnp }
          (rxO, rxA, evO, evA) = modemDuplexStream (cfg Originate) (cfg Answer) 20 4 payload payload 20
      assertBool ("originate came up: " ++ show (take 2 evO)) (any isMnpUp evO)
      assertBool ("answer came up: " ++ show (take 2 evA)) (any isMnpUp evA)
      assertEqual "answer to originate" payload rxO
      assertEqual "originate to answer" payload rxA
  , testCase "the framing switch survives a lost acknowledgement" $ do
      -- at this signal to noise ratio the frame that closes establishment
      -- is regularly lost.  The far end goes on repeating its link
      -- request, and the station that already switched has to notice and
      -- go back to the framing the far end can still read.
      let payload = map (fromIntegral . (`mod` 251)) [1 .. 120 :: Int]
          mnp = Just ((defaultMnpConfig 1200 True) { mnClass = 4, mnN401 = 16, mnK = 4, mnLrTries = 6 })
          cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True, mcMnp = mnp }
          (rxO, rxA, evO, _) = modemDuplexStream (cfg Originate) (cfg Answer) 5 1 payload payload 45
      assertBool ("came up: " ++ show (take 2 evO)) (any isMnpUp evO)
      assertEqual "answer to originate" payload rxO
      assertEqual "originate to answer" payload rxA
  , testCase "without the protocol the modem behaves exactly as before" $ do
      -- mcMnp defaults to Nothing, and then not one byte takes a
      -- different path through modemStep
      let text = map (fromIntegral . fromEnum) "plain bytes\r\n"
          cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplexStream (cfg Originate) (cfg Answer) 20 1 text text 8
      assertEqual "answer to originate" text rxO
      assertEqual "originate to answer" text rxA
  ]
  where
    isMnpUp e = case e of { EvMnp (MnpUp {}) -> True; _ -> False }

modemTests :: TestTree
modemTests = testGroup "full modem duplex"
  [ testCase "automode call with V.8bis -> V.22bis 2400 bit/s, roles reversed, text both ways" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate allStandards) (defaultModemConfig 8000 Answer allStandards) 30 textO textA 18
      -- the station that received MS (the caller) becomes the answering modem on the high channel
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link HighChannel LowChannel R2400) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V22bis (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "automode call without V.8bis -> V.22bis at 2400 bit/s" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (noV8 (defaultModemConfig 8000 Originate allStandards)) (noV8 (defaultModemConfig 8000 Answer allStandards)) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V22bis (V22Link HighChannel LowChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.8bis answerer, caller without V.8bis -> classic start-up" $ do
      let (rxO, rxA, evO, _) = modemDuplex (noV8 (defaultModemConfig 8000 Originate allStandards)) (defaultModemConfig 8000 Answer allStandards) 30 textO textA 20
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22-only caller (no S1) -> 1200 bit/s" $ do
      let noS1 = defaultModemConfig 8000 Originate [V22]   -- V.22 only: never offers S1
          (rxO, rxA, evO, _) = modemDuplex noS1 (defaultModemConfig 8000 Answer allStandards) 30 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link _ _ R1200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "originate fixed V.21, answer automode -> V.21" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V21]) (defaultModemConfig 8000 Answer allStandards) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V21 _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 fixed both sides, 20 dB -> 2400 bit/s" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V22bis, V22]) (defaultModemConfig 8000 Answer [V22bis, V22]) 20 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link _ _ R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 no handshake, 15 dB" $ do
      let cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 15 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  , testCase "a carrier that stops delivers no junk behind it" $ do
      -- A far end that hangs up mid-call used to cost about a hundred
      -- characters of noise: the receiver kept framing its own decisions
      -- while the carrier decayed, and the DTE had no way to tell those
      -- from the banner that came before them.  The receiver knows,
      -- though -- its decision error goes from about 0.01 to past 100 --
      -- so it now stops handing bytes over.
      let cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True }
          textA = map (fromIntegral . fromEnum) "BANNER\r\n"
          (rxO, _, _, _) = modemDuplexCut (cfg Originate) (cfg Answer) 30 [] textA 12 6
      assertEqual "the banner, and nothing after it" textA rxO
  , testCase "Bell 212A both ends -> 1200 bit/s DPSK, text both ways" $ do
      let bell = [Bell212A, Bell103]
          (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate bell) (defaultModemConfig 8000 Answer bell) 25 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected Bell212A (V22Link LowChannel HighChannel R1200) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected Bell212A (V22Link HighChannel LowChannel R1200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "Bell 103 caller, Bell 212A capable answerer -> Bell 103" $ do
      let (rxO, _, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [Bell103]) (defaultModemConfig 8000 Answer [Bell212A, Bell103]) 25 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected Bell103 _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
  , testCase "a Bell-only modem never sends the ITU answer tone or V.8bis" $ do
      let (_, a) = simulateCall (defaultHsConfig Originate) { hcModes = [Bell103] } (defaultHsConfig Answer) { hcModes = [Bell212A, Bell103] } 30 12
      assertBool ("answer trace " ++ show (sdTrace a)) (not (any (\(_, c) -> c == TxTone 2100 || isDual c) (sdTrace a)))
  , testCase "Bell 103 fixed, no handshake, 20 dB" $ do
      let cfg r = (defaultModemConfig 8000 r [Bell103]) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 20 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  ]
  where
    isDual c = case c of { TxDual {} -> True; _ -> False }
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

mnpFrameTests :: TestTree
mnpFrameTests = testGroup "MNP frame structure (V.42 Annex A)"
  [ testCase "CRC-16/ARC check value" $
      -- the published check value, and deliberately unlike the 0x906E of
      -- the X.25 FCS above: different polynomial, preset and bit order
      assertEqual "0xBB3D" 0xBB3D (crc16arc (map (fromIntegral . fromEnum) "123456789"))
  , testCase "the reference initiator LR (A.6.4.1)" $
      assertEqual "21 golden octets" lrOctets (encodeFrame False (FrLR defaultLr))
  , testCase "framing mode 2 round trip" $ do
      let bodies = [[0x01], [0x14, 0x01, 0x02], [0, 255, 128], replicate 64 0xAA]
      forM_ bodies $ \b ->
        assertEqual ("body " ++ show b) [Right b] (snd (mode2RxOctets mode2RxInit (mode2Encode b)))
  , testCase "a DLE in the payload is stuffed and recovered" $ do
      forM_ [[0x10], [0x10, 0x10], [1, 2, 0x10], [0x10, 3, 4]] $ \b ->
        assertEqual ("body " ++ show b) [Right b] (snd (mode2RxOctets mode2RxInit (mode2Encode b)))
  , testCase "a DLE ETX pair inside the body does not end the frame" $ do
      -- the closing flag is DLE ETX; the same pair as data must survive
      let b = [1, 0x10, 0x03, 2]
      -- the payload's lone DLE is doubled, so the DLE ETX it forms with
      -- the next octet cannot be read as the closing flag
      assertEqual "payload is stuffed" [0x01, 0x10, 0x10, 0x03, 0x02] (take 5 (drop 4 (mode2Encode b)))
      assertEqual "body" [Right b] (snd (mode2RxOctets mode2RxInit (mode2Encode b)))
  , testCase "an FCS octet equal to DLE is not unstuffed" $ do
      -- the check sequence follows the closing flag and is never stuffed,
      -- so a receiver must take two raw octets there.  Body [0xC0] has
      -- FCS 0x0110, whose low octet is exactly DLE.
      let dleFcs = [ [k] | k <- [0 .. 255], crc16arc [k, 0x03] `mod` 256 == 0x10 ]
      assertBool "a witness exists" (not (null dleFcs))
      forM_ dleFcs $ \b -> do
        let wire = mode2Encode b
        assertEqual ("wire for " ++ show b) 0x10 (wire !! (length wire - 2))
        assertEqual ("body " ++ show b) [Right b] (snd (mode2RxOctets mode2RxInit wire))
  , testCase "a corrupted frame is reported, not delivered" $ do
      let wire = mode2Encode [1, 2, 3, 4]
          bad = take 5 wire ++ [0xFF] ++ drop 6 wire
      assertEqual "bad FCS" [Left BadFcs] (snd (mode2RxOctets mode2RxInit bad))
  , testCase "a truncated frame resynchronises on the next one" $ do
      let good = mode2Encode [9, 9]
          wire = take 6 (mode2Encode [1, 2, 3, 4]) ++ good
          (_, out) = mode2RxOctets mode2RxInit wire
      assertBool ("out " ++ show out) (Right [9, 9] `elem` out)
  , testCase "octets outside a frame are kept, then cleared by a good frame" $ do
      let (st1, o1) = mode2RxOctets mode2RxInit (map (fromIntegral . fromEnum) "Welcome to")
      assertEqual "nothing framed" [] o1
      assertEqual "junk kept" "Welcome to" (map (toEnum . fromIntegral) (mode2Junk st1))
      let (st2, _) = mode2RxOctets st1 (mode2Encode [1])
      assertEqual "cleared by a frame" [] (mode2Junk st2)
  , testCase "chunk invariance: a frame split across reads still decodes" $ do
      let wire = mode2Encode [1, 0x10, 3]
          feed st [] = ([], st)
          feed st (c : cs) = let (st', o) = mode2RxOctets st c
                                 (rest, st'') = feed st' cs
                             in (o ++ rest, st'')
          chunks = map (: []) wire
      assertEqual "one octet at a time" [Right [1, 0x10, 3]] (fst (feed mode2RxInit chunks))
  , testCase "every frame type round trips, in both optimizations" $ do
      let frames = [ FrLR defaultLr
                   , FrLR defaultLr { lrFraming = 3, lrK = 4, lrN401 = 256, lrDpo = 3 }
                   , FrLD 4 Nothing
                   , FrLD 255 (Just 7)
                   , FrLT 1 [65, 66, 67]
                   , FrLT 255 [0]
                   , FrLA 0 8
                   , FrLA 255 0
                   , FrLN 3 2
                   , FrLNA 3
                   ]
      forM_ frames $ \f -> forM_ [False, True] $ \dpo ->
        assertEqual (show (f, dpo)) (Right f) (decodeFrame (encodeFrame dpo f))
  , testCase "LT and LA decode without knowing the negotiated optimization" $ do
      -- the length indication tells the two forms apart on sight, so the
      -- two ends can never fall out of step over the latch
      assertEqual "long LT" (Right (FrLT 5 [1, 2, 3])) (decodeFrame (encodeFrame False (FrLT 5 [1, 2, 3])))
      assertEqual "short LT" (Right (FrLT 5 [1, 2, 3])) (decodeFrame (encodeFrame True (FrLT 5 [1, 2, 3])))
      assertEqual "long LA" (Right (FrLA 4 8)) (decodeFrame (encodeFrame False (FrLA 4 8)))
      assertEqual "short LA" (Right (FrLA 4 8)) (decodeFrame (encodeFrame True (FrLA 4 8)))
      assertEqual "LT header 5 octets" 5 (length (encodeFrame False (FrLT 5 [1])) - 1)
      assertEqual "optimized LT header 3 octets" 3 (length (encodeFrame True (FrLT 5 [1])) - 1)
      assertEqual "LA 8 octets" 8 (length (encodeFrame False (FrLA 4 8)))
      assertEqual "optimized LA 4 octets" 4 (length (encodeFrame True (FrLA 4 8)))
  , testCase "an LT may not carry an empty information field" $
      assertBool "rejected" (either (const True) (const False) (decodeFrame (encodeFrame False (FrLT 1 []))))
  , testCase "an unknown frame type is kept whole rather than aborting the link" $
      assertEqual "kept" (Right (FrOther 9 [1, 2])) (decodeFrame [3, 9, 1, 2])
  , testCase "unrecognised LR parameters are preserved" $ do
      let lr = defaultLr { lrOther = [(9, [1, 2])] }
      assertEqual "round trip" (Right (FrLR lr)) (decodeFrame (encodeFrame False (FrLR lr)))
  , testProperty "the length indication counts the header from the type octet" $
      \n info dpo ->
        let f = FrLT n (if null info then [1] else take 200 info)
            o = encodeFrame dpo f
            hdr = if dpo then 2 else 4
        in fromIntegral (head o) == (hdr :: Int)
  , testProperty "any body survives framing mode 2" $ withMaxSuccess 200 $
      \body ->
        let b = take 300 body
        in not (null b) ==> snd (mode2RxOctets mode2RxInit (mode2Encode b)) == [Right b]
  , testCase "framing mode 3 carries the same frames through HDLC" $ do
      -- a payload with a flag and a run of six ones, so zero insertion is
      -- actually exercised
      let body = encodeFrame False (FrLT 7 [0x7E, 0xFF, 0x7E, 0x3F])
          line = replicate 16 True ++ mode3Encode body ++ replicate 16 True
          (_, frames) = hdlcRxBits hdlcRxInit line
      assertEqual "frames" [body] frames
      assertEqual "decoded" (Right (FrLT 7 [0x7E, 0xFF, 0x7E, 0x3F])) (decodeFrame body)
  , testCase "negotiation takes the smaller of each value" $ do
      let ours = defaultLr { lrFraming = 3, lrK = 8, lrN401 = 256, lrDpo = 3 }
          theirs = defaultLr { lrFraming = 2, lrK = 4, lrN401 = 64, lrDpo = 1 }
      case negotiateLr ours theirs of
        Left r -> assertFailure ("refused with reason " ++ show r)
        Right n -> do
          assertEqual "framing" 2 (lrFraming n)
          assertEqual "k" 4 (lrK n)
          assertEqual "N401" 64 (lrN401 n)
          assertEqual "optimization is the intersection" 1 (lrDpo n)
  , testCase "negotiation refuses an unknown protocol level" $
      assertEqual "reason 2" (Left 2) (negotiateLr defaultLr defaultLr { lrConst1 = 3 })
  , testCase "negotiation refuses impossible parameters" $ do
      assertEqual "framing 0" (Left 3) (negotiateLr defaultLr defaultLr { lrFraming = 0 })
      assertEqual "k 0" (Left 3) (negotiateLr defaultLr defaultLr { lrK = 0 })
  , testCase "a station that cannot go synchronous settles on mode 2" $ do
      -- no special rejection is needed: an FSK link offers mode 2 and the
      -- minimum does the rest
      let fsk = defaultLr { lrFraming = 2 }
          sync = defaultLr { lrFraming = 3 }
      assertEqual "settles at 2" (Right 2) (fmap lrFraming (negotiateLr fsk sync))
  ]

-- | Two protocol machines cross connected through a line the impairment
-- may damage, stepped in 20 ms ticks.  Each side is offered @perTick@
-- octets of its text per tick, so the window and the timers see a
-- continuous stream rather than one burst.  No audio: these run in
-- milliseconds, and octets can be dropped or duplicated on purpose.
mnpPair :: MnpConfig -> MnpConfig
        -> (Int -> MnpLineOut -> MnpLineOut)
        -> Int -> [Word8] -> [Word8] -> Int
        -> ([Word8], [Word8], [MnpEvent], [MnpEvent])
mnpPair cfgI cfgR damage perTick textI textR ticks =
  go 0 (mnpInit cfgI MnpInitiator) (mnpInit cfgR MnpResponder)
       (OutOctets []) (OutOctets []) textI textR [] [] [] []
  where
    dt = 0.02
    go i si sr toI toR bufI bufR rxI rxR evI evR
      | i >= ticks = (reverse rxI, reverse rxR, reverse evI, reverse evR)
      | otherwise =
          let (feedI, bufI') = splitAt perTick bufI
              (feedR, bufR') = splitAt perTick bufR
              (si', oi) = mnpStep cfgI si (MnpIn dt (asIn toI) feedI 0 maxBound)
              (sr', orr) = mnpStep cfgR sr (MnpIn dt (asIn toR) feedR 0 maxBound)
          in go (i + 1) si' sr' (damage i (moLine orr)) (damage i (moLine oi))
                bufI' bufR'
                (reverse (moDte oi) ++ rxI) (reverse (moDte orr) ++ rxR)
                (reverse (moEvents oi) ++ evI) (reverse (moEvents orr) ++ evR)
    asIn = lineIn

-- | The far end's output, seen as this end's input.
lineIn :: MnpLineOut -> MnpLineIn
lineIn (OutOctets os) = LineOctets os
lineIn (OutBits bs) = LineBits bs

-- | A line that passes everything through untouched.
clean :: Int -> MnpLineOut -> MnpLineOut
clean _ = id

-- | Lose every @n@th block that carries anything: a dropout long enough
-- to swallow whole frames, which is what go-back-N exists for.
dropEvery :: Int -> Int -> MnpLineOut -> MnpLineOut
dropEvery n i (OutOctets os)
  | not (null os) && i `mod` n == 0 = OutOctets []
dropEvery _ _ l = l

-- | Corrupt a burst of three octets in the middle of every @n@th block,
-- leaving the octet clock alone: a line hit, which the check sequence has
-- to catch.
--
-- Note that inverting a whole block instead would /not/ be a fair test.
-- CRC-16\/ARC is preset to zero and not complemented, so it is linear:
-- inverting every octet of a long stream is a constant offset that can
-- synthesise a frame boundary satisfying the check, which no line does.
burstEvery :: Int -> Int -> MnpLineOut -> MnpLineOut
burstEvery n i (OutOctets os)
  | not (null os) && i `mod` n == 0 =
      OutOctets [ if j >= mid && j < mid + 3 then o `xor` 0x5A else o
                | (j, o) <- zip [0 :: Int ..] os ]
  where mid = length os `div` 2
burstEvery _ _ l = l

mnpTests :: TestTree
mnpTests = testGroup "MNP protocol (V.42 Annex A)"
  [ testCase "the retransmission timer matches Table A.9" $ do
      let c1200 = defaultMnpConfig 1200 False
          c2400 = defaultMnpConfig 2400 False
      -- 1200 bit/s, k = 8, N401 = 64, no optimization: about six seconds
      assertBool "1200 near 6 s" (abs (mnpT401 c1200 FramingOctet False 8 64 - 6) < 1)
      assertBool "2400 near 4 s" (abs (mnpT401 c2400 FramingOctet False 8 64 - 4) < 1)
      -- bit framing carries eight bits per octet instead of ten, so it is
      -- strictly quicker for the same payload
      assertBool "bit framing is faster"
        (mnpT401 c1200 FramingBit False 8 64 < mnpT401 c1200 FramingOctet False 8 64)
  , testCase "a link comes up and carries text both ways" $ do
      let c = defaultMnpConfig 1200 False
          (rxI, rxR, evI, evR) = mnpPair c c clean 4 textO textA 200
      assertBool ("initiator events " ++ show evI) (any isUp evI)
      assertBool ("responder events " ++ show evR) (any isUp evR)
      assertEqual "responder to initiator" textA rxI
      assertEqual "initiator to responder" textO rxR
  , testCase "what is negotiated is the responder's answer" $ do
      let ci = (defaultMnpConfig 1200 False) { mnK = 8, mnN401 = 256 }
          cr = (defaultMnpConfig 1200 False) { mnK = 4, mnN401 = 64, mnClass = 2 }
          (_, _, evI, _) = mnpPair ci cr clean 4 textO textA 120
      case [ e | e@(MnpUp {}) <- evI ] of
        (MnpUp cls k n : _) -> do
          assertEqual "class falls to 2" 2 cls
          assertEqual "k is the smaller" 4 k
          assertEqual "N401 is the smaller" 64 n
        _ -> assertFailure ("no link: " ++ show evI)
  , testCase "dropouts that swallow whole frames lose nothing" $ do
      let c = (defaultMnpConfig 1200 False) { mnN401 = 32 }
          payload = map (fromIntegral . (`mod` 251)) [1 .. 400 :: Int]
          (rxI, rxR, _, _) = mnpPair c c (dropEvery 11) 6 payload payload 1500
      assertEqual "responder to initiator" payload rxI
      assertEqual "initiator to responder" payload rxR
  , testCase "line hits are caught and the frames come back correct" $ do
      let c = (defaultMnpConfig 1200 False) { mnN401 = 32 }
          payload = map (fromIntegral . (`mod` 251)) [1 .. 400 :: Int]
          (rxI, rxR, _, _) = mnpPair c c (burstEvery 11) 6 payload payload 1500
      assertEqual "responder to initiator" payload rxI
      assertEqual "initiator to responder" payload rxR
  , testCase "sequence numbers wrap through 255 without losing anything" $ do
      -- one octet per frame forces the sequence number all the way round
      let c = (defaultMnpConfig 1200 False) { mnN401 = 1, mnK = 4 }
          payload = map fromIntegral [1 .. 300 :: Int]
          (rxI, _, _, _) = mnpPair c c clean 4 [] payload 2000
      assertEqual "300 frames past the wrap" payload rxI
  , testCase "no protocol at the far end falls through without a disconnect" $ do
      -- the responder never answers, and never says anything either
      let c = defaultMnpConfig 1200 False
          st0 = mnpInit c MnpInitiator
          step (s, outs) _ =
            let (s', o) = mnpStep c s (MnpIn 0.02 (LineOctets []) [] 0 maxBound)
            in (s', outs ++ [o])
          -- long enough for every link request the configuration will
          -- send, plus a little: derived rather than hard coded, so
          -- changing how persistently it probes cannot quietly turn this
          -- into a test that passes for the wrong reason
          ticks = ceiling ((mnT401Lr c * fromIntegral (mnLrTries c) + 2) / 0.02) :: Int
          (stEnd, os) = foldl step (st0, []) [1 .. ticks]
          sent = concat [ o | OutOctets o <- map moLine os ]
      assertEqual "falls through" MnpTransparent (mnpPhase stEnd)
      assertBool "reported" (any (== MnpTransparentFallback) (concatMap moEvents os))
      -- link requests were tried, but no disconnect was ever transmitted
      assertBool "a link request was sent" (not (null sent))
      assertBool "no LD on the wire"
        (null [ () | Right b <- snd (mode2RxOctets mode2RxInit sent)
                   , Right (FrLD _ _) <- [decodeFrame b] ])
  , testCase "a far end that only sends data is detected at once" $ do
      let c = defaultMnpConfig 1200 False
          banner = map (fromIntegral . fromEnum) "\r\nWelcome to the board\r\n"
          st0 = mnpInit c MnpInitiator
          step (s, outs) k =
            let line = if k == (3 :: Int) then LineOctets banner else LineOctets []
                (s', o) = mnpStep c s (MnpIn 0.02 line [] 0 maxBound)
            in (s', outs ++ [o])
          (stEnd, os) = foldl step (st0, []) [1 .. 20]
      assertEqual "transparent" MnpTransparent (mnpPhase stEnd)
      assertEqual "the banner reaches the terminal" banner (concatMap moDte os)
  , testCase "an unrecognised protocol level is refused with reason 2" $ do
      let c = defaultMnpConfig 1200 False
          st0 = mnpInit c MnpResponder
          bad = mode2Encode (encodeFrame False (FrLR defaultLr { lrConst1 = 3 }))
          (st1, o1) = mnpStep c st0 (MnpIn 0.02 (LineOctets bad) [] 0 maxBound)
      assertEqual "closed" MnpClosed (mnpPhase st1)
      let sent = case moLine o1 of { OutOctets os -> os; _ -> [] }
      assertEqual "an LD with reason 2"
        [FrLD 2 Nothing]
        [ f | Right b <- snd (mode2RxOctets mode2RxInit sent), Right f <- [decodeFrame b] ]
  , testCase "an attention is acknowledged rather than stalling the link" $ do
      let c = defaultMnpConfig 1200 False
          -- bring a pair up, then inject an LN at the initiator
          (si, _) = bringUp c
          (_, o) = mnpStep c si (MnpIn 0.02 (LineOctets (mode2Encode (encodeFrame False (FrLN 3 2)))) [] 0 maxBound)
          sent = case moLine o of { OutOctets os -> os; _ -> [] }
      assertBool "an LNA went back"
        (FrLNA 3 `elem` [ f | Right b <- snd (mode2RxOctets mode2RxInit sent), Right f <- [decodeFrame b] ])
  , testCase "framing mode 3 is negotiated and carries the data" $ do
      let c = defaultMnpConfig 1200 True
          (rxI, rxR, evI, _) = mnpPair c c clean 4 textO textA 300
      case [ e | e@(MnpUp {}) <- evI ] of
        (MnpUp cls _ _ : _) -> assertEqual "class 4 over bit framing" 4 cls
        _ -> assertFailure ("no link: " ++ show evI)
      assertEqual "responder to initiator" textA rxI
      assertEqual "initiator to responder" textO rxR
  , testCase "noise is not mistaken for a far end with no protocol" $ do
      -- a far end that does speak the protocol, on a line bad enough that
      -- none of its frames survive, produces nothing but wreckage.  Giving
      -- up on it there would abandon error correction exactly where it is
      -- needed.  Only something that reads as characters counts.
      let c = defaultMnpConfig 1200 False
          junk = [ fromIntegral (i * 37 + 11) | i <- [0 .. 40 :: Int] ]
          st0 = mnpInit c MnpInitiator
          step (st, ph) _ =
            let (st', _) = mnpStep c st (MnpIn 0.02 (LineOctets junk) [] 0 maxBound)
            in (st', ph ++ [mnpPhase st'])
          (_, phases) = foldl step (st0, []) [1 .. 40 :: Int]
      assertBool ("stayed in establishment: " ++ show (take 3 phases))
        (all (== MnpEstablish) phases)
      assertBool "the text case still falls through"
        (looksLikeTextCase (map (fromIntegral . fromEnum) "\r\nWelcome to the board\r\n"))
  , testCase "the first repeat of the last frame draws no acknowledgement" $ do
      -- A.7.3.2.2.  Answering a duplicate is what turns one lost
      -- acknowledgement into an exchange that never settles.
      let c = (defaultMnpConfig 1200 False) { mnN401 = 16 }
          (si, _) = bringUp c
          (si1, out1, dte1) = stepFrames c si [FrLT 1 [65, 66]] [] maxBound
          (si2, out2, _) = stepFrames c si1 [FrLT 1 [65, 66]] [] maxBound
          (_, out3, _) = stepFrames c si2 [FrLT 1 [65, 66]] [] maxBound
      assertEqual "the frame is taken" [65, 66] dte1
      assertBool ("acknowledged " ++ show out1) (any isLa out1)
      assertBool ("the first repeat is ignored " ++ show out2) (not (any isLa out2))
      assertBool ("the second repeat is answered " ++ show out3) (any isLa out3)
  , testCase "an acknowledgement repeating N(R) forces a retransmission" $ do
      let c = (defaultMnpConfig 1200 False) { mnN401 = 4 }
          (si, _) = bringUp c
          -- give it something to send, and let it go out
          (si1, out1, _) = stepFrames c si [] [1, 2, 3, 4] maxBound
          -- N(R) = 0 acknowledges nothing, since sequence numbers start at 1
          (si2, out2, _) = stepFrames c si1 [FrLA 0 8] [] maxBound
          (_, out3, _) = stepFrames c si2 [FrLA 0 8] [] maxBound
      assertBool ("a frame went out " ++ show out1) (any isLt out1)
      assertBool ("the first acknowledgement is not a loss report " ++ show out2)
        (not (any isLt out2))
      assertBool ("the repeat retransmits " ++ show out3) (any isLt out3)
  , testCase "credit falls to zero and is restored without being asked" $ do
      -- the deadlock this guards against: with no credit the far end sends
      -- nothing, so nothing arrives to prompt the acknowledgement that
      -- would give the credit back
      let c = (defaultMnpConfig 1200 False) { mnN401 = 16, mnRxWindow = 2 }
          (si, _) = bringUp c
          -- the terminal takes nothing, so the receive buffer fills
          (si1, _, _) = stepFrames c si [FrLT 1 (replicate 16 65)] [] 0
          (si2, out2, _) = stepFrames c si1 [FrLT 2 (replicate 16 66)] [] 0
          credits = [ k | FrLA _ k <- out2 ]
          -- now the terminal drains, with nothing new arriving at all
          (_, out3, dte3) = stepFrames c si2 [] [] maxBound
      assertBool ("credit reaches zero " ++ show credits) (0 `elem` credits)
      assertEqual "the buffer drains to the terminal" 32 (length dte3)
      assertBool ("credit is offered again unprompted " ++ show out3)
        (any (\f -> case f of { FrLA _ k -> k > 0; _ -> False }) out3)
  , testCase "class 4 shortens its frames on a bad line and lets them grow back" $ do
      -- the retransmission timer is pinned so this measures the policy
      -- rather than how long the timer happens to be: with a 256-octet
      -- maximum at 1200 bit/s it is about nineteen seconds, longer than
      -- any sensible test would run
      let c ad = (defaultMnpConfig 1200 False)
                   { mnClass = 4, mnN401 = 256, mnK = 8, mnAdaptive = ad, mnT401 = Just 0.3 }
          payload = map (fromIntegral . (`mod` 251)) [1 .. 251 :: Int]
          step ad dmg (si, sr, toI, toR) k =
            let (si', oi) = mnpStep (c ad) si (MnpIn 0.02 (lineIn toI) (take 16 payload) 0 maxBound)
                (sr', orr) = mnpStep (c ad) sr (MnpIn 0.02 (lineIn toR) [] 0 maxBound)
            in (si', sr', dmg k (moLine orr), dmg k (moLine oi))
          run0 ad dmg n from = foldl (step ad dmg) from [1 .. n :: Int]
          start ad = (mnpInit (c ad) MnpInitiator, mnpInit (c ad) MnpResponder,
                      OutOctets [], OutOctets [])
          sizeOf (si, _, _, _) = mnpSendSize si
          phaseOf (si, _, _, _) = mnpPhase si
      -- bring the link up on a clean line first: half the blocks missing
      -- from the outset would stop it establishing at all, and then there
      -- would be nothing to measure
      let up ad = run0 ad clean 200 (start ad)
      assertEqual "clean line keeps the negotiated maximum" 256
        (sizeOf (run0 True clean 300 (up True)))
      -- a line losing most blocks does shorten them
      -- long enough to shorten the frames, short enough not to exhaust
      -- the retransmission limit and take the link down with it
      let lossy = run0 True (dropEvery 5) 200 (up True)
      assertBool ("a lossy line shortens the frames: " ++ show (sizeOf lossy))
        (sizeOf lossy < 256)
      assertEqual "and the link is still up to measure" MnpData (phaseOf lossy)
      -- and once it clears, the frames grow again rather than leaving the
      -- rest of the call paying for a burst of noise
      let recovered = run0 True clean 600 lossy
      assertBool ("and they grow back: " ++ show (sizeOf lossy) ++ " -> " ++ show (sizeOf recovered)
                  ++ " phase " ++ show (phaseOf recovered))
        (sizeOf recovered > sizeOf lossy)
      assertEqual "none of it happens when the behaviour is switched off" 256
        (sizeOf (run0 False (dropEvery 5) 200 (up False)))
  , testCase "the retransmission limit disconnects with reason 4" $ do
      -- nothing is ever acknowledged, so every attempt times out
      let c = (defaultMnpConfig 1200 False) { mnN401 = 4, mnT401 = Just 0.1 }
          (si, _) = bringUp c
          go 0 st acc = (st, acc)
          go n st acc =
            let (st', out, _) = stepFrames c st [] (if null acc then [1, 2, 3, 4] else []) maxBound
            in go (n - 1 :: Int) st' (acc ++ out)
          (stEnd, outs) = go 400 si []
      assertEqual "closed" MnpClosed (mnpPhase stEnd)
      assertBool ("a disconnect with reason 4 " ++ show (filter isLd outs))
        (FrLD 4 Nothing `elem` outs)
      -- N400 is 12, so the frame is sent once and retried twelve times
      assertBool ("retries bounded " ++ show (length (filter isLt outs)))
        (length (filter isLt outs) <= 14)
  ]
  where
    isUp e = case e of { MnpUp {} -> True; _ -> False }
    -- a banner really does still trigger the fall-through
    looksLikeTextCase banner =
      let c = defaultMnpConfig 1200 False
          st0 = mnpInit c MnpInitiator
          step (st, _) k =
            let line = if k == (3 :: Int) then LineOctets banner else LineOctets []
                (st', o) = mnpStep c st (MnpIn 0.02 line [] 0 maxBound)
            in (st', o)
          (stEnd, _) = foldl step (st0, undefined) [1 .. 20]
      in mnpPhase stEnd == MnpTransparent
    isLa f = case f of { FrLA {} -> True; _ -> False }
    isLt f = case f of { FrLT {} -> True; _ -> False }
    isLd f = case f of { FrLD {} -> True; _ -> False }
    -- one step with the given frames arriving, returning the frames that
    -- went back.  Frames are written in the long form and read back
    -- whatever form they come in, which is the point of the LI dispatch.
    stepFrames c st fs dte ready =
      let wire = concatMap (mode2Encode . encodeFrame False) fs
          (st', o) = mnpStep c st (MnpIn 0.02 (LineOctets wire) dte 0 ready)
          sent = case moLine o of { OutOctets os -> os; OutBits _ -> [] }
      in ( st'
         , [ f | Right b <- snd (mode2RxOctets mode2RxInit sent), Right f <- [decodeFrame b] ]
         , moDte o )
    textO = map (fromIntegral . fromEnum) "Hello from the caller, 0123456789 !\r\n"
    textA = map (fromIntegral . fromEnum) "Answerer here; all bytes: \255\0\128 end\r\n"
    -- run a clean pair far enough that both ends are in the data phase
    bringUp c = go (30 :: Int) (mnpInit c MnpInitiator) (mnpInit c MnpResponder)
                   (OutOctets []) (OutOctets [])
      where
        go 0 si sr _ _ = (si, sr)
        go n si sr toI toR =
          let (si', oi) = mnpStep c si (MnpIn 0.02 (lineIn toI) [] 0 maxBound)
              (sr', orr) = mnpStep c sr (MnpIn 0.02 (lineIn toR) [] 0 maxBound)
          in go (n - 1) si' sr' (moLine orr) (moLine oi)

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

pipewireTests :: TestTree
pipewireTests = testGroup "PipeWire device discovery"
  [ testCase "parse pw-dump Node output" $
      assertEqual "nodes" expected (parseNodes dump)
  , testCase "match by node id" $
      assertEqual "id 56" (Unique (nodes !! 1)) (matchNode "56" nodes)
  , testCase "match by exact node name and description" $ do
      assertEqual "name" (Unique (head nodes)) (matchNode "alsa_output.pci-0000_00_1f.3.analog-stereo" nodes)
      assertEqual "description" (Unique (nodes !! 2)) (matchNode "USB Audio" nodes)
  , testCase "match by case-insensitive substring" $
      assertEqual "usb" (Unique (nodes !! 2)) (matchNode "usb" nodes)
  , testCase "an ambiguous substring is refused, not guessed" $
      assertEqual "analog" (Ambiguous [head nodes, nodes !! 1]) (matchNode "ANALOG" nodes)
  , testCase "no match" $
      assertEqual "nothing" NoMatch (matchNode "hdmi" nodes)
  , testCase "unknown ids and malformed input do not throw" $ do
      assertEqual "id" NoMatch (matchNode "999" nodes)
      assertEqual "garbage" [] (parseNodes (BC.pack "not json"))
      assertEqual "empty" [] (parseNodes (BC.pack "[]"))
  , testCase "read the volume a session manager applied to a stream" $ do
      -- 0.421824 is 0.75 cubed: a mixer slider left at three quarters,
      -- which WirePlumber restores onto every stream sharing the
      -- application name.  On the transmit stream that is 7.5 dB the far
      -- end never gets back, so it has to be visible.
      assertEqual "gains"
        [ ("modec-tx", 0.421824, False), ("modec-rx", 1.0, False), ("muted-one", 1.0, True) ]
        (parseGains volDump)
      assertEqual "no volume control, no report" [] (parseGains (BC.pack "[]"))
  ]
  where
    volDump = BC.pack (concat
      [ "[ {\"id\": 70, \"info\": { \"props\": { \"node.name\": \"modec-tx\" },"
      , " \"params\": { \"Props\": [ { \"volume\": 1.0, \"mute\": false,"
      , " \"channelVolumes\": [0.421824] } ] } } },"
      , " {\"id\": 71, \"info\": { \"props\": { \"node.name\": \"modec-rx\" },"
      , " \"params\": { \"Props\": [ { \"mute\": false, \"channelVolumes\": [1.0, 1.0] } ] } } },"
      , " {\"id\": 72, \"info\": { \"props\": { \"node.name\": \"muted-one\" },"
      , " \"params\": { \"Props\": [ { \"mute\": true, \"channelVolumes\": [1.0] } ] } } },"
      , " {\"id\": 73, \"info\": { \"props\": { \"node.name\": \"no-props\" } } } ]" ])
    dump = BC.pack (concat
      [ "[ {\"id\": 52, \"type\": \"PipeWire:Interface:Node\", \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_output.pci-0000_00_1f.3.analog-stereo\","
      , " \"node.description\": \"Built-in Audio Analog Stereo\", \"media.class\": \"Audio/Sink\" } } },"
      , " {\"id\": 56, \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_input.pci-0000_00_1f.3.analog-stereo\","
      , " \"node.description\": \"Built-in Audio Analog Stereo\", \"media.class\": \"Audio/Source\" } } },"
      , " {\"id\": 61, \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_input.usb-Focusrite\", \"node.description\": \"USB Audio\","
      , " \"media.class\": \"Audio/Source\" } } },"
      , " {\"id\": 29, \"info\": { \"props\": { \"node.name\": \"Dummy-Driver\" } } } ]" ])
    expected =
      [ PwNode 52 "alsa_output.pci-0000_00_1f.3.analog-stereo" "Built-in Audio Analog Stereo" PwSink
      , PwNode 56 "alsa_input.pci-0000_00_1f.3.analog-stereo" "Built-in Audio Analog Stereo" PwSource
      , PwNode 61 "alsa_input.usb-Focusrite" "USB Audio" PwSource
      , PwNode 29 "Dummy-Driver" "" (PwOther "") ]
    nodes = take 3 expected

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain (testGroup "modec" [wavTests, fx, chunkTests, propertyTests, errorRateTests, channelTests, detectTests, handshakeTests, modemTests, telnetTests, v22Tests, hdlcTests, mnpFrameTests, mnpTests, mnpModemTests, hayesTests, baresipTests, pipewireTests, v8Tests])

v8Tests :: TestTree
v8Tests = testGroup "V.8 menus and ANSam"
  [ testCase "a CM carries the preamble, call function and modulation octets" $ do
      let bits = sequenceBits SeqCM ourMenu
      assertEqual "ten ONEs then the CM sync" (map (== '1') "11111111110000001111") (take 20 bits)
      -- four octets: call function and three modulation octets, each
      -- with a start and a stop bit
      assertEqual "length" (20 + 4 * 10) (length bits)
      assertEqual "every octet framed" (replicate 4 (False, True))
        [ (o !! 0, o !! 9) | i <- [0 .. 3], let o = take 10 (drop (20 + 10 * i) bits) ]
  , testCase "CM round-trips through the receiver" $ do
      -- a sequence is emitted when its framing stops holding, which on
      -- the line is the ten ONEs opening whatever comes next
      let (_, evs) = v8RxBits v8RxInit
            (sequenceBits SeqCM ourMenu ++ sequenceBits SeqCM ourMenu ++ replicate 10 True)
      case evs of
        [V8Sequence SeqCM a, V8Sequence SeqCM b] -> do
          assertEqual "call function" (Just CfData) (v8Call a)
          assertEqual "modulations" [MV22, MV21] (v8Mods a)
          assertEqual "three modulation octets" 3 (v8ModOctets a)
          assertEqual "both alike" a b
        _ -> assertFailure ("expected two CM sequences, got " ++ show (length evs))
  , testCase "a menu with every option set survives the round trip" $ do
      let full = emptyMenu { v8Call = Just CfFaxFromCaller
                           , v8Mods = [MV34Duplex, MV32, MV22, MV27ter, MV23Duplex, MV21]
                           , v8Lapm = True
                           , v8Pcm = Just (True, False, False)
                           , v8Access = Just (False, False, True) }
          (_, evs) = v8RxBits v8RxInit (sequenceBits SeqCM full ++ replicate 10 True)
      case evs of
        [V8Sequence _ m] -> do
          assertEqual "call function" (Just CfFaxFromCaller) (v8Call m)
          assertEqual "modulations" [MV34Duplex, MV32, MV22, MV27ter, MV23Duplex, MV21] (v8Mods m)
          assertEqual "LAPM" True (v8Lapm m)
          assertEqual "PCM" (Just (True, False, False)) (v8Pcm m)
          assertEqual "access" (Just (False, False, True)) (v8Access m)
        _ -> assertFailure ("expected one sequence, got " ++ show evs)
  , testCase "no HDLC flag can appear in a signal" $ do
      -- the fixed bits in 5.1 and 5.2 exist so that a T.30 receiver on
      -- the same V.21 channel never mistakes JM for a frame
      let menus = [ emptyMenu { v8Call = Just cf, v8Mods = ms }
                  | cf <- [minBound .. maxBound]
                  , ms <- [[], [MV21], [MV22, MV21], [minBound .. maxBound]] ]
          flag = map (== '1') "01111110"
          hasFlag bs = any (\i -> take 8 (drop i bs) == flag) [0 .. length bs - 8]
      assertEqual "flags" [] [ describeMenu m | m <- menus, hasFlag (sequenceBits SeqCM m) ]
  , testCase "CJ is three zero octets and is recognised" $ do
      assertEqual "30 bits" 30 (length cjBits)
      let (_, evs) = v8RxBits v8RxInit (sequenceBits SeqCM ourMenu ++ cjBits)
      assertBool "CJ seen" (V8CJ `elem` evs)
  , testCase "the lowest item number wins" $ do
      -- 7.4: of the modes in common, the one with the lowest item number
      let far = emptyMenu { v8Mods = [MV34Duplex, MV32, MV22, MV21] }
      assertEqual "V.22 beats V.21" (Just MV22) (commonModulation ourMenu far)
      assertEqual "only V.21 in common" (Just MV21)
        (commonModulation ourMenu far { v8Mods = [MV34Duplex, MV21] })
      assertEqual "nothing in common" Nothing
        (commonModulation ourMenu far { v8Mods = [MV34Duplex, MV32] })
      assertEqual "item order" [1, 3, 4, 12] (map modItem [MV34Duplex, MV32, MV22, MV21])
  , testCase "a JM saying nothing in common still matches the CM octet count" $ do
      -- 8.2.3: same number of modulation octets, all zeros
      let jm = emptyMenu { v8Call = Just CfData, v8ModOctets = 3 }
          (_, evs) = v8RxBits v8RxInit (sequenceBits SeqJM jm ++ replicate 10 True)
      case evs of
        [V8Sequence _ m] -> do
          assertEqual "no modulations" [] (v8Mods m)
          assertEqual "three octets all the same" 3 (v8ModOctets m)
        _ -> assertFailure "expected one sequence"
  , testCase "ANSam is told apart from a plain answer tone" $ do
      let fs = 8000 :: Double
          tone f = VS.generate (round (fs * 3)) (\i -> 0.2 * sin (2 * pi * f * fromIntegral i / fs))
          saw x = snd (ansamBlock (ansamInit fs) x)
      assertBool "ANSam" (saw (ansamSignal fs 0.2 3 False))
      assertBool "ANSam with phase reversals" (saw (ansamSignal fs 0.2 3 True))
      assertBool "plain ANS is not ANSam" (not (saw (tone 2100)))
      assertBool "the Bell answer tone is not ANSam" (not (saw (tone 2225)))
      assertBool "silence is not ANSam" (not (saw (VS.replicate (round (fs * 3)) 0)))
  , testCase "a modem carrier does not read as ANSam" $ do
      -- any fluctuating envelope has some 15 Hz in it, so the detector
      -- has to insist that 2100 Hz is actually what is on the line
      let fs = 8000 :: Double
          noisy = VS.generate (round (fs * 3)) (\i ->
            let t = fromIntegral i / fs
            in 0.2 * sin (2 * pi * 1200 * t) * (1 + 0.3 * sin (2 * pi * 15 * t)))
      assertBool "1200 Hz carrier" (not (snd (ansamBlock (ansamInit fs) noisy)))
  ]
  where
    -- what modec can offer: V.8 has no codepoint for the Bell modes
    ourMenu = emptyMenu { v8Call = Just CfData, v8Mods = [MV22, MV21] }
