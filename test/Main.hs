module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Control.Monad (forM_, replicateM)
import Data.List (isInfixOf, isSuffixOf, nub, sort)
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
import Modec.Scrambler (lfsr)
import qualified Modec.Scrambler as Scr
import Modec.Handshake
import Modec.DSP
import Modec.Metrics
import Modec.Modem
import Modec.Telnet
import Modec.Async
import Modec.V22
import Modec.V32
import Modec.QAM
import Modec.V32Pump
import Modec.V32Start
import Modec.Echo
import qualified Modec.V32 as V32
import Modec.Hdlc
import Modec.MnpFrame
import Modec.Mnp
import Modec.V8
import Modec.V8bis
import Modec.Hayes
import Modec.Dtmf
import Modec.Progress
import Modec.Baudot
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
            (hs', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep cfg h fr noHsIn in (h', acc ++ [(hoTx o, hoStatus o)])) (sdHs s, []) frames
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
  , call "V.23 duplex both ends" [V23] [V23] V23
  , testCase "V.23 gives each end the other's channel" $ do
      -- the asymmetry is the point: the caller sends 75 bit/s and
      -- receives 1200, and a link that had it the other way round would
      -- still connect but never decode anything
      assertEqual "originate" (FskLink v23Backward v23Forward) (linkFor Originate V23)
      assertEqual "answer" (FskLink v23Forward v23Backward) (linkFor Answer V23)
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
      let (o, a) = simulateCall (withModes so (defaultHsConfig Originate)) { hcV8bis = False } (withModes sa (defaultHsConfig Answer)) { hcV8bis = False } 30 20
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

-- | As 'modemDuplex', but each side also hears its own transmit coming
-- back at it.  That is far-end echo: our signal reflected by the hybrid
-- at the other end of the line, which nothing at either end removes --
-- the far modem's own canceller subtracts its /own/ transmit from its
-- /own/ receiver, and our reflection happens outside that loop.  It is
-- the echo Modec.Echo exists for, and the one a V.32 call meets on any
-- real line, because both directions share the 1800 Hz carrier.
--
-- The taps are delays in samples and linear gains, fractional delays
-- included, because a hybrid returns a dispersive smear rather than one
-- clean reflection.
modemDuplexEcho :: [(Double, Double)] -> ModemConfig -> ModemConfig -> Double
                -> [Word8] -> [Word8] -> Double
                -> ([Word8], [Word8], [ModemEvent], [ModemEvent], Double, Double)
modemDuplexEcho taps cfgO cfgA snr textO textA maxT =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0)
     (replicate hist quiet) (replicate hist quiet) False False [] [] [] [] 0 0
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    quiet = VS.replicate blk 0
    -- enough transmit history behind us to cover the longest tap
    hist = 4 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    -- the echo of our own earlier transmits arriving during this block;
    -- index n0 is the first sample of the block being received now, so
    -- every tap reaches back into the history
    echoNow h =
      let ext = VS.concat h
          n0 = VS.length ext
      in VS.generate blk $ \i ->
           sum [ g * sampleAt ext (fromIntegral (n0 + i) - d) | (d, g) <- taps ]
    go t so sa fromA fromO hO hA sentO sentA rxO rxA evO evA erO erA
      | t >= maxT = (reverse rxO, reverse rxA, reverse evO, reverse evA, erO, erA)
      | otherwise =
          let queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              heardO = VS.zipWith (+) fromA (echoNow hO)
              heardA = VS.zipWith (+) fromO (echoNow hA)
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t heardO) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t heardA) queueA
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO
                (drop 1 hO ++ [audioO]) (drop 1 hA ++ [audioA])
                (sentO || not (null queueO)) (sentA || not (null queueA))
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA) (reverse eO ++ evO) (reverse eA ++ evA)
                (maybe erO (max erO) (modemEchoErle so')) (maybe erA (max erA) (modemEchoErle sa'))

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
  [ testCase "a V.32bis call: Figure 4, then 9600 bit/s trellis coded, text both ways" $ do
      -- Two whole modems this time, not just the start-up machine: the
      -- V.32 exchange, the rate signals settling on the best rate both
      -- ends offer, the handover to the data pump, and the echo canceller
      -- in the path.
      let cfg r = defaultModemConfig 8000 r [V32]
          (rxO, rxA, evO, evA) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32 (V32Link Originate V32R9600T) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA)
        (case evA of (EvConnected V32 (V32Link Answer V32R9600T) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "the echo canceller earns its place on a V.32 call" $ do
      -- The same dispersive path Modec.Echo's own tests use.  What this
      -- pins is that the canceller in a live call actually removes
      -- something: it was wired in, and switched off, for as long as it
      -- has existed -- the adapt flag hardcoded False, the reference
      -- never fed once the call reached data, and the bulk delay the
      -- start-up measures read from the config rather than the state.
      -- Every one of those failed silently, and the tell is that the
      -- return loss sat at exactly 0 dB.
      --
      -- It does not pin that a call survives echo.  It does not: see the
      -- next case.
      let cfg r = defaultModemConfig 8000 r [V32]
          path = [(200, 0.20), (203.5, 0.10), (209.2, 0.04)]
          (_, _, evO, evA, erleO, erleA) =
            modemDuplexEcho path (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32 _ : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA)
        (case evA of (EvConnected V32 _ : _) -> True; _ -> False)
      assertBool ("calling side return loss " ++ show erleO ++ " dB") (erleO > 12)
      assertBool ("answering side return loss " ++ show erleA ++ " dB") (erleA > 8)

  , testCase "text survives a hybrid from -26 dB to -10 dB" $ do
      -- How loud a reflection a 9600 bit/s call carries.  The first tap
      -- is the hybrid's own return and the other two its dispersion; the
      -- gains run from a quiet ATA to a badly matched one.
      --
      -- The return loss the canceller reports rises with the echo rather
      -- than staying flat, which is what it should do: it is measured
      -- against everything that arrived, so a quiet echo leaves little to
      -- take out and reads as a small number even when it is taking all
      -- of it out.
      let cfg r = defaultModemConfig 8000 r [V32]
      forM_ [(0.05, 5), (0.10, 9), (0.20, 14), (0.30, 17)] $ \(g, wantErle) -> do
        let path = [(200, g), (203.5, g / 2), (209.2, g / 5)]
            (rxO, rxA, _, _, erleO, erleA) =
              modemDuplexEcho path (cfg Originate) (cfg Answer) 30 textO textA 45
        assertEqual ("echo " ++ show g ++ ": answer to originate") textA rxO
        assertEqual ("echo " ++ show g ++ ": originate to answer") textO rxA
        assertBool ("echo " ++ show g ++ ": return loss " ++ show (erleO, erleA))
          (erleO > wantErle && erleA > wantErle)

  , testCase "a V.32bis call held down to 7200 bit/s, text both ways" $ do
      -- 7200 is V.32bis's addition below 9600, for a line that will not
      -- carry 9600: the rate signal names it in a bit V.32 had reserved,
      -- so a V.32 modem on the other end would simply not see it.
      let cfg role = (defaultModemConfig 8000 role [V32]) { mcV32Rates = chosen V32R7200 }
          (rxO, rxA, evO, _) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32 (V32Link Originate V32R7200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      -- The other direction arrives, but behind a dozen bytes of rubbish
      -- the framer finds while the descrambler is still coming into step.
      -- That is why 7200 is not in the default offer: the rate works, the
      -- head of the session does not.
      assertBool ("text from originate to answer: " ++ show rxA) (textO `isSuffixOf` rxA)
  , testCase "automode call with V.8bis -> V.22bis 2400 bit/s, roles reversed, text both ways" $ do
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
  , testCase "V.23 duplex fixed both sides -> 1200 down, 75 up" $ do
      -- the caller's line is 37 characters at 75 bit/s, five seconds of
      -- transmission on its own, so this call runs longer than the
      -- symmetric ones do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V23]) (defaultModemConfig 8000 Answer [V23]) 30 textO textA 20
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V23 (FskLink tx rx) : _) -> fskBaud tx == 75 && fskBaud rx == 1200; _ -> False)
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
  , testCase "a far end already sending data is not abandoned as silent" $ do
      -- This is what a real call did: our link request went unanswered
      -- (or its answer was lost), the far end opened its data phase
      -- anyway, and we timed out and fell through to passing bytes
      -- straight up -- which handed the terminal frame headers and check
      -- sequences as though they were characters.  Any well formed frame
      -- is proof there is a protocol over there.
      let c = defaultMnpConfig 1200 False
          lt n = mode2Encode (encodeFrame False (FrLT n (map (fromIntegral . fromEnum) "Synchronet ")))
          st0 = mnpInit c MnpInitiator
          step (st, evs) k =
            let line = if k `mod` (25 :: Int) == 0 then LineOctets (lt (fromIntegral (k `div` 25))) else LineOctets []
                (st', o) = mnpStep c st (MnpIn 0.02 line [] 0 maxBound)
            in (st', evs ++ moEvents o)
          ticks = ceiling ((mnT401Lr c * fromIntegral (mnLrTries c) + 3) / 0.02) :: Int
          (stEnd, evs) = foldl step (st0, []) [1 .. ticks]
      assertBool ("must not fall through: " ++ show (mnpPhase stEnd))
        (mnpPhase stEnd /= MnpTransparent)
      assertBool ("and must not report the far end silent: " ++ show evs)
        (notElem MnpTransparentFallback evs)
  , testCase "noise that happens to pass a check is not taken as evidence" $ do
      -- a sixteen-bit check passes on one candidate in sixty-five
      -- thousand, so an unknown frame type, or a link request naming a
      -- protocol level that does not exist, must not hold the link open
      let c = defaultMnpConfig 1200 False
          junkFrames = mode2Encode (encodeFrame False (FrOther 9 [1, 2, 3]))
                    ++ mode2Encode (encodeFrame False (FrLR defaultLr { lrConst1 = 7 }))
          st0 = mnpInit c MnpInitiator
          step (st, evs) k =
            let line = if k == (5 :: Int) then LineOctets junkFrames else LineOctets []
                (st', o) = mnpStep c st (MnpIn 0.02 line [] 0 maxBound)
            in (st', evs ++ moEvents o)
          ticks = ceiling ((mnT401Lr c * fromIntegral (mnLrTries c) + 3) / 0.02) :: Int
          (stEnd, _) = foldl step (st0, []) [1 .. ticks]
      assertBool ("should still fall through: " ++ show (mnpPhase stEnd))
        (mnpPhase stEnd == MnpTransparent || mnpPhase stEnd == MnpClosed)
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

-- | What real modems actually sent us, taken off recordings of calls and
-- kept as fixtures so the decoder is pinned against hardware rather than
-- against our own encoder.  No calls are placed to run these.
mnpFieldTests :: TestTree
mnpFieldTests = testGroup "MNP against recorded modems"
  [ testCase "A-Net Online, class 2: the whole exchange decodes" $ do
      octets <- B.unpack <$> B.readFile (fixtureDir </> "mnp" </> "anet-online-class2.octets")
      let (_, out) = mode2RxOctets mode2RxInit octets
          frames = [ f | Right b <- out, Right f <- [decodeFrame b] ]
          bad = length [ () | Left _ <- out ]
          payload = concat [ inf | FrLT _ inf <- frames ]
      assertEqual "frames recovered" 5 (length frames)
      -- one frame in this recording failed its check sequence: real line
      -- damage, caught rather than delivered
      assertEqual "frames the check sequence rejected" 1 bad
      case frames of
        (FrLR lr : _) -> do
          assertEqual "protocol level" 2 (lrConst1 lr)
          assertEqual "start-stop framing" 2 (lrFraming lr)
          assertEqual "outstanding frames" 8 (lrK lr)
          assertEqual "information field" 64 (lrN401 lr)
          assertEqual "no data phase optimization" 0 (lrDpo lr)
          -- and the part that matters for interworking: this modem does
          -- not send the constant the Recommendation prints.  Validating
          -- it strictly would have refused a link that works.
          assertEqual "constant parameter 2, as sent" [7, 1, 247, 0, 0, 1] (lrConst2 lr)
          assertBool "our class 2 offer negotiates against it"
            (case negotiateLr (offerLr ((defaultMnpConfig 2400 False) { mnClass = 2 })) lr of
               Right n -> lrFraming n == 2 && lrK n == 8 && lrN401 n == 64
               Left _ -> False)
        _ -> assertFailure ("first frame was not a link request: " ++ show (take 1 frames))
      assertBool ("an acknowledgement came back: " ++ show frames)
        (any (\f -> case f of { FrLA {} -> True; _ -> False }) frames)
      assertBool ("the banner arrived in information frames: " ++ show payload)
        ("Synchronet External PO" `isInfixOf` map (toEnum . fromIntegral) payload)
  , testCase "Basement BBS, class 4: synchronous framing decodes" $ do
      raw <- B.unpack <$> B.readFile (fixtureDir </> "mnp" </> "basement-bbs-class4.bits")
      let bits = [ testBit o i | o <- raw, i <- [0 .. 7 :: Int] ]
          -- the link request is start-stop framed; everything after the
          -- switch is bit oriented
          (_, octets) = asyncRxBits (asyncRxInit framing8N1) bits
          (_, out) = mode2RxOctets mode2RxInit octets
          lrs = [ lr | Right b <- out, Right (FrLR lr) <- [decodeFrame b] ]
          (_, bodies) = hdlcRxBits hdlcRxInit bits
          frames = [ f | b <- bodies, Right f <- [decodeFrame b] ]
          payload = concat [ inf | FrLT _ inf <- frames ]
          text = map (toEnum . fromIntegral) payload :: String
      case lrs of
        (lr : _) -> do
          assertEqual "bit-oriented framing offered" 3 (lrFraming lr)
          assertEqual "both optimization bits" 3 (lrDpo lr)
          assertEqual "seven outstanding frames" 7 (lrK lr)
          assertEqual "sixteen-octet information field" 16 (lrN401 lr)
        [] -> assertFailure "no link request in the start-stop part of the stream"
      assertBool ("frames after the switch: " ++ show (length frames)) (length frames >= 40)
      assertBool ("the banner reassembles: " ++ show (take 80 text))
        ("Synchronet External POTS Support v1.30" `isInfixOf` text)
      assertBool "and carries on past the banner" ("VT52 Colour" `isInfixOf` text)
  ]

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
          sig = v32Modulate 8000 Calling V32R9600 0.5 bits
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

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain (testGroup "modec" [wavTests, fx, dspTests, scramblerTests, stageTests, toneFrameTests, chunkTests, propertyTests, errorRateTests, channelTests, detectTests, handshakeTests, modemTests, telnetTests, v22Tests, v32Tests, v32PumpTests, v32SignalTests, v32StartTests, echoTests, hdlcTests, mnpFrameTests, mnpTests, mnpModemTests, mnpFieldTests, hayesTests, baresipTests, pipewireTests, v8Tests, ttyTests, dtmfTests, progressTests])

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

-- | The 5-bit text telephone: the character code, and the carrierless
-- line it runs on.
ttyTests :: TestTree
ttyTests = testGroup "text telephone (5-bit)"
  [ testCase "text survives the character code" $
      assertEqual "round trip" ttyText (ttyCodec ttyText)

  , testCase "lower case and unrepresentable characters are folded, not dropped" $
      assertEqual "folded" "HELLO, WORLD! $1 100/" (ttyCodec "hello, world! #1 100%")

  , testCase "LTRS opens the call and is re-sent every 72 characters" $ do
      let (_, codes) = baudotEncode baudotTxInit (replicate 80 (ascii 'A'))
      assertEqual "shift positions" [0, 73] [ i | (i, c) <- zip [0 :: Int ..] codes, c == ltrsCode ]

  , testCase "a space does not unshift: figures carry across it" $
      -- the RTTY convention would give "12 CD" here, and minimodem's
      -- tdd mode applies it by default; V.18 and TIA-825 do not
      assertEqual "figures held" "12 34" (ttyCodec "12 34")

  , testCase "the four positions where V.18 differs from US teleprinter code" $ do
      -- a decoder ported from an RTTY program gets exactly these wrong
      assertEqual "S has no figure" Nothing (baudotChar Figs 0x05)
      assertEqual "G is plus"  (Just '+') (baudotChar Figs 0x1A)
      assertEqual "H is equals" (Just '=') (baudotChar Figs 0x14)
      assertEqual "0 is backspace in both" (Just '\b', Just '\b')
        (baudotChar Ltrs 0x00, baudotChar Figs 0x00)

  , testCase "DEL from the keyboard puts the far end back into letters" $
      assertEqual "resync" [figsCode, 0x17, ltrsCode, figsCode, 0x17]
        (snd (baudotEncode baudotTxInit (map ascii "1\DEL1")))

  , testCase "a character is one start bit, five data bits and two stop bits" $
      -- V.18 asks for a minimum of 1.5; we send 2, which is what
      -- minimodem's tdd preset requires and what every receiver accepts
      assertEqual "bit times" 8
        (sum [ d | (_, d) <- frameKeyed tddFraming 1 [0] ])

  , testCase "text through the 45.45 baud line, 20 dB" $
      assertEqual "decoded" ttyText (ttyLine tdd45 20)
  , testCase "text through the 45.45 baud line, 4 dB" $
      assertEqual "decoded" ttyText (ttyLine tdd45 4)
  , testCase "text through the 50 baud line, 4 dB" $
      assertEqual "decoded" ttyText (ttyLine tdd50 4)

  , testCase "a burst that opens with the start bit itself still frames" $ do
      -- no lead-in mark at all, which is what the continuous-carrier
      -- framer cannot acquire: it wants half a bit of carried mark first
      let codes = snd (baudotEncode baudotTxInit (map ascii "GA SK"))
          sig = ttyAudio tdd45 (Burst 0 0.3) codes
      assertEqual "decoded" "GA SK" (ttyDecodeText tdd45 sig)

  , testCase "silence between bursts frames nothing" $ do
      -- one burst per character with 120 ms of noise between them: an
      -- absolute squelch alone turns that noise into characters
      let codes = snd (baudotEncode baudotTxInit (map ascii "GA SK"))
          sig = VS.concat [ VS.concat [ VS.replicate 960 0, ttyAudio tdd45 defaultBurst [c] ]
                          | c <- codes ]
      assertEqual "no junk" codes
        (ttyDecode tdd45 (applyChannel 8000 (telephoneChannel 20) sig))

  , testCase "45.45 and 50 baud are different modes, not a tolerance" $ do
      -- 10 % apart, and this family of receivers gives up around 3 %
      let sig = ttyAudio tdd50 defaultBurst (snd (baudotEncode baudotTxInit (map ascii ttyText)))
      assertBool "not interchangeable" (ttyDecodeText tdd45 sig /= ttyText)
  ]
  where
    ascii = fromIntegral . fromEnum
    ttyText = "HELLO GA THIS IS A TEST 1234567890 SK"
    ttyCodec t = map (toEnum . fromIntegral)
                     (snd (baudotDecode baudotRxInit (snd (baudotEncode baudotTxInit (map ascii t)))))
    ttyLine spec snr = ttyDecodeText spec
      (applyChannel 8000 (telephoneChannel snr)
        (ttyAudio spec defaultBurst (snd (baudotEncode baudotTxInit (map ascii ttyText)))))


-- | 5-bit codes to audio, keyed the way a text telephone keys the line.
ttyAudio :: FskSpec -> Burst -> [Word8] -> Signal
ttyAudio spec burst codes =
  txFilter 8000 spec (VS.map (* 0.5) (modulateKeyed 8000 spec keyed))
  where keyed = (Off, 0.2) : keyedBurst tddFraming (fskBaud spec) burst codes ++ [(Off, 0.3)]

ttyDecode :: FskSpec -> Signal -> [Word8]
ttyDecode spec x =
  concatStage (fskDiscriminator 8000 spec defaultDemodParams
               >>> fskBurstDeframer 8000 spec tddFraming defaultDemodParams defaultBurstParams)
              [x, flushSilence 8000 spec]

ttyDecodeText :: FskSpec -> Signal -> String
ttyDecodeText spec =
  map (toEnum . fromIntegral) . snd . baudotDecode baudotRxInit . ttyDecode spec

-- | Two tones of amplitude @amp@ each, for @sec@ seconds.
tones :: Double -> Double -> [Double] -> Signal
tones amp sec fss = VS.generate (round (8000 * sec)) $ \i ->
  let t = fromIntegral i / 8000 in amp * sum [ sin (2 * pi * f * t) | f <- fss ]

silence :: Double -> Signal
silence sec = VS.replicate (round (8000 * sec)) 0

-- | A cadence: @n@ bursts of @on@ seconds separated by @off@ seconds.
cadence :: [Double] -> Double -> Double -> Int -> Signal
cadence fss on off n = VS.concat (concat (replicate n [tones 0.25 on fss, silence off]))

dtmfTests :: TestTree
dtmfTests = testGroup "DTMF detection (Q.23, Q.24)"
  [ testCase "the keypad table agrees with the tone pairs" $
      sequence_ [ assertEqual [c] (Just (dtmfLowTones !! r, dtmfHighTones !! c'))
                    (dtmfPair c)
                | r <- [0 .. 3], c' <- [0 .. 3], let c = dtmfKey r c' ]
  , testCase "every keypad character survives its own dial signal" $
      assertEqual "digits" "0123456789*#ABCD"
        (dtmfDigits 8000 (dtmfDialSignal 8000 0.3 "0123456789*#ABCD"))
  -- Q.24 asks a receiver to accept a tone of 40 ms and reject one of
  -- 23 ms.  Where the tone falls against the receiver's block grid
  -- decides how many blocks it fills, so both have to hold at every
  -- offset, not at a convenient one: this is the test that block
  -- counting cannot pass and measuring the tone can.
  , testCase "40 ms is a digit at every alignment and 23 ms at none" $ do
      let at off n = VS.concat [ silence 0.3 VS.++ VS.replicate off 0
                               , tones 0.3 (fromIntegral n / 8000) [697, 1209]
                               , silence 0.3 ]
          heard off n = map ddChar (dtmfDecode 8000 defaultDtmfParams (at off n))
          block = round (8000 * dtBlockSec defaultDtmfParams) :: Int
      assertEqual "40 ms accepted" (replicate block "1") [ heard off 320 | off <- [0 .. block - 1] ]
      assertEqual "23 ms rejected" (replicate block "") [ heard off 184 | off <- [0 .. block - 1] ]
  , testCase "duration and start are measured inside the block grid" $ do
      let sig = VS.concat [silence 0.3, tones 0.3 0.1 [852, 1477], silence 0.3]
      case dtmfDecode 8000 defaultDtmfParams sig of
        [d] -> do
          assertEqual "digit" '9' (ddChar d)
          assertBool ("start " ++ show (ddStart d)) (abs (ddStart d - 0.3) < 0.005)
          assertBool ("duration " ++ show (ddDuration d)) (abs (ddDuration d - 0.1) < 0.007)
        ds -> assertFailure ("expected one digit, got " ++ show (map ddChar ds))
  -- The tests that keep speech out.  A single tone is not a pair; a
  -- harmonic stack has energy at a row and a column but spends most of
  -- its power elsewhere, which is what a vowel does; noise has no
  -- structure at all.
  , testCase "noise, a single tone and a harmonic stack are not digits" $ do
      let buzz = VS.generate 16000 $ \i ->
            let t = fromIntegral i / 8000
            in 0.25 * sum [ sin (2 * pi * f * t)
                          | f <- [110, 220, 330, 440, 550, 660, 770, 880, 990, 1100, 1210, 1320, 1430] ]
      assertEqual "single tone" "" (dtmfDigits 8000 (tones 0.4 1 [770]))
      assertEqual "harmonic stack" "" (dtmfDigits 8000 buzz)
      assertEqual "noise" "" (concat [ dtmfDigits 8000 (gaussianNoise sd 40000 0.15) | sd <- [1 .. 8] ])
  , testCase "through the telephone channel down to 3 dB SNR" $ do
      let sig = dtmfDialSignal 8000 0.3 "5551212"
          heard snr = dtmfDigits 8000
            (addNoise 11 (0.3 * 0.707 / fromDb snr) (applyChannel 8000 (telephoneChannel 40) sig))
      sequence_ [ assertEqual ("at " ++ show snr ++ " dB") "5551212" (heard snr)
                | snr <- [20, 12, 6, 3 :: Double] ]
  , testCase "the answer does not depend on how the audio is cut up" $ do
      let sig = dtmfDialSignal 8000 0.3 "0123456789"
          streamed n = concat (runStage (dtmfDetector 8000 defaultDtmfParams) (chunksOf n sig))
      sequence_ [ assertEqual ("chunks of " ++ show n) (dtmfDecode 8000 defaultDtmfParams sig) (streamed n)
                | n <- [37, 160, 1000] ]
  ]

progressTests :: TestTree
progressTests = testGroup "call progress tones"
  [ testCase "North American signals" $ do
      kindIs "busy" Busy (cadence [480, 620] 0.5 0.5 5)
      kindIs "congestion" Reorder (cadence [480, 620] 0.25 0.25 8)
      kindIs "ringing" Ringback (cadence [440, 480] 2.0 4.0 3)
      kindIs "dial tone" DialTone (tones 0.25 4 [350, 440])
  -- One 425 Hz tone is the dial tone, the busy tone, the congestion
  -- tone and the ringing tone of most of the world, and only the
  -- cadence separates them.  These four differ in nothing else.
  , testCase "one E.180 tone, four meanings" $ do
      kindIs "busy" Busy (cadence [425] 0.5 0.5 5)
      kindIs "congestion" Reorder (cadence [425] 0.25 0.25 8)
      kindIs "ringing" Ringback (cadence [425] 1.0 4.0 3)
      kindIs "dial tone" DialTone (tones 0.25 5 [425])
  , testCase "a double ring is ringing" $
      kindIs "UK ringing" Ringback
        (VS.concat (concat (replicate 3 [ tones 0.25 0.4 [400, 450], silence 0.2
                                        , tones 0.25 0.4 [400, 450], silence 2.0 ])))
  , testCase "a fax calling tone and an answer tone" $ do
      kindIs "CNG" FaxCalling (cadence [1100] 0.5 3.0 3)
      kindIs "answer tone" AnswerTone (VS.concat [silence 0.5, tones 0.25 3.3 [2100]])
  -- The three segments are reported as measured rather than named, so
  -- what is checked is that the frequencies come back right -- and they
  -- are 36 Hz apart, which a 60 ms window cannot resolve into separate
  -- bins but can still pick between, since the comparison is between
  -- candidates that are each exactly on a bin of their own.
  , testCase "special information tone segments are measured" $ do
      let sit fss = VS.concat ([silence 0.3] ++ [ tones 0.25 d f | (f, d) <- fss ] ++ [silence 0.5])
      case map peKind (callProgress 8000 (sit [([913.8], 0.274), ([1370.6], 0.274), ([1776.7], 0.38)])) of
        [Sit segs] -> do
          assertEqual "frequencies" [913.8, 1370.6, 1776.7] (map ssFreq segs)
          assertBool ("durations " ++ show (map ssDuration segs))
            (and [ abs (ssDuration s - d) < 0.05 | (s, d) <- zip segs [0.274, 0.274, 0.38] ])
        ks -> assertFailure ("North American SIT: " ++ show ks)
      case map peKind (callProgress 8000 (sit [([950], 0.33), ([1400], 0.33), ([1800], 0.33)])) of
        [Sit segs] -> assertEqual "frequencies" [950, 1400, 1800] (map ssFreq segs)
        ks -> assertFailure ("E.180 SIT: " ++ show ks)
  , testCase "a call that rings and then goes to busy reports both" $
      assertEqual "kinds" [Ringback, Busy]
        (map peKind (callProgress 8000 (VS.concat [ cadence [440, 480] 2.0 4.0 2
                                                  , cadence [480, 620] 0.5 0.5 5 ])))
  -- The one that matters, because a busy tone hangs the call up: over
  -- 221 recorded calls the only false positive was a stretch of speech
  -- that produced two bursts near 400 Hz with the spacing of
  -- congestion.  Nothing without structure may produce one.
  , testCase "silence, noise and a modem carrier are not progress tones" $ do
      assertEqual "silence" [] (map peKind (callProgress 8000 (silence 6)))
      assertEqual "noise" [] (map peKind (callProgress 8000 (gaussianNoise 5 48000 0.1)))
      sequence_ [ assertBool ("carrier " ++ fskName spec ++ ": " ++ show ks) (not (any refusal ks))
                | spec <- fskStandards
                , let ks = map peKind (callProgress 8000
                             (encodeBytes 8000 spec framing8N1 0.3 0.2 0.2
                                [ fromIntegral (i * 37) | i <- [1 .. 80 :: Int] ])) ]
  , testCase "busy survives the telephone channel and a quiet line" $ do
      let busy = cadence [480, 620] 0.5 0.5 6
      sequence_ [ assertEqual ("at " ++ show snr ++ " dB") [Busy]
                    (map peKind (callProgress 8000
                      (addNoise 3 (0.25 * 0.707 / fromDb snr) (applyChannel 8000 (telephoneChannel 20) busy))))
                | snr <- [20, 10, 6, 3 :: Double] ]
      assertEqual "30 dB below full scale" [Busy] (map peKind (callProgress 8000 (VS.map (* 0.03) busy)))
  , testCase "the answer does not depend on how the audio is cut up" $ do
      let sig = VS.concat [cadence [440, 480] 2.0 4.0 2, cadence [480, 620] 0.5 0.5 5]
          streamed n = concat (runStage (progressDetector 8000 defaultProgressParams) (chunksOf n sig))
      sequence_ [ assertEqual ("chunks of " ++ show n) (callProgress 8000 sig) (streamed n)
                | n <- [37, 160, 999] ]
  ]
  where
    kindIs what k sig = assertEqual what [k] (map peKind (callProgress 8000 sig))
    refusal k = case k of { Busy -> True; Reorder -> True; Sit _ -> True; _ -> False }
