-- | HDLC framing, and MNP classes 2 to 4 over it.
module Suite.Mnp (hdlcTests, mnpFrameTests, mnpPair, lineIn, clean, dropEvery, burstEvery, mnpTests, mnpFieldTests, mnpModemTests) where

import qualified Data.ByteString as B
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import System.FilePath ((</>))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Modec.Link
import Modec.Channel
import Modec.Handshake
import Modec.Metrics
import Modec.Modem
import Modec.Async
import Modec.V22
import Modec.Hdlc
import Modec.MnpFrame
import Modec.Mnp
import Data.Bits (complement, testBit, xor)
import Modec.Stream
import Modec.FSK
import Modec.Standards
import Harness
import Corpus

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
  , testCase "an HDLC frame over V.21 through the line" $ do
      let fs = 8000
          -- any octets will do; these are what a V.8bis capabilities
          -- list used to be, from back when this modem spoke one
          info = [0x22, 0x80, 0x80, 0x80, 0x81, 0x09, 0x00, 0xCE]
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
  , testCase "a far end talking another protocol is fallen through to, not disconnected" $ do
      -- What a LAPM modem looks like from here: bytes that are not
      -- characters and not frames either, over and over, and never a
      -- reply to the link request.  It used to earn an LD, which a V.42
      -- modem answers by hanging up.  Now it earns silence and a plain
      -- link, which is what its own fallback wanted.
      let c = defaultMnpConfig 1200 False
          -- a frame with its body corrupted: the framer sees the framing
          -- and a check that fails, which is 'bad' without being a frame
          damaged = let f = mode2Encode (encodeFrame False (FrLT 1 (map (fromIntegral . fromEnum) "XID SABME ")))
                        i = length f `div` 2
                    in take i f ++ [complement (f !! i)] ++ drop (i + 1) f
          st0 = mnpInit c MnpInitiator
          step (st, outs) k =
            let line = if k `mod` (25 :: Int) == 0 then LineOctets damaged else LineOctets []
                (st', o) = mnpStep c st (MnpIn 0.02 line [] 0 maxBound)
            in (st', outs ++ [o])
          ticks = ceiling ((mnT401Lr c * fromIntegral (mnLrTries c) + 3) / 0.02) :: Int
          (stEnd, os) = foldl step (st0, []) [1 .. ticks]
          sent = concat [ o | OutOctets o <- map moLine os ]
      assertEqual "falls through" MnpTransparent (mnpPhase stEnd)
      assertBool "reported as no error correction" (any (== MnpTransparentFallback) (concatMap moEvents os))
      assertBool "no LD on the wire"
        (null [ () | Right b <- snd (mode2RxOctets mode2RxInit sent)
                   , Right (FrLD _ _) <- [decodeFrame b] ])
  , testCase "a far end that sent a link request and then gave up is followed" $ do
      -- V.42 auto-reliable, as a CX93001 in \N3 does it: one link
      -- request, its own establishment timer, then the DTE's data in
      -- the clear.  The link request must not disable data detection
      -- for the rest of the call, or this end offers MNP to a far end
      -- that has stopped listening and then disconnects a working call.
      let c = defaultMnpConfig 2400 False
          lr = mode2Encode (encodeFrame False (FrLR defaultLr))
          text = map (fromIntegral . fromEnum) "B0000 pack my box with five dozen liquor jugs 9876543210\r\n"
          st0 = mnpInit c MnpResponder
          step (st, evs) k =
            let line | k == (5 :: Int) = LineOctets lr
                     -- the far end's own timer runs out, then it sends data
                     | k > round (mnT401Lr c / 0.02) + 5 = LineOctets text
                     | otherwise = LineOctets []
                (st', o) = mnpStep c st (MnpIn 0.02 line [] 0 maxBound)
            in (st', evs ++ moEvents o)
          ticks = ceiling ((mnT401Lr c * fromIntegral (mnLrTries c) + 3) / 0.02) :: Int
          (stEnd, evs) = foldl step (st0, []) [1 .. ticks]
      assertEqual "falls through to a plain link" MnpTransparent (mnpPhase stEnd)
      assertBool ("and does not report the link down: " ++ show evs)
        (null [ () | MnpDown _ <- evs ])
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
