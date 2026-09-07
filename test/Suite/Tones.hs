-- | Modems and signals that carry meaning in tones rather than bytes:
-- the text telephone, DTMF, and call progress.
module Suite.Tones (ttyAudio, ttyDecode, ttyDecodeText, tones, silence, cadence, ttyTests, dtmfTests, progressTests) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Channel
import Modec.DSP
import Modec.Dtmf
import Modec.Progress
import Modec.Baudot
import Modec.Stream
import Modec.FSK
import Modec.Standards

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
