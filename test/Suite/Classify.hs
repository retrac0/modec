-- | Saying what answered a call: the voice detector, the rules that
-- weigh one kind of evidence against another, and the recorded calls
-- those rules were settled on.
module Suite.Classify (speechTests, classifyTests, callFixtureTests) where

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (isInfixOf, isSuffixOf, sort)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (dropExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Modec.Channel
import Modec.Classify
import Modec.DSP
import Modec.FSK (modulateBits)
import Modec.Hdlc (hdlcFrameBits)
import Modec.Speech
import Modec.Standards (v21Channel2)
import Modec.V8 (ansamSignal)
import Modec.Wav
import Suite.Tones (cadence, silence, tones)

fs :: Double
fs = 8000

-- | Something with the shape of speech and none of its meaning: voiced
-- syllables of a few hundred milliseconds, their pitch wandering
-- between 110 and 220 Hz and their harmonics shaped by a moving
-- resonance, with short gaps between syllables and longer ones between
-- words.  Everything is drawn from a seeded generator, so the same seed
-- is the same utterance.
babble :: Int -> Double -> Signal
babble seed secs = VS.take n (VS.concat (go (mkRand seed) 0))
  where
    n = round (fs * secs)
    go r t
      | t >= secs = []
      | otherwise =
          let (u1, r1) = next r
              (u2, r2) = next r1
              (u3, r3) = next r2
              (u4, r4) = next r3
              dur = 0.12 + 0.18 * u1
              gap = if u2 < 0.2 then 0.25 + 0.3 * u3 else 0.04 + 0.08 * u3
              f0a = 110 + 110 * u4
              (u5, r5) = next r4
              f0b = 110 + 110 * u5
              (u6, r6) = next r5
              formant = 400 + 1800 * u6
          in syllable dur f0a f0b formant (seed + round (t * 1000)) : silence' gap : go r6 (t + dur + gap)
    silence' g = VS.replicate (round (fs * g)) 0
    syllable dur f0a f0b formant nseed =
      let m = round (fs * dur)
          env i = let x = fromIntegral i / fromIntegral m in sin (pi * x) ** 0.7
          f0 i = f0a + (f0b - f0a) * fromIntegral i / fromIntegral m
          -- the phase of harmonic k is k times the fundamental's
          phases = VS.scanl' (\p i -> p + 2 * pi * f0 i / fs) 0 (VS.enumFromN (0 :: Int) m)
          weight k fk = exp (negate (((fk - formant) / 500) ^ (2 :: Int))) + 0.3 / fromIntegral k
          voiced = VS.imap (\i ph ->
                     let fz = f0 i
                     in env i * sum [ weight k (fromIntegral k * fz) * sin (fromIntegral k * ph)
                                    | k <- [1 .. floor (3400 / fz) :: Int] ]) (VS.take m phases)
          breath = gaussianNoise nseed m 0.02
          raw = VS.zipWith (+) voiced (VS.imap (\i v -> v * env i) breath)
          peak = max 1e-9 (VS.maximum (VS.map abs raw))
      in VS.map (* (0.3 / peak)) raw

-- | A small linear congruential generator; the tests need repeatable
-- variety, not good randomness.
newtype Rand = Rand Integer

mkRand :: Int -> Rand
mkRand s = Rand (fromIntegral s * 6364136223846793005 + 1442695040888963407)

next :: Rand -> (Double, Rand)
next (Rand s) =
  let s' = (s * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (64 :: Int))
  in (fromIntegral (s' `div` 2048) / fromIntegral (2 ^ (53 :: Int) :: Integer), Rand s')

hasSpeech :: Signal -> Double
hasSpeech = speechSeconds . speechRuns fs defaultSpeechParams

speechTests :: TestTree
speechTests = testGroup "voice detection"
  [ testCase "babble is speech, at any seed and down to 10 dB SNR" $
      sequence_ [ assertBool ("seed " ++ show sd ++ " at " ++ show snr ++ " dB: " ++ show secs) (secs >= 6)
                | sd <- [1, 2, 3], snr <- [30, 20, 10 :: Double]
                , let x = babble sd 10
                      secs = hasSpeech (addNoise sd (rms x / fromDb snr) (applyChannel fs (telephoneChannel 60) x)) ]
  -- Each of these is one of the things a line carries that is not a
  -- voice, and each defeats one of the two tests on its own.
  , testCase "noise, tones, cadences and carriers are not" $ do
      let none what x = assertEqual what 0 (hasSpeech x)
      none "silence" (silence 10)
      none "steady noise" (gaussianNoise 4 80000 0.05)
      none "busy" (cadence [480, 620] 0.5 0.5 10)
      none "congestion" (cadence [480, 620] 0.25 0.25 20)
      none "ringing" (cadence [440, 480] 2.0 4.0 3)
      none "E.180 ringing" (cadence [425] 1.0 4.0 4)
      none "answer tone" (tones 0.25 5 [2100])
      none "V.21 channel 2" (modulateBits fs v21Channel2 (take 3000 (cycle [True, False, False, True, True])))
      none "V.32 AC" (tones 0.2 10 [600, 3000])
  -- A ring starting is one deep change of level that nothing tonal
  -- vetoes, which a single window cannot tell from a syllable.  Speech
  -- is a run of windows, and rings are six seconds apart.
  , testCase "a ring's onset is not a run of speech" $
      assertEqual "ringing with a noisy floor" 0
        (hasSpeech (addNoise 9 0.002 (cadence [440, 480] 2.0 4.0 5)))
  ]

-- | A T.30 answer as a fax machine that picked up would send it: CED,
-- a pause, then CSI and DIS on V.21 channel 2 after a second of flags.
faxAnswer :: String -> Signal
faxAnswer station = VS.concat
  [ silence 0.5, tones 0.25 3.0 [2100], silence 0.075
  , VS.map (* 0.25) (modulateBits fs v21Channel2 frames), silence 2 ]
  where
    csi = [0xFF, 0x03, 0x40] ++ reverse (map (fromIntegral . fromEnum) (take 20 (station ++ repeat ' ')))
    dis = [0xFF, 0x13, 0x80, 0x00, 0xEE, 0xF8] :: [Word8]
    frames = hdlcFrameBits 37 1 csi ++ hdlcFrameBits 0 3 dis

classOf :: Maybe Signal -> Signal -> CallClass
classOf tx rx = fst (classifyCall fs tx rx)

nameIs :: String -> String -> CallClass -> Assertion
nameIs what want got = assertEqual (what ++ ": " ++ describeClass got) want (className got)

classifyTests :: TestTree
classifyTests = testGroup "weighing the evidence"
  [ testCase "the network's refusals" $ do
      nameIs "busy" "busy" (classOf Nothing (cadence [480, 620] 0.5 0.5 6))
      nameIs "congestion" "congestion" (classOf Nothing (cadence [480, 620] 0.25 0.25 12))
      nameIs "SIT" "sit" (classOf Nothing (VS.concat [ silence 0.3, tones 0.25 0.274 [913.8]
                                                    , tones 0.25 0.274 [1370.6], tones 0.25 0.38 [1776.7], silence 1 ]))
  , testCase "ringing that nobody answers, and lines that say nothing" $ do
      nameIs "ringing" "no answer" (classOf Nothing (addNoise 2 0.0005 (cadence [440, 480] 2.0 4.0 4)))
      nameIs "zeros" "no audio" (classOf Nothing (silence 20))
      nameIs "a quiet line" "silence" (classOf Nothing (gaussianNoise 3 160000 0.0005))
  , testCase "a modem, by its answer tone, by ANSam, and by its carrier" $ do
      nameIs "answer tone" "modem" (classOf Nothing (VS.concat [cadence [440, 480] 2.0 4.0 2, tones 0.25 3.3 [2100]]))
      case classOf Nothing (VS.concat [silence 1, ansamSignal fs 0.25 3.3 True]) of
        Modem why -> assertBool why ("ANSam" `isInfixOf` why)
        c -> assertFailure ("ANSam: " ++ describeClass c)
      nameIs "Bell answer tone" "modem" (classOf Nothing (tones 0.25 3 [2225]))
      nameIs "V.32 AC, no answer tone" "modem" (classOf Nothing (VS.concat [silence 2, tones 0.15 6 [600, 3000]]))
  -- A fax is only a fax once T.30 says so: CED is the same 2100 Hz as a
  -- modem's answer tone.  The station identifier is sent backwards and
  -- has to come out forwards.
  , testCase "a fax, and the station it names" $
      case classOf Nothing (faxAnswer "+1 256 895 4786") of
        Fax why -> assertBool why ("+1 256 895 4786" `isInfixOf` why)
        c -> assertFailure ("fax: " ++ describeClass c)
  , testCase "a voice, and speech that ends in a refusal" $ do
      nameIs "babble" "voice" (classOf Nothing (VS.concat [cadence [440, 480] 2.0 4.0 2, babble 5 12]))
      nameIs "announcement, then congestion" "congestion"
        (classOf Nothing (VS.concat [babble 6 15, cadence [480, 620] 0.25 0.25 6]))
  -- Our own answer tone, coming back quieter than it went out, is not
  -- the far end answering.
  , testCase "our own echo is set aside" $ do
      let ours = VS.concat [silence 1, tones 0.25 3.3 [2100], silence 3]
          echo = VS.map (* 0.1) ours
      nameIs "without the transmit side" "modem" (classOf Nothing echo)
      assertBool "with it" (className (classOf (Just ours) echo) /= "modem")
  -- Noise is set against each signal's own level: babble spends a third
  -- of its time in the gaps between syllables, so a fixed noise floor
  -- would test it at a far worse ratio than the tones beside it.
  , testCase "through the telephone channel" $
      sequence_
        [ nameIs (what ++ " at " ++ show snr ++ " dB") want
            (classOf Nothing (addNoise 5 (rms x / fromDb snr) (applyChannel fs (telephoneChannel 60) x)))
        | (what, want, x) <- [ ("busy", "busy", cadence [480, 620] 0.5 0.5 6)
                             , ("answer tone", "modem", VS.concat [silence 1, tones 0.25 3.3 [2100]])
                             , ("babble", "voice", babble 7 12) ]
        , snr <- [30, 15 :: Double] ]
  ]

-- | Recorded calls, each a @NAME.wav@ with a @NAME.class@ beside it:
-- @class:@ is the class the call must get, and any @detail:@ lines must
-- appear in its description.  A @NAME-tx.wav@, if there is one, is what
-- we sent.
callFixtureTests :: IO TestTree
callFixtureTests = do
  let dir = "test/fixtures/calls"
  names <- sort . filter (".class" `isSuffixOf`) <$> listDirectory dir
  cases <- forM names $ \n -> do
    spec <- lines <$> readFile (dir </> n)
    let stem = dir </> dropExtension n
        field k = [ trim (drop (length k + 1) l) | l <- spec, (k ++ ":") == take (length k + 1) l ]
    return $ testCase (dropExtension n) $ do
      rx <- wavSamples <$> readWav (stem ++ ".wav")
      hasTx <- doesFileExist (stem ++ "-tx.wav")
      tx <- if hasTx then Just . wavSamples <$> readWav (stem ++ "-tx.wav") else return Nothing
      let (c, ev) = classifyCall fs tx rx
          said = describeClass c ++ "; " ++ describeEvidence ev
      case field "class" of
        [want] -> assertEqual said want (className c)
        _ -> assertFailure "a .class file needs exactly one class: line"
      sequence_ [ assertBool ("detail " ++ show d ++ " in " ++ said) (d `isInfixOf` said) | d <- field "detail" ]
  return (testGroup "recorded calls" cases)
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
