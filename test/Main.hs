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

import Modec.DSP
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

-- | Levenshtein distance over byte strings, O(n*m) with two unboxed rows.
editDistance :: [Word8] -> [Word8] -> Int
editDistance as bs = VS.last (foldl step row0 as)
  where
    n = length bs
    bv = VS.fromList bs
    row0 = VS.generate (n + 1) fromIntegral :: VS.Vector Int
    step prev a = VS.constructN (n + 1) $ \cur ->
      let j = VS.length cur
      in if j == 0 then VS.head prev + 1
         else minimum [ VS.unsafeIndex prev j + 1
                      , VS.unsafeIndex cur (j - 1) + 1
                      , VS.unsafeIndex prev (j - 1) + (if VS.unsafeIndex bv (j - 1) == a then 0 else 1) ]

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

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain (testGroup "modec" [wavTests, fx, propertyTests, errorRateTests])
