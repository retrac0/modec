-- | The sample formats: bytes to signal and back.
--
-- The claim "Modec.Sample" makes is that decoding a code and encoding
-- it again gives the code back, for every code of every format.  That
-- is what lets a call be recorded as 16-bit PCM whatever the line
-- delivered it in, so it is asserted here code by code rather than
-- believed.  The rest measures what each format costs, in the same
-- units the channel simulator's G.711 tests use.
module Suite.Sample (sampleTests) where

import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit

import Modec.DSP (Signal, fromDb, rms)
import Modec.G711 (ulawDecode)
import Modec.Sample
import Modec.Wav

fs :: Double
fs = 8000

tone :: Double -> Double -> Int -> Signal
tone hz amp n = VS.generate n (\i -> amp * sin (2 * pi * hz * fromIntegral i / fs))

-- | Signal to everything-else ratio through a format, in dB.
throughDb :: SampleFormat -> Double -> Double -> Double
throughDb fmt lvl hz =
  let x = tone hz (fromDb lvl * 0.9) 40000
      y = decodeSamples fmt (encodeSamples fmt x)
  in 20 * logBase 10 (rms x / max 1e-30 (rms (VS.zipWith (-) y x)))

-- | Codes, decoded and encoded again.
roundTrip :: SampleFormat -> B.ByteString -> B.ByteString
roundTrip fmt = encodeSamples fmt . decodeSamples fmt

-- | A code as its little-endian bytes.
leBytes :: Int -> Int -> B.ByteString
leBytes n v = B.pack [ fromIntegral ((v `shiftR` (8 * i)) .&. 0xFF) | i <- [0 .. n - 1] ]

sampleTests :: TestTree
sampleTests = testGroup "sample formats"
  [ testCase "every code survives a decode and re-encode" $ do
      let bytes = B.pack [0 .. 255]
      forM_ [S8, U8, Alaw] $ \f -> assertEqual (formatName f) bytes (roundTrip f bytes)
      -- mu-law has two zeros and 0x7F is the one that does not come back
      let bad = [ c | (c, c') <- B.zip bytes (roundTrip Ulaw bytes), c /= c' ]
      assertEqual "ulaw" [0x7F] bad
      let all16 = B.concat [ leBytes 2 v | v <- [0 .. 65535] ]
      assertEqual "s16" all16 (roundTrip S16 all16)
      -- 14 bits left-justified: every code whose low two bits are clear
      let all14 = B.concat [ leBytes 2 ((v `shiftL` 2) .&. 0xFFFF) | v <- [-8192 .. 8191] ]
      assertEqual "pcm14" all14 (roundTrip Pcm14 all14)
      -- the wide ones: both ends, and two thousand codes across the range
      forM_ [(S24, 24), (S32, 32)] $ \(f, bits) -> do
        let half = 2 ^ (bits - 1) :: Int
            step = (2 * half) `div` 2000
            vs = [-half, -half + 1, -1, 0, 1, half - 2, half - 1] ++ take 2000 [-half, -half + step ..]
            codes = B.concat [ leBytes (bits `div` 8) (v .&. (2 * half - 1)) | v <- vs ]
        assertEqual (formatName f) codes (roundTrip f codes)
      let floats = VS.fromList [0, 0.5, -1, 1, 1 / 3, -0.999]
      assertEqual "f32" (VS.toList (VS.map (realToFrac . (realToFrac :: Double -> Float)) floats))
        (VS.toList (decodeSamples F32 (encodeSamples F32 floats)))

  , testCase "the scale is the one the module claims" $ do
      let one f bs = decodeSamples f bs VS.! 0
      assertEqual "u8 128 is zero" 0 (one U8 (B.pack [128]))
      assertEqual "s8 -128 is -1" (-1) (one S8 (B.pack [0x80]))
      -- left-justified, as measured off a CX93001: the top 14-bit code
      -- is 0x1FFF shifted up two, and the bottom one fills the word
      assertEqual "pcm14 8191 is one step under full scale" (8191 / 8192) (one Pcm14 (B.pack [0xFC, 0x7F]))
      assertEqual "pcm14 -8192 is -1" (-1) (one Pcm14 (B.pack [0x00, 0x80]))
      assertEqual "mu-law is G.711" (ulawDecode 0x9A) (one Ulaw (B.pack [0x9A]))
      assertEqual "an 8-bit code is a 16-bit code with a zero low byte"
        (B.pack [0x00, 0x60]) (encodeSamples S16 (decodeSamples S8 (B.pack [0x60])))
      assertEqual "full scale clamps to the top code" (B.pack [0xFF, 0x7F]) (encodeSamples S16 (VS.fromList [1.5]))
      assertEqual "and -1 is the bottom one" (B.pack [0x00, 0x80]) (encodeSamples S16 (VS.fromList [-1]))
      assertEqual "a partial sample is dropped" 1 (VS.length (decodeSamples S16 (B.pack [1, 2, 3])))

  , testCase "companding holds its ratio, linear pays a bit for six decibels" $ do
      let lvls = [0, -1 .. -40]
          band f = [ throughDb f l hz | l <- lvls, hz <- [1004, 2100] ]
          mus = band Ulaw; as = band Alaw
      assertBool ("mu-law band " ++ show (minimum mus, maximum mus)) (minimum mus > 31 && maximum mus < 40)
      assertBool ("A-law band " ++ show (minimum as, maximum as)) (minimum as > 31 && maximum as < 40)
      forM_ [0, -10, -20] $ \l -> do
        let d14 = throughDb Pcm14 l 1004 - throughDb U8 l 1004
            d16 = throughDb S16 l 1004 - throughDb Pcm14 l 1004
            d8 = throughDb S8 l 1004 - throughDb U8 l 1004
        assertBool ("14 bits over 8 at " ++ show l ++ " dBFS: " ++ show d14) (abs (d14 - 36) < 3)
        assertBool ("16 bits over 14 at " ++ show l ++ " dBFS: " ++ show d16) (abs (d16 - 12) < 3)
        assertBool ("s8 and u8 are one quantiser: " ++ show d8) (abs d8 < 0.5)

  , testCase "every name reads back" $
      forM_ [minBound .. maxBound] $ \f -> assertEqual (formatName f) (Just f) (formatNamed (formatName f))

  , testGroup "wav"
    [ testCase "each format writes a file that reads back as itself" $
        forM_ [U8, S16, S24, S32, F32, Ulaw, Alaw] $ \f -> do
          let x = tone 1004 0.7 1000
          case decodeWav (BL.toStrict (encodeWavMono f 8000 x)) of
            Left e -> assertFailure (formatName f ++ ": " ++ e)
            Right w -> do
              assertEqual (formatName f) f (wavFormat w)
              assertEqual "rate" 8000 (wavRate w)
              assertEqual "samples" (VS.toList (decodeSamples f (encodeSamples f x))) (VS.toList (wavSamples w))

    , testCase "s8 and 14-bit PCM have no tag and go out as 16-bit, exactly" $
        forM_ [S8, Pcm14] $ \f -> do
          let x = decodeSamples f (encodeSamples f (tone 1004 0.7 999))
          case decodeWav (BL.toStrict (encodeWavMono f 8000 x)) of
            Left e -> assertFailure e
            Right w -> do
              assertEqual "format" S16 (wavFormat w)
              assertEqual "samples" (VS.toList x) (VS.toList (wavSamples w))

    , testCase "an odd number of 8-bit samples gets its pad byte" $ do
        let bs = BL.toStrict (encodeWavMono U8 8000 (VS.replicate 5 0))
            le32At o = sum [ fromIntegral (B.index bs (o + i)) * 256 ^ i | i <- [0 .. 3] ] :: Int
        assertEqual "even file" 0 (B.length bs `mod` 2)
        assertEqual "RIFF length counts the pad" (B.length bs - 8) (le32At 4)
        either assertFailure (assertEqual "samples" 5 . VS.length . wavSamples) (decodeWav bs)

    , testCase "only the first channel of a stereo file is kept" $ do
        -- a mono header with the channel fields patched: chans at 22,
        -- byte rate at 28, block align at 32
        let left = [0.25, 0.5, -0.25]; right = [0.75, -0.75, 0.125]
            inter = VS.fromList (concat (zipWith (\l r -> [l, r]) left right))
            mono = BL.toStrict (encodeWavMono S16 8000 inter)
            stereo = B.take 22 mono <> leBytes 2 2 <> B.take 4 (B.drop 24 mono) <> leBytes 4 32000 <> leBytes 2 4 <> B.drop 34 mono
        case decodeWav stereo of
          Left e -> assertFailure e
          Right w -> do
            assertEqual "channels" 2 (wavChannels w)
            assertEqual "left" left (VS.toList (wavSamples w))
    ]
  ]
