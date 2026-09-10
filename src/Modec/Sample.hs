-- | Sample formats: what a stream of audio bytes means.
--
-- Everything above this module is 'Signal', doubles on [-1, 1], and
-- everything below it is bytes -- a sound card's s16le, a WAV file's
-- u8, a USB modem's mu-law.  This is the one place the two meet, so
-- the arithmetic is written once; it had been written three times for
-- s16 alone, and the copy in the executable was the one no test could
-- reach.
--
-- The rule for the integer formats: a code @v@ with @b@ significant
-- bits decodes to @v / 2^(b-1)@, and a sample @x@ encodes to
-- @round (x * 2^(b-1))@ clamped to the code range.  The same power of
-- two both ways means decoding a code and encoding it again gives the
-- code back, for every code of every format.  That is what makes
-- recording a call as 16-bit PCM lossless whatever the line delivered:
-- s16 is the identity, an 8-bit code is a 16-bit code with a zero low
-- byte, G.711 decodes into a 14-bit range that 16 bits holds exactly,
-- and 14-bit PCM arrives already left-justified in a 16-bit word.  The
-- old writer scaled by 32767, which is one step short of an identity:
-- -32768 came back as -32767.
--
-- 'Pcm14' is the odd one out, being nobody's sound card format.  It is
-- the "14 bit PCM" a Conexant-class USB modem offers in voice mode:
-- two bytes a sample, low byte first.  More resolution than G.711 can
-- carry, and the reason to prefer that path over a SIP trunk.
--
-- Which end of the word the 14 bits sit in was the one assumption here
-- no test could check, so it was measured instead.  A CX93001 held
-- off-hook on dial tone (350 + 440 Hz, 99.1 % of the energy in those
-- two bins) returns a little-endian word peaking at 17659 -- more than
-- twice the 8191 a right-justified value could reach.  The value is
-- therefore left-justified: the 14 bits sit in bits 15..2, and the
-- scale is 32768, not 8192.  Bit 0 is set in every sample the modem
-- sends and bit 1 in two of sixteen thousand, which is a word-framing
-- marker rather than signal -- so decoding the whole word by 32768 and
-- reading a 14-bit value by 8192 are the same arithmetic, and this
-- takes the first.  Going the other way there is no marker to imitate:
-- a sample is quantised to the 14 bits the modem actually has and
-- shifted up, so what is encoded is what it can represent.
module Modec.Sample
  ( SampleFormat (..)
  , bytesPerSample
  , decodeSamples
  , encodeSamples
    -- * Names
  , formatName
  , formatNamed
  , formatNames
  , describeFormat
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Int (Int8, Int16, Int32)
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)

import Modec.DSP (Signal)
import Modec.G711

data SampleFormat
  = S8      -- ^ signed 8-bit PCM
  | U8      -- ^ unsigned 8-bit PCM, 128 for zero: what an 8-bit WAV holds
  | S16     -- ^ signed 16-bit little-endian: sound cards, pipes, recordings
  | S24     -- ^ signed 24-bit little-endian
  | S32     -- ^ signed 32-bit little-endian
  | F32     -- ^ IEEE single precision little-endian, already on [-1, 1]
  | Ulaw    -- ^ G.711 mu-law
  | Alaw    -- ^ G.711 A-law
  | Pcm14   -- ^ 14-bit linear in a 16-bit little-endian word, left-justified
  deriving (Eq, Show, Enum, Bounded)

bytesPerSample :: SampleFormat -> Int
bytesPerSample f = case f of
  S8 -> 1; U8 -> 1; Ulaw -> 1; Alaw -> 1
  S16 -> 2; Pcm14 -> 2
  S24 -> 3
  S32 -> 4; F32 -> 4

le16 :: B.ByteString -> Int -> Int
le16 bs i = fromIntegral (B.index bs i) .|. (fromIntegral (B.index bs (i + 1)) `shiftL` 8)

le32 :: B.ByteString -> Int -> Int
le32 bs i = le16 bs i .|. (le16 bs (i + 2) `shiftL` 16)

-- | Bytes to samples.  A trailing partial sample is dropped.
decodeSamples :: SampleFormat -> B.ByteString -> Signal
decodeSamples fmt bs = VS.generate n (\i -> dec (i * bps))
  where
    bps = bytesPerSample fmt
    n = B.length bs `div` bps
    dec :: Int -> Double
    dec = case fmt of
      S8    -> \o -> fromIntegral (fromIntegral (B.index bs o) :: Int8) / 128
      U8    -> \o -> (fromIntegral (B.index bs o) - 128) / 128
      S16   -> \o -> fromIntegral (fromIntegral (le16 bs o) :: Int16) / 32768
      Pcm14 -> \o -> fromIntegral (fromIntegral (le16 bs o) :: Int16) / 32768
      S24   -> \o ->
        let v = le16 bs o .|. (fromIntegral (B.index bs (o + 2)) `shiftL` 16)
            s = if v >= 0x800000 then v - 0x1000000 else v
        in fromIntegral s / 8388608
      S32   -> \o -> fromIntegral (fromIntegral (le32 bs o) :: Int32) / 2147483648
      F32   -> \o -> realToFrac (castWord32ToFloat (fromIntegral (le32 bs o) :: Word32))
      Ulaw  -> \o -> ulawDecode (B.index bs o)
      Alaw  -> \o -> alawDecode (B.index bs o)

-- | Samples to bytes.  Anything outside [-1, 1] is clamped first.
encodeSamples :: SampleFormat -> Signal -> B.ByteString
encodeSamples fmt x =
  BL.toStrict (BB.toLazyByteString (VS.foldr (\v acc -> enc v <> acc) mempty x))
  where
    enc :: Double -> BB.Builder
    enc = case fmt of
      S8    -> BB.int8 . fromIntegral . quant 128
      U8    -> BB.word8 . fromIntegral . (+ 128) . quant 128
      S16   -> BB.int16LE . fromIntegral . quant 32768
      Pcm14 -> BB.int16LE . fromIntegral . (`shiftL` 2) . quant 8192
      S24   -> \v ->
        let c = quant 8388608 v .&. 0xFFFFFF
        in BB.word8 (fromIntegral c) <> BB.word8 (fromIntegral (c `shiftR` 8))
           <> BB.word8 (fromIntegral (c `shiftR` 16))
      S32   -> BB.int32LE . fromIntegral . quant 2147483648
      F32   -> BB.floatLE . realToFrac . clamp
      Ulaw  -> BB.word8 . ulawEncode
      Alaw  -> BB.word8 . alawEncode
    -- the code for a sample, with @half@ the code for 1.0 -- one past
    -- the largest positive code, which is why the top clamps at half - 1
    quant :: Int -> Double -> Int
    quant half v = max (negate half) (min (half - 1) (round (clamp v * fromIntegral half)))
    clamp v = max (-1) (min 1 v)

-- | The one spelling of each format, for command lines and logs.
formatName :: SampleFormat -> String
formatName f = case f of
  S8 -> "s8"; U8 -> "u8"; S16 -> "s16"; S24 -> "s24"; S32 -> "s32"; F32 -> "f32"
  Ulaw -> "ulaw"; Alaw -> "alaw"; Pcm14 -> "pcm14"

formatNamed :: String -> Maybe SampleFormat
formatNamed s = lookup (map toLower s) [ (formatName f, f) | f <- [minBound .. maxBound] ]

-- | Every name 'formatNamed' accepts, for a help string.
formatNames :: [String]
formatNames = map formatName [minBound .. maxBound]

describeFormat :: SampleFormat -> String
describeFormat f = case f of
  S8 -> "8-bit signed PCM"
  U8 -> "8-bit unsigned PCM"
  S16 -> "16-bit PCM"
  S24 -> "24-bit PCM"
  S32 -> "32-bit PCM"
  F32 -> "32-bit float"
  Ulaw -> "G.711 mu-law"
  Alaw -> "G.711 A-law"
  Pcm14 -> "14-bit PCM in 16-bit words"
