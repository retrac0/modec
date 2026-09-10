{-# LANGUAGE OverloadedStrings #-}
-- | Minimal RIFF/WAVE reader and writer.  Reads PCM 8/16/24/32-bit and
-- IEEE float 32-bit, any channel count (only the first channel is kept),
-- and writes 16-bit mono PCM.
module Modec.Wav
  ( Wav (..)
  , decodeWav
  , encodeWav16Mono
  , readWav
  , writeWav16Mono
    -- * Raw 16-bit little-endian mono
  , decodeS16
  , encodeS16
    -- * Streaming writer
  , WavWriter
  , openWav16Mono
  , wavAppendRaw
  , closeWav
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.IORef
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32)
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import System.IO

import Modec.DSP (Signal)

data Wav = Wav
  { wavRate     :: !Int
  , wavChannels :: !Int
  , wavSamples  :: !Signal   -- ^ first channel only, scaled to [-1, 1]
  }

le16 :: B.ByteString -> Int -> Int
le16 bs i = fromIntegral (B.index bs i) .|. (fromIntegral (B.index bs (i + 1)) `shiftL` 8)

le32 :: B.ByteString -> Int -> Int
le32 bs i = le16 bs i .|. (le16 bs (i + 2) `shiftL` 16)

decodeWav :: B.ByteString -> Either String Wav
decodeWav bs
  | B.length bs < 12 = Left "file too short for a WAV header"
  | B.take 4 bs /= "RIFF" || B.take 4 (B.drop 8 bs) /= "WAVE" = Left "not a RIFF/WAVE file"
  | otherwise = walk 12 Nothing
  where
    walk off fmt
      | off + 8 > B.length bs = Left "no data chunk found"
      | otherwise =
          let cid = B.take 4 (B.drop off bs)
              len = le32 bs (off + 4)
              body = B.take len (B.drop (off + 8) bs)
              next = off + 8 + len + (len .&. 1)
          in case cid of
               "fmt " -> walk next (Just body)
               "data" -> case fmt of
                 Nothing -> Left "data chunk before fmt chunk"
                 Just f  -> decodeData f body
               _ -> walk next fmt

    decodeData f body
      | B.length f < 16 = Left "fmt chunk too short"
      | otherwise =
          let tag0 = le16 f 0
              chans = le16 f 2
              rate = le32 f 4
              bits = le16 f 14
              -- WAVE_FORMAT_EXTENSIBLE carries the real tag in the sub-format GUID
              tag = if tag0 == 0xFFFE && B.length f >= 26 then le16 f 24 else tag0
              frame = chans * (bits `div` 8)
              nFrames = if frame == 0 then 0 else B.length body `div` frame
              sampleAt i = readSample tag bits body (i * frame)
          in if chans < 1 then Left "zero channels"
             else case (tag, bits) of
               (1, 8)  -> ok chans rate nFrames sampleAt
               (1, 16) -> ok chans rate nFrames sampleAt
               (1, 24) -> ok chans rate nFrames sampleAt
               (1, 32) -> ok chans rate nFrames sampleAt
               (3, 32) -> ok chans rate nFrames sampleAt
               _ -> Left ("unsupported WAV format tag " ++ show tag ++ " with " ++ show bits ++ " bits")

    ok chans rate nFrames sampleAt = Right (Wav rate chans (VS.generate nFrames sampleAt))

    readSample :: Int -> Int -> B.ByteString -> Int -> Double
    readSample tag bits body o = case (tag, bits) of
      (1, 8)  -> (fromIntegral (B.index body o) - 128) / 128
      (1, 16) -> fromIntegral (fromIntegral (le16 body o) :: Int16) / 32768
      (1, 24) ->
        let v = le16 body o .|. (fromIntegral (B.index body (o + 2)) `shiftL` 16)
            s = if v >= 0x800000 then v - 0x1000000 else v
        in fromIntegral s / 8388608
      (1, 32) -> fromIntegral (fromIntegral (le32 body o) :: Int32) / 2147483648
      (3, 32) -> realToFrac (castWord32ToFloat (fromIntegral (le32 body o) :: Word32))
      _ -> 0

encodeWav16Mono :: Int -> Signal -> BL.ByteString
encodeWav16Mono rate x = BB.toLazyByteString $
     BB.byteString "RIFF" <> w32 (36 + dataLen) <> BB.byteString "WAVE"
  <> BB.byteString "fmt " <> w32 16 <> w16 1 <> w16 1 <> w32 rate <> w32 (rate * 2) <> w16 2 <> w16 16
  <> BB.byteString "data" <> w32 dataLen <> BB.byteString (encodeS16 x)
  where
    dataLen = VS.length x * 2
    w32, w16 :: Int -> BB.Builder
    w32 = BB.word32LE . fromIntegral
    w16 = BB.word16LE . fromIntegral

-- | Samples exactly as they come off a sound card or go down a pipe:
-- 16-bit little-endian, one channel, no header.
--
-- The same arithmetic the PCM 16 case of 'decodeWav' and the body of
-- 'encodeWav16Mono' do, and it had been written out a third time in the
-- executable for the audio device -- which is the one copy no test could
-- reach.
decodeS16 :: B.ByteString -> Signal
decodeS16 bs = VS.generate (B.length bs `div` 2) $ \i ->
  fromIntegral (fromIntegral (le16 bs (2 * i)) :: Int16) / 32768

encodeS16 :: Signal -> B.ByteString
encodeS16 x = BL.toStrict (BB.toLazyByteString (VS.foldr (\v acc -> BB.int16LE (toI16 v) <> acc) mempty x))
  where
    toI16 :: Double -> Int16
    toI16 v = round (max (-1) (min 1 v) * 32767)

readWav :: FilePath -> IO Wav
readWav path = do
  bs <- B.readFile path
  either (\e -> ioError (userError (path ++ ": " ++ e))) return (decodeWav bs)

writeWav16Mono :: FilePath -> Int -> Signal -> IO ()
writeWav16Mono path rate x = BL.writeFile path (encodeWav16Mono rate x)

-- | A WAV file being written incrementally.  The length fields are
-- rewritten every half second as well as by 'closeWav', so a recording
-- that is killed mid-call is still a playable file, missing at most the
-- last half second.
data WavWriter = WavWriter Handle (IORef (Int, Int))

-- | Open a 16-bit mono WAV file for appending sample blocks.
openWav16Mono :: FilePath -> Int -> IO WavWriter
openWav16Mono path rate = do
  h <- openBinaryFile path WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  B.hPut h (BL.toStrict (encodeWav16Mono rate VS.empty))
  n <- newIORef (0, 0)
  return (WavWriter h n)

-- | Append little-endian 16-bit samples exactly as they came off the wire.
wavAppendRaw :: WavWriter -> B.ByteString -> IO ()
wavAppendRaw w@(WavWriter h ref) bs = do
  B.hPut h bs
  (n, k) <- readIORef ref
  let n' = n + B.length bs
  if k >= 24
    then writeIORef ref (n', 0) >> patchLengths w n'
    else writeIORef ref (n', k + 1)

-- | Rewrite the RIFF and data lengths, leaving the handle at the end.
patchLengths :: WavWriter -> Int -> IO ()
patchLengths (WavWriter h _) dataLen = do
  hFlush h
  let w32 off v = do
        hSeek h AbsoluteSeek off
        B.hPut h (BL.toStrict (BB.toLazyByteString (BB.word32LE (fromIntegral (v :: Int)))))
  w32 4 (36 + dataLen)
  w32 40 dataLen
  hFlush h
  hSeek h SeekFromEnd 0

-- | Patch the lengths a final time, then close.
closeWav :: WavWriter -> IO ()
closeWav w@(WavWriter h ref) = do
  (dataLen, _) <- readIORef ref
  patchLengths w dataLen
  hClose h
