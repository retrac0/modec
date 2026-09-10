{-# LANGUAGE OverloadedStrings #-}
-- | Minimal RIFF/WAVE reader and writer.
--
-- Reads PCM 8/16/24/32-bit, IEEE float 32-bit and G.711 mu-law and
-- A-law, any channel count (only the first channel is kept), and
-- writes mono in any of those.  The sample arithmetic is
-- "Modec.Sample"'s; this module is the container.
module Modec.Wav
  ( Wav (..)
  , decodeWav
  , encodeWavMono
  , encodeWav16Mono
  , readWav
  , writeWavMono
  , writeWav16Mono
    -- * Raw 16-bit little-endian mono
  , decodeS16
  , encodeS16
    -- * Streaming writer
  , WavWriter
  , openWav16Mono
  , wavAppend
  , wavAppendRaw
  , closeWav
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.IORef
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Storable as VS
import System.IO

import Modec.DSP (Signal)
import Modec.Sample

data Wav = Wav
  { wavRate     :: !Int
  , wavChannels :: !Int
  , wavFormat   :: !SampleFormat
  , wavSamples  :: !Signal   -- ^ first channel only, scaled to [-1, 1]
  }

le16 :: B.ByteString -> Int -> Int
le16 bs i = fromIntegral (B.index bs i) .|. (fromIntegral (B.index bs (i + 1)) `shiftL` 8)

le32 :: B.ByteString -> Int -> Int
le32 bs i = le16 bs i .|. (le16 bs (i + 2) `shiftL` 16)

-- | The format a WAVE format tag and bit depth name.
formatOfTag :: Int -> Int -> Maybe SampleFormat
formatOfTag tag bits = case (tag, bits) of
  (1, 8)  -> Just U8
  (1, 16) -> Just S16
  (1, 24) -> Just S24
  (1, 32) -> Just S32
  (3, 32) -> Just F32
  (6, 8)  -> Just Alaw
  (7, 8)  -> Just Ulaw
  _ -> Nothing

-- | The tag a format is written under.  S8 and Pcm14 have none -- an
-- 8-bit WAV is unsigned by definition, and nothing outside a Conexant
-- data sheet knows what 14-bit PCM is -- so they go out as 16-bit PCM,
-- which holds either exactly.
wavTagOf :: SampleFormat -> (Int, SampleFormat)
wavTagOf f = case f of
  U8 -> (1, U8); S16 -> (1, S16); S24 -> (1, S24); S32 -> (1, S32)
  F32 -> (3, F32); Alaw -> (6, Alaw); Ulaw -> (7, Ulaw)
  S8 -> (1, S16); Pcm14 -> (1, S16)

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
      | chans < 1 = Left "zero channels"
      | otherwise = case formatOfTag tag bits of
          Nothing -> Left ("unsupported WAV format tag " ++ show tag ++ " with " ++ show bits ++ " bits")
          Just fmt ->
            let frame = chans * bytesPerSample fmt
                nFrames = B.length body `div` frame
                -- decode everything, keep channel 0 of each frame
                everything = decodeSamples fmt (B.take (nFrames * frame) body)
                first = if chans == 1 then everything
                        else VS.generate nFrames (\i -> everything VS.! (i * chans))
            in Right (Wav rate chans fmt first)
      where
        tag0 = le16 f 0
        chans = le16 f 2
        rate = le32 f 4
        bits = le16 f 14
        -- WAVE_FORMAT_EXTENSIBLE carries the real tag in the sub-format GUID
        tag = if tag0 == 0xFFFE && B.length f >= 26 then le16 f 24 else tag0

-- | A mono WAV in the given format.  The non-PCM tags get the 18-byte
-- format chunk and the fact chunk the specification asks of them, so
-- that other readers accept the file; PCM gets the 16-byte one every
-- reader has always understood.
encodeWavMono :: SampleFormat -> Int -> Signal -> BL.ByteString
encodeWavMono fmt0 rate x = BB.toLazyByteString $
     BB.byteString "RIFF" <> w32 (4 + 8 + fmtLen + factLen + 8 + dataLen + pad) <> BB.byteString "WAVE"
  <> BB.byteString "fmt " <> w32 fmtLen
       <> w16 tag <> w16 1 <> w32 rate <> w32 (rate * bps) <> w16 bps <> w16 (8 * bps)
       <> (if pcm then mempty else w16 0)
  <> (if pcm then mempty else BB.byteString "fact" <> w32 4 <> w32 (VS.length x))
  <> BB.byteString "data" <> w32 dataLen <> BB.byteString (encodeSamples fmt x)
  <> (if pad == 1 then BB.word8 0 else mempty)
  where
    (tag, fmt) = wavTagOf fmt0
    pcm = tag == 1
    bps = bytesPerSample fmt
    fmtLen = if pcm then 16 else 18
    factLen = if pcm then 0 else 12
    dataLen = VS.length x * bps
    pad = dataLen .&. 1
    w32, w16 :: Int -> BB.Builder
    w32 = BB.word32LE . fromIntegral
    w16 = BB.word16LE . fromIntegral

encodeWav16Mono :: Int -> Signal -> BL.ByteString
encodeWav16Mono = encodeWavMono S16

-- | Samples exactly as they come off a sound card or go down a pipe:
-- 16-bit little-endian, one channel, no header.
decodeS16 :: B.ByteString -> Signal
decodeS16 = decodeSamples S16

encodeS16 :: Signal -> B.ByteString
encodeS16 = encodeSamples S16

readWav :: FilePath -> IO Wav
readWav path = do
  bs <- B.readFile path
  either (\e -> ioError (userError (path ++ ": " ++ e))) return (decodeWav bs)

writeWavMono :: SampleFormat -> FilePath -> Int -> Signal -> IO ()
writeWavMono fmt path rate x = BL.writeFile path (encodeWavMono fmt rate x)

writeWav16Mono :: FilePath -> Int -> Signal -> IO ()
writeWav16Mono = writeWavMono S16

-- | A WAV file being written incrementally.  The length fields are
-- rewritten every half second as well as by 'closeWav', so a recording
-- that is killed mid-call is still a playable file, missing at most the
-- last half second.
--
-- Always 16-bit PCM: a recording's job is to be readable by anything,
-- and 16 bits hold every format the line can arrive in without loss
-- (see "Modec.Sample").  The length offsets in 'patchLengths' are the
-- 16-byte PCM header's, and rely on that.
data WavWriter = WavWriter Handle (IORef (Int, Int))

-- | Open a 16-bit mono WAV file for appending sample blocks.
openWav16Mono :: FilePath -> Int -> IO WavWriter
openWav16Mono path rate = do
  h <- openBinaryFile path WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  B.hPut h (BL.toStrict (encodeWav16Mono rate VS.empty))
  n <- newIORef (0, 0)
  return (WavWriter h n)

-- | Append a block of samples.
wavAppend :: WavWriter -> Signal -> IO ()
wavAppend w x = wavAppendRaw w (encodeS16 x)

-- | Append little-endian 16-bit samples already encoded.
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
