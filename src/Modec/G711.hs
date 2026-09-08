-- | G.711 companding: the 8-bit codec every telephone call in this
-- project has actually gone through.
--
-- A µ-law byte is a sign bit, a three-bit segment and a four-bit
-- mantissa: a piecewise-linear approximation to a logarithm, with the
-- step size doubling every segment.  That is the whole point.  A linear
-- 8-bit quantiser has one step size, so its signal-to-noise ratio falls
-- a decibel for every decibel the signal drops; a companded one has a
-- step proportional to the signal, so its ratio stays put.  Measured
-- with 'ulawRound' on a 2100 Hz sine: 38.7 dB at full scale and 37.3 dB
-- thirty decibels down, where eight-bit linear gives 49.5 dB and then
-- 18.3 dB.  Companding trades headroom it does not need for dynamic
-- range it does.
--
-- The consequence for a modem is that the quantisation noise is
-- /signal-correlated/: it tracks the envelope rather than sitting at a
-- fixed level, so it cannot be imitated by adding white noise at a
-- fixed signal-to-noise ratio, which is all "Modec.Channel"'s @chSnrDb@
-- can do.
--
-- Two details worth knowing before using the codes directly.  Both laws
-- store the byte inverted -- µ-law complements every bit, A-law
-- alternates -- so that an idle line, which is all ones or all zeros,
-- carries plenty of transitions for the span to keep timing on.  Which
-- means \"all ones\" is not a loud noise: @0xFF@ in µ-law decodes to
-- exactly zero, while @0x00@ decodes to full-scale negative.
--
-- The reference is the Sun implementation of ITU-T G.711, and the
-- constants here are its constants.
module Modec.G711
  ( -- * µ-law
    ulawEncode
  , ulawDecode
  , ulawRound
    -- * A-law
  , alawEncode
  , alawDecode
  , alawRound
    -- * Whole signals
  , codeSignal
  , decodeSignal
  ) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.DSP (Signal)

-- | Sample scale: the codecs work in 16-bit integers, this codebase in
-- doubles on [-1, 1].
full :: Double
full = 32768

toPcm :: Double -> Int
toPcm x = max (-32768) (min 32767 (round (x * full)))

fromPcm :: Int -> Double
fromPcm v = fromIntegral v / full

-- | The segment a magnitude falls in: how many segment ends it exceeds.
segmentOf :: [Int] -> Int -> Int
segmentOf ends v = length (takeWhile (< v) ends)

-- µ-law ---------------------------------------------------------------

-- | 33 in the 14-bit domain the standard works in, 132 here.  The bias
-- is what makes the smallest segment behave like the others: without it
-- the first chord would need a different step size and the decoder
-- would need a special case for zero.
ulawBias :: Int
ulawBias = 0x84

-- | Just under full scale.  Anything louder is a clip, not a code.
ulawClip :: Int
ulawClip = 32635

ulawEnds :: [Int]
ulawEnds = [0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF, 0x3FFF, 0x7FFF]

ulawEncode :: Double -> Word8
ulawEncode x =
  let p = toPcm x
      sign = if p < 0 then 0x80 else 0 :: Int
      mag = min ulawClip (abs p) + ulawBias
      seg = min 7 (segmentOf ulawEnds mag)
      man = (mag `shiftR` (seg + 3)) .&. 0x0F
  in fromIntegral (complement (sign .|. (seg `shiftL` 4) .|. man) .&. 0xFF)

ulawDecode :: Word8 -> Double
ulawDecode w =
  let c = complement (fromIntegral w) .&. 0xFF :: Int
      seg = (c `shiftR` 4) .&. 0x07
      man = c .&. 0x0F
      t = (((man `shiftL` 3) + ulawBias) `shiftL` seg) - ulawBias
  in fromPcm (if c .&. 0x80 /= 0 then negate t else t)

-- | One sample through an encoder and straight back out of a decoder.
ulawRound :: Double -> Double
ulawRound = ulawDecode . ulawEncode

-- A-law ---------------------------------------------------------------

-- | A-law inverts alternate bits rather than all of them.
alawMask :: Int
alawMask = 0x55

alawEnds :: [Int]
alawEnds = [0x1F, 0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF]

alawEncode :: Double -> Word8
alawEncode x =
  let p = toPcm x `shiftR` 3          -- A-law works 13-bit
      (mag, mask) = if p >= 0 then (p, alawMask .|. 0x80) else (negate p - 1, alawMask)
      seg = segmentOf alawEnds mag
  in fromIntegral $ if seg >= 8
       then (0x7F `xor` mask) .&. 0xFF
       else let man = if seg < 2 then (mag `shiftR` 1) .&. 0x0F
                                 else (mag `shiftR` seg) .&. 0x0F
            in ((seg `shiftL` 4) .|. man) `xor` mask .&. 0xFF

alawDecode :: Word8 -> Double
alawDecode w =
  let c = (fromIntegral w `xor` alawMask) .&. 0xFF :: Int
      seg = (c `shiftR` 4) .&. 0x07
      man = c .&. 0x0F
      base = man `shiftL` 4
      t = case seg of
            0 -> base + 0x08
            1 -> base + 0x108
            _ -> (base + 0x108) `shiftL` (seg - 1)
  in fromPcm (if c .&. 0x80 /= 0 then t else negate t)

alawRound :: Double -> Double
alawRound = alawDecode . alawEncode

-- Whole signals -------------------------------------------------------

-- | A signal as the bytes a span would carry.  Working at the code
-- level rather than on samples is what lets a bit error or a stuck code
-- mean what it means on a real span.
codeSignal :: (Double -> Word8) -> Signal -> VS.Vector Word8
codeSignal enc = VS.map enc

decodeSignal :: (Word8 -> Double) -> VS.Vector Word8 -> Signal
decodeSignal dec = VS.map dec
