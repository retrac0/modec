-- | G.711 companding: the 8-bit codec every telephone call in this
-- project has actually gone through.
--
-- A µ-law byte is a sign bit, a three-bit segment and a four-bit
-- mantissa: a piecewise-linear approximation to a logarithm, with the
-- step size doubling every segment.  That is the whole point.  A linear
-- 8-bit quantiser has one step size, so its signal-to-noise ratio falls
-- a decibel for every decibel the signal drops; a companded one has a
-- step proportional to the signal, so its ratio stays put.
--
-- Measured here, sine in and sine out, swept a decibel at a time from
-- full scale to -40 dBFS at 1004 and 2100 Hz: µ-law stays between
-- __32.2 and 38.8 dB__ across the whole range and A-law between 31.9
-- and 39.2, while eight-bit linear runs 49.2, 38.9, 28.5, 18.2, 10.7 dB
-- at 0, -10, -20, -30 and -40 -- a decibel lost for every decibel down,
-- exactly as the arithmetic says.  Companding trades headroom it does
-- not need for dynamic range it does.
--
-- The figure is a band rather than a number because it ripples as the
-- sine's peak crosses a chord boundary; anything asserting a point
-- value of it will be flaky.
--
-- The consequence for a modem is that the quantisation noise is
-- /signal-correlated/: it tracks the envelope rather than sitting at a
-- fixed level, so it cannot be imitated by adding white noise at a
-- fixed signal-to-noise ratio, which is all "Modec.Channel"'s @chSnrDb@
-- can do.
--
-- Three details worth knowing before using the codes directly.
--
-- Both laws store the byte inverted -- µ-law complements every bit,
-- A-law alternates -- so that an idle line, which is all ones or all
-- zeros, carries plenty of transitions for the span to keep timing on.
-- Which means \"all ones\" is not a loud noise: @0xFF@ in µ-law decodes
-- to exactly zero, while @0x00@ decodes to full-scale negative.
--
-- Neither round trip is gain transparent.  The largest magnitude µ-law
-- can represent is 32124 and A-law 32256, so a full-scale sine comes
-- back 0.17 dB or 0.14 dB down.
--
-- A-law has no code for zero.  @alawRound 0@ is @+8/32768@, about
-- -72 dBFS, so an A-law span turns digital silence into a small DC
-- offset.  µ-law does have a zero, twice over: @0xFF@ and @0x7F@ both
-- decode to it, which is why exactly one code does not survive a
-- decode-then-encode round trip.
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

-- | Segment ends in the 14-bit domain the encoder works in.
ulawEnds :: [Int]
ulawEnds = [0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF]

-- | The encoder narrows to 14 bits /before/ taking the magnitude, and
-- the shift is arithmetic, so a negative sample floors towards minus
-- infinity and its magnitude rounds up.  That asymmetry is not
-- something the Recommendation asks for -- it is an artefact of the
-- reference implementation everyone checks against -- but 381 of the
-- 65536 inputs encode differently without it, all of them negative, and
-- being able to diff against @sox@ or spandsp is worth more than the
-- decibel that symmetric rounding would buy.
ulawEncode :: Double -> Word8
ulawEncode x =
  let p = toPcm x `shiftR` 2
      sign = if p < 0 then 0x80 else 0 :: Int
      mag = min (ulawClip `div` 4) (abs p) + (ulawBias `div` 4)
      seg = min 7 (segmentOf ulawEnds mag)
      man = (mag `shiftR` (seg + 1)) .&. 0x0F
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
            in (((seg `shiftL` 4) .|. man) `xor` mask) .&. 0xFF

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
