{-# LANGUAGE BangPatterns #-}
-- | Asynchronous binary FSK modem (Bell 103, V.21).
--
-- Receiver: non-coherent detection.  Each sample is correlated against
-- the mark and space tones over a one-bit window; the sign of the
-- energy difference is the instantaneous mark/space decision.  A UART
-- style deframer then hunts for a mark-to-space edge (start bit) and
-- samples the data and stop bits at bit centres.  Everything is
-- sample-rate agnostic: the only timing parameter is samples per bit.
--
-- Transmitter: continuous-phase FSK from a phase accumulator.
module Modec.FSK
  ( Framing (..)
  , framing8N1
  , DemodParams (..)
  , defaultDemodParams
  , discriminate
  , deframe
  , demodulate
  , frameBits
  , modulateBits
  , encodeBytes
  ) where

import Data.Bits (testBit)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)

import Modec.DSP
import Modec.Standards

-- | Asynchronous character framing.  Start bit is always one space;
-- data bits are sent LSB first; no parity support yet.
data Framing = Framing
  { frDataBits :: !Int
  , frStopBits :: !Int
  } deriving (Eq, Show)

framing8N1 :: Framing
framing8N1 = Framing 8 1

newtype DemodParams = DemodParams
  { dpSquelch :: Double  -- ^ minimum tone amplitude (full scale = 1) to consider carrier present
  } deriving (Show)

defaultDemodParams :: DemodParams
defaultDemodParams = DemodParams { dpSquelch = 0.01 }

-- | Mark and space tone energies over a one-bit sliding window.
discriminate :: Double -> FskSpec -> Signal -> (Signal, Signal)
discriminate fs spec x = (toneEnergy fs (fskMark spec) len x, toneEnergy fs (fskSpace spec) len x)
  where len = bitWindow fs spec

bitWindow :: Double -> FskSpec -> Int
bitWindow fs spec = max 1 (round (fs / fskBaud spec))

-- | UART-style deframer over per-sample mark/space energies.
-- @spb@ is samples per bit; @squelchE@ is the minimum summed window
-- energy for carrier presence (see 'toneAmplitude').
deframe :: Double -> Framing -> Double -> Signal -> Signal -> [Word8]
deframe spb (Framing nData _nStop) squelchE em es = go (max 1 (round spb))
  where
    n = VS.length em
    isMark i = VS.unsafeIndex em i >= VS.unsafeIndex es i
    present i = VS.unsafeIndex em i + VS.unsafeIndex es i > squelchE
    at :: Int -> Double -> Int
    at i p = round (fromIntegral i + p * spb)
    go !i
      | i >= n = []
      | present i && not (isMark i) && isMark (i - 1) =
          let startC = at i 0.5
              stopC = at i (fromIntegral nData + 1.5)
          in if stopC >= n
               then []
               else if present startC && not (isMark startC) && isMark stopC
                 then
                   let bits = [isMark (at i (fromIntegral k + 0.5)) | k <- [1 .. nData]]
                       byte = foldr (\b acc -> acc * 2 + (if b then 1 else 0)) 0 bits
                   in byte : go stopC
                 else go (i + 1)
      | otherwise = go (i + 1)

-- | Full receive chain: samples in, bytes out.
demodulate :: Double -> FskSpec -> Framing -> DemodParams -> Signal -> [Word8]
demodulate fs spec fr params x = deframe spb fr squelchE em es
  where
    (em, es) = discriminate fs spec x
    spb = fs / fskBaud spec
    len = bitWindow fs spec
    squelchE = (dpSquelch params * fromIntegral len / 2) ^ (2 :: Int)

-- | Bytes to a bit stream with start/stop framing.  'True' is mark.
frameBits :: Framing -> [Word8] -> [Bool]
frameBits (Framing nData nStop) = concatMap one
  where
    one b = False : [testBit b k | k <- [0 .. nData - 1]] ++ replicate nStop True

-- | Continuous-phase FSK of a bit stream at the spec's baud rate.
modulateBits :: Double -> FskSpec -> [Bool] -> Signal
modulateBits fs spec bitsL
  | VU.null bits = VS.empty
  | otherwise = VS.unfoldrN total step (0 :: Int, 0 :: Double)
  where
    bits = VU.fromList bitsL
    spb = fs / fskBaud spec
    total = ceiling (fromIntegral (VU.length bits) * spb)
    twoPi = 2 * pi
    step (!k, !ph) =
      let bi = min (VU.length bits - 1) (floor (fromIntegral k / spb))
          f = if bits VU.! bi then fskMark spec else fskSpace spec
          ph' = ph + twoPi * f / fs
          ph'' = if ph' >= twoPi then ph' - twoPi else ph'
      in Just (sin ph, (k + 1, ph''))

-- | Frame and modulate bytes, with @pre@ and @post@ seconds of mark
-- (idle) around the data, at amplitude @amp@.
encodeBytes :: Double -> FskSpec -> Framing -> Double -> Double -> Double -> [Word8] -> Signal
encodeBytes fs spec fr amp pre post bytes = VS.map (* amp) (modulateBits fs spec bits)
  where
    idle secs = replicate (round (secs * fskBaud spec)) True
    bits = idle pre ++ frameBits fr bytes ++ idle post
