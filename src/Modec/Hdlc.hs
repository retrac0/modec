-- | HDLC framing as used by V.8bis messages (ISO/IEC 3309 frame
-- structure): flags 01111110, zero-bit insertion after five ones, and
-- the 16-bit FCS (x^16 + x^12 + x^5 + 1, register preset to ones, ones
-- complement transmitted most significant bit first).  Octets go on the
-- line bit 1 (least significant) first.
module Modec.Hdlc
  ( fcs16
  , fcsResidual
  , hdlcFlagBits
  , hdlcFrameBits
  , HdlcRx
  , hdlcRxInit
  , hdlcRxBits
  ) where

import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Word (Word8, Word16)

-- | Feed one bit into the FCS register.
fcsStep :: Word16 -> Bool -> Word16
fcsStep reg b =
  let feedback = b /= testBit reg 15
      shifted = (reg `shiftL` 1) .&. 0xFFFF
  in if feedback then shifted `xor` 0x1021 else shifted

-- | FCS of a bit sequence (in transmission order): the complemented
-- remainder, to be transmitted most significant bit first.
fcs16 :: [Bool] -> Word16
fcs16 bits = foldl fcsStep 0xFFFF bits `xor` 0xFFFF

-- | The register value left after a good frame (data followed by its FCS).
fcsResidual :: Word16
fcsResidual = 0x1D0F

octetBits :: Word8 -> [Bool]
octetBits o = [ testBit o i | i <- [0 .. 7] ]

bitsToOctet :: [Bool] -> Word8
bitsToOctet bs = foldr (\(i, b) acc -> if b then acc .|. (1 `shiftL` i) else acc) 0 (zip [0 .. 7] bs)

-- | The flag, 01111110, in transmission order.  Also the interframe fill:
-- ISO 3309 sends contiguous flags between frames, which is what a
-- synchronous MNP transmitter idles on (see "Modec.MnpFrame").
hdlcFlagBits :: [Bool]
hdlcFlagBits = [False, True, True, True, True, True, True, False]

flagBits :: [Bool]
flagBits = hdlcFlagBits

-- | Bit-stuff a payload (insert a zero after five consecutive ones).
stuff :: [Bool] -> [Bool]
stuff = go (0 :: Int)
  where
    go _ [] = []
    go ones (b : bs)
      | b && ones == 4 = True : False : go 0 bs
      | b = True : go (ones + 1) bs
      | otherwise = False : go 0 bs

-- | A complete frame as line bits: @openFlags@ opening flags, the
-- information octets and FCS (stuffed), and @closeFlags@ closing flags.
hdlcFrameBits :: Int -> Int -> [Word8] -> [Bool]
hdlcFrameBits openFlags closeFlags info =
  concat (replicate openFlags flagBits) ++ stuff (payload ++ fcsBits) ++ concat (replicate closeFlags flagBits)
  where
    payload = concatMap octetBits info
    f = fcs16 payload
    fcsBits = [ testBit f i | i <- [15, 14 .. 0] ]

-- | Receiver state: the last eight raw line bits (flag detector), whether
-- we are inside a frame, the ones counter for destuffing, and the
-- destuffed bits collected so far (most recent first).
data HdlcRx = HdlcRx [Bool] Bool Int [Bool]

hdlcRxInit :: HdlcRx
hdlcRxInit = HdlcRx [] False 0 []

-- | Feed line bits; returns completed, FCS-checked frames (information
-- octets without the FCS).
hdlcRxBits :: HdlcRx -> [Bool] -> (HdlcRx, [[Word8]])
hdlcRxBits st0 bits = go st0 bits []
  where
    go st [] acc = (st, reverse acc)
    go (HdlcRx win inFrame ones body) (b : bs) acc =
      let win' = take 8 (b : win)              -- newest first
          isFlag = win' == reverse flagBits    -- reverse: newest-first order
          -- a flag was completed with this bit: the 7 bits before it belonged to it
      in if isFlag
           then
             let frameBody = drop 7 body       -- remove the flag's first seven bits
                 frames = case checkFrame (reverse frameBody) of
                   Just octets | inFrame -> octets : acc
                   _ -> acc
             in go (HdlcRx win' True 0 []) bs frames
           else
             let (ones', body')
                   | not inFrame = (0, [])
                   | b = (ones + 1, b : body)
                   | ones == 5 = (0, body)          -- stuffed zero: drop it
                   | otherwise = (0, b : body)
                 -- seven or more ones is an abort
                 inFrame' = inFrame && ones' < 7
             in go (HdlcRx win' inFrame' ones' (if inFrame' then body' else [])) bs acc

    checkFrame :: [Bool] -> Maybe [Word8]
    checkFrame bs
      | length bs < 24 || length bs `mod` 8 /= 0 = Nothing
      | foldl fcsStep 0xFFFF bs /= fcsResidual = Nothing
      | otherwise = Just (map bitsToOctet (chunks (take (length bs - 16) bs)))
    chunks [] = []
    chunks xs = take 8 xs : chunks (drop 8 xs)

_unusedShiftR :: Word16 -> Word16
_unusedShiftR = (`shiftR` 0)
