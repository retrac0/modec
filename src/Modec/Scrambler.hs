-- | The self-synchronising scrambler that every voiceband modem from
-- V.22 onwards uses, with the generating polynomial left as a parameter.
--
-- The line bit is the data bit plus two tapped bits of the /line/
-- history, and the register then remembers the line bit rather than the
-- data bit.  That is what makes it self-synchronising: a receiver that
-- has seen the last @n@ line bits is in step with the far transmitter
-- whatever it missed before them, so no framing or agreement on a
-- starting state is needed.  The cost is error multiplication -- one bit
-- wrong on the line comes out wrong three times -- which is why the
-- error-correcting protocol above it works in frames rather than bits.
--
-- The polynomials in use here:
--
-- * V.22 and V.22bis (§6.2): 1 + x^-14 + x^-17, both directions.
-- * V.32 (§4): 1 + x^-18 + x^-23 calling, 1 + x^-5 + x^-23 answering.
--   V.29 and V.17 use the calling one; V.34 keeps both.
--
-- The two directions differ so that a modem hearing its own echo cannot
-- descramble it into the ones it is transmitting and mistake the echo
-- for the far end.
module Modec.Scrambler
  ( Lfsr
  , lfsr
  , scramble
  , descramble
  , scrambleRun
  , descrambleRun
  ) where

import Data.Bits (shiftL, testBit, (.&.), (.|.))

-- | A scrambler polynomial: the two tap positions and the register
-- width that follows from them.
data Lfsr = Lfsr
  { lfTap1 :: !Int
  , lfTap2 :: !Int
  , lfMask :: !Int
  } deriving (Eq, Show)

-- | @lfsr a b@ is the polynomial 1 + x^-a + x^-b.  The exponents are the
-- delays the Recommendations name, so they are written here as they are
-- written there; the register holds @max a b@ bits and the taps are one
-- place lower, because x^-1 is the bit that went out last.
lfsr :: Int -> Int -> Lfsr
lfsr a b = Lfsr (a - 1) (b - 1) ((1 `shiftL` max a b) - 1)

-- | The parity of the two taps -- the correction the scrambler adds and
-- the descrambler subtracts, which over GF(2) is the same operation.
tapped :: Lfsr -> Int -> Bool
tapped l reg = testBit reg (lfTap1 l) /= testBit reg (lfTap2 l)

-- | Shift the line bit in.  Both ends do this with the same bit, which
-- is the whole trick.
push :: Lfsr -> Int -> Bool -> Int
push l reg b = ((reg `shiftL` 1) .|. (if b then 1 else 0)) .&. lfMask l

-- | Scramble one data bit, returning the new register and the line bit.
scramble :: Lfsr -> Int -> Bool -> (Int, Bool)
scramble l reg d = (push l reg out, out)
  where out = d /= tapped l reg

-- | Descramble one line bit, returning the new register and the data bit.
descramble :: Lfsr -> Int -> Bool -> (Int, Bool)
descramble l reg line = (push l reg line, line /= tapped l reg)

-- | Scramble a run of bits, in order.
scrambleRun :: Lfsr -> Int -> [Bool] -> (Int, [Bool])
scrambleRun = runWith scramble

-- | Descramble a run of bits, in order.
descrambleRun :: Lfsr -> Int -> [Bool] -> (Int, [Bool])
descrambleRun = runWith descramble

runWith :: (Lfsr -> Int -> Bool -> (Int, Bool)) -> Lfsr -> Int -> [Bool] -> (Int, [Bool])
runWith f l = go []
  where
    go acc reg [] = (reg, reverse acc)
    go acc reg (b : bs) = let (reg', o) = f l reg b in go (o : acc) reg' bs
