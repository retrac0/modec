-- | Bit-level asynchronous character framing (start bit, data bits LSB
-- first, stop bit) over a hard-decision bit stream, e.g. the descrambled
-- 1200 bit/s output of the V.22 receiver.
module Modec.Async
  ( AsyncRx
  , asyncRxInit
  , asyncRxBits
  , asyncRxStage
  ) where

import Data.Bits (setBit)
import Data.Word (Word8)

import Modec.Standards (Framing (..))
import Modec.Stream

-- | Idle (waiting for a start bit after mark), or inside a character
-- with the number of bits collected and the accumulator; and the last
-- bit seen, because a start bit is an edge.
--
-- A start bit is a space that follows a mark -- the stop bit of the
-- character before it, or idle.  Taking any space as a start bit is
-- what a framer does when it is right and cannot recover when it is
-- wrong: one bit lost from the stream, and it latches onto a data zero,
-- reads the next character's start bit as a bad stop bit, drops it,
-- and hunts again from the very next bit, which is a data bit, and
-- latches onto the next data zero.  Simulated, that framer never
-- realigned after a single deleted bit -- twenty trials of twenty --
-- and emitted a steady stream of bytes with the high bit set, which is
-- precisely the garbage every burst on the bench was followed by, on
-- both modems' outputs, for lines at a time.  Insisting on the edge,
-- it realigns every time, within fifteen characters.
data AsyncRx = AsyncRx !Framing !Bool !Int !Int !Bool

asyncRxInit :: Framing -> AsyncRx
asyncRxInit fr = AsyncRx fr False 0 0 True

-- | Feed bits ('True' = mark); returns completed bytes.
asyncRxBits :: AsyncRx -> [Bool] -> (AsyncRx, [Word8])
asyncRxBits st0 bits = go st0 bits []
  where
    go :: AsyncRx -> [Bool] -> [Word8] -> (AsyncRx, [Word8])
    go st [] acc = (st, reverse acc)
    go (AsyncRx fr@(Framing nData _) inChar cnt acc0 prev) (b : bs) acc
      | not inChar =
          if b || not prev then go (AsyncRx fr False 0 0 b) bs acc   -- mark, or a space that follows a space: idle
               else go (AsyncRx fr True 0 0 b) bs acc                -- mark then space: a start bit
      | cnt < nData =
          go (AsyncRx fr True (cnt + 1) (if b then setBit acc0 cnt else acc0) b) bs acc
      | otherwise =                                                   -- stop bit
          if b then go (AsyncRx fr False 0 0 b) bs (fromIntegral acc0 : acc)
               else go (AsyncRx fr False 0 0 b) bs acc                -- framing error: drop

asyncRxStage :: Framing -> Stage [Bool] [Word8]
asyncRxStage fr = Stage (asyncRxInit fr) asyncRxBits
