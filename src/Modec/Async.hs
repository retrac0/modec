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

import Modec.FSK (Framing (..))
import Modec.Stream

-- | Idle (waiting for a start bit after mark), or inside a character
-- with the number of bits collected and the accumulator.
data AsyncRx = AsyncRx !Framing !Bool !Int !Int

asyncRxInit :: Framing -> AsyncRx
asyncRxInit fr = AsyncRx fr False 0 0

-- | Feed bits ('True' = mark); returns completed bytes.
asyncRxBits :: AsyncRx -> [Bool] -> (AsyncRx, [Word8])
asyncRxBits st0 bits = go st0 bits []
  where
    go :: AsyncRx -> [Bool] -> [Word8] -> (AsyncRx, [Word8])
    go st [] acc = (st, reverse acc)
    go (AsyncRx fr@(Framing nData _) inChar cnt acc0) (b : bs) acc
      | not inChar =
          if b then go (AsyncRx fr False 0 0) bs acc          -- mark: idle
               else go (AsyncRx fr True 0 0) bs acc           -- space: start bit
      | cnt < nData =
          go (AsyncRx fr True (cnt + 1) (if b then setBit acc0 cnt else acc0)) bs acc
      | otherwise =                                           -- stop bit
          if b then go (AsyncRx fr False 0 0) bs (fromIntegral acc0 : acc)
               else go (AsyncRx fr False 0 0) bs acc          -- framing error: drop

asyncRxStage :: Framing -> Stage [Bool] [Word8]
asyncRxStage fr = Stage (asyncRxInit fr) asyncRxBits
