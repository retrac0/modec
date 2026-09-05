-- | Error metrics for comparing decoded byte streams with what was sent.
module Modec.Metrics
  ( editDistance
  , byteErrorRate
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

-- | Levenshtein distance, O(n*m) time with two unboxed rows.
editDistance :: [Word8] -> [Word8] -> Int
editDistance as bs = VS.last (foldl step row0 as)
  where
    n = length bs
    bv = VS.fromList bs
    row0 = VS.generate (n + 1) fromIntegral :: VS.Vector Int
    step prev a = VS.constructN (n + 1) $ \cur ->
      let j = VS.length cur
      in if j == 0
           then VS.head prev + 1
           else minimum
             [ VS.unsafeIndex prev j + 1
             , VS.unsafeIndex cur (j - 1) + 1
             , VS.unsafeIndex prev (j - 1) + (if VS.unsafeIndex bv (j - 1) == a then 0 else 1)
             ]

-- | Edit distance divided by the length of the sent payload.
byteErrorRate :: [Word8] -> [Word8] -> Double
byteErrorRate sent got = fromIntegral (editDistance sent got) / fromIntegral (max 1 (length sent))
