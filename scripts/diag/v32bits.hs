-- v32bits.hs FILE.wav RATE -- run the answering V.32 start-up over a
-- recording of the caller, as Modec.Modem would, and print every phase
-- change plus the descrambled bit stream the rate-signal detector is
-- looking at while it waits for R2 and E.  Bits are printed oldest
-- first, in groups of 16 so a rate sequence (0000....1...1...1) or an E
-- (1111....) can be read by eye.  RATE is what --v32-rate pinned:
-- 4800 7200 9600 9600t 12000 14400.
import Control.Monad (when)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import Text.Printf (printf)

import Modec.DSP (chunksOf)
import Modec.Link (V32Rate (..))
import Modec.Standards (Role (..))
import Modec.V32Start
import Modec.Wav

main :: IO ()
main = do
  [file, rateArg] <- getArgs
  Right w <- decodeWav <$> B.readFile file
  let rate = case rateArg of
        "4800" -> V32R4800; "7200" -> V32R7200; "9600" -> V32R9600
        "9600t" -> V32R9600T; "12000" -> V32R12000; _ -> V32R14400
      st0 = v32StartAfterAnswerTone 8000 Answer (chosen rate)
      go _ _ [] = return ()
      go st t (blk : rest) = do
        let (st', _, _) = v32StartStep st blk
            ph = v32Phase st'
            watch = show ph `elem` ["ATrainR2", "AR3", "AE"]
        when (ph /= v32Phase st) $ printf "%7.2f  %s\n" t (show ph)
        when (watch && (round (t * 50) :: Int) `mod` 5 == 0) $
          printf "%7.2f  %-9s %s\n" t (show ph) (groups (map bit (reverse (take 96 (v32Bits st')))))
        go st' (t + 0.02) rest
  go st0 (0 :: Double) (chunksOf 160 (wavSamples w))
  where
    bit b = if b then '1' else '0'
    groups s = case splitAt 16 s of
      (a, []) -> a
      (a, b) -> a ++ " " ++ groups b
