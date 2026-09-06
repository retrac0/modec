import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import Text.Printf
import Data.List (group, sort)
import Modec.V22
import Modec.Wav

-- What is actually coming out of the descrambler?
main :: IO ()
main = do
  (p : chS : rateS : _) <- getArgs
  w <- readWav p
  let fs = fromIntegral (wavRate w) :: Double
      x = wavSamples w
      ch = if chS == "low" then LowChannel else HighChannel
      rate = if rateS == "2400" then R2400 else R1200
      blk = round (fs * 0.02) :: Int
      go st i acc
        | i * blk >= VS.length x = reverse acc
        | otherwise =
            let n = min blk (VS.length x - i * blk)
                (st', o) = v22RxBlock fs ch (VS.slice (i * blk) n x) st
            in go st' (i + 1) (reverse (roBits o) ++ acc)
      bits = go (v22RxSetRate rate (v22RxInit fs)) 0 []
      n = length bits
      ones = length (filter id bits)
      runs = [ length g | g <- group bits, head g ]
      zruns = [ length g | g <- group bits, not (head g) ]
  printf "%s  %s channel at %s\n" p chS rateS
  printf "  %d bits, %.2f%% ones\n" n (100 * fromIntegral ones / fromIntegral n :: Double)
  printf "  longest run of ones %d, of zeros %d\n" (maximum (0:runs)) (maximum (0:zruns))
  printf "  zero runs by length: %s\n" (show (take 12 (map (\g -> (head g, length g)) (group (sort zruns)))))
  printf "  number of zero runs: %d (an idle line should have none)\n" (length zruns)
  -- Async characters sit on a 10-bit grid: start bit, 8 data, stop.  If
  -- these zeros are characters we are mis-framing, their start positions
  -- cluster modulo 10; if they are bit errors they are spread evenly.
  let hist = [ (m, length [ () | s0 <- startsOf bits, s0 `mod` 10 == m ]) | m <- [0 .. 9] ]
  printf "  zero-run starts mod 10: %s\n" (show hist)
  let tot = sum (map snd hist); mx = maximum (map snd hist)
  printf "  most-populated residue holds %.1f%% (10%% = no framing structure)\n"
    (100 * fromIntegral mx / fromIntegral (max 1 tot) :: Double)

startsOf :: [Bool] -> [Int]
startsOf = go 0
  where
    go _ [] = []
    go i (b : bs)
      | not b = i : go (i + 1 + length z) r
      | otherwise = go (i + 1) bs
      where (z, r) = span not bs
