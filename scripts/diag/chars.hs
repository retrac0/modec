import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import Data.Char (chr, isPrint)
import Text.Printf
import Modec.Async
import Modec.FSK (framing8N1)
import Modec.V22
import Modec.Wav
main :: IO ()
main = do
  (p : chS : rateS : _) <- getArgs
  w <- readWav p
  let fs = fromIntegral (wavRate w) :: Double
      x = wavSamples w
      ch = if chS == "low" then LowChannel else HighChannel
      rate = if rateS == "2400" then R2400 else R1200
      blk = round (fs * 0.02) :: Int
      go st ar i acc
        | i * blk >= VS.length x = reverse acc
        | otherwise =
            let n = min blk (VS.length x - i * blk)
                (st', o) = v22RxBlock fs ch (VS.slice (i * blk) n x) st
                (ar', bs) = asyncRxBits ar (roBits o)
            in go st' ar' (i + 1) (reverse bs ++ acc)
      bytes = go (v22RxSetRate rate (v22RxInit fs)) (asyncRxInit framing8N1) 0 []
      shown = map (\b -> let c = chr (fromIntegral b) in if isPrint c then c else '.') bytes
  printf "%d characters\n" (length bytes)
  putStrLn shown
