import qualified Data.Vector.Storable as VS
import Modec.Wav
import System.Environment (getArgs)
import Text.Printf
import Modec.V22
main :: IO ()
main = do
  (p:_) <- getArgs
  w <- readWavFile p
  let fs = 8000 :: Double
      blk = 8000
      go :: V22RxState -> Int -> IO ()
      go st i
        | i * blk >= VS.length w = return ()
        | otherwise = do
            let n = min blk (VS.length w - i * blk)
                (st', o) = v22RxBlock fs HighChannel (VS.slice (i * blk) n w) st
            printf "  %2d s  sps %.5f (nominal 13.33333, offset %+.0f ppm)  evm %.4f\n"
              (i :: Int) (rxSpsEstimate st') ((rxSpsEstimate st' / (8000/600) - 1) * 1e6) (rxEvmEstimate st')
            go st' (i + 1)
  go (v22RxSetRate R1200 (v22RxInit fs)) 0
readWavFile :: FilePath -> IO (VS.Vector Double)
readWavFile = fmap wavSamples . readWav
