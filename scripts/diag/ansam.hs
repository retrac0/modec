import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import Text.Printf
import Modec.V8
import Modec.Wav

-- report, per 0.5 s of a recording, whether ANSam was seen
main :: IO ()
main = do
  as <- getArgs
  mapM_ one as

one :: FilePath -> IO ()
one p = do
  w <- readWav p
  let fs = fromIntegral (wavRate w) :: Double; x = wavSamples w
  let blk = round (fs * 0.5) :: Int
      go _ i acc | i * blk >= VS.length x = reverse acc
      go st i acc =
        let (st', hit) = ansamBlock st (VS.slice (i * blk) (min blk (VS.length x - i * blk)) x)
        in go st' (i + 1) ((fromIntegral i * 0.5 :: Double, hit) : acc)
      hits = [ t | (t, True) <- go (ansamInit fs) 0 [] ]
  printf "%-58s %s\n" p
    (if null hits then "no ANSam"
     else "ANSam at " ++ unwords [ printf "%.1f" t | t <- take 8 hits ] ++ " s")
