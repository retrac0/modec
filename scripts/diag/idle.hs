import qualified Data.Vector.Storable as VS
import Data.List (group)
import Text.Printf
import Modec.FSK (framing8N1)
import Modec.V22
import Modec.Async
import Data.Char (chr, isPrint)

-- modec's own V.22 idle, straight back into modec's own receiver.
main :: IO ()
main = do
  let fs = 8000 :: Double
      blk = 160
      nblk = 1000                      -- 20 s
      gen 0 _ acc = VS.concat (reverse acc)
      gen k st acc = let (st', s) = v22TxBlock fs HighChannel framing8N1 0.5 False R1200 TxScrambledOnes [] blk st
                     in gen (k - 1 :: Int) st' (s : acc)
      sig = gen nblk v22TxInit []
      go st ar i acc
        | i * blk >= VS.length sig = reverse acc
        | otherwise =
            let n = min blk (VS.length sig - i * blk)
                (st', o) = v22RxBlock fs HighChannel (VS.slice (i * blk) n sig) st
                (ar', bs) = asyncRxBits ar (roBits o)
            in go st' ar' (i + 1) ((roBits o, bs) : acc)
      out = go (v22RxSetRate R1200 (v22RxInit fs)) (asyncRxInit framing8N1) 0 []
      bits = concatMap fst out
      chars = concatMap snd out
      zruns = [ length g | g <- group bits, not (head g) ]
  printf "own idle, 20 s at 1200 in the high channel\n"
  printf "  %d bits, %.2f%% ones\n" (length bits) (100 * fromIntegral (length (filter id bits)) / fromIntegral (length bits) :: Double)
  printf "  zero runs: %d  longest ones run: %d\n" (length zruns) (maximum (0 : [ length g | g <- group bits, head g ]))
  printf "  characters delivered: %d (an idle line must deliver none)\n" (length chars)
  putStrLn ("  " ++ map (\b -> let c = chr (fromIntegral b) in if isPrint c then c else '.') (take 90 chars))
