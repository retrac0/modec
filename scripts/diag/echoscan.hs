-- | Drive the data-mode echo canceller over a recorded call -- modec's
-- own transmit and what it received, block by block, as the modem
-- would -- and print what its search makes of it.
--
--   cabal exec -- ghc -O1 -package modec -outputdir /tmp/x -o /tmp/echoscan scripts/diag/echoscan.hs
--   /tmp/echoscan recordings/STAMP-1001-tx.wav recordings/STAMP-1001.wav START_SECONDS
import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import Text.Printf (printf)
import Modec.Echo
import Modec.Wav (readWav, writeWav16Mono, Wav (..))

main :: IO ()
main = do
  [txF, rxF, startS, outF] <- getArgs
  w0 <- readWav rxF
  tx <- fmap wavSamples (readWav txF)
  let rx = wavSamples w0
  let fs = 8000 :: Double
      start = round (read startS * fs) :: Int
      cfg = defaultEchoConfig
      blk = 160
      -- the reference history must already hold what went out before
      -- the window, so begin pushing a little earlier than reading
      pre = 3 * 8000
      go i st acc
        | i + blk > min (VS.length tx) (VS.length rx) = return (reverse acc)
        | otherwise = do
            let rxB = VS.slice i blk rx; txB = VS.slice i blk tx
                (st1, out) = if i < start then echoBlock cfg False rxB st else echoBlockData cfg rxB st
                st2 = echoPush cfg txB st1
            if i >= start && (i - start) `mod` (100 * blk) == 0
              then printf "%6.1f s  %s\n" (fromIntegral i / fs :: Double) (echoDebug st2) else return ()
            go (i + blk) st2 (out : acc)
  outs <- go (max 0 (start - pre)) (echoInit cfg) []
  let first = max 0 (start - pre)
      cleaned = VS.replicate first 0 VS.++ VS.concat outs
  writeWav16Mono outF 8000 cleaned
