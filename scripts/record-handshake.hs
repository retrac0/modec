-- | Record a modem call between two modec instances to WAV files, for
-- documentation and for feeding back into `modec detect` / `modec probe`.
--
--   cabal exec -- runghc -isrc scripts/record-handshake.hs OUTDIR
--
-- Writes, at 8 kHz 16-bit mono:
--   handshake-answer-side.wav  the answering modem's transmission alone
--   handshake-call-side.wav    the calling modem's transmission alone
--   handshake-both-sides.wav   both directions summed and band limited,
--                              which is what a tap on the line would hear
import Control.Monad (forM_)
import qualified Data.Vector.Storable as VS
import System.Environment (getArgs)
import Text.Printf (printf)

import Modec.Channel
import Modec.DSP
import Modec.Handshake
import Modec.Modem
import Modec.Wav

main :: IO ()
main = do
  args <- getArgs
  let dir = case args of { (d : _) -> d; [] -> "." }
      fs = 8000 :: Double
      blk = 160 :: Int
      cfgO = defaultModemConfig fs Originate Nothing
      cfgA = defaultModemConfig fs Answer Nothing
      -- each modem hears the other attenuated, with a little noise
      heard k t x = addNoise (k * 100003 + round (t * 1000)) 0.0016 (VS.map (* 0.1) x)
      go t so sa fromA fromO accO accA connectedAt
        | t > 30 || maybe False (\c -> t - c > 3) connectedAt = (reverse accO, reverse accA)
        | otherwise =
            let (so', audioO, _, evO) = modemStep cfgO so (heard 1 t fromA) (payload t)
                (sa', audioA, _, evA) = modemStep cfgA sa (heard 2 t fromO) []
                bothUp = modemConnected so' && modemConnected sa'
                connectedAt' = case connectedAt of
                  Just c -> Just c
                  Nothing | bothUp && (not (null evO) || not (null evA)) -> Just t
                          | bothUp -> Just t
                          | otherwise -> Nothing
            in go (t + fromIntegral blk / fs) so' sa' audioA audioO (audioO : accO) (audioA : accA) connectedAt'
      -- once connected, send a line of text so the recording has data on it
      payload t = if t > 0 && abs (t - 9.0) < 0.011 then map (fromIntegral . fromEnum) "HELLO BBS\r\n" else []
      z = VS.replicate blk 0
      (callBlocks, ansBlocks) = go 0 (modemInit cfgO) (modemInit cfgA) z z [] [] Nothing
      call = VS.concat callBlocks
      answer = VS.concat ansBlocks
      line = applyChannel fs idealChannel { chBandpass = Just (300, 3400) } (VS.zipWith (+) call answer)
      secs v = fromIntegral (VS.length v) / fs :: Double
  writeWav16Mono (dir ++ "/handshake-answer-side.wav") (round fs) (VS.take (round (10 * fs)) answer)
  writeWav16Mono (dir ++ "/handshake-call-side.wav") (round fs) (VS.take (round (10 * fs)) call)
  writeWav16Mono (dir ++ "/handshake-both-sides.wav") (round fs) line
  forM_ [("answer side (10 s)", VS.take (round (10 * fs)) answer), ("call side (10 s)", VS.take (round (10 * fs)) call), ("both sides (full)", line)] $
    \(name, v) -> printf "%-20s %6.2f s  rms %.3f\n" (name :: String) (secs v) (rms v)
