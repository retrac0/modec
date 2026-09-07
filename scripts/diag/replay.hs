-- | Replay a recording through the whole modem, offline.
--
-- Decoding a recording by pointing the V.22 receiver at the top of the
-- file does not work and quietly looks as though it does: the receiver
-- acquires on ringback and the answer tone, locks to nothing, and
-- returns a constant, which the descrambler turns into a page of U or w
-- characters.  A live modem never does that -- the handshake starts the
-- data receiver at the right instant, at the right rate, in the right
-- channel.  So the only faithful way to read a recording back is to run
-- the real modem over it, which is what this does.
--
-- The far end's audio is fixed, so our transmissions go nowhere.  That is
-- sound for these recordings because a live modec already elicited the
-- responses in them; what it cannot do is explore how the far end would
-- have replied to something different.
--
--   replay FILE.wav MODES [IMPAIRMENT VALUE [SEED]]
--
-- e.g.  replay call.wav v22bis snr 18
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector.Storable as VS
import Data.Char (chr, isPrint)
import Data.Word (Word8)
import Control.Monad (when)
import System.Environment (getArgs, lookupEnv)
import System.IO
import Text.Printf

import Modec.Channel
import Modec.Handshake
import Modec.Modem
import Modec.V22 (rxEvmEstimate, rxSpsEstimate)
import Modec.Wav

modeOf :: String -> Standard
modeOf s = case s of
  "bell103" -> Bell103; "v21" -> V21; "bell212a" -> Bell212A
  "v22" -> V22; "v32" -> V32; _ -> V22bis

impair :: [String] -> Channel
impair (k : v : rest) = case k of
  "snr"     -> base { chSnrDb = Just val }
  "freq"    -> base { chFreqOffsetHz = val }
  "rate"    -> base { chRateOffset = val }
  "dropout" -> base { chDropout = Just (0.02, val) }
  "echo"    -> base { chEcho = Just (0.02, val) }
  "clip"    -> base { chClip = Just val }
  "hum"     -> base { chHum = Just (50, val) }
  "jitter"  -> base { chJitter = WalkJitter val (4 * val) }
  "slips"   -> base { chJitter = Slips 1.0 val }
  _         -> base
  where
    val = read v :: Double
    base = idealChannel { chSeed = case rest of { (s : _) -> read s; [] -> 1 } }
impair _ = idealChannel

main :: IO ()
main = do
  hSetBinaryMode stdout True
  args <- getArgs
  let (path : modesS : rest0) = args
      (v8, rest) = case rest0 of { ("v8" : r) -> (True, r); r -> (False, r) }
      modes = map modeOf (words (map (\c -> if c == ',' then ' ' else c) modesS))
  evmOn <- fmap (/= Nothing) (lookupEnv "MODEC_EVM")
  w <- readWav path
  let fs = fromIntegral (wavRate w) :: Double
      x = applyChannel fs (impair rest) (wavSamples w)
      cfg0 = defaultModemConfig fs Originate modes
      cfg = cfg0 { mcHandshake = (mcHandshake cfg0) { hcV8 = v8 } }
      blk = round (fs * 0.02) :: Int
      go st i acc evs
        | i * blk >= VS.length x = return (reverse acc, reverse evs)
        | otherwise = do
            let n = min blk (VS.length x - i * blk)
                (st', _, bytes, es) = modemStep cfg st (VS.slice (i * blk) n x) []
            -- every phase change, with the time
            when (modemPhase st' /= modemPhase st) $
              hPrintf stderr "  %6.2fs  %s\n" (fromIntegral i * 0.02 :: Double) (modemPhase st')
            -- every half second, what the receiver thinks of the line
            when (evmOn && i `mod` 25 == 0) $ case fst (modemV22Rx st') of
              Just (_, r) -> hPrintf stderr "  %5.1fs evm %7.4f sps %8.5f  %d bytes so far\n"
                               (fromIntegral i * 0.02 :: Double) (rxEvmEstimate r) (rxSpsEstimate r)
                               (length acc + length bytes)
              Nothing -> return ()
            go st' (i + 1) (reverse bytes ++ acc) (reverse es ++ evs)
  (bytes, events) <- go (modemInit cfg) 0 [] []
  mapM_ (hPutStrLn stderr . describe) events
  hPrintf stderr "%d bytes\n" (length bytes)
  BC.hPutStr stdout (BC.pack (map (chr . fromIntegral) (bytes :: [Word8])))
  where
    describe e = case e of
      EvConnected s l -> "CONNECT " ++ show s ++ " " ++ show l
      EvDropped -> "NO CARRIER"
      EvFailed why -> "failed: " ++ why
      EvV8Menu m -> "V.8 menu"
