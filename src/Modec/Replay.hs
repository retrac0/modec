-- | Running the whole modem over a recording, offline.
--
-- Pointing a receiver at the top of a WAV file does not work, and it
-- fails in a way that looks like success: it acquires on ringback and
-- the answer tone, locks to nothing, and returns a constant that the
-- descrambler turns into a page of @U@ or @w@ characters.  A live modem
-- never does that -- the handshake starts the data receiver at the right
-- instant, at the right rate, in the right channel.  So the only
-- faithful way to read a recording back is to run the real modem over
-- it, which is what this does, and it reproduces the live decode exactly,
-- banner for banner.
--
-- The far end's audio is fixed, so our transmissions go nowhere.  That
-- is sound for a recording a live modec already drew the responses out
-- of; what it cannot do is ask how the far end would have answered
-- something different.
--
-- Everything here is pure.  Reading the file is the caller's business.
module Modec.Replay
  ( ReplayConfig (..)
  , defaultReplayConfig
  , ReplayResult (..)
  , replay
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.DSP (Signal)
import Modec.Modem
import Modec.QAM (QamSym)
import Modec.V32Pump (V32Diag (..))
import Modec.Echo (EchoConfig (..))
import Modec.V22 (rxEvmEstimate, rxSpsEstimate)

data ReplayConfig = ReplayConfig
  { rcModem :: ModemConfig
  , rcBlock :: Double        -- ^ block length in seconds, as the live modem uses
  , rcLimit :: Maybe Double  -- ^ stop after this many seconds of recording
  , rcEvery :: Maybe Double  -- ^ sample the receiver this often, for 'rrLine'
  , rcSyms  :: Bool          -- ^ keep every decided V.32 symbol, for 'rrSyms'
  }

-- | Twenty millisecond blocks, the whole file, no line trace.
defaultReplayConfig :: ModemConfig -> ReplayConfig
defaultReplayConfig cfg = ReplayConfig cfg 0.02 Nothing Nothing False

data ReplayResult = ReplayResult
  { rrBytes  :: [Word8]                  -- ^ what the DTE would have seen
  , rrEvents :: [(Double, ModemEvent)]   -- ^ connects, drops, menus, with the time
  , rrPhases :: [(Double, String)]       -- ^ every phase change: the timeline a call log shows
  , rrLine   :: [(Double, Double, Double)]
    -- ^ time, the receiver's decision error and its samples per symbol,
    -- whenever 'rcEvery' asks and there is a receiver to ask.  Whichever
    -- receiver is carrying the call: the V.22 one, or the V.32 data pump
    -- once the start-up has handed over to it.
  , rrEcho   :: [(Double, Maybe Int, Double)]
    -- ^ time, the delay the echo canceller is aimed at, and its return
    -- loss, on the same cadence, whenever there is a canceller
  , rrTruth  :: [(Double, Double, Double, Int, Int, Bool, Int, Int)]
    -- ^ every block of V.32 data mode, with 'mcV32Diag' asking for the
    -- truth: time, mean nearest-point error, mean true error, symbols
    -- with a prediction, symbols measured since it was armed, whether
    -- it is still live, and the ones among the data bits put out and
    -- how many bits there were.  Empty otherwise.
  , rrSyms   :: [(Double, [QamSym])]
    -- ^ every block's decided symbols, with 'rcSyms'
  , rrTaps   :: [(Double, String, [(Double, Double)])]
    -- ^ the equaliser's taps at a few moments: the last start-up block,
    -- the first data block, and 12, 50 and 250 blocks into data mode
  , rrPower  :: [(Double, String, (Double, Double, Double, Double))]
    -- ^ time, the modem's phase, and the V.32 receiver's received power
    -- estimate, on the 'rcEvery' cadence, in the start-up and in data
    -- mode alike
  }

replay :: ReplayConfig -> Signal -> ReplayResult
replay rc x = go (modemInit cfg) 0 [] [] [] [] [] [] [] [] [] (-1)
  where
    -- Data-mode echo cancellation is off in a replay, whatever the
    -- config says.  A replayed modem regenerates its transmit from its
    -- own data and scrambler state, which matches what was actually
    -- sent only until the first payload byte diverges them; the echo in
    -- the recording is of the original.  The search would aim at
    -- nothing, or at noise, and a fixture's bytes would depend on it.
    -- To measure the canceller on a recorded call, drive it over the
    -- recorded transmit as well: scripts/diag/echoscan.hs.
    cfg = let c = rcModem rc in c { mcEcho = (mcEcho c) { ecFarSearch = 0 } }
    fs = mcRate cfg
    blk = max 1 (round (fs * rcBlock rc)) :: Int
    limit = maybe (VS.length x) (\s -> min (VS.length x) (round (s * fs))) (rcLimit rc)
    every = fmap (\s -> max 1 (round (s / rcBlock rc)) :: Int) (rcEvery rc)
    secs i = fromIntegral (i * blk) / fs
    go st i bytes evs phs line echo truth syms power taps dataAt
      | i * blk >= limit =
          ReplayResult (concat (reverse bytes)) (reverse evs) (reverse phs) (reverse line) (reverse echo)
                       (reverse truth) (reverse syms) (reverse taps) (reverse power)
      | otherwise =
          let n = min blk (limit - i * blk)
              (st', _, bs, es) = modemStep cfg st (VS.slice (i * blk) n x) []
              t = secs i
              phs' = if modemPhase st' /= modemPhase st then (t, modemPhase st') : phs else phs
              evs' = [ (t, e) | e <- reverse es ] ++ evs
              line' = case every of
                -- The V.32 pump first: the V.22 receiver the automode
                -- probe left behind is still there during a V.32 call,
                -- and reporting it means reporting a receiver that is
                -- not carrying the call and has not been fed since the
                -- start-up began.
                Just k | i `mod` k == 0 -> case modemV32Line st' of
                  Just (e, sps) -> (t, e, sps) : line
                  Nothing -> case fst (modemV22Rx st') of
                    Just (_, r) -> (t, rxEvmEstimate r, rxSpsEstimate r) : line
                    Nothing -> line
                _ -> line
              echo' = case every of
                Just k | i `mod` k == 0, Just _ <- modemEchoErle st' ->
                  (t, modemEchoDelay st', maybe 0 id (modemEchoErle st')) : echo
                _ -> echo
              -- Every block that had a prediction, and the first that
              -- did not after one that did -- so the drop shows.
              truth' = case (vgTruth (mcV32Diag cfg), modemV32Truth st', modemV32Ones st') of
                (True, Just (near, true, nT, total, live), Just (ones, bits)) ->
                  (t, near, true, nT, total, live, ones, bits) : truth
                _ -> truth
              syms' = case (rcSyms rc, modemV32Syms st') of
                (True, Just ss) | not (null ss) -> (t, ss) : syms
                _ -> syms
              power' = case every of
                Just k | i `mod` k == 0, Just pw <- modemV32Power st' -> (t, modemPhase st', pw) : power
                _ -> power
              inData = case modemV32Truth st' of { Just _ -> True; Nothing -> False }
              dataAt' = if dataAt < 0 && inData then i else dataAt
              wantTaps = (dataAt < 0 && inData) || (dataAt >= 0 && (i - dataAt) `elem` [12, 50, 250, 1000])
                         || (dataAt < 0 && not inData && case modemV32Truth st of { Nothing -> False; _ -> True })
              taps' = case (wantTaps, modemV32Taps st') of
                (True, Just ts) -> (t, modemPhase st', ts) : taps
                _ -> taps
              -- and the last start-up block: kept every block until data
              -- mode begins, then only the newest of those survives
              taps'' = if dataAt < 0 && not inData
                         then case modemV32Taps st' of
                                Just ts -> (t, modemPhase st', ts) : filter (\(_, ph, _) -> take 4 ph /= "Star") taps'
                                Nothing -> taps'
                         else taps'
          in go st' (i + 1) (bs : bytes) evs' phs' line' echo' truth' syms' power' taps'' dataAt'
