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
import Modec.Echo (EchoConfig (..))
import Modec.V22 (rxEvmEstimate, rxSpsEstimate)

data ReplayConfig = ReplayConfig
  { rcModem :: ModemConfig
  , rcBlock :: Double        -- ^ block length in seconds, as the live modem uses
  , rcLimit :: Maybe Double  -- ^ stop after this many seconds of recording
  , rcEvery :: Maybe Double  -- ^ sample the receiver this often, for 'rrLine'
  }

-- | Twenty millisecond blocks, the whole file, no line trace.
defaultReplayConfig :: ModemConfig -> ReplayConfig
defaultReplayConfig cfg = ReplayConfig cfg 0.02 Nothing Nothing

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
  }

replay :: ReplayConfig -> Signal -> ReplayResult
replay rc x = go (modemInit cfg) 0 [] [] [] [] []
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
    go st i bytes evs phs line echo
      | i * blk >= limit =
          ReplayResult (concat (reverse bytes)) (reverse evs) (reverse phs) (reverse line) (reverse echo)
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
          in go st' (i + 1) (bs : bytes) evs' phs' line' echo'
