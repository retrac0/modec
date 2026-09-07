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
    -- ^ time, the V.22 receiver's decision error and its samples per
    -- symbol, whenever 'rcEvery' asks and the receiver exists
  }

replay :: ReplayConfig -> Signal -> ReplayResult
replay rc x = go (modemInit cfg) 0 [] [] [] []
  where
    cfg = rcModem rc
    fs = mcRate cfg
    blk = max 1 (round (fs * rcBlock rc)) :: Int
    limit = maybe (VS.length x) (\s -> min (VS.length x) (round (s * fs))) (rcLimit rc)
    every = fmap (\s -> max 1 (round (s / rcBlock rc)) :: Int) (rcEvery rc)
    secs i = fromIntegral (i * blk) / fs
    go st i bytes evs phs line
      | i * blk >= limit =
          ReplayResult (concat (reverse bytes)) (reverse evs) (reverse phs) (reverse line)
      | otherwise =
          let n = min blk (limit - i * blk)
              (st', _, bs, es) = modemStep cfg st (VS.slice (i * blk) n x) []
              t = secs i
              phs' = if modemPhase st' /= modemPhase st then (t, modemPhase st') : phs else phs
              evs' = [ (t, e) | e <- reverse es ] ++ evs
              line' = case (every, fst (modemV22Rx st')) of
                (Just k, Just (_, r)) | i `mod` k == 0 ->
                  (t, rxEvmEstimate r, rxSpsEstimate r) : line
                _ -> line
          in go st' (i + 1) (bs : bytes) evs' phs' line'
