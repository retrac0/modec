-- | Keeping the transmit cushion through a PipeWire stream's hiccups.
--
-- The live loop writes exactly one block of transmit for each block of
-- capture it reads, on top of a cushion of silence laid down once when
-- the audio starts.  That holds the playback buffer level only for as
-- long as the capture stream delivers every sample the graph clock
-- produces.  It does not.  Measured on the bench (2026-09-16), each time
-- baresip connected or dropped its streams at the start or end of a SIP
-- call, capture came up 32 ms short, and the playback stream went on
-- consuming at the graph's rate regardless.  Every such event took 32 ms
-- out of the 100 ms cushion.  After the third, pw-top counted an underrun
-- on modec-tx about once a second for the rest of the process, and each
-- one is a hole in the carrier: a far end that misses S1 in it settles
-- for V.22 while we wait for 2400, and one that is already in data mode
-- reads the hole as a start bit.  That is why one modec process carried
-- two calls and failed the third (bench test C9).
--
-- Nothing in pw-cat reports how full its buffer is, so the level is
-- inferred.  Capture samples read, set against the monotonic clock, fall
-- back by exactly what a hiccup lost; what we have written tracks what
-- we have read, so the same figure is the playback level's.
--
-- It is only known at one kind of moment.  Capture arrives in bursts the
-- size of the graph quantum -- 256 ms of it at a time on the bench -- and
-- the loop works through a burst block by block with the modem running
-- between reads.  A read that had to wait found the pipe empty: every
-- sample delivered before it had been read, and the instant it returned
-- is the instant the next burst landed.  Samples read before such a
-- read, against the time it returned, is the capture clock exactly, less
-- whatever part of a block was left waiting -- and that remainder cycles
-- through a handful of values, so the highest of the last few arrivals
-- is the clock itself.  When that falls a step below where it settled,
-- the step is written back as silence.
--
-- Nothing else is measured, and two earlier ways of doing it are why.
-- Stamping every block as it was read let the figure sag by a burst's
-- processing time whenever the modem's load rose, and the keeper topped
-- up a carrier mid-call for a loss that never happened.  Stamping every
-- block of a burst with the burst's arrival fixed that and broke
-- differently: it is right only while the loop finishes one burst before
-- the next lands.  At 14400 the pump took thirteen of every twenty
-- milliseconds, a burst of thirteen blocks took most of its 256 ms, and
-- when it took all of them no read waited -- so blocks of the next burst
-- were set against the last arrival the loop had seen, the level read
-- high by however long that went on, the settled level followed it up,
-- and when the loop caught up the level "fell" by what it had never
-- gained.  Measured on the bench (2026-10-04): 12 to 44 ms of silence
-- written into the middle of a 14400 carrier, four calls in twenty.  The
-- echo of our own signal then came back that much later than the
-- canceller's taps were set for, the receiver went from 28 dB to 17, and
-- the call retrained and died.  The recordings show the echo's delay
-- stepping by exactly the silence added, which is what no loss at
-- capture can do.
--
-- A figure taken only at arrivals cannot read high: the samples counted
-- were all delivered by then.  It can only be missing, when the loop is
-- too busy for any read to wait, and then the keeper does nothing --
-- which is the right thing to do with no measurement.
--
-- Two things move the figure that are not losses.  The graph clock and
-- the monotonic clock drift apart by some parts per million, which the
-- settled level follows slowly rather than topping up for.  And capture
-- can come up long.  Sometimes that is a burst early and the next one
-- short by the same, which changes nothing; sometimes it is latency that
-- stays.  Either way the settled level only drifts towards it, so a gain
-- and the loss that cancels it are both let pass, and a gain that lasts
-- becomes the new normal within a minute.  Nothing is ever taken back out
-- of the transmit to shed latency: that would be a hole of our own
-- making.
module Modec.Cushion
  ( Cushion
  , CushionParams (..)
  , defaultCushionParams
  , cushionInit
  , cushionStep
  , cushionLevel
  ) where

-- | How the keeper decides.
data CushionParams = CushionParams
  { cpSettle    :: !Double  -- ^ seconds after the start before the level is trusted
  , cpWindow    :: !Double  -- ^ seconds the peak is taken over, at the least
  , cpStep      :: !Double  -- ^ seconds short that count as a loss rather than drift
  , cpFollow    :: !Double  -- ^ time constant, in seconds, of following drift
  , cpWaited    :: !Double  -- ^ a read this long had to wait: a burst arrived when it returned
  , cpEnough    :: !Int     -- ^ arrivals the window must hold before its peak is believed
  }

-- | A 12 ms step.  The losses measured were 16 to 37 ms.
--
-- Five arrivals, because the part of a block left waiting in the pipe
-- when a burst lands takes the level down by up to a block, and with
-- 2048-sample bursts read 160 at a time it comes round every fifth.
defaultCushionParams :: CushionParams
defaultCushionParams = CushionParams
  { cpSettle = 2.0, cpWindow = 1.5, cpStep = 0.012, cpFollow = 30, cpWaited = 0.002, cpEnough = 5 }

-- | The keeper's state.  Times are the caller's monotonic seconds.
data Cushion = Cushion
  { cuT0      :: !Double
  , cuRead    :: !Int                    -- ^ samples read since the start
  , cuAdded   :: !Int                    -- ^ samples of silence added since the start
  , cuPeaks   :: [(Double, Double)]      -- ^ (time, level) at the arrivals inside the window, newest first
  , cuBase    :: !(Maybe Double)         -- ^ the settled level, once there is one
  , cuLastT   :: !Double                 -- ^ when the last burst arrived
  , cuGap     :: !Double                 -- ^ the shortest time between two arrivals: one burst
  }

cushionInit :: Double -> Cushion
cushionInit t0 = Cushion t0 0 0 [] Nothing t0 0

-- | The level estimate in seconds, relative to where it settled: zero
-- when the playback buffer holds what it held then, negative when it
-- holds less.  'Nothing' before it has settled.
cushionLevel :: Cushion -> Maybe Double
cushionLevel c = fmap (\b -> peak c - b) (cuBase c)

peak :: Cushion -> Double
peak c = case cuPeaks c of
  [] -> 0
  ps -> maximum (map snd ps)

-- | A block of this many samples has just been read: the read began at
-- the first time and returned at the second.  Returns how many samples
-- of silence to write to the playback stream now, beyond the block's own,
-- and the new state.
cushionStep :: CushionParams -> Double -> Double -> Double -> Int -> Cushion -> (Int, Cushion)
cushionStep p fs began returned n c0
  -- it was already there: no burst arrived, and there is nothing to measure
  | returned - began < cpWaited p = (0, c0 { cuRead = cuRead c0 + n })
  | otherwise = (add, c3)
  where
    -- what had been delivered before this burst, against when it landed
    level = fromIntegral (cuRead c0 + cuAdded c0) / fs - (returned - cuT0 c0)
    dt = max 0 (returned - cuLastT c0)
    -- One burst, as the shortest gap between arrivals: a loop that is
    -- behind skips arrivals and so only ever makes the gap longer.  It
    -- is let creep up, so a graph that changes its quantum is followed.
    -- The window is long enough to hold the arrivals it needs.
    gap | cuGap c0 <= 0 = dt
        | dt > 0 = min (1.01 * cuGap c0) dt
        | otherwise = cuGap c0
    window = max (cpWindow p) (fromIntegral (cpEnough p + 1) * gap)
    peaks = (returned, level) : takeWhile ((> returned - window) . fst) (cuPeaks c0)
    c1 = c0 { cuRead = cuRead c0 + n, cuPeaks = peaks, cuLastT = returned, cuGap = gap }
    pk = peak c1
    -- Too few arrivals is a loop too busy to be waiting on its reads,
    -- and a peak over the ones it did make may be a whole remainder low.
    enough = length peaks >= cpEnough p
    (add, c3) = case cuBase c1 of
      Nothing
        | returned - cuT0 c1 >= cpSettle p, enough -> (0, c1 { cuBase = Just pk })
        | otherwise -> (0, c1)
      Just b
        | not enough -> (0, c1)
        | b - pk >= cpStep p ->
            -- a loss: write it back, and count it in every level the
            -- window still holds, so the same step is not written twice
            let k = round ((b - pk) * fs) :: Int
                s = fromIntegral k / fs
            in (k, c1 { cuAdded = cuAdded c1 + k
                      , cuPeaks = [ (t, l + s) | (t, l) <- cuPeaks c1 ] })
        | otherwise ->
            let a = min 1 (dt / cpFollow p)
            in (0, c1 { cuBase = Just (b + a * (pk - b)) })
