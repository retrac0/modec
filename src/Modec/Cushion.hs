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
-- we have read, so the same figure is the playback level's.  Reads come
-- in bursts the size of the graph quantum, which makes the instantaneous
-- figure a sawtooth a quarter of a second deep, but its peak over a
-- second or so is steady, provided every block is timed by when its
-- burst arrived and not by when the loop got round to reading it.  The
-- loop runs the modem between reads, and a burst of twelve blocks can
-- take a hundred milliseconds to work through, more with a V.32 call up:
-- stamped as read, the peak wandered by ten milliseconds with the load,
-- and a keeper watching it topped up a carrier mid-call for losses that
-- never happened.  A read that had to wait is the arrival of a burst; a
-- read that did not wait was already there when it arrived.  When the
-- peak falls a step below where it settled, the step is written back as
-- silence.
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
  , cpWindow    :: !Double  -- ^ seconds the peak is taken over; longer than a burst
  , cpStep      :: !Double  -- ^ seconds short that count as a loss rather than drift
  , cpFollow    :: !Double  -- ^ time constant, in seconds, of following drift
  , cpWaited    :: !Double  -- ^ a read this long had to wait: a burst arrived when it returned
  }

-- | A 12 ms step.  The losses measured were 16 to 37 ms.
defaultCushionParams :: CushionParams
defaultCushionParams = CushionParams
  { cpSettle = 2.0, cpWindow = 1.5, cpStep = 0.012, cpFollow = 30, cpWaited = 0.002 }

-- | The keeper's state.  Times are the caller's monotonic seconds.
data Cushion = Cushion
  { cuT0      :: !Double
  , cuRead    :: !Int                    -- ^ samples read since the start
  , cuAdded   :: !Int                    -- ^ samples of silence added since the start
  , cuPeaks   :: [(Double, Double)]      -- ^ (time, level) inside the window, newest first
  , cuBase    :: !(Maybe Double)         -- ^ the settled level, once there is one
  , cuLastT   :: !Double
  , cuArrived :: !Double                 -- ^ when the burst being read arrived
  }

cushionInit :: Double -> Cushion
cushionInit t0 = Cushion t0 0 0 [] Nothing t0 t0

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
cushionStep p fs began returned n c0 = (add, c3)
  where
    arrived = if returned - began >= cpWaited p then returned else cuArrived c0
    rd = cuRead c0 + n
    level = fromIntegral (rd + cuAdded c0) / fs - (arrived - cuT0 c0)
    peaks = (returned, level) : takeWhile ((> returned - cpWindow p) . fst) (cuPeaks c0)
    c1 = c0 { cuRead = rd, cuPeaks = peaks, cuLastT = returned, cuArrived = arrived }
    pk = peak c1
    dt = max 0 (returned - cuLastT c0)
    (add, c3) = case cuBase c1 of
      Nothing
        | returned - cuT0 c1 >= cpSettle p -> (0, c1 { cuBase = Just pk })
        | otherwise -> (0, c1)
      Just b
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
