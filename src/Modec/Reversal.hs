{-# LANGUAGE BangPatterns #-}
-- | Timestamping the phase reversals V.32's start-up is built from.
--
-- This lived in "Modec.QAM", where it was a fifth of the module and had
-- nothing to do with a QAM receiver: it is a sliding coherent
-- correlation against a steady tone, used only by "Modec.V32Start" and
-- its retrain listener.  Moving it out is what makes the receiver
-- readable as one thing.
module Modec.Reversal
  ( RevTracker
  , revInit
  , revRearm
  , revBlock
  , revLevel
  , revPower
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal)

-- | Finds the instant a steady tone reverses phase, to the sample.
--
-- The whole of V.32's start-up turns on phase reversals: the calling
-- modem's AA and CC are both a steady 1800 Hz tone and differ only by
-- 180 degrees, and the answering modem's AC and CA are the same trick at
-- 600 and 3000 Hz.  A magnitude tone bank cannot see any of it -- the
-- amplitude is identical either side of the event -- which is why V.32
-- signal detection does not go through "Modec.Detect" and why no 1800 Hz
-- bin is added to it.
--
-- Sample accuracy is not a refinement here but a requirement: §5.4.1
-- fixes the turnaround from hearing a reversal to sending one at
-- 64 +/- 2 symbol periods, which at 2400 baud is 26.67 +/- 0.83 ms,
-- and the handshake state machine only runs every 20 ms.  So the
-- receiver timestamps the event and the transmitter is given a sample
-- index to act on, rather than the tick being asked to do something it
-- cannot.
--
-- The method is a sliding coherent correlation against the tone.  Its
-- projection onto the phase established before the event runs from
-- strongly positive to strongly negative, and the crossing, interpolated
-- between the two straddling samples, is the reversal.
data RevTracker = RevTracker
  { rtW      :: !Double          -- ^ radians per sample at the tone
  , rtN      :: !Int             -- ^ global sample index
  , rtWin    :: !Int
  , rtHist   :: [(Double, Double)]  -- ^ recent mixed samples, newest first
  , rtAcc    :: !(Double, Double)   -- ^ running sum over the window
  , rtRef    :: !(Maybe (Double, Double))  -- ^ phase before the event
  , rtProj   :: !Double
  , rtLevel  :: !Double
  , rtPow    :: !Double          -- ^ tracked mean square of the input
  , rtAge    :: !Int             -- ^ samples since the tracker was armed
  , rtSeen   :: !Bool            -- ^ the tone has been steady in this phase
  , rtHold   :: !Int             -- ^ samples to wait before reporting again
  }

revInit :: Double -> Double -> RevTracker
revInit fs f = RevTracker
  { rtW = 2 * pi * f / fs
  , rtN = 0
  -- A fixed 5 ms, not a fixed number of cycles.  Sizing the window by
  -- the tone's own period gives the high tones too little frequency
  -- resolution to reject the low ones: three cycles of 3000 Hz is 8
  -- samples, over which a 2100 Hz answer tone does not average away at
  -- all, and the tracker reads it as its own.
  , rtWin = max 8 (round (fs / 200))
  , rtHist = [], rtAcc = (0, 0)
  , rtRef = Nothing, rtProj = 0, rtLevel = 0, rtPow = 0, rtAge = 0, rtSeen = False, rtHold = 0 }

-- | Forget what has been heard so far, but not what time it is.
--
-- The start-up hands a tracker a different signal several times over,
-- and the phase reference it established for the last one is worse than
-- useless for the next.  The sample counter has to survive, though: the
-- round trip is the difference between two reversal timestamps taken
-- either side of a re-arm, and restarting the clock between them
-- measures a negative delay.
revRearm :: RevTracker -> RevTracker
revRearm t = t
  { rtHist = [], rtAcc = (0, 0), rtRef = Nothing
  , rtProj = 0, rtLevel = 0, rtPow = 0, rtAge = 0, rtSeen = False, rtHold = 0 }

-- | How much of what is arriving is this tone, from 0 to about 0.71.
--
-- The measurement is the coherent correlation divided by the signal's
-- own root mean square, which is the only form of it that means
-- anything: an absolute threshold says \"this is loud\", and at any
-- realistic signal to noise ratio noise alone will clear it.  A single
-- tone reads about 0.71, either sideband of the alternating AC signal
-- about 0.5, and white noise about one over the square root of the
-- window length -- around 0.16 here.
revLevel :: RevTracker -> Double
revLevel = rtLevel

-- | The mean square of what the tracker is listening to.
--
-- 'revLevel' divides by this, so on a line with nothing on it the ratio
-- is noise over noise and can read anything at all.  Anyone using a
-- level as evidence that a particular tone is present has to check
-- there is a signal to have a tone in.
revPower :: RevTracker -> Double
revPower = rtPow

-- | Feed a block; returns the global sample indices at which the tone
-- reversed phase.
revBlock :: Signal -> RevTracker -> (RevTracker, [Int])
revBlock chunk st0 = go 0 st0 []
  where
    n = VS.length chunk
    go !i st acc
      | i >= n = (st, reverse acc)
      | otherwise =
          let t = rtN st
              v = VS.unsafeIndex chunk i
              c = cos (rtW st * fromIntegral t)
              sn = sin (rtW st * fromIntegral t)
              p = (v * c, negate v * sn)
              hist' = take (rtWin st) (p : rtHist st)
              (ar, ai) = foldl (\(x, y) (a, b) -> (x + a, y + b)) (0, 0) hist'
              mag = sqrt (ar * ar + ai * ai) / fromIntegral (rtWin st)
              pow = 0.995 * rtPow st + 0.005 * (v * v)
              -- until there is something on the line at all, the ratio
              -- is meaningless rather than large: an empty line divided
              -- by an empty line must not read as a tone
              lvl = if pow > 1e-12
                      then 0.98 * rtLevel st + 0.02 * (mag / sqrt pow)
                      else 0
              -- everything below is gated on the band actually holding
              -- this tone, not on the line being loud
              -- A level is a correlation over the tracked mean square of
              -- the input, and that average starts at nothing, so for
              -- the first few milliseconds after the tracker is armed it
              -- divides by almost zero and reads high whatever is on the
              -- line.  Measured over a settled 50 ms window the
              -- separation is not close -- the alternating pair reads
              -- 0.32 and 0.63 on its two sidebands, and data, TRN and a
              -- rate signal all read 0.07 or less -- so the threshold is
              -- not the difficulty; the warm-up is.  A retrain builds
              -- fresh trackers, and without this the first blocks of the
              -- far end's data counted as the answering modem's tone,
              -- which took this end through AA and CC and into the
              -- silence of 5.4.1's fifth paragraph, where it stopped
              -- transmitting the very signal 5.5.2 needs to see.
              tone = rtAge st >= 4 * rtWin st && lvl > 0.25 && pow > 1e-10
              full = length hist' >= rtWin st
              -- the phase to measure against: whatever was established
              -- before, adopted once the tone is steady
              ref = case rtRef st of
                Just r | rtHold st > 0 -> Just r
                Just r -> Just r
                Nothing | full && tone -> Just (ar / (mag * fromIntegral (rtWin st)), ai / (mag * fromIntegral (rtWin st)))
                _ -> Nothing
              proj = case ref of
                Just (rr, ri) -> (ar * rr + ai * ri) / fromIntegral (rtWin st)
                Nothing -> 0
              -- The correlation does not step from one phase to the other:
              -- the window slides across the event over its own length,
              -- so the projection walks down through zero.  The event is
              -- that crossing, and it only counts if the tone had been
              -- steady in the old phase first -- which is what rtSeen
              -- records, and what stops noise from ringing the bell.
              seen = rtSeen st || (full && tone && proj > 0.5 * mag)
              crossed = full && rtHold st == 0 && seen && tone
                        && rtProj st > 0 && proj <= 0
              st1 = st { rtN = t + 1, rtHist = hist', rtAcc = (ar, ai)
                       , rtRef = if crossed then Nothing else ref
                       , rtProj = proj, rtLevel = lvl, rtPow = pow
                       , rtAge = rtAge st + 1
                       , rtSeen = not crossed && seen && tone
                       , rtHold = if crossed then rtWin st * 2 else max 0 (rtHold st - 1) }
          in if crossed
               -- the crossing lies between this sample and the last;
               -- the correlation is linear across it, so interpolate
               -- Interpolate between the two samples that straddle the
               -- crossing.  Clamped: when both projections are close to
               -- zero the ratio is numerically meaningless and would
               -- place the event anywhere at all.
               then let d = rtProj st - proj
                        frac = if abs d < 1e-18 then 0.5
                               else max 0 (min 1 (rtProj st / d))
                        at = t - rtWin st `div` 2 + round frac
                    in go (i + 1) st1 (at : acc)
               else go (i + 1) st1 acc
