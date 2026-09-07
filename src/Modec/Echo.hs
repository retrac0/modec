{-# LANGUAGE BangPatterns #-}
-- | An echo canceller for the modes that need one.
--
-- Every mode this modem ran before V.32 is frequency duplex: the two
-- directions occupy different bands, so a modem's own signal coming back
-- at it lands where its receiver already filters, and no cancellation is
-- called for.  V.32 puts both directions on the same 1800 Hz carrier
-- across the whole band, and the returning signal is then
-- indistinguishable from the wanted one by any filter.
--
-- What returns, and from where, is worth being precise about.  /Near-end/
-- echo is our own transmit reflected by our own hybrid: loud, close, and
-- absent on a VoIP leg, where there is no hybrid because the two
-- directions are separate streams.  /Far-end/ echo is our transmit
-- reflected by the hybrid at the other end -- an ATA's FXS port, or the
-- subscriber loop at the far end of any real call -- and it is not
-- absent, because nothing at either end removes it: the far modem's own
-- canceller subtracts /its/ transmit from /its/ receiver, and our
-- reflection happens outside that loop entirely.  So this cancels the
-- far end, which is the one that survives the move to VoIP.
--
-- The filter is real and runs at the line rate rather than on a complex
-- baseband.  That matches the way "Modec.Channel" models an echo, so a
-- test can assert a number of dB rather than "it improved"; it composes
-- with everything downstream without changing any of it, since the tone
-- bank and both data pumps simply see a cleaner signal; and it needs no
-- complex-vector layer, which this codebase does not have.  The price is
-- that it does not track a frequency offset in the echo path, which a
-- long-haul FDM circuit can impose and which nothing in the simulator
-- reproduces.  That is the first thing to look for on real hardware.
module Modec.Echo
  ( EchoConfig (..)
  , defaultEchoConfig
  , EchoState
  , echoInit
  , echoPush
  , echoBlock
  , echoSetFar
  , echoErle
  , echoRefDelay
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP

data EchoConfig = EchoConfig
  { ecTaps  :: !Int      -- ^ filter length, samples
  , ecDelay :: !Int      -- ^ bulk delay to the start of the filter, samples
  , ecMu    :: !Double   -- ^ normalised LMS step, 0 to 2
  , ecLeak  :: !Double   -- ^ tap leakage per sample
  } deriving (Eq, Show)

-- | 96 taps is 12 ms at 8 kHz, which covers the dispersion a hybrid adds
-- around its bulk delay without trying to cover the delay itself: that
-- is what 'echoSetFar' is for, once the start-up has measured it.
defaultEchoConfig :: EchoConfig
defaultEchoConfig = EchoConfig
  { ecTaps = 96, ecDelay = 160, ecMu = 0.3, ecLeak = 1e-7 }

data EchoState = EchoState
  { esRef    :: !Signal   -- ^ what we have transmitted, oldest first
  , esRefEnd :: !Int      -- ^ global index just past the last transmitted sample
  , esRxAt   :: !Int      -- ^ global index of the next sample to be received
  , esDelay  :: !Int      -- ^ bulk delay in force, samples.  'ecDelay'
                          -- seeds it and 'echoSetFar' moves it, and it is
                          -- this rather than the config that the filter
                          -- reads -- otherwise 'echoSetFar' drops the taps
                          -- and retargets nothing.
  , esTaps   :: !Signal
  , esEchoP  :: !Double   -- ^ tracked power before cancellation
  , esResP   :: !Double   -- ^ and after
  }

echoInit :: EchoConfig -> EchoState
echoInit cfg = EchoState
  { esRef = VS.empty, esRefEnd = 0, esRxAt = 0
  , esDelay = ecDelay cfg
  , esTaps = VS.replicate (ecTaps cfg) 0
  , esEchoP = 0, esResP = 0 }

-- | Remember a block we have just transmitted.  Called at the end of a
-- modem step, with the audio that step produced.
echoPush :: EchoConfig -> Signal -> EchoState -> EchoState
echoPush cfg blk st = st
  { esRef = VS.drop drop_ kept
  , esRefEnd = esRefEnd st + VS.length blk }
  where
    kept = esRef st VS.++ blk
    -- keep the bulk delay, the filter, and a block of slack
    want = esDelay st + ecTaps cfg + 1024
    drop_ = max 0 (VS.length kept - want)

-- | The delay, in samples, between the newest reference sample we hold
-- and the next sample we will be asked to cancel.  A modem generates a
-- block of transmit audio only after consuming the receive block it was
-- given, so the canceller is always at least one block behind its own
-- signal, and cannot reach an echo that returns faster than that.  On a
-- VoIP leg nothing does: the near-end hybrid that would is not there.
echoRefDelay :: EchoState -> Int
echoRefDelay st = esRxAt st - esRefEnd st

-- | Point the filter at a new bulk delay.  The start-up measures the
-- round trip (NT and MT in Figure 4\/V.32) and that measurement is what
-- belongs here: the filter then has to cover only the dispersion a
-- hybrid adds around it, not the delay itself.  The taps are dropped,
-- because they described a different place on the line.
echoSetFar :: Int -> EchoState -> EchoState
echoSetFar d st = st { esDelay = max 0 d, esTaps = VS.map (const 0) (esTaps st) }

-- | Cancel our own echo out of a received block.  @adapt@ says whether
-- the far end is silent, which is the only time the taps may move: with
-- both ends transmitting, the far end's signal enters the error term and
-- drives the filter away from the echo path it is trying to learn.  The
-- start-up of Figure 4\/V.32 is built out of half-duplex periods for
-- exactly this reason, so a V.32 modem never needs to guess.
-- The filter adapts whenever it is told to; what it may not do is make
-- the signal worse.  A least-mean-squares filter adapting against a
-- reference it cannot predict wanders, and puts back a fraction of its
-- step size as noise.  On a line carrying no echo -- a four-wire VoIP
-- leg, or two modems wired together through a pair of pipes -- that
-- noise is the only thing it can produce, and at a step size that
-- converges quickly it is enough to take 9600 bit\/s apart.  It did,
-- too: two modems that had been talking cleanly started talking
-- nonsense the moment the canceller was allowed to adapt.
echoBlock :: EchoConfig -> Bool -> Signal -> EchoState -> (EchoState, Signal)
echoBlock cfg adapt rx st0 = (st', out)
  where
    n = VS.length rx
    taps = ecTaps cfg
    ref = esRef st0
    refLen = VS.length ref
    -- ref[0] is the transmitted sample with global index
    -- esRefEnd - refLen, so the sample d before received sample i is:
    refAt i d =
      let k = (esRxAt st0 + i - d) - (esRefEnd st0 - refLen)
      in if k < 0 || k >= refLen then 0 else VS.unsafeIndex ref k

    go !i !w !ep !rp acc
      | i >= n = (w, ep, rp, reverse acc)
      | otherwise =
          let xs = VS.generate taps (\k -> refAt i (esDelay st0 + k))
              y = VS.sum (VS.zipWith (*) w xs)
              d = VS.unsafeIndex rx i
              e = d - y
              nrm = VS.sum (VS.map (\v -> v * v) xs)
              g = if adapt && nrm > 1e-12 then ecMu cfg * e / nrm else 0
              lk = 1 - ecLeak cfg
              w' = if adapt
                     then VS.zipWith (\wk xk -> lk * wk + g * xk) w xs
                     else w
              ep' = 0.99 * ep + 0.01 * (d * d)
              rp' = 0.99 * rp + 0.01 * (e * e)
              -- Subtract only while subtracting is measurably helping,
              -- and decide that per sample rather than per block, so the
              -- answer does not depend on where the audio was cut.
              helping = ep' > 1e-18 && rp' < 0.95 * ep'
          in go (i + 1) w' ep' rp' ((if helping then e else d) : acc)

    (w1, ep1, rp1, outs) = go 0 (esTaps st0) (esEchoP st0) (esResP st0) []
    out = VS.fromList outs
    st' = st0 { esTaps = w1, esEchoP = ep1, esResP = rp1
              , esRxAt = esRxAt st0 + n }

-- | Echo return loss enhancement, in dB: how much of what arrived has
-- been taken out.  Traced so a test can assert it does not regress.
echoErle :: EchoState -> Double
echoErle st
  | esResP st <= 0 || esEchoP st <= 0 = 0
  | otherwise = 10 * logBase 10 (esEchoP st / esResP st)
