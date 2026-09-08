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
  , echoSearch
  , echoAim
  , echoDelay
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
  , ecSearch :: !Int     -- ^ how far back to look for the echo, samples
  , ecPre   :: !Int      -- ^ how far in front of the peak the filter starts
  , ecPeak  :: !Double   -- ^ peak to mean a correlation must beat to be believed
  , ecOnRatio :: !Double -- ^ predicted echo over received power worth subtracting
  } deriving (Eq, Show)

-- | 256 taps is 32 ms at 8 kHz -- far wider than the few milliseconds a
-- hybrid smears its return over, because the bulk delay is not known
-- nearly as well as it looks.  See 'echoSetFar'.
-- The search reaches 500 ms because a real one does.  Dialling the
-- voip.ms echo test, which returns everything it is sent, put our own
-- signal back at 116 ms: a filter spanning 20 to 52 ms -- which is what
-- ecDelay and ecTaps came to on their own -- never had a chance at it.
-- 4000 samples of reference history is 32 kB, which is not worth being
-- clever about.
defaultEchoConfig :: EchoConfig
defaultEchoConfig = EchoConfig
  { ecTaps = 256, ecDelay = 160, ecMu = 0.3, ecLeak = 1e-7
  , ecSearch = 4000, ecPre = 64, ecPeak = 4, ecOnRatio = 0.01 }

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
  , esEchoY  :: !Double   -- ^ and how much of it the filter predicts
  , esOn     :: !Bool     -- ^ whether subtracting is worth doing
  , esRxHist :: !Signal   -- ^ recent received audio, for the delay search
  , esFound  :: !(Maybe Int)  -- ^ the delay the search settled on
  }

echoInit :: EchoConfig -> EchoState
echoInit cfg = EchoState
  { esRef = VS.empty, esRefEnd = 0, esRxAt = 0
  , esDelay = ecDelay cfg
  , esTaps = VS.replicate (ecTaps cfg) 0
  , esEchoP = 0, esResP = 0, esEchoY = 0, esOn = False
  , esRxHist = VS.empty, esFound = Nothing }

-- | Remember a block we have just transmitted.  Called at the end of a
-- modem step, with the audio that step produced.
echoPush :: EchoConfig -> Signal -> EchoState -> EchoState
echoPush cfg blk st = st
  { esRef = VS.drop drop_ kept
  , esRefEnd = esRefEnd st + VS.length blk }
  where
    kept = esRef st VS.++ blk
    -- the bulk delay, the filter, whatever the search may reach for,
    -- and a block of slack
    want = max (esDelay st + ecTaps cfg) (ecSearch cfg) + 1024
    drop_ = max 0 (VS.length kept - want)

-- | The delay, in samples, between the newest reference sample we hold
-- and the next sample we will be asked to cancel.  A modem generates a
-- block of transmit audio only after consuming the receive block it was
-- given, so the canceller is always at least one block behind its own
-- signal, and cannot reach an echo that returns faster than that.  On a
-- VoIP leg nothing does: the near-end hybrid that would is not there.
echoRefDelay :: EchoState -> Int
echoRefDelay st = esRxAt st - esRefEnd st

-- | Point the filter at a bulk delay derived from the measured round
-- trip, and start its taps again, because they described a different
-- place on the line.
--
-- The window reaches /back/ from the measurement rather than sitting on
-- it, and that is not slack for its own sake.  NT and MT time the far
-- modem's answer, and an answer contains that modem's own processing
-- delay; the reflection off its hybrid does not, so the echo returns
-- sooner than the measurement says -- by however long the far end takes
-- to respond, which is nothing this end can know.  Aiming the filter at
-- the measurement therefore looks straight past the echo: with the round
-- trip at 320 samples and a hybrid 200 samples away, every tap sits
-- behind the thing it is meant to cancel.
--
-- It never reaches back past 'ecDelay', because a modem produces its
-- transmit block only after consuming the receive block: the reference
-- is always at least one block old, and asking for less than that gets
-- zeros -- silently, and differently for different block sizes.
echoSetFar :: Int -> EchoState -> EchoState
echoSetFar d st = st
  { esDelay = max (esDelay st) (d - 3 * n `div` 4)
  , esTaps = VS.map (const 0) (esTaps st) }
  where n = VS.length (esTaps st)

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

    go !i !w !ep !rp !yp !on acc
      | i >= n = (w, ep, rp, yp, on, reverse acc)
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
              yp' = 0.99 * yp + 0.01 * (y * y)
              -- Whether to subtract is decided only while the far end is
              -- silent, and held the rest of the time.
              --
              -- That is the one moment the question can be answered.
              -- With the far end talking, the received signal is mostly
              -- its signal, and cancelling even a perfect -14 dB echo
              -- only takes the total power down to 0.96 of what came in
              -- -- indistinguishable from noise on the measurement.  With
              -- the far end quiet, what arrives /is/ the echo, and
              -- cancelling it drives the residual down by tens of dB.
              -- Figure 4's half-duplex windows exist so that an echo
              -- canceller can train; they are equally the only place it
              -- can find out whether it has.
              --
              -- and it is asked again every block rather than once.
              -- What was here decided only while adapting, which is to
              -- say once, in a training window lasting half a second --
              -- and then held that answer for the rest of a call that
              -- may run for twenty minutes over a path whose delay
              -- moves every time a jitter buffer resizes.  A filter
              -- that was right when it was asked and is wrong now goes
              -- on subtracting either way.
              --
              -- The second test is the one that works while both ends
              -- are talking: y is what the filter thinks the echo is,
              -- so comparing it against what actually arrived measures
              -- the echo's share of the line directly.  A filter
              -- reaching empty line predicts nothing -- leakage pulls
              -- an unexcited filter to zero -- and says so.
              --
              -- The predicted-echo rule applies only once the search has
              -- found something.  Dialled at an echo test that turned
              -- out not to reflect, the filter sat at its default delay
              -- adapting on a far end that was talking, grew taps out of
              -- gradient noise, and that rule read their output as an
              -- echo worth removing: measured, a return loss of minus
              -- 0.8 dB, which is a canceller making the line worse than
              -- it found it.  A filter that has not been aimed at
              -- anything has no business predicting anything, and the
              -- half-duplex rule -- which measures the residual against
              -- what arrived, and is only valid while the far end is
              -- quiet -- is the only evidence left in that case.
              aimed = esFound st0 /= Nothing
              on' | ep' <= 1e-18 = on
                  | adapt, rp' < 0.5 * ep' = True
                  | adapt, rp' > 0.9 * ep' = False
                  | aimed, yp' > ecOnRatio cfg * ep' = True
                  | aimed, yp' < 0.25 * ecOnRatio cfg * ep' = False
                  | not aimed = False
                  | otherwise = on
          in go (i + 1) w' ep' rp' yp' on' ((if on' then e else d) : acc)

    (w1, ep1, rp1, yp1, on1, outs) = go 0 (esTaps st0) (esEchoP st0) (esResP st0) (esEchoY st0) (esOn st0) []
    out = VS.fromList outs
    st' = st0 { esTaps = w1, esEchoP = ep1, esResP = rp1, esEchoY = yp1, esOn = on1
              , esRxAt = esRxAt st0 + n
              , esRxHist = keepTail (ecSearch cfg `div` 2) (esRxHist st0 VS.++ rx) }

-- | Where the echo is, by looking for it.
--
-- Aiming the filter at a delay chosen in advance is what the bulk delay
-- did, and it works exactly as long as the guess does.  A four-wire VoIP
-- leg put the reflection back at 116 ms, which is not near any number
-- worth guessing; the round trip a modem measures for itself is no help
-- either, because NT and MT time the far modem's /turnaround/, which is
-- its processing delay as much as the line's.  So measure the thing
-- itself: our own transmit is known exactly, and where it reappears in
-- what arrives is where the echo is.
--
-- On the waveform, and not on its envelope.  The envelope is the
-- cheaper thing to correlate and it is useless here: the signal the
-- half-duplex windows offer is TRN, whose four states all have the same
-- magnitude, so its envelope is nearly flat and carries almost no
-- structure to match.  The waveform carries all of it.  A coherent
-- correlation of two signals centred on 1800 Hz does have a sidelobe
-- every carrier period, but those sit within half a millisecond of the
-- true peak and the filter reaches 'ecPre' in front of wherever it is
-- aimed, so being a carrier period out costs nothing.
--
-- Returns the lag in samples and the peak-to-mean ratio that justified
-- it.  'Nothing' when nothing stands out, which is the answer on a leg
-- with no echo on it and has to stay the answer: a canceller that
-- believes a noise peak subtracts a signal that was never there.
echoSearch :: EchoConfig -> EchoState -> Maybe (Int, Double)
echoSearch cfg st
  | VS.length rxW < 256 = Nothing
  | null scores = Nothing
  | best > ecPeak cfg * avg, avg > 0
  , best > 2 * rival = Just (bestLag, best / avg)
  | otherwise = Nothing
  where
    ref = esRef st
    refLen = VS.length ref
    -- the most recent stretch of what arrived
    rxW = keepTail 1000 (esRxHist st)
    w = VS.length rxW
    rxMean = VS.sum rxW / fromIntegral w
    rxC = VS.map (subtract rxMean) rxW
    rxNorm = sqrt (VS.sum (VS.map (\v -> v * v) rxC))
    rxFrom = esRxAt st - w
    startOf l = (rxFrom - l) - (esRefEnd st - refLen)
    -- an echo cannot come back sooner than our own transmit is old
    lo = max 0 (esRxAt st - esRefEnd st)
    lags = [ l | l <- [lo .. ecSearch cfg]
               , let o = startOf l, o >= 0, o + w <= refLen ]
    scores = [ (score l, l) | l <- lags ]
    score l =
      let o = startOf l
          e = VS.slice o w ref
          m = VS.sum e / fromIntegral w
          c = VS.map (subtract m) e
          nrm = sqrt (VS.sum (VS.map (\v -> v * v) c))
      in if nrm <= 0 || rxNorm <= 0 then 0
         else abs (VS.sum (VS.zipWith (*) rxC c)) / (nrm * rxNorm)
    best = maximum (map fst scores)
    bestLag = snd (head [ p | p <- scores, fst p == best ])
    avg = sum (map fst scores) / fromIntegral (length scores)
    -- The best peak anywhere but next to the winner.  A reflection is
    -- one place on the line and correlates nowhere else; a signal that
    -- repeats correlates with itself at every multiple of its period,
    -- and the first two segments of the conditioning signal alternate
    -- two states and so repeat every two symbols.  Searching over those
    -- finds a confident answer at a delay set by arithmetic rather than
    -- by the line, aims the filter there, and drops the taps that were
    -- converging.  Comparing against the mean does not catch it -- a
    -- comb of peaks lifts the mean too -- and this does.
    rival = maximum (0 : [ c | (c, l) <- scores, abs (l - bestLag) > 160 ])

-- | Point the filter at a delay the search found, reaching 'ecPre' in
-- front of it so the leading edge of the reflection is inside the span.
echoAim :: EchoConfig -> Int -> EchoState -> EchoState
echoAim cfg l st = st
  { esDelay = max (ecDelay cfg) (l - ecPre cfg)
  , esTaps = VS.map (const 0) (esTaps st)
  , esFound = Just l }

-- | The delay the search settled on, for tracing.
echoDelay :: EchoState -> Maybe Int
echoDelay = esFound

-- | Echo return loss enhancement, in dB: how much of what arrived has
-- been taken out.  Traced so a test can assert it does not regress.
keepTail :: Int -> Signal -> Signal
keepTail k x = VS.drop (max 0 (VS.length x - k)) x

echoErle :: EchoState -> Double
echoErle st
  | esResP st <= 0 || esEchoP st <= 0 = 0
  | otherwise = 10 * logBase 10 (esEchoP st / esResP st)
