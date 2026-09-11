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
  , echoConfigAt
  , EchoState
  , echoInit
  , echoPush
  , echoBlock
  , echoBlockData
  , echoSetFar
  , echoSearch
  , echoAim
  , echoDelay
  , echoErle
  , echoRefDelay
  , echoDebug
  , echoDataState
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
    -- The data-mode search and adaptation: see 'echoBlockData'.
  , ecFarSearch :: !Int  -- ^ how far back the data-mode scan reaches, samples; 0 turns data mode off
  , ecFarWindow :: !Int  -- ^ the scan's correlation window, samples
  , ecFarSlice  :: !Int  -- ^ lags the scan scores per block
  , ecDataMu    :: !Double -- ^ normalised LMS step while the far end talks; 0 leaves the taps to the scan's estimate
  , ecDataOn    :: !Double -- ^ predicted echo over received power worth subtracting, with the far end talking
  } deriving (Eq, Show)

-- | The configuration for a sample rate.  32 ms of taps is far wider
-- than the few milliseconds a hybrid smears its return over, because
-- the bulk delay is not known nearly as well as it looks; see
-- 'echoSetFar'.  The search reaches 500 ms because a real one does.
-- Dialling the voip.ms echo test, which returns everything it is sent,
-- put our own signal back at 116 ms: a filter spanning 20 to 52 ms --
-- which is what ecDelay and ecTaps came to on their own -- never had a
-- chance at it.  Half a second of reference history is 32 kB at 8 kHz,
-- which is not worth being clever about.
--
-- These were 256, 160, 4000 and 64 samples, which are those times at
-- 8 kHz and nothing in particular at any other rate.
echoConfigAt :: Double -> EchoConfig
echoConfigAt fs = EchoConfig
  { ecTaps = ms 32, ecDelay = ms 20, ecMu = 0.3, ecLeak = 1e-7
  , ecSearch = ms 500, ecPre = ms 8, ecPeak = 4, ecOnRatio = 0.01
  -- Measured through an HT802V2 and baresip: the reflection of our own
  -- data comes back 644 ms after it went out, 34 dB down, and it does
  -- not move for the length of a call.  900 ms reaches it with room.
  -- A two-second window is what lets a reflection 22 dB under the far
  -- end's signal clear the rival test -- the correlation's noise floor
  -- goes down with the square root of the window, and at one second a
  -- -22 dB echo was a coin toss against it.  Ninety-six lags a block
  -- is a scan every second and a half at a cost the block can afford.
  --
  -- Forty-eight, not ninety-six: measured, the scan at ninety-six cost
  -- six milliseconds of a twenty-millisecond block, on top of a 12000
  -- bit/s pump, and the first live call under it had the reference
  -- asking for a retrain with ten kilobytes of junk read -- which is
  -- what a starved transmit loop sounds like from the far end.
  , ecFarSearch = ms 900, ecFarWindow = round (2 * fs), ecFarSlice = 48
  -- With the far end talking, its signal is noise to the update, and
  -- the residual the filter leaves is that noise times the step over
  -- two: 0.003 puts it 28 dB under the far end, which 12000 bit/s can
  -- read through and 14400 nearly can, and takes about ten seconds to
  -- get there.  A canceller's usual step would put back a third of the
  -- far signal as noise -- which is the fact this file's other comments
  -- keep meeting, and the reason data mode adapted not at all.
  --
  -- Then 'estimate' made the update's speed beside the point: the taps
  -- come from the scan and the step only has to hold them, so it is a
  -- third of what it was, for a floor 33 dB under the far end.
  --
  -- 0.0005, not 0.002.  On a recorded call the filter's prediction was
  -- 0.11 % of the line at one scan and 0.26 % at the next, against an
  -- echo that is 1 % of it: the estimate is about half the echo, and
  -- 0.2 % was a coin toss.  A filter switched on by a noise estimate
  -- predicts nothing and subtracts nothing, so the threshold can sit a
  -- decade under the echo it is waiting for.
  , ecDataMu = 0.001, ecDataOn = 0.0005 }
  where
    ms t = round (t * fs / 1000)

-- | 'echoConfigAt' 8 kHz, for the tests and the offline tools that run there.
defaultEchoConfig :: EchoConfig
defaultEchoConfig = echoConfigAt 8000

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
  , esScan   :: !(Maybe Scan) -- ^ a data-mode search in progress
  , esVote   :: !(Maybe Int)  -- ^ what the last completed scan found
  , esEst    :: !(Maybe Est)  -- ^ an estimate of the taps in progress
  , esLast   :: !(Int, Double, Double, Double)  -- ^ the last finished scan: lag, best, mean, rival
  , esVotes  :: [Int]         -- ^ the best lags of the last few scans, newest first
  , esRefBp  :: [Signal]      -- ^ the reference, band-limited, for the scan: blocks, newest first
  , esRefBpN :: !Int          -- ^ how many samples those blocks hold
  , esRxBp   :: [Signal]      -- ^ the line, band-limited, likewise
  , esRxBpN  :: !Int
  , esRefFir :: !Signal       -- ^ the band-pass's history on the reference
  , esRxFir  :: !Signal       -- ^ and on the line
  }

-- | The scan and the estimate read band-limited copies of both
-- histories.  Measured on a recorded call: the reflection was the
-- best lag in eight scans of twelve, at 0.055 against a mean of 0.008
-- -- and against a noise maximum of 0.036 across seven thousand lags,
-- which the rival test refused at a ratio of 1.5.  The same recording
-- band-passed to the modem's own 600-3000 Hz gave the same peak three
-- to five times its rival: what the raw windows carry outside the band
-- is noise that lowers the echo's normalised correlation and nothing
-- else.  Sixty-five taps on each new block is a cost the block does
-- not notice.
scanBand :: Double -> Signal
scanBand fs = VS.reverse (firBandpass fs 600 3000 65)

-- | The taps being read off a finished scan, a few per block.
--
-- Done in the one block the scan finished in, the estimate was four
-- million multiplies on top of a pump already using half the block --
-- and the first live calls under it had the far end reading junk and
-- asking for a retrain, with the decision error spiking at the moment
-- a scan would first have finished.  A real-time loop is judged by its
-- worst block, not its average, and the replay's average of half real
-- time said nothing about that block.
data Est = Est
  { etScan :: !Scan          -- ^ the frozen windows the taps are read from
  , etTodo :: [Int]          -- ^ taps still to compute
  , etDone :: [(Int, Double)]
  }

-- | A search spread across blocks.  The whole-window search of
-- 'echoSearch' scores four thousand lags in one block, which is fine for
-- half a second of reach and a thousand-sample window and three times
-- too slow for the reach and the window a late reflection needs -- a
-- full-resolution search to 1.5 s starved the real-time loop and lost
-- three start-ups in a row.  So the windows are taken once, frozen, and
-- a slice of lags is scored each block until the range is done.
data Scan = Scan
  { scRxC     :: !Signal   -- ^ the received window, centred
  , scRxNorm  :: !Double
  , scRxFrom  :: !Int      -- ^ global index of its first sample
  , scRef     :: !Signal   -- ^ what we had transmitted, frozen
  , scRefFrom :: !Int      -- ^ global index of its first sample
  , scLo      :: !Int      -- ^ the smallest lag the range allowed
  , scLags    :: [Int]     -- ^ still to score
  , scScores  :: [(Double, Int)]
  }

echoInit :: EchoConfig -> EchoState
echoInit cfg = EchoState
  { esRef = VS.empty, esRefEnd = 0, esRxAt = 0
  , esDelay = ecDelay cfg
  , esTaps = VS.replicate (ecTaps cfg) 0
  , esEchoP = 0, esResP = 0, esEchoY = 0, esOn = False
  , esRxHist = VS.empty, esFound = Nothing, esScan = Nothing, esVote = Nothing, esEst = Nothing, esLast = (0, 0, 0, 0)
  , esVotes = [], esRefBp = [], esRefBpN = 0, esRxBp = [], esRxBpN = 0
  , esRefFir = VS.replicate 64 0, esRxFir = VS.replicate 64 0 }

-- | Remember a block we have just transmitted.  Called at the end of a
-- modem step, with the audio that step produced.
echoPush :: EchoConfig -> Signal -> EchoState -> EchoState
echoPush cfg blk st = st
  { esRef = VS.drop drop_ kept
  , esRefEnd = esRefEnd st + VS.length blk
  , esRefBp = bp', esRefBpN = bpN'
  , esRefFir = fir' }
  where
    kept = esRef st VS.++ blk
    -- The band-limited copy is kept as blocks and joined only when a
    -- scan starts.  Appending a block to a twenty-four-thousand-sample
    -- vector copies the vector, and four such histories copied fifty
    -- times a second was half a megabyte of short-lived allocation a
    -- block -- and on top of a 14400 bit/s pump the collector's pauses
    -- were what the far end heard as junk.  Consing a block onto a list
    -- allocates a cell.
    (blkBp, fir') = if ecFarSearch cfg > 0 then firStream (scanBand 8000) (esRefFir st) blk else (VS.empty, esRefFir st)
    (bp', bpN') = if ecFarSearch cfg > 0
                    then trimBlocks (ecFarSearch cfg + ecFarWindow cfg + 1024) (blkBp : esRefBp st) (esRefBpN st + VS.length blkBp)
                    else ([], 0)
    -- the bulk delay, the filter, whatever the quiet-window search may
    -- reach for, and a block of slack -- the far scan reads its own copy
    want = max (esDelay st + ecTaps cfg) (ecSearch cfg) + 1024
    drop_ = max 0 (VS.length kept - want)

-- | Keep only as many newest-first blocks as hold @n@ samples.
trimBlocks :: Int -> [Signal] -> Int -> ([Signal], Int)
trimBlocks n blocks total = go blocks 0 []
  where
    go [] acc kept = (reverse kept, acc)
    go (b : bs) acc kept
      | acc >= n = (reverse kept, acc)
      | otherwise = go bs (acc + VS.length b) (b : kept)
    _ = total

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
echoBlock cfg adapt = echoRun cfg (if adapt then EchoQuiet else EchoHold)

-- | Cancel our own echo out of a received block while the far end is
-- talking -- which is every block of a data call.
--
-- Everything above says the taps may only move while the far end is
-- quiet, and for a canceller's usual step that is true.  What it leaves
-- out is a reflection the quiet windows never see.  Through an ATA and
-- a softphone the echo of our own data came back 644 ms after it was
-- sent, 34 dB down, 20 dB under the far end's signal, and the same to
-- the sample for the length of the call; the search reached 500 ms and
-- looked only in the start-up's quiet windows, so nothing ever aimed at
-- it, nothing adapted, and every V.32 rate needing more than 20 dB of
-- slicer was under a floor no signal-to-noise ratio could lift.
--
-- So, in data mode: an incremental search ('Scan') finds the reflection
-- against the talking far end -- a long window is what makes that
-- possible -- and the filter is aimed only when two scans in a row agree
-- on where it is.  Then it adapts with a step small enough that the far
-- end's signal, which is noise to the update, leaves a residual well
-- under the echo it removes.  And it switches on by its own rule: the
-- one above compares residual against received power and cannot
-- trigger while the far end is most of what is received, so here the
-- filter's own prediction is weighed against the line, with a runaway
-- guard above it.
echoBlockData :: EchoConfig -> Signal -> EchoState -> (EchoState, Signal)
echoBlockData cfg rx st0
  | ecFarSearch cfg <= 0 = echoRun cfg EchoHold rx st0
  | otherwise = let (st1, out) = echoRun cfg EchoData rx st0 in (stepScan cfg st1, out)

data EchoMode = EchoQuiet | EchoHold | EchoData deriving (Eq)

echoRun :: EchoConfig -> EchoMode -> Signal -> EchoState -> (EchoState, Signal)
echoRun cfg mode rx st0 = (st', out)
  where
    quiet = mode == EchoQuiet
    adapt = quiet
    aimed0 = esFound st0 /= Nothing
    mu | quiet = ecMu cfg
       | mode == EchoData && aimed0 = ecDataMu cfg
       | otherwise = 0
    adapting = mu > 0
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
              g = if adapting && nrm > 1e-12 then mu * e / nrm else 0
              lk = 1 - ecLeak cfg
              w' = if adapting
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
                  | mode == EchoData = dataOn
                  | adapt, rp' < 0.5 * ep' = True
                  | adapt, rp' > 0.9 * ep' = False
                  | aimed, yp' > ecOnRatio cfg * ep' = True
                  | aimed, yp' < 0.25 * ecOnRatio cfg * ep' = False
                  | not aimed = False
                  | otherwise = on
              -- With the far end talking, what the filter predicts is
              -- the only measure there is of what it is removing.  Off
              -- until aimed; off again if it ever claims a quarter of
              -- the line, which is a filter that has run away.
              dataOn | not aimed = False
                     | yp' > 0.25 * ep' = False
                     | yp' > ecDataOn cfg * ep' = True
                     | yp' < 0.25 * ecDataOn cfg * ep' = False
                     | otherwise = on
          in go (i + 1) w' ep' rp' yp' on' ((if on' then e else d) : acc)

    (w1, ep1, rp1, yp1, on1, outs) = go 0 (esTaps st0) (esEchoP st0) (esResP st0) (esEchoY st0) (esOn st0) []
    out = VS.fromList outs
    -- a filter that ran away in data mode starts its taps again
    runaway = mode == EchoData && ep1 > 1e-18 && yp1 > 0.25 * ep1
    (rxBp, rxFir') = if ecFarSearch cfg > 0 then firStream (scanBand 8000) (esRxFir st0) rx else (VS.empty, esRxFir st0)
    (rxBp', rxBpN') = if ecFarSearch cfg > 0
                        then trimBlocks (ecFarWindow cfg + 1024) (rxBp : esRxBp st0) (esRxBpN st0 + VS.length rxBp)
                        else ([], 0)
    st' = st0 { esTaps = if runaway then VS.map (const 0) w1 else w1
              , esEchoP = ep1, esResP = rp1, esEchoY = yp1, esOn = on1 && not runaway
              , esRxAt = esRxAt st0 + n
              , esRxHist = keepTail (ecSearch cfg `div` 2) (esRxHist st0 VS.++ rx)
              , esRxBp = rxBp', esRxBpN = rxBpN', esRxFir = rxFir' }

-- | One block's worth of the data-mode search: start one if none is
-- running, score the next slice of lags, and when the range is done
-- judge it exactly as 'echoSearch' judges its own.  Aim only when two
-- scans in a row agree, and only when the answer has moved -- aiming
-- drops the taps, and a filter that is converging must not be reset
-- for being told what it already knew.
stepScan :: EchoConfig -> EchoState -> EchoState
stepScan cfg st = case esEst st of
  Just est -> stepEst cfg st est
  Nothing -> case esScan st of
   Nothing -> st { esScan = startScan cfg st }
   Just sc ->
    let (now, rest) = splitAt (ecFarSlice cfg) (scLags sc)
        scored = [ (scoreLag (scRef sc) (scRefFrom sc) (scRxC sc) (scRxNorm sc) (scRxFrom sc) l, l) | l <- now ]
        sc' = sc { scLags = rest, scScores = scored ++ scScores sc }
    -- Forced here, in this block.  Consed onto a list that nothing
    -- reads until the scan is judged, every score was a thunk, and all
    -- seven thousand of them -- two hundred million multiplies -- were
    -- evaluated in the one block where the scan finished.  Measured in
    -- the live loop: blocks of 336 to 370 ms against a budget of 20,
    -- seventeen blocks of audio gone each time, which the far end read
    -- as junk and answered with a retrain.  With the canceller off the
    -- worst block was 39 ms.
    in forceScores scored `seq` (if null rest then finishScan cfg st sc' else st { esScan = Just sc' })

-- | Evaluate every score now.
forceScores :: [(Double, Int)] -> ()
forceScores = foldr (\(c, l) r -> c `seq` l `seq` r) ()

-- | A dozen taps a block, then the scale and the average in the block
-- after the last.
stepEst :: EchoConfig -> EchoState -> Est -> EchoState
stepEst cfg st est = case splitAt 12 (etTodo est) of
  ([], _) -> (finishEst cfg (etScan est) (etDone est) st) { esEst = Nothing }
  (now, rest) ->
    let sc = etScan est
        fresh = [ (k, tapOf sc (esDelay st) k) | k <- now ]
        done = fresh ++ etDone est
    -- forced now, for the same reason 'forceScores' exists
    in foldr (\(k, v) r -> k `seq` v `seq` r) () fresh `seq` st { esEst = Just est { etTodo = rest, etDone = done } }

startScan :: EchoConfig -> EchoState -> Maybe Scan
startScan cfg st
  | esRxBpN st < ecFarWindow cfg = Nothing
  | length lags < 512 = Nothing
  | otherwise = Just Scan
      { scRxC = rxC, scRxNorm = rxNorm, scRxFrom = rxFrom
      , scRef = ref, scRefFrom = refFrom, scLo = lo, scLags = lags, scScores = [] }
  where
    -- joined once, here, from blocks appended for nothing
    rxW = keepTail (ecFarWindow cfg) (VS.concat (reverse (esRxBp st)))
    w = VS.length rxW
    rxMean = VS.sum rxW / fromIntegral w
    rxC = VS.map (subtract rxMean) rxW
    rxNorm = sqrt (VS.sum (VS.map (\v -> v * v) rxC))
    rxFrom = esRxAt st - w
    ref = VS.concat (reverse (esRefBp st))
    refFrom = esRefEnd st - VS.length ref
    lo = max 0 (esRxAt st - esRefEnd st)
    lags = [ l | l <- [lo .. ecFarSearch cfg]
               , let o = (rxFrom - l) - refFrom, o >= 0, o + w <= VS.length ref ]

finishScan :: EchoConfig -> EchoState -> Scan -> EchoState
finishScan cfg st sc
  -- A plurality, not one scan's word.  On a recorded call the echo was
  -- the best lag in two scans of three and a noise peak in the third,
  -- somewhere different each time; the reflection is one place on the
  -- line and comes back to it.  Three of the last four within a few
  -- samples, and the scan that aims showing the peak itself -- above
  -- the mean by 'ecPeak', and above the best of everywhere else by a
  -- margin -- is what a noise peak cannot supply.
  | agreed, showing, moved = (estimateFrom (echoAim cfg bestLag st1)) { esVote = Just bestLag }
  | agreed, showing = (estimateFrom st1) { esVote = Just bestLag }
  | otherwise = st1
  where
    votes = take 4 (bestLag : esVotes st)
    -- Two of three to aim for the first time, three of four to move an
    -- aim already made.  A first aim that is wrong predicts nothing --
    -- an estimate of noise is a filter of nearly nothing, and it never
    -- switches on -- and the vote goes on and corrects it; an aim that
    -- is right and gets moved by two noise peaks in a row would drop
    -- taps that were cancelling something, which is worth asking for
    -- more evidence before doing.
    agreed | esFound st == Nothing = length [ v | v <- take 3 votes, abs (v - bestLag) <= 4 ] >= 2
           | otherwise = length [ v | v <- votes, abs (v - bestLag) <= 4 ] >= 3
    showing = best > ecPeak cfg * avg && avg > 0 && best > 1.25 * rival
              && bestLag > scLo sc + edge && bestLag < ecFarSearch cfg - edge
    moved = maybe True (\f -> abs (f - bestLag) > 4) (esFound st1)
    -- only around where the aim put the peak -- 'ecPre' in from the
    -- start -- and read off a few per block: see 'Est'
    estimateFrom s = s { esEst = Just (Est sc [ k | k <- [0 .. ecTaps cfg - 1], abs (k - ecPre cfg) <= 48 ] []) }
    st1 = st { esScan = Nothing, esLast = (bestLag, best, avg, rival), esVotes = votes }
    scores = scScores sc
    best = maximum (map fst scores)
    bestLag = snd (head [ p | p <- scores, fst p == best ])
    avg = sum (map fst scores) / fromIntegral (length scores)
    rival = maximum (0 : [ c | (c, l) <- scores, abs (l - bestLag) > 160 ])
    edge = 2 * ecPre cfg

-- | Set the taps from the scan itself.
--
-- With the far end talking, a least-mean-squares update against the
-- line converges toward the echo path, and stops short of it by the
-- far end's signal times the step over two: at a step of 0.003 that is
-- 28 dB under the far end, only 8 dB below an echo sitting 20 dB under
-- it, and it takes tens of seconds to get there.  The scan already
-- holds two seconds of our transmit against two seconds of the line,
-- and the unnormalised correlation of the two across the filter's
-- window is the echo's impulse response, smeared by the reference's
-- own autocorrelation -- which for a modem signal, flat across its
-- band, is a few samples -- and noisy by the far end's signal over
-- the square root of the window.  Over 256 taps that is a floor near
-- 27 dB under the far end, reached in one scan rather than crept up
-- on, and averaged scan to scan it goes on improving.  The slow update
-- then only has to hold it.
tapOf :: Scan -> Int -> Int -> Double
tapOf sc d0 k =
  let w = VS.length (scRxC sc)
      ref = scRef sc
      o = (scRxFrom sc - (d0 + k)) - scRefFrom sc
  in if o < 0 || o + w > VS.length ref then 0
     else let e = VS.slice o w ref
              go !i !dt !nsq
                | i >= w = (dt, nsq)
                | otherwise = go (i + 1) (dt + VS.unsafeIndex (scRxC sc) i * VS.unsafeIndex e i)
                                         (nsq + VS.unsafeIndex e i * VS.unsafeIndex e i)
              (dt', nsq') = go 0 0 0
          in if nsq' <= 0 then 0 else dt' / nsq'

finishEst :: EchoConfig -> Scan -> [(Int, Double)] -> EchoState -> EchoState
finishEst cfg sc done st = st { esTaps = taps' }
  where
    w = VS.length (scRxC sc)
    d0 = esDelay st
    ref = scRef sc
    every = VS.accum (\_ v -> v) (VS.replicate (ecTaps cfg) 0) done
    -- Only the taps around the peak.  Two hundred and fifty-six taps
    -- each a tenth wrong sum to more than the echo -- in the arithmetic,
    -- 256 x 100 / 8000 is five decibels above it -- and a scalar cannot
    -- fix noise.  A hybrid disperses over a few milliseconds and the
    -- reference's autocorrelation smears a few samples more, so twenty
    -- taps either side of the peak hold the response, and the rest hold
    -- only what would ruin it.
    pk = VS.maxIndex (VS.map abs every)
    raw = VS.imap (\k v -> if abs (k - pk) <= 20 then v else 0) every
    -- The reference is band-limited, so a correlation divided by its
    -- energy is the response passed through the reference's own
    -- autocorrelation, whose gain inside the band is the sample rate
    -- over the bandwidth -- 8000 over 2400, three and a third.  Applied
    -- as it stood, the filter predicted three times the echo and left a
    -- residual five decibels above it.  Inside the band the shape is
    -- right, so the one scalar that minimises the residual over the
    -- window -- the estimate's own output against the line -- puts it
    -- right too.
    yEst = VS.generate w $ \i ->
      let go !k !acc
            | k >= ecTaps cfg = acc
            | otherwise =
                let v = VS.unsafeIndex raw k
                in if v == 0 then go (k + 1) acc
                   else let o = (scRxFrom sc - (d0 + k)) - scRefFrom sc + i
                            x = if o < 0 || o >= VS.length ref then 0 else VS.unsafeIndex ref o
                        in go (k + 1) (acc + v * x)
      in go 0 0
    num = VS.sum (VS.zipWith (*) yEst (scRxC sc))
    den = VS.sum (VS.map (\v -> v * v) yEst)
    scale = if den <= 0 then 0 else num / den
    fresh = VS.map (* scale) raw
    old = esTaps st
    -- The first estimate is taken whole; later ones are averaged in.
    -- That is not a nicety.  Each tap of one estimate carries the far
    -- end's signal as noise, a tenth of the main tap here, and a quarter
    -- of the weight each time means the noise of a dozen scans adds up
    -- to a twelfth of one -- which is the difference between a filter
    -- that removes the echo and one that does not.
    taps' | VS.all (== 0) old = fresh
          | otherwise = VS.zipWith (\a b -> 0.75 * a + 0.25 * b) old fresh

-- | The normalised correlation of one lag of the reference against a
-- centred receive window: 'echoSearch''s scorer, shared with the scan.
-- Two accumulator passes, and the same sums in the same order as it
-- always had, so the lag it returns is the lag it always returned.
scoreLag :: Signal -> Int -> Signal -> Double -> Int -> Int -> Double
scoreLag ref refFrom rxC rxNorm rxFrom l =
  let w = VS.length rxC
      o = (rxFrom - l) - refFrom
      e = VS.slice o w ref
      sumE !i !acc
        | i >= w = acc
        | otherwise = sumE (i + 1) (acc + VS.unsafeIndex e i)
      m = sumE 0 0 / fromIntegral w
      go !i !nsq !dt
        | i >= w = (nsq, dt)
        | otherwise =
            let c = VS.unsafeIndex e i - m
            in go (i + 1) (nsq + c * c) (dt + VS.unsafeIndex rxC i * c)
      (nsq', dt') = go 0 0 0
      nrm = sqrt nsq'
  in if nrm <= 0 || rxNorm <= 0 then 0 else abs dt' / (nrm * rxNorm)

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
  | length scores < 512 = Nothing
  -- A peak at either end of the range is the range's edge, not an echo.
  -- There is nothing beyond it to compare against, so the mean and the
  -- rival are both taken over a one-sided sample and both flatter it.
  -- Dialled at a board that had no echo to give, the search reported
  -- 499 ms -- one millisecond inside a 500 ms window -- for the whole
  -- of a call, at a return loss of 0.0 dB.
  | bestLag <= lo + edge || bestLag >= ecSearch cfg - edge = Nothing
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
    -- Two accumulator passes over the window, rather than five passes and
    -- three thousand-element vectors.  The vector form was the whole cost
    -- of the search: two 'VS.map's and a 'VS.zipWith' is 24 kB written and
    -- read back per lag, four thousand lags to a call, which stays in no
    -- cache and kept the collector busy -- one V.32 call allocated 65 GB
    -- and spent all of its time in here.
    --
    -- The answer is the same 'Double', bit for bit, and that is not
    -- tolerance: 'VS.sum' is @foldl' (+) 0@ over the stream, so an
    -- accumulator taking the same elements in the same order from the same
    -- zero is the same sum.  @m@ is therefore identical, so every @c@ is,
    -- so @nsq@ and @dt@ are.  What would give that up is folding the first
    -- pass into the second -- prefix sums of the reference and its square,
    -- so the mean and the norm come out in O(1) -- because that reaches
    -- them as @sum e^2 - w*m^2@, which is a different sum of different
    -- numbers.  Worth knowing, and not worth taking: the lag this returns
    -- aims the filter, and a filter aimed one sample over trains
    -- differently and hands the data pump a different call.
    score l = scoreLag ref (esRefEnd st - refLen) rxC rxNorm rxFrom l
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
    edge = 2 * ecPre cfg

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

-- | In data mode: whether the filter is subtracting, and the share of
-- the line it predicts, for the live line report.
echoDataState :: EchoState -> Maybe (Bool, Double)
echoDataState st
  | esFound st == Nothing = Nothing
  | otherwise = Just (esOn st, if esEchoP st <= 0 then 0 else esEchoY st / esEchoP st)

-- | What the canceller is doing, for a test that failed to say why.
echoDebug :: EchoState -> String
echoDebug st =
  let t = esTaps st
      k = VS.maxIndex (VS.map abs t)
  in "delay " ++ show (esDelay st) ++ " found " ++ show (esFound st) ++ " vote " ++ show (esVote st)
     ++ " on " ++ show (esOn st) ++ " peak tap " ++ show k ++ " = " ++ show (VS.unsafeIndex t k)
     ++ " echoP " ++ show (esEchoP st) ++ " predP " ++ show (esEchoY st) ++ " resP " ++ show (esResP st)
     ++ " scanning " ++ show (maybe False (const True) (esScan st))
     ++ " last scan " ++ show (esLast st) ++ " votes " ++ show (esVotes st)

echoErle :: EchoState -> Double
echoErle st
  | esResP st <= 0 || esEchoP st <= 0 = 0
  | otherwise = 10 * logBase 10 (esEchoP st / esResP st)
