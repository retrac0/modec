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
  , echoScanStep
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
import qualified Data.Vector.Unboxed as VU
import Modec.Xcorr (Spectrum, correlationFrom, crossSpectrum, prefixSums, transformReal, transformSize)

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
  , ecFarEvery  :: !Int  -- ^ samples from one scan to the next
  , ecDataMu    :: !Double -- ^ normalised LMS step while the far end talks; 0 leaves the taps to the scan's estimate
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
  -- -22 dB echo was a coin toss against it.
  --
  -- A scan a second.  It used to be a few dozen lags a block, a dot
  -- product each, because seven thousand of them in one block starved
  -- the loop -- and so a scan took three seconds of a call, and two had
  -- to agree before the filter was aimed.  All the lags at once through
  -- a transform ('Modec.Xcorr') is five milliseconds in the block that
  -- asks, so the cadence is set by what the votes want instead: windows
  -- that overlap by half, so that a noise peak in one is not simply the
  -- same peak again in the next.
  , ecFarSearch = ms 900, ecFarWindow = round (2 * fs), ecFarEvery = round fs
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
  , ecDataMu = 0.001 }
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
  , esScanAt :: !Int          -- ^ the received sample the next far scan is due at
  , esScan   :: !(Maybe ScanJob) -- ^ a far scan under way
  , esVote   :: !(Maybe Int)  -- ^ what the last completed scan found
  , esLast   :: !(Int, Double, Double, Double)  -- ^ the last finished scan: lag, best, mean, rival
  , esVotes  :: [Int]         -- ^ the best lags of the last few scans, newest first
  , esRefBp  :: [Signal]      -- ^ the reference, band-limited, for the scan: blocks, newest first
  , esRefBpN :: !Int          -- ^ how many samples those blocks hold
  , esRxBp   :: [Signal]      -- ^ the line, band-limited, likewise
  , esRxBpN  :: !Int
  , esRefFir :: !Signal       -- ^ the band-pass's history on the reference
  , esRxFir  :: !Signal       -- ^ and on the line
  , esEstimated :: !Bool      -- ^ an estimate has been taken since the filter was last aimed
    -- Whether subtracting what the filter predicts makes the line
    -- quieter, measured: see 'echoRun'.
  , esCorYD  :: !Double       -- ^ the prediction against the line, summed with a slow leak
  , esCorYY  :: !Double       -- ^ the prediction against itself, likewise
  , esCorN   :: !Int          -- ^ samples those sums have seen since the taps were last replaced
  , esTrained :: !Bool        -- ^ the taps were adapted against the echo alone, and took it down
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

-- | One look for the reflection: two seconds of the line against
-- everything we sent that could have come back in them.
--
-- The windows are taken once, frozen, and correlated at every lag
-- together ('Modec.Xcorr').  What is kept is what judging the scan and
-- reading the taps off it both need: the raw correlation at every
-- offset, and the running sums of the reference to scale it by.
data Scan = Scan
  { scRxC     :: !Signal   -- ^ the received window, centred
  , scRxNorm  :: !Double
  , scRxSum   :: !Double   -- ^ what centring left of its sum
  , scRxFrom  :: !Int      -- ^ global index of its first sample
  , scRef     :: !Signal   -- ^ what we had transmitted, frozen
  , scRefFrom :: !Int      -- ^ global index of its first sample
  , scLo      :: !Int      -- ^ the smallest lag the range allowed
  , scHi      :: !Int      -- ^ and the largest it scores
  , scLags    :: [Int]     -- ^ the lags worth scoring
  , scSums    :: !Signal   -- ^ running sums of the reference
  , scSq      :: !Signal   -- ^ and of its square
  , scEnough  :: !Double   -- ^ the least reference energy under the window worth dividing by
  , scXc      :: Signal    -- ^ the window against the reference, by offset into it
  }

-- | A scan under way.  It is three transforms, and they are taken a
-- block apart: one transform of this size is two milliseconds on an
-- idle machine and seven on a busy one, and a real-time loop is judged
-- by its worst block.
data ScanJob
  = ScanHeard !Scan !Spectrum            -- ^ the line's window is transformed
  | ScanSent !Scan !Spectrum !Spectrum   -- ^ and the reference
  | ScanDone !Scan                       -- ^ and the two are correlated: to be judged

echoInit :: EchoConfig -> EchoState
echoInit cfg = EchoState
  { esRef = VS.empty, esRefEnd = 0, esRxAt = 0
  , esDelay = ecDelay cfg
  , esTaps = VS.replicate (ecTaps cfg) 0
  , esEchoP = 0, esResP = 0, esEchoY = 0, esOn = False
  , esRxHist = VS.empty, esFound = Nothing, esScanAt = 0, esScan = Nothing, esVote = Nothing, esLast = (0, 0, 0, 0)
  , esVotes = [], esRefBp = [], esRefBpN = 0, esRxBp = [], esRxBpN = 0
  , esRefFir = VS.replicate 64 0, esRxFir = VS.replicate 64 0, esEstimated = False
  , esCorYD = 0, esCorYY = 0, esCorN = 0, esTrained = False }

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
-- So, in data mode: a search ('Scan') finds the reflection against the
-- talking far end -- a long window is what makes that possible -- and
-- the filter is aimed only when scans agree on where it is.  Then it
-- adapts with a step small enough that the far end's signal, which is
-- noise to the update, leaves a residual well under the echo it
-- removes.  And it switches on by its own rule: the one above compares
-- residual against received power and cannot trigger while the far end
-- is most of what is received, so here the filter's prediction is
-- correlated with the line -- which says whether taking it out makes
-- the line quieter -- with a runaway guard above it.
echoBlockData :: EchoConfig -> Signal -> EchoState -> (EchoState, Signal)
echoBlockData cfg rx st0
  | ecFarSearch cfg <= 0 = echoRun cfg EchoHold rx st0
  | otherwise = let (st1, out) = echoRun cfg EchoData rx st0 in (stepScan cfg 1 st1, out)

-- | Look for a late reflection while the start-up is still running:
-- the far search, with nothing else of data mode.
--
-- In data mode the search started from nothing when the data did, and a
-- 14400 receiver spent its first seconds with our own echo 20 dB under
-- the far end's signal, which is its whole margin.  The start-up has
-- better to offer than that.  Its conditioning signals are aperiodic,
-- which is all the search asks of what we send; and the first of them
-- is sent into a far end that Figure 4 keeps silent, so the reflection
-- comes back alone and the search cannot miss it.  Call it after
-- 'echoBlock', which keeps the histories it reads.
--
-- @quiet@ is the start-up saying the far end is silent.  Then there is
-- no far signal for a long window to average away, and a long window is
-- a liability: it still holds whatever the far end was sending before
-- it stopped, at a hundred times the echo's power, and the reflection
-- has to fill most of two seconds before it shows over that.  A quarter
-- of the window sees it half a second after it arrives -- which leaves
-- the rest of the conditioning signal for the filter to train on it.
echoScanStep :: EchoConfig -> Bool -> EchoState -> EchoState
echoScanStep cfg quiet st
  | ecFarSearch cfg <= 0 = st
  | quiet = stepScan cfg 4 st
  | otherwise = stepScan cfg 1 st

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
    -- esRefEnd - refLen, so tap k of received sample i reads
    -- ref[x0 + i - k], or nothing where that is off either end.
    --
    -- Read where it lies.  This used to copy the filter's window out of
    -- the reference for every sample -- a vector of 'ecTaps', then two
    -- more for the product and the squares -- which is the same sums
    -- and three allocations a sample, a megabyte a block, in a loop
    -- that runs for every block of every V.32 call whether or not the
    -- filter is doing anything.
    x0 = (esRxAt st0 - esDelay st0) - (esRefEnd st0 - refLen)
    xAt !j = if j < 0 || j >= refLen then 0 else VS.unsafeIndex ref j
    nTaps = min taps (VS.length (esTaps st0))
    dotRef !w !j0 = sumTo 0 0
      where
        sumTo !k !acc
          | k >= nTaps = acc
          | otherwise = sumTo (k + 1) (acc + VS.unsafeIndex w k * xAt (j0 - k))
    sumSqRef !j0 = sumTo 0 0
      where
        sumTo :: Int -> Double -> Double
        sumTo !k !acc
          | k >= taps = acc
          | otherwise = let v = xAt (j0 - k) in sumTo (k + 1) (acc + v * v)

    -- Whether subtracting the prediction makes the line quieter.
    --
    -- With the far end talking the residual says nothing -- a perfect
    -- echo 20 dB under the far signal comes out as one part in a
    -- hundred of the power -- but the prediction's correlation with the
    -- line says it exactly: taking y out of d lowers the power when
    -- twice their product exceeds y squared, and that ratio is one for
    -- an echo the taps have right and nought for an echo that is not
    -- there.  Summed over half a second it is steady to a tenth with
    -- the far end twenty decibels over the echo.
    --
    -- It answers what the filter's own output could not.  Through the
    -- bench's ATA a canceller of its own usually takes our echo out,
    -- but takes half a second to settle on a new signal, so the far
    -- search can find a reflection that is gone a moment later; and a
    -- filter read off a noise peak predicts something just the same.
    -- Either was switched on by how much it predicted and went on
    -- subtracting what was not there.
    corSpan = max 1 (ecFarWindow cfg `div` 4)
    corKeep = 1 - 1 / fromIntegral corSpan
    corWarm = max 1 (ecFarWindow cfg `div` 8)
    far = ecFarSearch cfg > 0

    go !i !w !ep !rp !yp !on !cyd !cyy !cn acc
      | i >= n = (w, ep, rp, yp, on, cyd, cyy, cn, reverse acc)
      | otherwise =
          let j0 = x0 + i
              y = dotRef w j0
              d = VS.unsafeIndex rx i
              e = d - y
              nrm = sumSqRef j0
              g = if adapting && nrm > 1e-12 then mu * e / nrm else 0
              lk = 1 - ecLeak cfg
              w' = if adapting
                     then VS.imap (\k wk -> lk * wk + g * xAt (j0 - k)) w
                     else w
              ep' = 0.99 * ep + 0.01 * (d * d)
              rp' = 0.99 * rp + 0.01 * (e * e)
              yp' = 0.99 * yp + 0.01 * (y * y)
              cyd' = corKeep * cyd + y * d
              cyy' = corKeep * cyy + y * y
              cn' = if cn >= corWarm then cn else cn + 1
              helps | cn' < corWarm = Nothing
                    | cyd' > 0.5 * cyy' = Just True
                    | cyd' < 0.25 * cyy' = Just False
                    | otherwise = Nothing
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
                  -- measured, where there is a far search to have aimed it
                  | far, aimed, Just h <- helps = h
                  | aimed, yp' > ecOnRatio cfg * ep' = True
                  | aimed, yp' < 0.25 * ecOnRatio cfg * ep' = False
                  | not aimed = False
                  | otherwise = on
              -- With the far end talking: off until aimed; off if it
              -- ever claims a quarter of the line, which is a filter
              -- that has run away; and otherwise on while taking its
              -- prediction out is measured to make the line quieter.
              -- Taps just read off a scan are trusted for the quarter
              -- second the measurement needs.
              dataOn | not aimed = False
                     | yp' > 0.25 * ep' = False
                     | Just h <- helps = h
                     | otherwise = on
          in go (i + 1) w' ep' rp' yp' on' cyd' cyy' cn' ((if on' then e else d) : acc)

    (w1, ep1, rp1, yp1, on1, cyd1, cyy1, cn1, outs) =
      go 0 (esTaps st0) (esEchoP st0) (esResP st0) (esEchoY st0) (esOn st0) (esCorYD st0) (esCorYY st0) (esCorN st0) []
    out = VS.fromList outs
    -- a filter that ran away in data mode starts its taps again
    runaway = mode == EchoData && ep1 > 1e-18 && yp1 > 0.25 * ep1
    (rxBp, rxFir') = if ecFarSearch cfg > 0 then firStream (scanBand 8000) (esRxFir st0) rx else (VS.empty, esRxFir st0)
    (rxBp', rxBpN') = if ecFarSearch cfg > 0
                        then trimBlocks (ecFarWindow cfg + 1024) (rxBp : esRxBp st0) (esRxBpN st0 + VS.length rxBp)
                        else ([], 0)
    stoppedHelping = mode /= EchoQuiet && esOn st0 && not on1
    st' = st0 { esTaps = if runaway then VS.map (const 0) w1 else w1
              , esEchoP = ep1, esResP = rp1, esEchoY = yp1, esOn = on1 && not runaway
              , esCorYD = cyd1, esCorYY = cyy1, esCorN = cn1
              -- Taps that the full step has fitted to the echo standing
              -- alone are better than anything a scan can read off with
              -- the far end talking, and are kept from it: see
              -- 'finishScan'.  Six decibels taken out is the mark, and a
              -- filter later measured not to be helping loses it.
              , esTrained = (esTrained st0 || (quiet && ep1 > 1e-18 && rp1 < 0.25 * ep1))
                            && not stoppedHelping
              -- ...and the next scan's reading of the taps replaces them
              -- outright rather than being averaged into what was wrong.
              , esEstimated = esEstimated st0 && not stoppedHelping
              , esRxAt = esRxAt st0 + n
              , esRxHist = keepTail (ecSearch cfg `div` 2) (esRxHist st0 VS.++ rx)
              , esRxBp = rxBp', esRxBpN = rxBpN', esRxFir = rxFir' }

-- | The far search: when one is due, take the windows; then a
-- transform a block until every lag is scored; then judge the result
-- exactly as 'echoSearch' judges its own.  Aim only when scans agree,
-- and only when the answer has moved -- aiming drops the taps, and a
-- filter that is converging must not be reset for being told what it
-- already knew.
--
-- @short@ divides the window and the wait between scans: one with the
-- far end talking, more when it is known to be silent.
stepScan :: EchoConfig -> Int -> EchoState -> EchoState
stepScan cfg short0 st = case esScan st of
  Just (ScanHeard sc heard) ->
    let sent = transformReal (VU.length (fst heard)) (scRef sc)
    in ready sent `seq` st { esScan = Just (ScanSent sc heard sent) }
  Just (ScanSent sc heard sent) ->
    let xc = correlationFrom (VS.length (scRef sc) - VS.length (scRxC sc) + 1) (crossSpectrum heard sent)
    in xc `seq` st { esScan = Just (ScanDone sc { scXc = xc }) }
  Just (ScanDone sc) -> (finishScan cfg st sc) { esScan = Nothing }
  Nothing
    | esRxAt st < esScanAt st -> st
    | otherwise -> case startScan cfg (ecFarWindow cfg `div` short) st of
        -- not enough heard, or nothing sent to look for: ask again soon
        Nothing -> st { esScanAt = esRxAt st + ecFarEvery cfg `div` 8 }
        Just sc ->
          let heard = transformReal (transformSize (VS.length (scRef sc))) (scRxC sc)
          in ready heard `seq` st { esScan = Just (ScanHeard sc heard)
                                 , esScanAt = esRxAt st + ecFarEvery cfg `div` short }
  where
    ready (re, im) = re `seq` im `seq` ()
    -- Often and short only until the reflection is found: after that
    -- the filter is adapting to it and the search has nothing to add
    -- that is worth four transforms a quarter second, in the stretch of
    -- the start-up where a late block is a hole in our own TRN.
    short = if esFound st == Nothing then short0 else 1

startScan :: EchoConfig -> Int -> EchoState -> Maybe Scan
startScan cfg window st
  | esRxBpN st - behind < window = Nothing
  | length lags < 512 = Nothing
  | otherwise = Just Scan
      { scRxC = rxC, scRxNorm = rxNorm, scRxSum = VS.sum rxC, scRxFrom = rxFrom
      , scRef = ref, scRefFrom = refFrom, scLo = lo, scHi = maximum lags, scLags = lags
      , scSums = prefixSums ref, scSq = sq, scEnough = enough, scXc = VS.empty }
  where
    -- The window stops where the reference does.  This is asked after
    -- a block has been heard and before what it is answered with has
    -- been sent, so the newest block of the line has no reference under
    -- its nearest lags yet.  Leaving it out lets the lags start at
    -- nought, as 'echoSearch' has them -- and that is what lets the edge
    -- rule below pass a hybrid 25 ms away, which is inside the span the
    -- filter starts with and was being refused as the edge of a range
    -- that began a block late.
    behind = max 0 (esRxAt st - esRefEnd st)
    -- joined once, here, from blocks appended for nothing
    heard = VS.concat (reverse (esRxBp st))
    rxW = keepTail window (VS.take (VS.length heard - behind) heard)
    w = VS.length rxW
    rxMean = VS.sum rxW / fromIntegral w
    rxC = VS.map (subtract rxMean) rxW
    rxNorm = sqrt (VS.sum (VS.map (\v -> v * v) rxC))
    rxFrom = esRxAt st - behind - w
    -- No more of the reference than the lags can reach: the transforms
    -- are as long as this is, and with a short window that is half the
    -- work.
    ref = keepTail (w + ecFarSearch cfg) (VS.concat (reverse (esRefBp st)))
    refFrom = esRefEnd st - VS.length ref
    lo = 0
    inRange = [ l | l <- [lo .. ecFarSearch cfg]
                  , let o = (rxFrom - l) - refFrom, o >= 0, o + w <= VS.length ref ]
    -- Only where we were sending.  A lag whose two seconds of reference
    -- are mostly silence has nothing to be correlated with, and counting
    -- it anyway is how a filter came to be aimed at nothing: the first
    -- scan after an answering modem's silence found the reference empty
    -- at nine lags in ten, so the mean score was nearly nought and the
    -- rival was nought, and a score of nothing much at 38 ms -- sixteen
    -- samples of our own signal's first rise against the line -- stood
    -- nine times over the one and infinitely over the other.  The taps
    -- were then read off the same sixteen samples, each a correlation
    -- divided by the energy of a pulse's leading edge, and the filter
    -- predicted an echo ten to the twentieth times the line (bench call
    -- 20261004T230127).  So a lag is scored only where the reference
    -- under it holds at least half what the fullest window does, and a
    -- scan with too few such lags is no scan.
    sq = prefixSums (VS.map (\v -> v * v) ref)
    energy l = let o = (rxFrom - l) - refFrom in VS.unsafeIndex sq (o + w) - VS.unsafeIndex sq o
    fullest = maximum (0 : map energy inRange)
    enough = max (0.5 * fullest) (scanFloor * fromIntegral w)
    lags = [ l | l <- inRange, energy l >= enough ]

-- | Every scored lag's normalised correlation.
--
-- 'scoreLag', with its sums read off instead of added up: the
-- reference's mean and energy under the window from running sums, and
-- the window against the reference from the transforms.  The same
-- number to a part in a million million, which is all a search that
-- votes on its answer needs of it.
scanScores :: Scan -> [(Double, Int)]
scanScores sc = [ (score l, l) | l <- scLags sc ]
  where
    w = VS.length (scRxC sc)
    score l =
      let o = (scRxFrom sc - l) - scRefFrom sc
          s1 = VS.unsafeIndex (scSums sc) (o + w) - VS.unsafeIndex (scSums sc) o
          s2 = VS.unsafeIndex (scSq sc) (o + w) - VS.unsafeIndex (scSq sc) o
          m = s1 / fromIntegral w
          nsq = s2 - s1 * m
          dt = VS.unsafeIndex (scXc sc) o - m * scRxSum sc
      in if nsq <= scanFloor * fromIntegral w || scRxNorm sc <= 0 then 0 else abs dt / (sqrt nsq * scRxNorm sc)

-- | A reference quieter than this, mean square, is silence, whatever
-- fraction of it a window holds.  Ninety decibels under full scale:
-- what we send is at minus nine.
scanFloor :: Double
scanFloor = 1e-9

finishScan :: EchoConfig -> EchoState -> Scan -> EchoState
finishScan cfg st sc
  -- A plurality, not one scan's word.  On a recorded call the echo was
  -- the best lag in two scans of three and a noise peak in the third,
  -- somewhere different each time; the reflection is one place on the
  -- line and comes back to it.  Three of the last four within a few
  -- samples, and the scan that aims showing the peak itself -- above
  -- the mean by 'ecPeak', and above the best of everywhere else by a
  -- margin -- is what a noise peak cannot supply.
  | agreed, showing, moved =
      let st2 = echoAim cfg bestLag st1
      in (if esTrained st2 then st2 else estimateFrom st2) { esVote = Just bestLag }
  -- the same place again: refine the taps, unless they were trained
  -- against the echo alone, which no scan's reading of them improves
  | agreed, showing, not (esTrained st1) = (estimateFrom st1) { esVote = Just bestLag }
  | agreed, showing = st1 { esVote = Just bestLag }
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
    agreed | esFound st == Nothing = length [ v | v <- take 3 votes, abs (v - bestLag) <= 4 ] >= 2 || unmistakable
           | otherwise = length [ v | v <- votes, abs (v - bestLag) <= 4 ] >= 3
    -- One scan is enough when the peak is nothing a noise peak looks
    -- like.  The reflection on the bench's 14400 calls scored nine times
    -- the mean and nearly twice the best lag anywhere else, on its first
    -- scan; waiting for a second to agree cost two seconds of data at a
    -- 20 dB echo.  The noise peaks that made the vote necessary came in
    -- under 1.5 times their rival.
    unmistakable = best > 8 * avg && best > 1.7 * rival
    showing = best > ecPeak cfg * avg && avg > 0 && best > 1.25 * rival
              && bestLag > scLo sc + edge && bestLag < min (ecFarSearch cfg) (scHi sc) - edge
    moved = maybe True (\f -> abs (f - bestLag) > 4) (esFound st1)
    -- only around where the aim put the peak -- 'ecPre' in from the start
    estimateFrom s = finishEst cfg sc [ (k, tapOf sc (esDelay s) k) | k <- [0 .. ecTaps cfg - 1], abs (k - ecPre cfg) <= 48 ] s
    st1 = st { esLast = (bestLag, best, avg, rival), esVotes = votes }
    scores = scanScores sc
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
      o = (scRxFrom sc - (d0 + k)) - scRefFrom sc
  in if o < 0 || o + w > VS.length (scRef sc) then 0
     else let nsq = VS.unsafeIndex (scSq sc) (o + w) - VS.unsafeIndex (scSq sc) o
          in if nsq < scEnough sc then 0 else VS.unsafeIndex (scXc sc) o / nsq

finishEst :: EchoConfig -> Scan -> [(Int, Double)] -> EchoState -> EchoState
finishEst cfg sc done st
  -- taps that are new have earned nothing yet: see 'echoRun'
  | not (esEstimated st) = st { esTaps = taps', esEstimated = True, esOn = True, esCorYD = 0, esCorYY = 0, esCorN = 0 }
  | otherwise = st { esTaps = taps', esEstimated = True }
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
    --
    -- "Whole" used to mean "while the taps are all zero", and they are
    -- not zero for long: the slow update starts the block the filter is
    -- aimed, and the estimate takes eight blocks to read.  So the first
    -- estimate went in at a quarter of its weight, averaged against
    -- eight blocks of an update that had barely begun, and the filter
    -- crept towards the echo over tens of seconds -- on a 14400 call
    -- modec placed through the bench it was predicting half the echo
    -- at 40 s, and the receiver sat at 19.5 dB where the echo removed
    -- gives 25.  The first estimate after an aim is now taken whole.
    taps' | not (esEstimated st) = fresh
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
--
-- The taps describe the stretch of line the filter was looking at, so
-- they go when it looks somewhere else -- and stay when it does not.  A
-- reflection inside the span the filter starts with has been adapted to
-- since the far end went quiet, by the time any search has enough of it
-- to find; being told where it is must not cost what was learned there.
-- It used to: a hybrid 25 ms away and 10 dB down was trained on for half
-- a second, found, and the taps dropped with the quiet window nearly
-- over.
echoAim :: EchoConfig -> Int -> EchoState -> EchoState
echoAim cfg l st
  | delay == esDelay st = st { esFound = Just l }
  | otherwise = st
      { esDelay = delay
      , esTaps = VS.map (const 0) (esTaps st)
      , esFound = Just l, esEstimated = False, esTrained = False }
  where delay = max (ecDelay cfg) (l - ecPre cfg)

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
     ++ " line/prediction " ++ show (if esCorYY st <= 0 then 0 else esCorYD st / esCorYY st) ++ " over " ++ show (esCorN st)
     ++ " last scan " ++ show (esLast st) ++ " votes " ++ show (esVotes st)

echoErle :: EchoState -> Double
echoErle st
  | esResP st <= 0 || esEchoP st <= 0 = 0
  | otherwise = 10 * logBase 10 (esEchoP st / esResP st)
