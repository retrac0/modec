{-# LANGUAGE BangPatterns #-}
-- | V.32 on the line: the coding layer of "Modec.V32" carried by the
-- pump of "Modec.QAM", at 2400 baud on an 1800 Hz carrier.
--
-- Kept apart from "Modec.V32" so that module stays free of any sample
-- rate.  Everything the Recommendation states as a table can then be
-- tested with no DSP underneath it, which is where most of the ways to
-- get V.32 wrong live.
module Modec.V32Pump
  ( -- * Line parameters
    v32Params
  , v32RxCfg
  , v32AcqCfg
  , v32SeamCfg
  , v32StartCfg
  , v32RollOff
    -- * Coding to and from symbols
  , TxCoder
  , txCoderInit
  , txCoderFrom
  , encodeSymbols
  , RxCoder
  , rxCoderInit
  , decodeSymbols
  , Coded
  , codedQuads
  , decodeQuads
    -- * Start-up signalling
  , conditioningSymbols
  , stateSymbols
  , modulatePointsFor
    -- * A duplex data pump
  , V32Data
  , v32DataInit
  , v32DataFrom
  , v32DataResume
  , v32DataRxState
  , v32DataTxState
  , v32DataRx
  , v32DataTx
  , v32DataEvm
  , v32DataSps
  , v32DataPower
    -- * Offline helpers
  , v32Modulate
  , v32ModulateTrained
  , v32Demodulate
  , v32DemodulateWith
  , v32DemodulateTrained
  , v32DemodulateTrainedWith
  , v32DemodulateTrainedEvm
  , v32DemodulateTrainedEvmWith
  ) where

import Modec.Link
import Modec.DSP (Signal, chunksOf)
import Modec.Standards (Role (..))
import Modec.QAM
import Modec.Stream (concatStage)
import Modec.V32

import qualified Data.Vector.Storable as VS

-- | V.32 does not name a shaping filter; §2.2 constrains the transmitted
-- spectrum instead, requiring the energy density at 600 and 3000 Hz --
-- the edges of the 2400 baud Nyquist band around 1800 Hz -- to be down
-- 4.5 +/- 2.5 dB.  A root raised cosine is 3 dB down there whatever its
-- roll-off, so the roll-off is ours to choose; 0.25 puts the signal in
-- 300 to 3300 Hz, which is the whole telephone band and no more.
v32RollOff :: Double
v32RollOff = 0.25

-- The pulse is truncated at 12 symbols each side rather than V.22's 6.
-- A root raised cosine with a 0.25 roll-off has tails that fall away
-- like 1\/t^2, and cutting them at 6 symbols leaves enough intersymbol
-- interference to cost 13 dB of implementation signal to noise ratio --
-- invisible at 4800 bit\/s, where the four points are far apart, and
-- fatal at 9600.  V.22 gets away with 6 because its 0.75 roll-off decays
-- far faster, which is exactly the kind of constant that does not
-- survive being carried between modes.
v32Params :: Double -> QamParams
v32Params fs = QamParams
  { qpFs = fs, qpBaud = 2400, qpCarrier = 1800
  , qpRollOff = v32RollOff, qpSpan = 12 }

-- | The receiver's view of a rate.  On the trellis alternative the
-- slicer here is the immediate one: it feeds the carrier and timing
-- loops, which cannot wait for a traceback, while the Viterbi decoder
-- runs behind it on the same symbols.
v32RxCfg :: V32Rate -> QamRxCfg
v32RxCfg r
  | rateTrellis r = base { qrThKp = 0.03
                         , qrEvmFreeze = guessing * guessing
                         , qrEvmGiveUp = maxBound }
  | otherwise = base
  where
    base = narrowTiming r (settledAgc r (defaultRxCfg (slicePoint r) (constellation r)))
    -- The equaliser's freeze, in the constellation's own units.
    --
    -- 'defaultRxCfg' freezes adaptation at a decision error power of
    -- 0.4, which is an error of 0.63: past the boundary at 4800, where
    -- it was set, and where it means "the decisions are noise, stop
    -- learning from them".  On a hundred and twenty-eight points the
    -- boundary is 0.11 away, and a receiver whose decisions are pure
    -- guesswork reads 0.09 -- so the freeze at 0.63 never comes, and a
    -- decision-directed equaliser goes on adapting to its own wrong
    -- answers.  What that does is shrink: the mean of a wrong decision
    -- is the middle of the constellation, so every update pulls the
    -- output a little further in, which makes the next decision worse.
    -- Measured on a 44 dB recording that the start-up read perfectly:
    -- the data receiver's output power fell from 0.98 to 0.69 over
    -- thirty-five thousand symbols with the error pinned at the
    -- guessing level throughout.
    --
    -- The guessing level is what to freeze short of.  A point landing
    -- anywhere in a square decision cell of side d is d / sqrt 6 from
    -- its centre on average, and d is twice 'rateMargin'; four fifths
    -- of that is close enough to guessing that nothing learned there
    -- is worth keeping.  The give-up reset goes with it: a receiver in
    -- data mode that cannot read the line asks for a retrain, which is
    -- the recovery the Recommendation provides, and a reset in the
    -- middle of that only throws away taps the retrain would have
    -- started from.
    guessing = 0.8 * 2 * rateMargin r / sqrt 6

    -- Two carrier loops, one per stage.  The four-point receiver that
    -- runs the start-up acquires with the fast gain, which reaches +/-18
    -- Hz and follows timing wander; the trellis rates then inherit that
    -- receiver already locked, and track with a gain small enough that
    -- the Viterbi decoder is not shown the loop's own jitter.  Neither
    -- gain does both jobs: the fast one costs the trellis symbols at
    -- 30 dB, the slow one cannot acquire a 1 % clock offset at all.

-- | The receiver's configuration for the symbols just after the
-- handover, which is the fast gain again.
--
-- "Inherits that receiver already locked" is true of a start-up that
-- went well and not of one that did not, and the tracking gain cannot
-- tell the difference: it will hold a lock it is handed and cannot find
-- one it is not.  A handover landing badly left the decision error at
-- 0.031 for a whole call -- six times what the same link settles at,
-- plenty to make the thirty-two point decisions wrong, and flat, so no
-- amount of waiting helped.  Acquiring and tracking want different
-- gains, which the comment above already says; they want them at
-- different times as well.
-- | The four-point receiver at the seam, where the far end's E gives
-- way to its B1.
--
-- The two are not separated by any boundary this end can see.  An
-- answering modem reads E, spends its own 128 symbols of B1 sending E,
-- and goes on reading all the while -- but what it is reading by then
-- is the caller's B1, at the agreed rate, spread over as many as a
-- hundred and twenty-eight points.  A four-point slicer shown that
-- returns a phase error per symbol that is not a phase error, and the
-- carrier loop follows it: measured on the bench at 14400, theta walked
-- 0.65 radians across AE, and the rate's receiver inherited a
-- constellation turned far enough that every decision was wrong and its
-- own decision-directed loops could not find the way back.
--
-- So from R3 on -- by which point the equaliser is trained and the
-- receiver is reading the four states at about 0.03 -- a symbol landing
-- a third of the way or more to the decision boundary is not one of
-- them, and steers nothing.  Before R3 nothing is gated: a line with
-- real delay distortion starts far outside that and has to be allowed
-- to converge the ordinary way.
v32SeamCfg :: QamRxCfg
v32SeamCfg = v32StartCfg { qrLockAt = gate, qrLoopGate = gate }
  where gate = (0.35 * rateMargin V32R4800) ^ (2 :: Int)

v32AcqCfg :: V32Rate -> QamRxCfg
v32AcqCfg r = narrowTiming r (settledAgc r (defaultRxCfg (slicePoint r) (constellation r)))

-- | The gain's own noise, taken out for every constellation that has
-- more than one amplitude.
--
-- The power estimate the gain divides by is an exponential mean of the
-- symbol samples.  On the four training states, all at one amplitude,
-- a twenty-symbol mean is exact; on sixteen points or more, spread over
-- several amplitudes, it wanders by some percent from one symbol to the
-- next, and the gain wanders with it -- which is a decision error of
-- about 0.05 on every symbol at any signal to noise ratio, and nothing
-- downstream can take it back out.  Measured on the bench at 45 dB:
-- 0.063 with the twenty-symbol mean, 0.040 with a five-hundred-symbol
-- one.  On a hundred and twenty-eight points that 0.05 is more than
-- half the way to guessing, and a receiver arriving at the handover
-- with it never converges at all.
--
-- Slow once the receiver has read the line, and not before: a receiver
-- that starts cold, with no training behind it, needs the fast estimate
-- to find the level at all, and only once it is reading can it afford
-- to stop chasing its own data.  The four-point receiver that runs the
-- start-up keeps the fast estimate throughout, because on one amplitude
-- it costs nothing.
settledAgc :: V32Rate -> QamRxCfg -> QamRxCfg
settledAgc r c
  | r == V32R4800 = c
  | otherwise = c { qrAgcSettled = 0.002 }

-- | The timing loop's own noise, taken out once the loop no longer
-- needs the bandwidth it was acquiring with.
--
-- The Gardner detector reports the data as well as the timing -- see
-- 'Modec.QAM.qamRxBlock' -- so the acquiring gain of 0.12 dithers the
-- sampling instant every symbol, and the interpolator turns that into a
-- decision error of about 0.03 that no channel puts there and no
-- equaliser can take out.  It is the same 0.03 at every rate, which is
-- what makes it a V.32bis problem and not a V.32 one: at 9600 trellis
-- the points are 0.22 apart and it costs 12 % of the margin; at 14400
-- they are 0.11 apart and it costs 27 %, so a call connects, sits on
-- the byte gate, and retrains itself to death.  Measured over a real
-- 14400 call at 46 dB: 0.072 at the acquiring gain, 0.040 at a quarter
-- of it, and the difference between four seconds of link and the whole
-- recording.
--
-- A quarter and not a tenth.  The floor keeps falling past that -- 0.007
-- with no channel at a tenth against 0.009 at a quarter -- but so does
-- the pull-in range, and the start-up is where this receiver has to
-- catch a signal it knows nothing about: at a tenth the calling ladder
-- does not reach the data phase of a real recording at all.  A quarter
-- holds a 100 ppm clock offset and a 7 Hz carrier offset with three
-- quarters of the improvement in hand.
--
-- And not at every rate, because the bandwidth is not only a cost.  It
-- is what follows a line whose delay is moving, and the impairment
-- suite asks 4800 and 9600 to work through half a percent of speed
-- deviation at 2 Hz -- 3 samples of delay, swinging, which a narrow
-- loop cannot stay with.  So the trade is made where the margin cannot
-- pay for the noise and left alone where it can: 4800 spends 4 % of its
-- margin on the wide loop and 9600 13 %, and both keep it; 12000 and
-- 14400 would spend a fifth and a quarter, and do not.
--
-- 'qrTrackAt' is armed at every rate all the same -- it is only the
-- gains that differ -- because the latch has to be taken where the
-- signal is four points and the decisions are certain.  A fifth of the
-- rate's own decision margin: on the four states that is 0.14, against
-- the 0.03 the start-up settles at and the 0.3 or more it passes
-- through on the way, so it latches once, in the start-up, and the data
-- pump inherits a narrow loop it could not have earned for itself on a
-- hundred and twenty-eight points.
narrowTiming :: V32Rate -> QamRxCfg -> QamRxCfg
narrowTiming r c
  | selfNoise > rateMargin r / 6 = narrowed armed
  | otherwise = armed
  where
    armed = c { qrTrackAt = (0.2 * rateMargin r) ^ (2 :: Int) }
    -- What the acquiring loop costs, measured with no channel at all
    -- and the same at every rate, because it is the receiver's and not
    -- the line's.  Against 4800's margin of 0.71 it is nothing; against
    -- 14400's 0.11 it is a quarter of the way to a wrong answer.
    selfNoise = 0.03

-- | A quarter of the acquiring gains.
--
-- The knee, and the whole of the trade.  Measured at 14400 with no
-- channel: 0.030 at the acquiring gains, 0.0155 at a half, 0.0094 at a
-- quarter, 0.0069 at a tenth and 0.0063 at a fortieth -- so a quarter
-- takes three-quarters of what there is to take, and past it the return
-- goes flat while the cost does not.  The cost is pull-in: at a tenth
-- the calling ladder does not reach the data phase on a real recording
-- at all, and a receiver has to catch a signal before it can sit on it.
--
-- Both gains, because they are one loop.  The integral term is the
-- larger share of the noise -- it is a random walk in the symbol rate
-- driven by the detector, and nothing damps it -- but it is also the
-- only thing that tracks a clock that is moving, so narrowing it alone
-- buys 0.058 where both buy 0.040 and gives up delay modulation
-- entirely.
narrowed :: QamRxCfg -> QamRxCfg
narrowed c = c { qrKpTrack = 0.25 * qrKp c, qrKiTrack = 0.25 * qrKi c }

-- | The four-point receiver the start-up runs, which narrows whatever
-- rate the call is going to end up at.
--
-- The start-up is not a data pump that happens to be reading four
-- points, and this is where they part company.  Its signal is TRN --
-- the same four states every time, at a level both ends agreed on, for
-- a few seconds -- and its output is a clock, a carrier and an
-- equaliser handed to whatever constellation was negotiated.  What it
-- hands over is only as good as its own timing, and a data pump cannot
-- make up the difference afterwards: narrowing at the handover and not
-- before leaves the 14400 receiver holding a clock that was dithered
-- for the whole of the start-up, which measured worse over a real call
-- than not narrowing at all -- 0.090 against 0.068, and a retrain three
-- seconds sooner.
--
-- 'v32RxCfg' at 4800 stays wide because it is a different job: a call
-- that settles on 4800 holds that line for as long as the session
-- lasts, through whatever the line does to its delay, and 4800's margin
-- can afford every bit of the noise the bandwidth costs.
v32StartCfg :: QamRxCfg
v32StartCfg = narrowed (v32RxCfg V32R4800)

-- | How long that lasts.  §5.4.2's B1 is 128 symbol intervals of
-- scrambled ones between E and the data, put there so a receiver can
-- settle on the constellation it has just been handed; twice that
-- covers the far end starting early without reaching far into anything
-- the tracking gain would rather be holding.
v32AcqSymbols :: Int
v32AcqSymbols = 256

-- | Scrambler, differential encoder and (on the trellis alternative)
-- convolutional encoder, with any bits left over from the last block.
data TxCoder = TxCoder
  { tcScr  :: !Scrambler
  , tcPrev :: !(Bool, Bool)
  , tcConv :: !ConvState
  , tcBits :: [Bool]
  }

txCoderInit :: TxCoder
txCoderInit = TxCoder scramblerInit (False, False) convInit []

-- | A data coder that carries on the scrambler the start-up was already
-- running.  §5.4.1 changes the coding at signal E but not the scrambler,
-- and sets the convolutional encoder's delay elements to zero -- so this
-- takes the one and resets the other.
txCoderFrom :: Scrambler -> (Bool, Bool) -> TxCoder
txCoderFrom sc q = TxCoder sc q convInit []

-- | Scramble and code as many whole symbols as the given bits allow,
-- keeping the remainder for next time.
encodeSymbols :: Role -> V32Rate -> [Bool] -> TxCoder -> (TxCoder, [Point])
encodeSymbols dir r newBits st0 = go st0 { tcBits = [] } (tcBits st0 ++ scrambled) []
  where
    (scr', scrambled) = scrambleRun dir (tcScr st0) newBits
    k = rateBitsPerSymbol r
    go st bits acc
      | length bits < k = (st { tcScr = scr', tcBits = bits }, reverse acc)
      | otherwise =
          let (grp, rest) = splitAt k bits
              (st', p) = codeSymbol r st grp
          in go st' rest (p : acc)

-- | One symbol.  Every rate does the same three things in the same
-- order -- differentially encode the first two bits, run them through
-- the convolutional encoder if this rate is trellis coded, and map the
-- result with whatever bits are left over -- so the only differences are
-- which differential table (Table 1 without trellis coding, Table 2
-- with) and how many bits ride above.
codeSymbol :: V32Rate -> TxCoder -> [Bool] -> (TxCoder, Point)
codeSymbol r st (q1 : q2 : rest)
  | not (rateTrellis r) =
      let y@(y1, y2) = diffEncode1 (q1, q2) (tcPrev st)
      in (st { tcPrev = y }, constellation r (bitsToInt ([y1, y2] ++ rest)))
  | otherwise =
      let y@(y1, y2) = diffEncode2 (q1, q2) (tcPrev st)
          (cv, y0) = convStep (tcConv st) y
      in (st { tcPrev = y, tcConv = cv }, constellation r (bitsToInt ([y0, y1, y2] ++ rest)))
codeSymbol _ _ _ = error "Modec.V32Pump.codeSymbol: short group"

-- | One symbol's coded content: the differentially encoded pair, then
-- whatever uncoded bits the rate carries above it.
type Coded = (Bool, Bool, [Bool])

data RxCoder = RxCoder
  { rcDescr :: !Scrambler
  , rcPrev  :: !(Bool, Bool)
  }

rxCoderInit :: RxCoder
rxCoderInit = RxCoder scramblerInit (False, False)

-- | Turn decided symbols into data bits.  The trellis alternative is
-- decoded from the equalised points rather than the immediate
-- decisions, which is the whole point of it.
--
-- The far end scrambles with the polynomial of /its/ direction, so the
-- descrambler here is given the other one.
decodeSymbols :: Role -> V32Rate -> [QamSym] -> RxCoder -> (RxCoder, [Bool])
decodeSymbols dir r syms st = decodeQuads dir r (codedQuads r syms) st

-- | Decided symbols to the coded bits they carry, before any of the
-- undoing: on the trellis alternative that is the Viterbi decoder's job
-- and needs a run of symbols, on the others it is one slicing per
-- symbol.
--
-- This is separate from 'decodeQuads' because the two have opposite
-- needs at a block boundary.  A trellis decoder wants the previous
-- block's last symbols back, to keep its path metrics continuous; a
-- descrambler must see every line bit exactly once, and showing it the
-- overlap a second time puts it out of step with the far end -- which
-- looks like a receiver that locks perfectly and then decodes noise.
codedQuads :: V32Rate -> [QamSym] -> [Coded]
codedQuads r syms
  | rateTrellis r = viterbiDecode r viterbiDepth (map qsPoint syms)
  | otherwise =
      [ let i = qsIndex sym
            k = rateBitsPerSymbol r
            bits = [ odd (i `div` (2 ^ b)) | b <- reverse [0 .. k - 1] ]
        in case bits of
             (y1 : y2 : rest) -> (y1, y2, rest)
             _ -> (False, False, [])
      | sym <- syms ]

-- | Coded bits to data bits: undo the differential encoding, then the
-- scrambler.  Stateful, and advanced exactly once per symbol.
--
-- The far end scrambles with the polynomial of /its/ direction, so the
-- descrambler here is given the other one.
decodeQuads :: Role -> V32Rate -> [Coded] -> RxCoder -> (RxCoder, [Bool])
decodeQuads dir r quads st0 = (st1 { rcDescr = descr' }, dataBits)
  where
    far = case dir of { Originate -> Answer; Answer -> Originate }
    diff = if rateTrellis r then diffDecode2 else diffDecode1
    (st1, coded) = foldlAcc (\st (y1, y2, rest) ->
      let (q1, q2) = diff (y1, y2) (rcPrev st)
      in (st { rcPrev = (y1, y2) }, q1 : q2 : rest)) st0 quads
    (descr', dataBits) = descrambleRun far (rcDescr st0) coded

foldlAcc :: (s -> a -> (s, [b])) -> s -> [a] -> (s, [b])
foldlAcc f = go []
  where
    go acc st [] = (st, concat (reverse acc))
    go acc st (x : xs) = let (st', bs) = f st x in go (bs : acc) st' xs

-- | A run of one training state, as points.
stateSymbols :: Int -> TrainState -> [Point]
stateSymbols n st = replicate n (statePoint st)

-- | The receiver conditioning signal of §5.2, in full: segment 1 is 256
-- symbols alternating A and B, segment 2 is 16 alternating C and D --
-- the transition between them is the only sharply defined instant in
-- the whole start-up, and is what the far end takes its timing
-- reference from -- and segment 3 is @trn@ symbols of scrambled ones.
--
-- Segment 3 is the one that does the work: §5.2.3 requires at least
-- 1280 symbols of it, and says plainly what they are for -- training the
-- adaptive equaliser at the far end and the echo canceller at this one.
-- A receiver judged on data it was handed cold is being asked to do
-- something the Recommendation never asks of it.
conditioningSymbols :: Role -> Int -> [Point]
conditioningSymbols dir trn =
  take 256 (cycle [statePoint StA, statePoint StB])
  ++ take 16 (cycle [statePoint StC, statePoint StD])
  ++ map statePoint (trnStates dir trn)

-- | Modulate data behind a full receiver conditioning signal, the way a
-- V.32 modem actually puts data on a line.  Returns the signal and how
-- many symbols precede the data.
v32ModulateTrained :: Double -> Role -> V32Rate -> Double -> Int -> [Bool] -> (Signal, Int)
v32ModulateTrained fs dir r amp trn bits = (modulatePoints fs amp (pre ++ pts), length pre)
  where
    pre = conditioningSymbols dir trn
    (_, pts) = encodeSymbols dir r bits txCoderInit

-- | Points on the line at 8 kHz and the usual level, for tests and
-- for generating the start-up signals offline.
modulatePointsFor :: [Point] -> Signal
modulatePointsFor = modulatePoints 8000 0.5

modulatePoints :: Double -> Double -> [Point] -> Signal
modulatePoints fs amp pts0 = go qamTxInit pts0 []
  where
    p = v32Params fs
    go _ [] acc = VS.concat (reverse acc)
    go st syms acc =
      let (st', sig, rest) = qamTxBlock p amp 160 syms st
      in if length rest == length syms
           then VS.concat (reverse (sig : acc))
           else go st' rest (sig : acc)

-- | Modulate data bits into a signal.  Offline: the whole thing at once.
v32Modulate :: Double -> Role -> V32Rate -> Double -> [Bool] -> Signal
v32Modulate fs dir r amp bits = modulatePoints fs amp (snd (encodeSymbols dir r bits txCoderInit))

-- | Demodulate a whole signal back to data bits.
v32Demodulate :: Double -> Role -> V32Rate -> Signal -> [Bool]
v32Demodulate fs dir r = v32DemodulateWith fs dir r 160

v32DemodulateWith :: Double -> Role -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateWith fs dir r blk sig = snd (decodeSymbols dir r syms rxCoderInit)
  where
    p = v32Params fs
    syms = concatStage (qamReceiver p (v32RxCfg r)) (chunksOf blk sig)

-- | Demodulate a signal whose first @preSyms@ symbols are the receiver
-- conditioning signal, as 'v32ModulateTrained' produces and as the
-- start-up of §5.4 sends.
--
-- The slicer changes when the data does.  While the conditioning signal
-- is arriving the line carries only the four states A, B, C and D, so
-- the receiver must decide against those four and not against the data
-- constellation -- a 16- or 32-point slicer fed a four-point signal
-- reports errors that are not there, and an equaliser adapting on them
-- diverges rather than converges.  This is why a V.32 receiver has to
-- know where it is in the start-up, and cannot simply be pointed at the
-- line.
v32DemodulateTrained :: Double -> Role -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateTrained = v32DemodulateTrainedWith id

-- | The same, with the receiver's tuning adjusted -- for finding out
-- what the tuning should be.
v32DemodulateTrainedWith :: (QamRxCfg -> QamRxCfg) -> Double -> Role -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateTrainedWith tune fs dir r preSyms sig =
  fst (v32DemodulateTrainedEvmWith tune fs dir r preSyms sig)

-- | The same, and the receiver's decision error over the settled part of
-- the run.
--
-- On a clean channel that error is the implementation noise and nothing
-- else, which is the only way to see a floor that costs no bit errors at
-- all until it costs every one of them: a receiver can be eating four
-- tenths of its own decision margin and still decode a noiseless signal
-- perfectly.
--
-- A mean square over the run, and not the receiver's own running
-- estimate where it stopped -- which is what this used to return, and
-- which measured the wrong thing twice over.  That estimate is
-- exponential with a fifty-symbol memory, so reading it at the end reads
-- the last fifty symbols; and the last symbols of an offline signal are
-- the ones whose root raised cosine tail was never transmitted, because
-- the vector ends.  Truncation noise, sampled once: the same receiver on
-- the same signal read 8 % of its margin at one payload length and 61 %
-- at another, which is why the ceilings hanging off it had to be set at
-- 60 % before they were quiet.  The head goes too -- 'settleSyms' of it
-- -- because a receiver acquiring is not a receiver's floor.
v32DemodulateTrainedEvm :: Double -> Role -> V32Rate -> Int -> Signal -> ([Bool], Double)
v32DemodulateTrainedEvm = v32DemodulateTrainedEvmWith id

v32DemodulateTrainedEvmWith :: (QamRxCfg -> QamRxCfg) -> Double -> Role -> V32Rate -> Int -> Signal
                            -> ([Bool], Double)
v32DemodulateTrainedEvmWith tune fs dir r preSyms sig =
  (snd (decodeSymbols dir r syms rxCoderInit), settledEvm syms)
  where
    p = v32Params fs
    -- The 4800 receiver and not 'v32StartCfg', although the prefix is
    -- the start-up's own signal.  This harness exists to measure a data
    -- pump on a stated channel, and handing it a prefix read at the
    -- start-up's narrower timing bandwidth measures the start-up's
    -- tracking instead: on a line whose delay moves half a percent the
    -- prefix arrives out of step and 4800 and 9600 lose bits they hold
    -- comfortably on a real call, where the start-up has Figure 4's
    -- seconds rather than 1400 symbols to work with.
    trainCfg = tune (v32RxCfg V32R4800)
    dataCfg = tune (v32RxCfg r)
    preN = ceiling (fromIntegral preSyms * samplesPerSymbol p)
    (pre, dat) = VS.splitAt preN sig
    stAfter = snd (runBlocks p trainCfg pre (qamRxInit p trainCfg))
    (syms, _) = runBlocks p dataCfg dat stAfter

-- | Mean square decision error over the middle of a run: past the
-- acquisition, short of the truncated tail.
settledEvm :: [QamSym] -> Double
settledEvm syms
  | null keep = 0
  | otherwise = sum keep / fromIntegral (length keep)
  where
    n = length syms
    keep = map qsError (take (max 0 (n - settleSyms - tailSyms)) (drop settleSyms syms))

-- | How much of the head belongs to acquisition rather than to the
-- floor.  A receiver handed a trained equaliser and a settled clock is
-- reading properly well inside this; one that is not does not belong in
-- this measurement either way.
settleSyms :: Int
settleSyms = 400

-- | And how much of the tail is the signal running out.  The pulse spans
-- 12 symbols each side, so the last dozen symbols of any finite signal
-- are missing half their energy; a couple of times that is clear of it.
tailSyms :: Int
tailSyms = 32

-- | Drive the receiver over a whole signal, returning the state it ends
-- in as well as the symbols.  The trained start-up needs that state to
-- hand to a second pass under a different slicer, which is the one thing
-- a 'Stage' cannot give back, so this stays a plain fold.
runBlocks :: QamParams -> QamRxCfg -> Signal -> QamRxState -> ([QamSym], QamRxState)
runBlocks p cfg sig st0 = go st0 (chunksOf 160 sig)
  where
    go st [] = ([], st)
    go st (c : cs) =
      let (st', out) = qamRxBlock p cfg c st
          (more, stF) = go st' cs
      in (out ++ more, stF)

-- | A V.32 data pump for the connected state: a block of audio in, a
-- block out, bits both ways.
data V32Data = V32Data
  { vdTx    :: !QamTxState
  , vdCode  :: !TxCoder
  , vdRx    :: !QamRxState
  , vdDec   :: !RxCoder
  , vdPend  :: [QamSym]   -- ^ symbols held back for the decoder's traceback
  , vdWait  :: [QamSym]   -- ^ symbols decoded but held back for traceback
  , vdBits  :: [Bool]     -- ^ data bits not yet coded onto symbols
  , vdAcq   :: !Int       -- ^ symbols of acquisition gain left to run
  }

v32DataInit :: Double -> V32Rate -> V32Data
v32DataInit fs r = V32Data
  { vdTx = qamTxInit, vdCode = txCoderInit
  , vdRx = qamRxInit (v32Params fs) (v32RxCfg r), vdDec = rxCoderInit
  , vdPend = [], vdWait = [], vdBits = [], vdAcq = v32AcqSymbols }

-- | A data pump that inherits a receiver and transmitter the start-up
-- has already brought into lock.
v32DataFrom :: QamRxState -> QamTxState -> TxCoder -> V32Data -> V32Data
v32DataFrom rx tx code st = st { vdRx = qamRxUnlock rx, vdTx = tx, vdCode = code }

-- | The receiver and transmitter a retrain has to carry back into the
-- start-up, so the symbol clock and carrier phase do not restart in the
-- middle of a signal.
v32DataRxState :: V32Data -> QamRxState
v32DataRxState = vdRx

v32DataTxState :: V32Data -> QamTxState
v32DataTxState = vdTx

-- | Come out of a retrain, possibly at a different rate.
--
-- A fresh pump for the rate that was agreed, carrying the receiver,
-- transmitter and scrambler the start-up finished with, and the only
-- thing worth keeping from the old one: 'vdBits', which is data the
-- terminal handed over and the line has not carried yet.  Everything
-- else was counted in the old rate's symbols and would be nonsense in
-- the new one -- the traceback, the symbols held for it, and the part
-- of a symbol the coder had left over.
v32DataResume :: Double -> V32Rate -> QamRxState -> QamTxState -> TxCoder -> V32Data -> V32Data
v32DataResume fs r rx tx code old =
  (v32DataInit fs r) { vdRx = qamRxUnlock rx, vdTx = tx, vdCode = code, vdBits = vdBits old }

v32DataEvm :: V32Data -> Double
v32DataEvm = qamRxEvm . vdRx

-- | The receiver's samples per symbol, as its timing recovery has it.
-- 8000/2400 is 3.3333 when the two clocks agree.
v32DataSps :: V32Data -> Double
v32DataSps = qamRxSps . vdRx

-- | The received signal power the receiver is working from: an average
-- of the raw symbol magnitude before the AGC touches it, so it goes to
-- nothing when the far end does.
v32DataPower :: V32Data -> Double
v32DataPower = qamRxPower . vdRx

-- | How many symbols of context the decoder needs behind it.  A trellis
-- decoder judges a sequence, so restarting it at every block boundary
-- would throw away the very thing it is for; carrying the last few
-- symbols forward and decoding them again keeps the path metrics
-- continuous across a seam the far end knows nothing about.
vdOverlap :: Int
vdOverlap = 40

-- | Traceback depth, in symbols.  The same number the offline decoder
-- uses, and the number of symbols the block decoder must hold back.
viterbiDepth :: Int
viterbiDepth = 16

-- | Receive one block.
--
-- Transmit and receive are separate calls on purpose.  A modem does both
-- every block, but not at the same point in the block: the receiver runs
-- at the top, on audio that has arrived, and the transmitter at the
-- bottom, on bits the protocol layer has by then decided to send.  One
-- function doing both has to be handed a dummy for whichever half the
-- caller does not mean, and it then advances that half's state anyway --
-- which is silent, and costs every byte on the link.
v32DataRx :: Double -> Role -> V32Rate -> V32Data -> Signal -> (V32Data, [Bool])
v32DataRx fs dir r st rx = (st', out)
  where
    p = v32Params fs
    cfg = if vdAcq st > 0 then v32AcqCfg r else v32RxCfg r
    (rx', syms) = qamRxBlock p cfg rx (vdRx st)
    -- A Viterbi decoder is only sure of a symbol once it has seen the
    -- traceback's worth of symbols after it.  Emitting a block's newest
    -- symbols the moment they arrive therefore hands out precisely the
    -- decisions the decoder has not finished making -- and then, next
    -- block, re-decodes them properly and throws the good answer away.
    -- At 9600 the difference rarely shows; at 12000 and 14400 it is the
    -- difference between a working link and a stream of noise.
    depth = if rateTrellis r then viterbiDepth else 0
    stream = vdPend st ++ vdWait st ++ syms
    nPend = length (vdPend st)
    emitTo = max nPend (length stream - depth)
    fresh = take (emitTo - nPend) (drop nPend (codedQuads r stream))
    (dec', out) = decodeQuads dir r fresh (vdDec st)
    st' = st { vdRx = rx', vdDec = dec'
             , vdWait = drop emitTo stream
             , vdPend = lastN vdOverlap (take emitTo stream)
             , vdAcq = max 0 (vdAcq st - length syms) }

-- | Transmit @n@ samples, carrying as many of @bits@ as will fit.  What
-- does not fit stays in the coder, so nothing has to be handed back.
v32DataTx :: Double -> Role -> V32Rate -> Double -> Int -> [Bool] -> V32Data
          -> (V32Data, Signal)
v32DataTx fs dir r amp n bits st = (st { vdTx = tx', vdCode = code', vdBits = keep }, audio)
  where
    p = v32Params fs
    want = qamTxSymbolsFor p n (vdTx st)
    needed = want * rateBitsPerSymbol r
    -- Code exactly the symbols this block will carry, and no more.
    -- encodeSymbols will happily turn every bit it is given into a
    -- symbol, and qamTxBlock takes only the ones it has room for and
    -- hands the surplus back -- so a burst larger than one block, which
    -- is any burst at all at 9600 bit/s, is coded and then dropped on
    -- the floor.  Holding the bits here instead means the queue drains
    -- over as many blocks as it takes.
    pendingAll = vdBits st ++ bits
    (send, keep) = splitAt needed pendingAll
    -- Idle on scrambled ones when there is nothing to say: that is what
    -- keeps the far end's carrier, timing and equaliser alive between
    -- characters.
    idle = replicate (max 0 (needed - length send)) True
    (code', pts) = encodeSymbols dir r (send ++ idle) (vdCode st)
    (tx', audio, _) = qamTxBlock p amp n pts (vdTx st)

lastN :: Int -> [a] -> [a]
lastN k xs = drop (max 0 (length xs - k)) xs
