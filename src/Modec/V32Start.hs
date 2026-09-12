{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- | The V.32 start-up procedure of §5.4 and Figure 4, on the sample
-- clock.
--
-- This does not go through "Modec.Handshake".  That machine ticks once
-- per 20 ms tone frame, which suits a V.25 answer tone and a probe
-- rotation, and cannot express what V.32 asks for: §5.4.1 fixes the
-- turnaround from hearing a phase reversal to sending one at
-- 64 +/- 2 symbol periods, which is 26.67 +/- 0.83 ms.  A 20 ms tick is
-- 48 symbols wide and cannot place an event inside that window at all.
-- So the rule here is that anything the Recommendation states in symbol
-- periods runs on the sample clock, and the handshake is told the
-- outcome rather than asked to produce it.
--
-- Nor does detection go through the tone bank.  AA and CC are the same
-- 1800 Hz tone 180 degrees apart, and AC and CA the same pair at 600 and
-- 3000 Hz: a bank that measures magnitude sees no event at any of the
-- four transitions the start-up is built from.  'Modec.Reversal.RevTracker'
-- measures the phase instead.
--
-- The shape of Figure 4, and why it is shaped that way: the two modems
-- take turns.  Each measures the round-trip delay by timing its own
-- signal's reflection in the other's response (NT at the calling end, MT
-- at the answering end), and each then trains its receiver while the
-- other is silent.  That silence is not politeness -- it is what lets an
-- echo canceller learn the echo path without the far end's signal in the
-- error term.  A modem that skipped it would have to detect double talk
-- instead, and get it right.
module Modec.V32Start
  ( V32Phase (..)
  , V32Status (..)
  , V32Start
  , v32StartInit
  , v32StartAfterAnswerTone
  , v32StartOffer
  , v32RetrainInit
  , V32Listen (..)
  , v32ListenInit
  , v32ListenBlock
  , v32ListenRetrain
  , v32StartStep
  , v32Phase
  , v32Elapsed
  , v32RoundTrip
  , v32Bis
  , v32EchoAdapt
  , v32Turns
  , v32LineError
  , v32StartRx
  , v32StartTx
  , v32StartCoder
  , v32Timeline
  , chosen
  , v32Bits
  , v32AnsReversals
  , v32AidB1
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.Vector.Storable as VS

import Modec.Link
import Modec.Standards (Role (..), answerToneItu)
import Modec.DSP (Signal, blockOf, chunksOf)
import Modec.QAM
import Modec.Reversal
import Modec.V32
import Modec.V32Pump

-- | Where the start-up has got to.  The O phases belong to the calling
-- modem and the A phases to the answering one; they interlock, so the
-- names are worth reading against Figure 4.
data V32Phase
  -- calling
  = OListen      -- ^ waiting for the answerer's alternating AC
  | OAA          -- ^ sending state A, timing the reversals in AC
  | OCC          -- ^ first reversal heard; sending state C, 64 symbols later
  | OSilent      -- ^ second reversal heard, NT known; off the line
  | OTrainR1     -- ^ receiving the answerer's S, S-bar, TRN and R1
  | OHoldS       -- ^ sending S for the measured NT
  | OCond        -- ^ our own S, S-bar and TRN
  | OR2          -- ^ sending R2, waiting for R3
  -- E is not a phase of its own: §5.3.2 has it end the rate signal and
  -- name what follows, and it goes out on the same continuously
  -- scrambled stream as the ones behind it.
  | OB1          -- ^ E, then scrambled ones, waiting for the answerer's E
  -- answering
  | AAns         -- ^ the V.25 answer tone, with reversals
  | AAC          -- ^ alternating A and C, listening for 1800 Hz
  | ACA          -- ^ 1800 Hz heard; alternating C and A, timing MT
  | AAC2         -- ^ reversal heard; back to alternating A and C
  | AGap         -- ^ the caller has stopped; 16 symbols of silence
  | ACond        -- ^ our S, S-bar and TRN
  | AR1          -- ^ sending R1, waiting for the caller's S
  | AWaitMT      -- ^ off the line for the measured MT
  | ATrainR2     -- ^ receiving the caller's S, S-bar, TRN and R2
  | ACond2       -- ^ our second S, S-bar and TRN
  | AR3          -- ^ sending R3, waiting for E
  | AE           -- ^ sending E, then scrambled ones
  -- both
  | V32Up V32Rate
  | V32Fail String
  deriving (Eq, Show)

data V32Status = V32Busy | V32Connected V32Rate | V32Failed String
  deriving (Eq, Show)

-- | What we are putting on the line.
data TxSrc
  = TxNothing
  | TxTone2100                  -- ^ the answer tone, reversed every 450 ms
  | TxState TrainState          -- ^ one state, repeated
  -- | The answering modem's alternating signal, as a parity rather than
  -- an order.  AC and CA are the same signal half a symbol apart, so the
  -- only thing that distinguishes them -- and the only thing that makes
  -- the change between them the phase reversal the calling modem is
  -- timing -- is which symbol index carries the A.  Writing it as a
  -- parity against the running symbol count makes that flip explicit and
  -- impossible to lose; writing it as a pair of states in some order
  -- leaves it to bookkeeping that is wrong as often as right.
  | TxAltAC !Bool
  -- | Any two states, alternating: the conditioning signal's segment 1.
  | TxAltPair TrainState TrainState
  | TxPoints [Point]            -- ^ a fixed run, then whatever follows it
  | TxCoded [Bool]              -- ^ bits, scrambled and Table 1 encoded as they go
  -- | Scrambled ones at the agreed rate and coding.  §5.4.1 is explicit
  -- that what follows signal E is \"at the data rate and with the coding
  -- called for in R3\", and it has to be: the far end has already pointed
  -- its slicer at the data constellation, and a four-point signal
  -- arriving instead is not merely undecodable but actively harmful --
  -- its equaliser adapts to it and does not recover.
  | TxData32 V32Rate
  deriving (Eq, Show)

data V32Start = V32Start
  { vsRole    :: !Role
  , vsPhase   :: !V32Phase
  , vsFs      :: !Double
  , vsSps     :: !Double
  , vsN       :: !Int            -- ^ samples elapsed
  , vsSince   :: !Int            -- ^ samples since entering this phase
  , vsOffer   :: !RateSeq        -- ^ what we can do
  , vsPeer    :: Maybe RateSeq   -- ^ what they said they can do
  , vsRate    :: Maybe V32Rate   -- ^ what was settled on
  -- transmit
  , vsTx      :: !QamTxState
  , vsSrc     :: !TxSrc
  , vsQueue   :: [Point]         -- ^ a scheduled run, emitted before vsSrc
  , vsTxSym   :: !Int            -- ^ symbols emitted
  , vsSwitch  :: Maybe (Int, [Point], TxSrc)  -- ^ at this symbol: these points, then this source
  , vsTxScr   :: !Scrambler     -- ^ our scrambler, running continuously
  , vsTxQ     :: !(Bool, Bool)  -- ^ the quadrant we last transmitted
  , vsData    :: !TxCoder       -- ^ the data coder, once a rate is agreed
  , vsAfterE  :: Maybe V32Rate  -- ^ change to this coding when E has gone out
  , vsAnsPh   :: !Double
  , vsAnsRev  :: !Bool          -- ^ reverse the answer tone's phase every 450 ms
  -- receive
  , vsRx      :: !QamRxState
  , vsRev1800 :: !RevTracker
  , vsRev600  :: !RevTracker
  , vsRev3000 :: !RevTracker
  , vsMark    :: Maybe Int       -- ^ sample index of the first reversal
  , vsTrip    :: Maybe Int       -- ^ NT or MT, in samples
  , vsTurns   :: [Int]           -- ^ recent quadrant changes, newest first
  , vsPrevSym :: (Double, Double) -- ^ last raw symbol, for the difference
  , vsPts     :: [(Double, Double)] -- ^ recent equalised points, newest first, for 'aidB1'
  , vsAidB1   :: !Bool           -- ^ train the receiver on B1's known symbols; see 'aidB1'
  , vsDescr   :: !Scrambler
  , vsBits    :: [Bool]          -- ^ recent descrambled bits, newest first
  , vsSeenS   :: !Bool
  , vsSeenTrn :: !Bool
  , vsFar     :: !Role      -- ^ the far end's scrambler
  , vsRevAt   :: [(Double, Int)] -- ^ reversals seen in this block
  , vsACLimit :: !Int            -- ^ symbols to hold the pair in AAC before giving up
  , vsACHold  :: !Int            -- ^ symbols of the pair to send before reacting to AA
  , vsACRun   :: !Int            -- ^ blocks the answerer's AC pair has been up
  , vs1800Run :: !Int            -- ^ samples the caller's 1800 Hz has been up
  , vsQuiet   :: !Int            -- ^ samples the line has been quiet
  , vsAdapt   :: !Bool           -- ^ the echo canceller may adapt now
  , vsLineErr :: !Double         -- ^ the far end's training, as a decision error
  }

v32Phase :: V32Start -> V32Phase
v32Phase = vsPhase

v32Elapsed :: V32Start -> Int
v32Elapsed = vsN

-- | The round trip this end measured, in samples: NT at the calling end,
-- MT at the answering one.  This is what an echo canceller's bulk delay
-- should be set from.
v32RoundTrip :: V32Start -> Maybe Int
v32RoundTrip = vsTrip

-- | Whether the far end is silent, so the echo canceller may adapt.
v32EchoAdapt :: V32Start -> Bool
v32EchoAdapt = vsAdapt

-- | What the far end's training looked like: the decision error the
-- receiver reached against the four training states, which is the line's
-- noise measured in the units 'rateDecisionMargin' is written in.  This
-- is what the rate offer is cut by, and the only look at the line either
-- end gets before it has to choose a rate.
v32LineError :: V32Start -> Double
v32LineError = vsLineErr

-- | The receiver told it has not yet read the line: entering AE
-- switches it to the agreed rate's configuration, and the lock it has
-- is on four points.
unlockRx :: V32Start -> V32Start
unlockRx s = s { vsRx = qamRxUnlock (vsRx s) }

-- | Recent quadrant changes, newest first (for tests and tracing).
v32Turns :: V32Start -> [Int]
v32Turns = vsTurns

-- | The receiver as the start-up leaves it: carrier locked, timing
-- locked, equaliser trained on TRN.  Handing this to the data pump is
-- the entire reason §5.2.3 spends 1280 symbols on TRN, and starting the
-- data receiver cold instead throws that away at the moment it is worth
-- most.
v32StartRx :: V32Start -> QamRxState
v32StartRx = vsRx

-- | And the transmitter, whose symbol clock and carrier phase should
-- likewise carry on rather than restart mid-signal.
v32StartTx :: V32Start -> QamTxState
v32StartTx = vsTx

-- | The data coder the start-up was already using for the scrambled
-- ones after signal E.
--
-- This has to carry across into data mode, and it is easy to miss why.
-- The far end's descrambler is self-synchronising, which sounds like it
-- makes the scrambler's state nobody's business -- but it comes into
-- step over 23 bits, and until it has it emits whatever it likes.  A
-- restart here therefore hands the far end 23 arbitrary bits at a moment
-- when its framer is already armed, and a zero followed by ones in there
-- is a start bit and a character.  One phantom byte, at the head of the
-- session, from a scrambler that had no reason to restart.
v32StartCoder :: V32Start -> TxCoder
v32StartCoder = vsData


-- | Recent descrambled bits, newest first.
v32Bits :: V32Start -> [Bool]
v32Bits = vsBits

-- | Whether the answer tone carries V.25's phase reversals.
--
-- The reversals are an instruction: G.164 and G.165 have every echo
-- canceller and suppressor on the path stand down when it hears
-- 2100 Hz reversed every 450 ms, on the understanding that a modem
-- sending it will cancel its own echo.  Through an ATA and a softphone
-- that understanding is wrong.  The ATA's hybrid returns our signal
-- 1159 ms later at -28 dB, 'Modec.Echo' cannot reach a reflection that
-- late, and the one canceller that can -- the ATA's own, sitting at the
-- hybrid -- had switched itself off because we asked it to.  A plain
-- tone leaves it running.  What the reversals buy is only wanted where
-- our own canceller can do the job.
v32AnsReversals :: Bool -> V32Start -> V32Start
v32AnsReversals b st = st { vsAnsRev = b }

-- | Whether the receiver trains on B1's known symbols; see 'aidB1'.
v32AidB1 :: Bool -> V32Start -> V32Start
v32AidB1 b st = st { vsAidB1 = b }

-- | Train the receiver on the far end's B1, whose symbols are known.
--
-- §5.4.2 puts 128 symbol intervals of scrambled binary ones, at the
-- agreed rate and coding, between the far end's E and its data.  Until
-- now the receiver read them decision-directed, on a constellation it
-- had just been pointed at: which is the moment the loops most need
-- the truth and have least of it, and what the handoff's luck was made
-- of -- 12000 at a decision error of 0.002 on one call and 0.02 on the
-- next, a minute apart on the same line.
--
-- The symbols are predictable because the scrambler is
-- self-synchronising: its register is the last 23 line bits, and this
-- end has received them.  From the register at the start of the far
-- end's E, that E is re-encoded exactly as the far end encoded it, and
-- the coder state it leaves -- register and quadrant -- with the
-- convolutional encoder started from zero as §5.4.1 says, encodes 128
-- symbols of ones into the points the far end put on the line.
--
-- Two things stand between those points and the receiver.  The frame:
-- V.32 is differentially encoded because absolute carrier phase is
-- unknowable, so the receiver holds the constellation turned by some
-- quarter turn, and a differentially encoded sequence begun from any
-- quadrant is the true one turned by a constant -- so the turn is one
-- unknown, and the eight symbols of E the receiver has just read settle
-- it against the eight predicted, or refuse to.  And time: the
-- detector says how many bits past E it fired, so how many of B1's
-- symbols have already gone by, and the reference begins at the next.
-- A reference one symbol out is worse than none, and the E comparison
-- is the check on that too: a misaligned E does not match at any turn.
aidB1 :: Int -> RateSeq -> V32Start -> V32Start
aidB1 off e s
  | not (vsAidB1 s) = s
  | Nothing <- vsRate s = s
  | n < 16 || length lineBits < off + 16 + 23 = s
  | best > 0.5 = s                        -- E not found in the point history
  | received < 8 || agree * 10 < received * 6 = s  -- checked against the B1 already in hand
  | otherwise = s { vsRx = qamRxRef (map (turn k) queued) (vsRx s) }
  where
    Just rate = vsRate s
    far = vsFar s
    -- The far end's B1 -- 128 symbol intervals of scrambled ones at the
    -- agreed rate and coding (§5.4.2) -- is predictable, because the
    -- scrambler is self-synchronising: its register is the last 23 line
    -- bits, and this end has received them.  So the E that precedes B1
    -- is re-encoded from that register, and the coder state it leaves
    -- encodes the 128 ones into the points the far end put on the line.
    --
    -- Two unknowns stand between those points and the receiver, and both
    -- are settled by evidence rather than computed, because a constant
    -- that is merely computed is the kind that is right on this receiver
    -- and wrong on the next.  The frame: V.32 is differentially encoded
    -- because absolute phase is unknowable, so the constellation is held
    -- turned by some quarter turn.  And where E sits among the received
    -- points: the detector's offset gives the register but not the
    -- point index, which lags it by a handful of symbols.  So the
    -- predicted E is searched for in the points near where the offset
    -- says, at every turn -- a real E match is unmistakable, 1e-5 per
    -- symbol against 2 for anything else -- and that settles both.
    --
    -- Both thresholds are set from that separation and not from
    -- tidiness, because both were set tight first and rejected every
    -- call at the one operating point this exists for.  A true match
    -- costs the line's own noise power per symbol -- 0.016 at 18 dB,
    -- which a threshold of 0.02 refuses -- so it is 0.5, still four
    -- times under a false match.  And the B1 check cannot ask for
    -- agreement the noise will not allow: at 16 dB a tenth of the
    -- received quadrants are wrong on their own, so nine in ten is
    -- unmeetable, while random agreement is one in four.  Six in ten
    -- separates 57-of-57 from 0-of-57 with room at both ends.
    --
    -- Then the prediction is checked before it is trusted: the B1
    -- symbols the far end has *already* sent are in hand, and the
    -- predicted B1 must agree with them by quadrant.  A false E match --
    -- the point search landing on a scrambler phase the register does
    -- not correspond to -- predicts B1 that disagrees completely (0 of
    -- 57 one call, 57 of 57 the next), and injecting the wrong points
    -- wrecks the very loops this is meant to help.  Checked, it only
    -- ever helps.
    lineBits = concat [ let (a, b) = dibitOfTurn t in [b, a] | t <- vsTurns s ]
    regBefore = scramblerFromBits (take 23 (drop (off + 16) lineBits))
    (sc', q', ePts) = codedPoints far regBefore (False, False) (eSeqBits e)
    (_, b1) = encodeSymbols far rate (replicate (128 * rateBitsPerSymbol rate) True) (txCoderFrom sc' q')
    pts = reverse (vsPts s)              -- oldest first
    n = length pts
    expectO = n - off `div` 2 - 8
    turn j (x, y) = case j `mod` (4 :: Int) of
      0 -> (x, y); 1 -> (negate y, x); 2 -> (negate x, negate y); _ -> (y, negate x)
    costAt o j = sum [ (a - c) * (a - c) + (b - d) * (b - d)
                     | ((a, b), p) <- zip (drop o pts) ePts, let (c, d) = turn j p ] / 8
    (best, o0, k) = minimum [ (costAt o j, o, j)
                            | o <- [max 0 (expectO - 16) .. min (n - 8) (expectO + 16)], j <- [0 .. 3] ]
    -- B1's symbols already received: everything after the E that was
    -- found, checked by quadrant against the prediction
    gotB1 = drop (o0 + 8) pts
    received = length gotB1
    quad (x, y) = (x >= 0, y >= 0)
    agree = length [ () | (g, p) <- zip gotB1 (map (turn k) b1), quad g == quad p ]
    -- Stop short of B1's end.  The reference must never outlast the
    -- signal it predicts: past the far end's 128th symbol it is sending
    -- data, and a receiver still training on predicted ones is being
    -- taught the wrong thing at the worst moment.  Queueing the whole
    -- remainder did that -- at 18 dB it turned 259 byte errors into
    -- 971 on one seed and 483 into 1113 on another -- and the margin
    -- costs a dozen symbols of training that were never the point.
    queued = take (max 0 (128 - received - 16)) (drop received b1)

-- | Whether this turned out to be a V.32bis call: Table 5 Note 1 makes
-- it V.32bis only if both rate signals announce it, so both halves of
-- the exchange are asked.  Meaningless before R1 or R2 has been read.
v32Bis :: V32Start -> Bool
v32Bis s = rateSeqV32bis (vsOffer s) && maybe False rateSeqV32bis (vsPeer s)

-- | Start the V.32 exchange with the answer tone already sent, as it has
-- been if V.8 or V.8bis brought us here: ANSam served as the V.25 answer
-- sequence, and §5.4.2's \"after the Recommendation V.25 answer
-- sequence\" is already satisfied.  Sending a second one would only
-- confuse a far end that has finished listening for it.
-- | Back into Figure 4 from a call already in progress, per 5.5.
--
-- A retrain needs no phases of its own, because the start-up already
-- has both of the entry points one wants.  A calling modem asking for
-- one transmits AA -- steady 1800 Hz -- and the answering modem's AAC
-- is written to react to exactly that; an answering modem asking
-- transmits its alternating AC at 600 and 3000, and the calling modem's
-- OListen is written to react to that.  So the whole of a retrain is
-- putting both ends back into the machine at the right phase and
-- letting it run: TRN retrains both equalisers, R1 R2 R3 re-agree the
-- rate for nothing, and E and B1 hand back to the data pump through the
-- same code the first handover uses.
--
-- The receiver and transmitter carry over rather than restarting, for
-- the reason 'v32DataFrom' carries them the other way: the symbol clock
-- and the carrier phase are still good even when the equaliser is not,
-- and restarting them mid-signal costs more than it saves.  The round
-- trip carries over too, as a fallback for the timers, and is measured
-- again anyway.
v32RetrainInit :: Double -> Role -> RateSeq -> Bool
               -> QamRxState -> QamTxState -> Maybe Int -> V32Start
v32RetrainInit fs dir offer initiating rx tx trip =
  (v32StartInit fs dir offer)
    { vsPhase = phase, vsSrc = src, vsRx = rx, vsTx = tx, vsTrip = trip }
  where
    (phase, src) = case (dir, initiating) of
      -- AAC both sends the alternating pair and listens for the
      -- caller's 1800, so the answering modem enters there either way
      (Answer, _) -> (AAC, TxAltAC True)
      (Originate, True) -> (OAA, TxState StA)
      (Originate, False) -> (OListen, TxNothing)

-- | Listening for the far end to go back to training, from data mode.
--
-- This runs on the raw block and owes nothing to the data receiver,
-- which is the point: the reason to want a retrain is that the receiver
-- has stopped working, so anything that noticed only through the
-- receiver would go deaf exactly when it was needed.  'RevTracker'
-- measures tone energy on the audio itself.
data V32Listen = V32Listen
  { vlRev1800 :: !RevTracker
  , vlRev600  :: !RevTracker
  , vlRev3000 :: !RevTracker
  , vlDir     :: !Role
  , vlRun     :: !Int        -- ^ blocks the opening tone has been up
  }

v32ListenInit :: Double -> Role -> V32Listen
v32ListenInit fs dir =
  V32Listen (revInit fs 1800) (revInit fs 600) (revInit fs 3000) dir 0

v32ListenBlock :: Signal -> V32Listen -> V32Listen
v32ListenBlock rx l = l'
  { vlRun = if heardOpening l' then vlRun l + 1 else 0 }
  where
    l' = l { vlRev1800 = fst (revBlock rx (vlRev1800 l))
           , vlRev600 = fst (revBlock rx (vlRev600 l))
           , vlRev3000 = fst (revBlock rx (vlRev3000 l)) }

-- | Whether the far end has started the signal that opens a retrain:
-- AA at 1800 Hz from a calling modem, the alternating pair at 600 and
-- 3000 from an answering one.  Neither is anything a data signal
-- produces -- both directions are spread across the band -- so a level
-- this high at one frequency is a modem that has stopped sending data.
-- | Is the far end's opening signal on the line this block?
heardOpening :: V32Listen -> Bool
heardOpening l = case vlDir l of
  -- we are the caller, so the far end is the answering modem
  Originate -> strong (vlRev600 l) || strong (vlRev3000 l)
  Answer -> strong (vlRev1800 l)
  where
    -- A level is a correlation divided by the signal's own root mean
    -- square, so on a line with nothing on it it is noise over noise and
    -- reads whatever it likes.
    strong t = revPower t > 1e-5 && revLevel t > 0.45

-- | ...and has been for long enough to mean it.
--
-- A modem restarting Figure 4 holds its opening signal for seconds.  A
-- few blocks of it is what a line makes when it stops: the level is
-- normalised by a signal that has just gone, so it spikes exactly once
-- as the far end hangs up -- and a hang-up read as a retrain takes the
-- call round the start-up again instead of ending it.
v32ListenRetrain :: Role -> V32Listen -> Bool
v32ListenRetrain _ l = vlRun l >= 5

-- | An answering modem offering V.32 on spec, per A.2.2, with a bound
-- on how long it will hold the pair.
--
-- The unbounded form waits twenty-five seconds in AAC, which is right
-- for a modem that knows V.32 was agreed and has nothing else to try.
-- An automode answerer is guessing, and every one of those seconds it
-- spends transmitting 600 and 3000 Hz that no V.22, V.21 or Bell caller
-- understands.  Note 5 forbids /disconnecting/ inside three seconds of
-- the pair; falling back to another rung is not disconnecting, and the
-- ladder below V.32 is outside this Recommendation's scope anyway.
v32StartOffer :: Double -> Role -> RateSeq -> Double -> V32Start
v32StartOffer fs dir offer secs =
  (v32StartAfterAnswerTone fs dir offer)
    { vsACLimit = max 1 (round (secs * 2400)), vsACHold = 512 }

v32StartAfterAnswerTone :: Double -> Role -> RateSeq -> V32Start
v32StartAfterAnswerTone fs dir offer =
  let st = v32StartInit fs dir offer
  in case vsRole st of
       Answer -> st { vsPhase = AAC, vsSrc = TxAltAC True }
       Originate -> st

v32StartInit :: Double -> Role -> RateSeq -> V32Start
v32StartInit fs dir offer = V32Start
  { vsRole = role, vsPhase = if role == Originate then OListen else AAns
  , vsFs = fs, vsSps = samplesPerSymbol (v32Params fs)
  , vsN = 0, vsSince = 0
  , vsOffer = offer, vsPeer = Nothing, vsRate = Nothing
  , vsTx = qamTxInit, vsSrc = TxNothing, vsQueue = [], vsTxSym = 0
  , vsSwitch = Nothing, vsTxScr = scramblerInit, vsTxQ = (False, False)
  , vsData = txCoderInit, vsAfterE = Nothing
  , vsAnsPh = 0, vsAnsRev = True
  , vsRx = qamRxInit p cfg
  , vsRev1800 = revInit fs 1800, vsRev600 = revInit fs 600, vsRev3000 = revInit fs 3000
  , vsMark = Nothing, vsTrip = Nothing
  , vsTurns = [], vsPrevSym = (0, 0), vsPts = [], vsAidB1 = True
  , vsDescr = scramblerInit, vsFar = far dir, vsBits = []
  , vsSeenS = False, vsSeenTrn = False, vsRevAt = [], vsQuiet = 0, vsAdapt = False
  , vsACLimit = 60000, vsACHold = 128, vsACRun = 0, vs1800Run = 0
  , vsLineErr = 1 }
  where
    role = case dir of { Originate -> Originate; Answer -> Answer }
    p = v32Params fs
    cfg = v32StartCfg

-- | One block in, one block out.  The block may be any length; every
-- deadline inside is kept in samples, so nothing depends on where the
-- audio happens to be cut.
v32StartStep :: V32Start -> Signal -> (V32Start, Signal, V32Status)
v32StartStep st0 rx = (st3, tx, status)
  where
    n = VS.length rx
    st1 = observe st0 rx
    st2 = advance st1 n
    (st3, tx) = emit st2 n
    status = case vsPhase st3 of
      V32Up r -> V32Connected r
      V32Fail e -> V32Failed e
      _ -> V32Busy

far :: Role -> Role
far Originate = Answer
far Answer = Originate

symbols :: V32Start -> Int -> Int
symbols st k = round (fromIntegral k * vsSps st)

-- | Take in a block: time the phase reversals, and decode whatever the
-- four-point receiver makes of it.
observe :: V32Start -> Signal -> V32Start
observe st rx = st
  { vsRev1800 = r18, vsRev600 = r6, vsRev3000 = r30
  -- How long the answering modem's opening pair has been on the line.
  --
  -- A level is a coherent correlation over the signal's own tracked mean
  -- square, and that average is slow, so any signal arriving after a
  -- gap reads high for two or three blocks while the average catches up.
  -- Measured on a real board: 1.8 s of silence, then its rate signal
  -- came back reading 0.66 at 3000 Hz for three blocks and 0.14
  -- thereafter.  A single block of that was enough to convince this end
  -- the far end had restarted, and it went back to AA -- which the far
  -- end then answered by restarting for real, and the two of them went
  -- round Figure 4 together until the call timed out.
  , vsACRun = if acNow then vsACRun st + 1 else 0
  -- 5.4.2 wants 1800 Hz "detected ... for 64 symbol periods" before the
  -- answering modem acts on it, which is a duration and not an instant.
  , vs1800Run = if tone1800 then vs1800Run st + VS.length rx else 0
  , vsRevAt = [ (1800, i) | i <- e18 ] ++ [ (600, i) | i <- e6 ] ++ [ (3000, i) | i <- e30 ]
  , vsRx = rxSt, vsTurns = turns', vsPrevSym = prev', vsDescr = descr', vsBits = bits'
  , vsPts = take 128 (reverse (map qsPoint syms) ++ vsPts st)
  -- The best look at the far end's training, not the last one.  The
  -- error is an exponential mean, so anything that disturbs it -- the
  -- receiver still converging when TRN starts, a click, the turn-around
  -- at either end of the window -- can only push it up.  Its floor over
  -- the window is the line; its value at whatever instant R1 happens to
  -- be recognised is the line plus whatever else was going on.
  , vsLineErr = if vsPhase st == OTrainR1 && qamRxPower rxSt > 1e-5
                  then min (vsLineErr st) (sqrt (qamRxEvm rxSt)) else vsLineErr st
  , vsQuiet = quiet' }
  where
    acNow = revPower r6 > 1e-5 && (revLevel r6 > 0.45 || revLevel r30 > 0.45)
    tone1800 = revPower r18 > 1e-5 && revLevel r18 > 0.45
    (r18, e18) = revBlock rx (vsRev1800 st)
    (r6, e6) = revBlock rx (vsRev600 st)
    (r30, e30) = revBlock rx (vsRev3000 st)
    p = v32Params (vsFs st)
    -- Once the rate is settled the far end stops sending anything this
    -- four-point receiver can read -- §5.4.1 has it move to the agreed
    -- coding straight after E -- so the equaliser must stop adapting to
    -- it.  Left running it would train itself on a signal it is not
    -- looking at, hit the watchdog, and start over: the data pump would
    -- then inherit an untrained receiver at exactly the moment the whole
    -- of TRN was meant to have prepared one.  At 4800 and 9600 it could
    -- re-acquire and the loss was invisible; at 12000 and 14400 it
    -- cannot, and that is the whole difference.
    -- Freeze only once the far end has stopped sending anything this
    -- four-point receiver can read, which is the moment its signal E has
    -- been seen -- not the moment the rate is settled.  The two are
    -- several phases apart, and not the same distance apart at the two
    -- ends: an answering modem knows the rate as soon as it reads R2 and
    -- then has a whole second conditioning signal still to send and a
    -- signal E still to wait for.  Freezing there leaves it holding an
    -- equaliser trained on the first conditioning signal only, and the
    -- link works in one direction and not the other.
    -- Once the far end's E has gone by it is sending B1 -- 128 symbol
    -- intervals of scrambled ones at the agreed rate and coding -- and
    -- the rate is agreed, so point the slicer at the constellation it
    -- is actually using and let the loops converge on it.  That is what
    -- 5.4.2 puts B1 there for, and it is the only stretch of the
    -- start-up where the receiver can see the constellation it is about
    -- to have to read.
    --
    -- What was here before kept the four-point slicer and turned the
    -- adapting off, which reached the equaliser and not the carrier
    -- loop: the loop went on steering by the difference between a
    -- sixty-four point signal and the nearest of four states.  Freezing
    -- the loop as well is worse again -- 'phErr' against four points is
    -- a crude phase estimate but not a meaningless one, and coasting on
    -- a frequency estimate that may already be off loses more than it
    -- saves.  Measured both ways: 12000 stops carrying one direction.
    cfgNow = case (afterFarE (vsPhase st), vsRate st) of
      (True, Just r) -> v32SeamDataCfg r
      (True, Nothing) -> v32StartCfg { qrAdapt = False, qrTrack = False }
      (False, _) | seam (vsPhase st) -> v32SeamCfg
                 | otherwise -> v32StartCfg
    afterFarE ph = case ph of { AE -> True; V32Up _ -> True; _ -> False }
    -- The last two phases before the data constellation arrives: see
    -- 'v32SeamCfg'.  AE is where it matters and AR3 is where the lock
    -- that arms it is taken, the receiver being settled by then.
    seam ph = case ph of { AR3 -> True; AE -> True; OB1 -> True; _ -> False }
    (rxSt, syms) = qamRxBlock p cfgNow rx (vsRx st)
    -- Everything the start-up has to recognise is a quadrant change:
    -- the conditioning signal is a quarter turn every symbol, the
    -- alternating AC of the earlier phases a half turn, and the rate
    -- signals are dibits differentially encoded by Table 1.  Measuring
    -- the change rather than the position costs nothing and owes
    -- nothing to a carrier loop that has no absolute reference anyway.
    (turns, prev') = quarterTurns syms (vsPrevSym st)
    turns' = take 128 (reverse turns ++ vsTurns st)
    dibits = concat [ [a, b] | k <- turns, let (a, b) = dibitOfTurn k ]
    (descr', got) = descrambleRun (vsFar st) (vsDescr st) dibits
    bits' = take 512 (reverse got ++ vsBits st)
    pw = if VS.null rx then 0 else VS.sum (VS.map (\v -> v * v) rx) / fromIntegral (VS.length rx)
    quiet' = if pw < 1e-5 then vsQuiet st + VS.length rx else 0

-- | Segments 1 and 2 of the conditioning signal go back and forth
-- between two states a quarter turn apart -- A to B and back, then C to
-- D and back -- so the quadrant change alternates a quarter turn one way
-- and a quarter the other, never the same way twice.  The alternating AC
-- of the earlier phases steps by a half turn every symbol, and TRN's
-- first 256 symbols use only A and C, so they step by nought or a half:
-- three signals told apart by their quadrant changes alone, with no
-- absolute phase reference needed anywhere.
isConditioning :: [Int] -> Bool
isConditioning ts =
  let recent = take 16 ts
  in length recent == 16 && all (`elem` [1, 3]) recent
     && and (zipWith (/=) recent (drop 1 recent))

-- | How far back the sequence detectors look.
--
-- This has to exceed one block of bits, or a transition from one signal
-- to the next falls between two blocks and is never looked at at all.
-- A 20 ms block carries 96 bits at 4800 bit/s and 192 at 9600, so the
-- 64 bits this started with skipped a third of every block at 4800 and
-- two thirds at 9600.  A rate signal repeats, so nothing showed there;
-- E is sent once, and one live call skipped it.  The modem sat in B1
-- for seventeen seconds and then took a noise coincidence for E,
-- reaching data mode long after the far end had sent its banner.
seqDepth :: Int
seqDepth = 256

-- | Every 32-bit window of the recent history, oldest bit first: a
-- 16-bit sequence and whatever follows it.  The bits arrive newest
-- first and are aligned to nothing, so every offset has to be tried.
-- Newest first, and paired with how many bits have arrived since the
-- window ended: what follows E is timed from the end of E, which may
-- have gone by a block or two ago.
seqWindows :: [Bool] -> [(Int, [Bool])]
seqWindows bits =
  [ (off, reverse w)
  | off <- [0 .. seqDepth - 32]
  , let w = take 32 (drop off bits)
  , length w == 32 ]

-- | A rate signal: two consecutive identical 16-bit sequences with the
-- synchronising bits right, which is the minimum §5.3.1 will accept.
detectRate :: [Bool] -> Maybe RateSeq
detectRate bits =
  listToMaybe [ a | (_, w) <- seqWindows bits
                  , Just a <- [decodeRateSeq (take 16 w)]
                  , Just b <- [decodeRateSeq (drop 16 w)]
                  , a == b ]

-- | Signal E, which §5.3.2 sends exactly once.
--
-- The two-identical-copies rule of §5.3.1 cannot apply to something sent
-- once, and one 16-bit sequence with seven fixed bits in it turns up in
-- noise about once in 128 tries per alignment.  What makes it safe is
-- where it sits: §5.3.2 has E follow a whole number of rate sequences,
-- so a genuine E is preceded by one and a coincidence is not.
--
-- Looking at what comes /after/ E instead would be a mistake, though an
-- inviting one: §5.4 does put scrambled ones there, but by then they are
-- at the agreed data rate and coding, which the start-up receiver -- still
-- slicing four points and undoing Table 1 -- cannot read at all.
-- Which rate sequence precedes it is known -- it is the one the far end
-- has been sending -- so requiring that exact sequence rather than any
-- well-formed one costs nothing and takes another seven bits out of the
-- chance of a coincidence.
--
-- That anchor is what pays for reading E itself as a nearest-match
-- rather than an exact one, which 'decodeESeqNear' explains: the rate
-- signal repeats and E does not, so a bit error in E is a bit error in
-- the only copy there will ever be.
-- Returns how many bits have arrived since E ended, along with it.
--
-- The anchor is read the same way, and for the same reason.  Against a
-- CX93001 at 4800 the answering receiver's bits in AR3 are the caller's
-- R2 repeating with about one error in every sixteen to thirty-two,
-- and an anchor that had to /decode/ -- four leading zeros, B7, B11
-- and B15 all exact, and then equal the R2 read earlier -- was refused
-- on most passes, so the one E was missed and the start-up sat in AR3
-- until the far end gave up.  Two bits of slack in sixteen still leave
-- the anchor eleven bits from an E and eight from any other rate
-- sequence's structure, which is more than the E half itself gets.
detectE :: Maybe RateSeq -> [Bool] -> Maybe (Int, RateSeq)
detectE peer bits =
  listToMaybe [ (off, e)
              | (off, w) <- seqWindows bits
              , anchored (take 16 w)
              , Just e <- [decodeESeqNear (drop 16 w)] ]
  where
    anchored h = case peer of
      Just p -> length (filter id (zipWith (/=) h (rateSeqBits p))) <= 2
      Nothing -> decodeRateSeq h /= Nothing

-- | Points for one of the repeating sources.
srcPoints :: V32Start -> TxSrc -> Int -> (V32Start, [Point])
srcPoints st src k = case src of
  TxNothing -> (st, replicate k (0, 0))
  TxTone2100 -> (st, replicate k (0, 0))     -- generated directly, not as symbols
  TxState s -> (st, replicate k (statePoint s))
  TxAltAC par ->
    let base = vsTxSym st
        ps = [ statePoint (if even (base + i) == par then StA else StC)
             | i <- [0 .. k - 1] ]
    in (st, ps)
  TxAltPair a b ->
    let base = vsTxSym st
        ps = [ statePoint (if even (base + i) then a else b) | i <- [0 .. k - 1] ]
    in (st, ps)
  TxPoints ps -> (st { vsSrc = TxPoints (drop k ps) }, take k (ps ++ repeat (0, 0)))
  -- One scrambler runs from the start of TRN to the end of the call.
  -- It has to: a descrambler is self-synchronising but takes 23 bits to
  -- come into step, and signal E is 16 bits long.  Restarting the
  -- scrambler for each new thing to send leaves E unreadable while a
  -- rate signal, which repeats its 16 bits until answered, still gets
  -- through -- so it fails in exactly one place and looks like a
  -- detector bug.
  TxCoded bits ->
    let (want, rest) = splitAt (2 * k) bits
        (sc', q', ps) = codedPoints (dirOf st) (vsTxScr st) (vsTxQ st) want
        st1 = st { vsTxScr = sc', vsTxQ = q' }
    in case (null rest, vsAfterE st) of
         -- E has gone out; everything after it is data coded
         (True, Just rate) ->
           let st2 = st1 { vsSrc = TxData32 rate, vsAfterE = Nothing
                         , vsData = txCoderFrom sc' q' }
               (st3, more) = srcPoints st2 (TxData32 rate) (k - length ps)
           in (st3, ps ++ more)
         _ -> (st1 { vsSrc = TxCoded rest }, ps)
  TxData32 rate ->
    let (code', ps) = encodeSymbols (dirOf st) rate (replicate (k * rateBitsPerSymbol rate) True) (vsData st)
    in (st { vsData = code' }, ps)

-- | Produce @n@ samples.
emit :: V32Start -> Int -> (V32Start, Signal)
emit st n
  | vsSrc st == TxTone2100 = (st { vsAnsPh = ph' }, tone)
  | otherwise = (st' { vsTx = tx', vsTxSym = vsTxSym st + length pts }, sig)
  where
    p = v32Params (vsFs st)
    -- the V.25 answer tone, reversed every 450 ms to stand down any echo
    -- canceller in the network: we are about to be our own.  Unless
    -- told otherwise -- see 'v32AnsReversals'.
    w = 2 * pi * answerToneItu / vsFs st
    tone = VS.generate n $ \i ->
      let t = vsN st + i
          seg = (t * 1000) `div` (round (vsFs st) * 450 `div` 1000) :: Int
          sgn = if not (vsAnsRev st) || even (seg `div` 1000) then 1 else -1
      in 0.35 * sgn * sin (vsAnsPh st + w * fromIntegral i)
    ph' = vsAnsPh st + w * fromIntegral n

    want = qamTxSymbolsFor p n (vsTx st)
    (st1, queued) = (st, take want (vsQueue st))
    st2 = st1 { vsQueue = drop (length queued) (vsQueue st1) }
    need = want - length queued
    -- a scheduled source change lands on a symbol index, which is how a
    -- 64 +/- 2 symbol turnaround is met from a state machine that only
    -- sees whole blocks
    (st3, rest) = case vsSwitch st2 of
      Just (at, pre, src') | at <= vsTxSym st2 + length queued + need ->
        let before = max 0 (at - (vsTxSym st2 + length queued))
            (sa, psA) = srcPoints st2 (vsSrc st2) before
            after = need - before
            (sb, psB) = srcPoints sa src' (max 0 (after - length pre))
        in (sb { vsSrc = src', vsSwitch = Nothing
               , vsQueue = drop (max 0 (after)) pre ++ vsQueue sb }
           , psA ++ take after (pre ++ psB))
      _ -> srcPoints st2 (vsSrc st2) need
    pts = queued ++ rest
    (tx', sig, _) = qamTxBlock p 0.5 n pts (vsTx st3)
    st' = st3

-- | Bits onto the four states, as §5.3 sends a rate sequence: scrambled,
-- then dibits differentially encoded by Table 1.
codedPoints :: Role -> Scrambler -> (Bool, Bool) -> [Bool] -> (Scrambler, (Bool, Bool), [Point])
codedPoints dir sc0 q0 bits = (sc1, q1, ps)
  where
    (sc1, line) = scrambleRun dir sc0 bits
    (q1, ps) = go q0 line []
    go q (a : b : rest) acc =
      let q' = diffEncode1 (a, b) q
      in go q' rest (statePoint (stateOfDibit q') : acc)
    go q _ acc = (q, reverse acc)

-- | The receiver conditioning signal, with the scrambler and quadrant it
-- leaves behind.  §5.2.3 starts TRN's scrambler at all zeros with the
-- differential encoding switched off, and §5.3 then has the rate signal
-- pick up the differential encoder from TRN's final symbol -- so these
-- two values are the seam between them, and have to be carried across it.
conditioningRun :: Role -> Int -> ([Point], Scrambler, (Bool, Bool))
conditioningRun dir trn = (ps, sc, q)
  where
    ps = conditioningSymbols dir trn
    (sc, _) = scrambleRun dir scramblerInit (replicate (2 * trn) True)
    q = case reverse (trnStates dir trn) of
      (lastSt : _) -> dibitOfState lastSt
      [] -> (False, False)

dirOf :: V32Start -> Role
dirOf st = case vsRole st of { Originate -> Originate; Answer -> Answer }

-- | The state machine of Figure 4.
advance :: V32Start -> Int -> V32Start
advance st0 n = step st { vsN = vsN st + n, vsSince = vsSince st + n }
  where
    st = st0
    dir = case vsRole st0 of { Originate -> Originate; Answer -> Answer }
    sym k = symbols st0 k
    enter ph s = s { vsPhase = ph, vsSince = 0 }
    -- The turnaround is 64 symbol periods after the reversal was heard,
    -- measured on the transmitter's own symbol clock.  That clock is not
    -- the sample count divided by the symbol period: the answer tone is
    -- generated directly and never goes through the symbol pump, so an
    -- answering modem reaches this point three seconds into the call
    -- with a symbol count of nearly zero.  Deriving the deadline from
    -- the absolute sample index schedules it seven thousand symbols late.
    switchAt s i =
      vsTxSym s + round (fromIntegral (i - (vsN s - n)) / vsSps s) + 64
    -- A tracker that has been watching one signal cannot judge the next:
    -- the answer tone giving way to AC is a change of phase reference,
    -- not a reversal of the tone it is now looking at.  Re-arm on every
    -- transition that changes what we expect to hear.
    -- The far end stops and starts several times in Figure 4, and each
    -- time the carrier phase, the equaliser and the descrambler that
    -- were tracking the last signal are worse than nothing for the next.
    restart s = s { vsRx = qamRxReset (v32Params (vsFs s)) v32StartCfg (vsRx s)
                  , vsTurns = [], vsBits = [], vsPrevSym = (0, 0)
                  , vsDescr = scramblerInit }
    rearm s = s { vsRev1800 = revRearm (vsRev1800 s)
                , vsRev600 = revRearm (vsRev600 s)
                , vsRev3000 = revRearm (vsRev3000 s)
                , vsRevAt = [] }
    revAt f = listToMaybe [ i | (g, i) <- vsRevAt st0, g == f ]
    heard :: Double -> Double
    heard f = revLevel (case f of { 1800 -> vsRev1800 st0; 600 -> vsRev600 st0; _ -> vsRev3000 st0 })
    -- the calling modem watches whichever of the two AC sidebands is
    -- stronger; either will do, and 5.4.1 names both
    acRev = case (revAt 600, revAt 3000) of
      (Just i, _) -> Just i
      (_, Just i) -> Just i
      _ -> Nothing
    -- either sideband of AC reads about 0.5; noise reads about 0.16
    acHeard = heard 600 > 0.3 || heard 3000 > 0.3
    -- Only in the calling modem's phases past the point where it has
    -- answered AC once, and only after a second of it, so a transient
    -- cannot throw a start-up that is otherwise going well.
    -- a quarter second of it, not one block: see 'observe'
    restarted = vsACRun st0 >= 12 && vsSince st0 > sym 2400 && case vsPhase st0 of
      OTrainR1 -> True
      OHoldS -> True
      OCond -> True
      OR2 -> True
      -- and OB1: a board that restarted once will do it again, and one
      -- did, while this end sat sending E at it for thirty seconds
      OB1 -> True
      _ -> False
    tooLong limit s = vsSince s > sym limit

    step s = case vsPhase s of
      -- ------------------------------------------------ calling modem
      OListen
        | acHeard -> enter OAA (rearm s) { vsSrc = TxState StA }
        | tooLong 60000 s -> enter (V32Fail "no answering modem") s
        | otherwise -> s
      OAA | vsSince s < sym 96 -> s
      OAA -> case acRev of
        -- 5.4.1: on the first reversal, start the clock and change AA to
        -- CC exactly 64 symbols later, measured at the line
        Just i -> enter OCC (rearm s) { vsMark = Just i
                              -- A to C is a half turn, so the switch is
                              -- a reversal on its own
                              , vsSwitch = Just (switchAt s i, [], TxState StC) }
        Nothing | tooLong 30000 s -> enter (V32Fail "no reversal in AC") s
                | otherwise -> s
      OCC | vsSince s < sym 32 -> s
      OCC -> case acRev of
        Just i -> enter OSilent (restart s) { vsTrip = fmap (\m -> i - m) (vsMark s)
                                  , vsSrc = TxNothing, vsSwitch = Nothing
                                  , vsAdapt = False }
        Nothing | tooLong 30000 s -> enter (V32Fail "no second reversal") s
                | otherwise -> s
      OSilent
        | isConditioning (vsTurns s) -> enter OTrainR1 s { vsSeenS = True, vsLineErr = 1 }
        | tooLong 60000 s -> enter (V32Fail "no conditioning signal") s
        | otherwise -> s
      OTrainR1 -> case detectRate (vsBits s) of
        -- R1 arrives at the end of the answering modem's TRN, and this
        -- is the one moment the caller has heard the far end and
        -- nothing else: our own conditioning signal has not started, so
        -- the decision error here is the line and not the turn-around.
        Just r1 -> enter OHoldS s { vsPeer = Just r1, vsSrc = TxAltPair StA StB }
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R1") s
                | otherwise -> s
      OHoldS
        | vsSince s >= maybe (sym 256) id (vsTrip s) ->
            let (ps, sc, q) = conditioningRun dir 1400
                -- 5.4.1: R2 excludes anything absent from R1
                r2 = maybe (vsOffer s) (restrictRates (vsOffer s)) (vsPeer s)
            in enter OCond s { vsQueue = ps, vsTxScr = sc, vsTxQ = q
                             , vsSrc = TxCoded (cycle (rateSeqBits r2))
                             , vsAdapt = True }
        | otherwise -> s
      OCond
        | null (vsQueue s) -> enter OR2 s { vsAdapt = False }
        | otherwise -> s
      -- The rate signals are also the last of the far end's training,
      -- so how long we send one cannot be left to how fast we happen to
      -- recognise the answer to it.  Detecting R3 in a single block --
      -- which the detector is well able to do, two copies being 16
      -- symbols -- and moving straight on to E cuts the answering
      -- modem's equaliser short and puts a burst of errors just after
      -- the handover, at the one moment there is nothing to hide it.
      -- The far end has gone back to the beginning.  An answering modem
      -- that gives up part way through Figure 4 does not say so: it
      -- simply starts again, alternating A and C at 600 and 3000 Hz and
      -- waiting for the calling modem's AA.  Nothing here used to look
      -- for that, so a caller sat in OR2 transmitting a rate signal at a
      -- modem that had stopped listening for one, until its own
      -- eighty-thousand-symbol timer ran out thirty seconds later.
      --
      -- Measured on two different boards, both of which restarted about
      -- four seconds after our conditioning signal and then held AC for
      -- the rest of the call.  Answering it is the whole of the fix: AC
      -- is what OListen waits for, and going back to OAA is what it does
      -- when it hears it.
      _ | restarted -> enter OAA (rearm s) { vsSrc = TxState StA }
      OR2 | vsSince s < sym 128 -> s
      OR2 -> case detectRate (vsBits s) of
        Just r3 | Just rate <- bestCommonRate (vsOffer s) r3 ->
          enter OB1 s { vsRate = Just rate
                      -- R3 is what the answering modem's E follows
                      , vsPeer = Just r3
                      , vsSrc = TxCoded (eSeqBits (chosen rate))
                      , vsAfterE = Just rate }
        Just _ -> enter (V32Fail "no common rate") s
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R3") s
                | otherwise -> s
      -- Straight to data on hearing E, rather than counting out B1's
      -- 128 symbol intervals first.  §5.4.2 would have us wait, and the
      -- wait is written and works; what does not yet work is decoding
      -- the far end's B1 while we do.  Coming up in the middle of a run
      -- of scrambled ones ought to yield an idle line and no bytes, and
      -- instead yields about two blocks of noise -- a start-up
      -- transient in the data receiver that going up on E hides,
      -- because the far end's real data then arrives just as we open.
      -- Worth fixing, and a separate thing from Figure 4.
      OB1 -> case detectE (vsPeer s) (vsBits s) of
        Just (off, e) | Just rate <- vsRate s -> enter (V32Up rate) (aidB1 off e s)
        _ | tooLong 80000 s -> enter (V32Fail "no E from the answering modem") s
          | otherwise -> s

      -- ----------------------------------------------- answering modem
      AAns
        | vsSince s >= sym 7200 -> enter AAC s { vsSrc = TxAltAC True }
        | otherwise -> s { vsSrc = TxTone2100 }
      AAC
        -- 5.4.2 asks for the pair "for an even number of symbol
        -- intervals greater than or equal to 128" and sets no upper
        -- bound.  The floor is right when both ends entered the start-up
        -- together, which is what V.8 or a V.32-only configuration
        -- gives.  An answering modem offering the pair out of its own
        -- ladder is talking to a caller that must first /notice/ it --
        -- about 120 ms -- and flipping at 53 ms put the first reversal
        -- on the line before the caller was watching for it, so
        -- 'v32StartOffer' asks for four times the floor.  A real V.32
        -- caller holds carrier state A and listens from 5.4.1's second
        -- paragraph onward, so the extra wait costs it nothing.
        | vs1800Run s >= sym 64 && vsSince s >= sym (vsACHold s) ->
            enter ACA (rearm s) { vsMark = Just (vsN s), vsSrc = TxAltAC False }
        | tooLong (vsACLimit s) s -> enter (V32Fail "no calling modem") s
        | otherwise -> s
      ACA | vsSince s < sym 32 -> s
      ACA -> case revAt 1800 of
        Just i -> enter AAC2 s { vsTrip = fmap (\m -> i - m) (vsMark s)
                               -- back to the parity we started with,
                               -- which is what makes this a reversal
                               , vsSwitch = Just (switchAt s i, [], TxAltAC True) }
        Nothing | tooLong 30000 s -> enter (V32Fail "no reversal in AA") s
                | otherwise -> s
      AAC2
        -- §5.4.2 waits for "an amplitude drop in the incoming tone", and
        -- means the tone rather than the line.  Waiting for the line to
        -- go quiet instead works right up until there is an echo on it:
        -- our own alternating AC comes back at us, the line is never
        -- quiet, and the answering modem waits for a silence that cannot
        -- happen while it is the one making the noise.  The caller's tone
        -- is at 1800 Hz and ours is at 600 and 3000, so measuring the
        -- right thing is also the thing that is immune to our own echo.
        | vsSince s > sym 32 && heard 1800 < 0.2 ->
            enter AGap (restart s) { vsSrc = TxNothing, vsSwitch = Nothing }
        | tooLong 60000 s -> enter (V32Fail "the calling modem did not stop") s
        | otherwise -> s
      AGap
        | vsSince s >= sym 16 ->
            let (ps, sc, q) = conditioningRun dir 1400
            in enter ACond s { vsQueue = ps, vsTxScr = sc, vsTxQ = q
                             , vsSrc = TxCoded (cycle (rateSeqBits (vsOffer s)))
                             , vsAdapt = True }
        | otherwise -> s
      ACond
        | null (vsQueue s) -> enter AR1 s { vsAdapt = False }
        | otherwise -> s
      AR1
        | isConditioning (vsTurns s) ->
            enter AWaitMT s { vsSrc = TxNothing, vsSeenS = True }
        | tooLong 80000 s -> enter (V32Fail "no conditioning signal from the caller") s
        | otherwise -> s
      -- §6.2: cease transmitting on the caller's S, wait MT, then train
      -- on the S that persists or reappears.  MT is there so the
      -- answerer's own echo of R1 has died away before its receiver is
      -- restarted, and the Recommendation is written for an MT of tens
      -- of milliseconds against a TRN of at least 533.  Through an ATA
      -- and a softphone MT measures two seconds -- CA sent at 12.84 s,
      -- CC heard at 14.86 -- so waiting it out means training 1.3 s
      -- after the caller's TRN has ended, on whatever it is sending
      -- by then, and living on how long it keeps repeating R2 before
      -- it gives up.  A CX93001 pinned to 7200 gave up 40 ms after this
      -- state ended.  The cap trains inside TRN on any terrestrial
      -- path; a longer real echo still has the taps ACond adapted.
      AWaitMT
        | vsSince s >= min (sym 512) (maybe (sym 64) id (vsTrip s)) ->
            enter ATrainR2 (restart s)
        | otherwise -> s
      -- No 'ratesForError' here, unlike the caller's R2.  The answering
      -- modem's look at the line is taken where its receiver has just
      -- been restarted for MT and is still converging on the caller's
      -- TRN, and a measurement taken there reads several times the
      -- line: at 45 dB on the bench it still refused 14400.  The
      -- caller's own offer is the one that bounds the choice, and a
      -- caller that judges its line cuts R2 before we ever see it.
      ATrainR2 -> case detectRate (vsBits s) of
        Just r2 | Just rate <- bestCommonRate (vsOffer s) r2 ->
          let (ps, sc, q) = conditioningRun dir 1400
          -- No adapting here, unlike ACond.  The calling modem is still
          -- sending the rate sequence it started in its own conditioning
          -- period and does not stop until E, so this is not one of
          -- Figure 4's half-duplex windows: a canceller told to adapt
          -- through it learns the far end's signal instead of its own
          -- echo and ends up injecting rather than removing.  The taps
          -- from ACond, when the caller really was silent, are the ones
          -- to keep.
          in enter ACond2 s { vsPeer = Just r2, vsRate = Just rate
                            , vsQueue = ps, vsTxScr = sc, vsTxQ = q
                            , vsSrc = TxCoded (cycle (rateSeqBits (chosen rate)))
                            , vsAdapt = False }
        Just _ -> enter (V32Fail "no common rate") s
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R2") s
                | otherwise -> s
      ACond2
        | null (vsQueue s) -> enter AR3 s { vsAdapt = False }
        | otherwise -> s
      AR3 | vsSince s < sym 128 -> s
      AR3 -> case detectE (vsPeer s) (vsBits s) of
        Just (off, e) | Just rate <- vsRate s ->
          enter AE (aidB1 off e (unlockRx s)) { vsSrc = TxCoded (eSeqBits (chosen rate))
                     , vsAfterE = Just rate, vsSince = 0 }
        _ | tooLong 80000 s -> enter (V32Fail "no E from the calling modem") s
          | otherwise -> s
      AE
        -- §5.4.2: scrambled ones for 128 symbols after E, then data --
        -- and not before the far end's own B1 has been seen to start,
        -- within reason
        | vsSince s >= sym 128, Just rate <- vsRate s -> enter (V32Up rate) s
        | otherwise -> s

      V32Up _ -> s
      V32Fail _ -> s

-- | The rate signal that names one rate and nothing else, which is what
-- E and R3 carry (Table 7, and §5.4.2 for R3).  B4 and B8 stay set on
-- the V.32bis rates so the far end can still tell which Recommendation
-- it is talking to.
chosen :: V32Rate -> RateSeq
chosen r = case r of
  V32R4800 -> base { rsCan4800 = True }
  V32R7200 -> bis { rsCan7200 = True }
  V32R9600 -> base { rsCan9600 = True }
  V32R9600T -> base { rsCan9600 = True, rsTrellis = True }
  V32R12000 -> bis { rsCan12000 = True }
  V32R14400 -> bis { rsCan14400 = True }
  where
    base = noRates
    bis = noRates { rsCan2400 = True, rsTrellis = True }

-- | What a V.32 recording contains, as a timeline: the answer tone, the
-- reversals in AC and AA, the conditioning signal, and any rate signals
-- -- everything the start-up's own detectors can see, run over a whole
-- file rather than a live line.  For reading a recorded call back, and
-- for comparing one recorded over a real trunk with one recorded over a
-- pair of pipes.
--
-- Each entry is (seconds, what).  @dir@ is which end /we/ are, since it
-- decides which scrambler the rate signals are read with.
v32Timeline :: Double -> Role -> Signal -> [(Double, String)]
v32Timeline fs dir sig = go st0 0 (chunksOf blk sig) []
  where
    blk = blockOf fs
    st0 = v32StartInit fs dir allRates
    go _ _ [] acc = reverse acc
    go st n (c : cs) acc =
      let st1 = observe st c
          t = fromIntegral n / fs
          revs = [ (t + fromIntegral (i - n) / fs, "phase reversal at " ++ show (round f :: Int) ++ " Hz")
                 | (f, i) <- vsRevAt st1 ]
          levels = [ (f, revLevel tr) | (f, tr) <- [ (1800, vsRev1800 st1), (600, vsRev600 st1), (3000, vsRev3000 st1) ] ]
          tone = [ (t, "tone at " ++ show (round f :: Int) ++ " Hz")
                 | (f, lv) <- levels, lv > 0.45, not (toneWas f st) ]
          cond = [ (t, "conditioning signal (S)") | isConditioning (vsTurns st1), not (vsSeenS st) ]
          -- Every rate signal whose content changes, rather than only
          -- the first one seen.  The first is R1; what a stalled
          -- start-up needs read back to it is whether R2 and R3 ever
          -- followed, and gating on "have we seen TRN yet" hid exactly
          -- that.
          heardRate = detectRate (vsBits st1)
          rate = [ (t, "rate signal " ++ showRates r)
                 | Just r <- [heardRate], Just r /= vsPeer st ]
          eSig = [ (t, "signal E " ++ showRates r) | Just (_, r) <- [detectE Nothing (vsBits st1)] ]
          st2 = st1 { vsSeenS = vsSeenS st || not (null cond)
                    , vsSeenTrn = vsSeenTrn st || not (null rate)
                    , vsPeer = case heardRate of { Just _ -> heardRate; Nothing -> vsPeer st } }
      in go st2 (n + blk) cs (reverse (revs ++ tone ++ cond ++ rate ++ eSig) ++ acc)
    toneWas f st = revLevel (case f of { 1800 -> vsRev1800 st; 600 -> vsRev600 st; _ -> vsRev3000 st }) > 0.45
    showRates r = unwords (["4800" | rsCan4800 r] ++ ["7200" | rsCan7200 r] ++ ["9600" | rsCan9600 r]
                           ++ ["tcm" | rsTrellis r] ++ ["12000" | rsCan12000 r] ++ ["14400" | rsCan14400 r]
                           ++ ["(V.32bis)" | rateSeqV32bis r])
