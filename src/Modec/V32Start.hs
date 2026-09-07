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
-- four transitions the start-up is built from.  'Modec.QAM.RevTracker'
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
  , v32StartStep
  , v32Phase
  , v32Elapsed
  , v32RoundTrip
  , v32EchoAdapt
  , v32Turns
  , v32Bits
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal)
import Modec.QAM
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
  deriving (Eq, Show)

data V32Start = V32Start
  { vsRole    :: !Role'
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
  , vsAnsPh   :: !Double
  -- receive
  , vsRx      :: !QamRxState
  , vsRev1800 :: !RevTracker
  , vsRev600  :: !RevTracker
  , vsRev3000 :: !RevTracker
  , vsMark    :: Maybe Int       -- ^ sample index of the first reversal
  , vsTrip    :: Maybe Int       -- ^ NT or MT, in samples
  , vsTurns   :: [Int]           -- ^ recent quadrant changes, newest first
  , vsPrevSym :: (Double, Double) -- ^ last raw symbol, for the difference
  , vsDescr   :: !Scrambler
  , vsBits    :: [Bool]          -- ^ recent descrambled bits, newest first
  , vsSeenS   :: !Bool
  , vsSeenTrn :: !Bool
  , vsFar     :: !Direction      -- ^ the far end's scrambler
  , vsRevAt   :: [(Double, Int)] -- ^ reversals seen in this block
  , vsQuiet   :: !Int            -- ^ samples the line has been quiet
  , vsAdapt   :: !Bool           -- ^ the echo canceller may adapt now
  }

data Role' = Calling' | Answering' deriving (Eq, Show)

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

-- | Recent quadrant changes, newest first (for tests and tracing).
v32Turns :: V32Start -> [Int]
v32Turns = vsTurns


-- | Recent descrambled bits, newest first.
v32Bits :: V32Start -> [Bool]
v32Bits = vsBits

v32StartInit :: Double -> Direction -> RateSeq -> V32Start
v32StartInit fs dir offer = V32Start
  { vsRole = role, vsPhase = if role == Calling' then OListen else AAns
  , vsFs = fs, vsSps = samplesPerSymbol (v32Params fs)
  , vsN = 0, vsSince = 0
  , vsOffer = offer, vsPeer = Nothing, vsRate = Nothing
  , vsTx = qamTxInit, vsSrc = TxNothing, vsQueue = [], vsTxSym = 0
  , vsSwitch = Nothing, vsTxScr = scramblerInit, vsTxQ = (False, False)
  , vsAnsPh = 0
  , vsRx = qamRxInit p cfg
  , vsRev1800 = revInit fs 1800, vsRev600 = revInit fs 600, vsRev3000 = revInit fs 3000
  , vsMark = Nothing, vsTrip = Nothing
  , vsTurns = [], vsPrevSym = (0, 0)
  , vsDescr = scramblerInit, vsFar = far dir, vsBits = []
  , vsSeenS = False, vsSeenTrn = False, vsRevAt = [], vsQuiet = 0, vsAdapt = False }
  where
    role = case dir of { Calling -> Calling'; Answering -> Answering' }
    p = v32Params fs
    cfg = v32RxCfg V32R4800

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

far :: Direction -> Direction
far Calling = Answering
far Answering = Calling

symbols :: V32Start -> Int -> Int
symbols st k = round (fromIntegral k * vsSps st)

-- | Take in a block: time the phase reversals, and decode whatever the
-- four-point receiver makes of it.
observe :: V32Start -> Signal -> V32Start
observe st rx = st
  { vsRev1800 = r18, vsRev600 = r6, vsRev3000 = r30
  , vsRevAt = [ (1800, i) | i <- e18 ] ++ [ (600, i) | i <- e6 ] ++ [ (3000, i) | i <- e30 ]
  , vsRx = rxSt, vsTurns = turns', vsPrevSym = prev', vsDescr = descr', vsBits = bits'
  , vsQuiet = quiet' }
  where
    (r18, e18) = revBlock rx (vsRev1800 st)
    (r6, e6) = revBlock rx (vsRev600 st)
    (r30, e30) = revBlock rx (vsRev3000 st)
    p = v32Params (vsFs st)
    (rxSt, syms) = qamRxBlock p (v32RxCfg V32R4800) rx (vsRx st)
    -- Everything the start-up has to recognise is a quadrant change:
    -- the conditioning signal is a quarter turn every symbol, the
    -- alternating AC of the earlier phases a half turn, and the rate
    -- signals are dibits differentially encoded by Table 1.  Measuring
    -- the change rather than the position costs nothing and owes
    -- nothing to a carrier loop that has no absolute reference anyway.
    (turns, prev') = quarterTurns syms (vsPrevSym st)
    turns' = take 64 (reverse turns ++ vsTurns st)
    dibits = concat [ [a, b] | k <- turns, let (a, b) = dibitOfTurn k ]
    (descr', got) = descrambleRun (vsFar st) (vsDescr st) dibits
    bits' = take 96 (reverse got ++ vsBits st)
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

-- | A rate signal: two consecutive identical 16-bit sequences with the
-- synchronising bits right, which is the minimum §5.3.1 will accept.
-- The bits arrive newest first and are aligned to nothing, so every
-- offset has to be tried.
detectRate :: [Bool] -> Maybe RateSeq
detectRate bits =
  case [ a | off <- [0 .. 15]
           , let w = take 32 (drop off (reverse (take 64 bits)))
           , length w == 32
           , Just a <- [decodeRateSeq (take 16 w)]
           , Just b <- [decodeRateSeq (drop 16 w)]
           , a == b ] of
    (a : _) -> Just a
    [] -> Nothing

-- | Signal E, which §5.3.2 sends exactly once.  The two-identical-copies
-- rule cannot apply, and one 16-bit sequence with seven fixed bits in it
-- turns up in noise about once in 128 tries per alignment -- often
-- enough to matter.  What makes it safe is what follows: §5.4 has both
-- modems transmit scrambled binary ones immediately after E, so a
-- genuine E is backed by a run of descrambled ones and a coincidence is
-- not.
detectE :: [Bool] -> Maybe RateSeq
detectE bits =
  case [ a | off <- [0 .. 23]
           , let w = take 24 (drop off (reverse (take 64 bits)))
           , length w == 24
           , Just a <- [decodeESeq (take 16 w)]
           , and (drop 16 w) ] of
    (a : _) -> Just a
    [] -> Nothing

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
    in (st { vsSrc = TxCoded rest, vsTxScr = sc', vsTxQ = q' }, ps)

-- | Produce @n@ samples.
emit :: V32Start -> Int -> (V32Start, Signal)
emit st n
  | vsSrc st == TxTone2100 = (st { vsAnsPh = ph' }, tone)
  | otherwise = (st' { vsTx = tx', vsTxSym = vsTxSym st + length pts }, sig)
  where
    p = v32Params (vsFs st)
    -- the V.25 answer tone, reversed every 450 ms to stand down any echo
    -- canceller in the network: we are about to be our own
    w = 2 * pi * 2100 / vsFs st
    tone = VS.generate n $ \i ->
      let t = vsN st + i
          seg = (t * 1000) `div` (round (vsFs st) * 450 `div` 1000) :: Int
          sgn = if even (seg `div` 1000) then 1 else -1
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
codedPoints :: Direction -> Scrambler -> (Bool, Bool) -> [Bool] -> (Scrambler, (Bool, Bool), [Point])
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
conditioningRun :: Direction -> Int -> ([Point], Scrambler, (Bool, Bool))
conditioningRun dir trn = (ps, sc, q)
  where
    ps = conditioningSymbols dir trn
    (sc, _) = scrambleRun dir scramblerInit (replicate (2 * trn) True)
    q = case reverse (trnStates dir trn) of
      (lastSt : _) -> dibitOfState lastSt
      [] -> (False, False)

dibitOfState :: TrainState -> (Bool, Bool)
dibitOfState s = case s of
  StA -> (False, False)
  StB -> (False, True)
  StC -> (True, True)
  StD -> (True, False)

dirOf :: V32Start -> Direction
dirOf st = case vsRole st of { Calling' -> Calling; Answering' -> Answering }

-- | The state machine of Figure 4.
advance :: V32Start -> Int -> V32Start
advance st0 n = step st { vsN = vsN st + n, vsSince = vsSince st + n }
  where
    st = st0
    dir = case vsRole st0 of { Calling' -> Calling; Answering' -> Answering }
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
    restart s = s { vsRx = qamRxReset (v32Params (vsFs s)) (v32RxCfg V32R4800) (vsRx s)
                  , vsTurns = [], vsBits = [], vsPrevSym = (0, 0)
                  , vsDescr = scramblerInit }
    rearm s = s { vsRev1800 = revRearm (vsRev1800 s)
                , vsRev600 = revRearm (vsRev600 s)
                , vsRev3000 = revRearm (vsRev3000 s)
                , vsRevAt = [] }
    revAt f = case [ i | (g, i) <- vsRevAt st0, g == f ] of
      (i : _) -> Just i
      [] -> Nothing
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
        | isConditioning (vsTurns s) -> enter OTrainR1 s { vsSeenS = True }
        | tooLong 60000 s -> enter (V32Fail "no conditioning signal") s
        | otherwise -> s
      OTrainR1 -> case detectRate (vsBits s) of
        Just r1 -> enter OHoldS s { vsPeer = Just r1, vsSrc = TxAltPair StA StB }
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R1") s
                | otherwise -> s
      OHoldS
        | vsSince s >= maybe (sym 256) id (vsTrip s) ->
            let (ps, sc, q) = conditioningRun dir 1400
            in enter OCond s { vsQueue = ps, vsTxScr = sc, vsTxQ = q
                             , vsSrc = TxCoded (cycle (rateSeqBits (vsOffer s)))
                             , vsAdapt = True }
        | otherwise -> s
      OCond
        | null (vsQueue s) -> enter OR2 s { vsAdapt = False }
        | otherwise -> s
      OR2 -> case detectRate (vsBits s) of
        Just r3 | Just rate <- bestCommonRate (vsOffer s) r3 ->
          enter OB1 s { vsRate = Just rate
                      , vsSrc = TxCoded (eSeqBits (chosen rate) ++ repeat True) }
        Just _ -> enter (V32Fail "no common rate") s
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R3") s
                | otherwise -> s
      OB1 -> case detectE (vsBits s) of
        Just _ | Just rate <- vsRate s -> enter (V32Up rate) s
        _ | tooLong 80000 s -> enter (V32Fail "no E from the answering modem") s
          | otherwise -> s

      -- ----------------------------------------------- answering modem
      AAns
        | vsSince s >= sym 7200 -> enter AAC s { vsSrc = TxAltAC True }
        | otherwise -> s { vsSrc = TxTone2100 }
      AAC
        | heard 1800 > 0.45 && vsSince s >= sym 128 ->
            enter ACA (rearm s) { vsMark = Just (vsN s), vsSrc = TxAltAC False }
        | tooLong 60000 s -> enter (V32Fail "no calling modem") s
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
        -- the caller goes off the line once it has timed the round trip
        | vsQuiet s > sym 8 -> enter AGap (restart s) { vsSrc = TxNothing, vsSwitch = Nothing }
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
      AWaitMT
        | vsSince s >= maybe (sym 64) id (vsTrip s) -> enter ATrainR2 (restart s)
        | otherwise -> s
      ATrainR2 -> case detectRate (vsBits s) of
        Just r2 | Just rate <- bestCommonRate (vsOffer s) r2 ->
          let (ps, sc, q) = conditioningRun dir 1400
          in enter ACond2 s { vsPeer = Just r2, vsRate = Just rate
                            , vsQueue = ps, vsTxScr = sc, vsTxQ = q
                            , vsSrc = TxCoded (cycle (rateSeqBits (chosen rate)))
                            , vsAdapt = True }
        Just _ -> enter (V32Fail "no common rate") s
        Nothing | tooLong 80000 s -> enter (V32Fail "no rate signal R2") s
                | otherwise -> s
      ACond2
        | null (vsQueue s) -> enter AR3 s { vsAdapt = False }
        | otherwise -> s
      AR3 -> case detectE (vsBits s) of
        Just _ | Just rate <- vsRate s ->
          enter AE s { vsSrc = TxCoded (eSeqBits (chosen rate) ++ repeat True)
                     , vsSince = 0 }
        _ | tooLong 80000 s -> enter (V32Fail "no E from the calling modem") s
          | otherwise -> s
      AE
        -- §5.4.2: scrambled ones for 128 symbols after E, then data
        | vsSince s >= sym 128, Just rate <- vsRate s -> enter (V32Up rate) s
        | otherwise -> s

      V32Up _ -> s
      V32Fail _ -> s

-- | The rate signal that names one rate and nothing else, which is what
-- E and R3 carry (Table 7, and §5.4.2 for R3).
chosen :: V32Rate -> RateSeq
chosen r = case r of
  V32R4800 -> RateSeq False True False False
  V32R9600 -> RateSeq False True True False
  V32R9600T -> RateSeq False True True True
