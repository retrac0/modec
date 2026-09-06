-- | Call establishment for Bell 103, V.21, Bell 212A, V.22 and V.22bis.
--
-- Which of those the modem will negotiate is configured with 'hcModes',
-- an ordered list (best first).  It decides what V.8bis advertises, which
-- signals the answering side probes with, and which of Bell 103 and
-- Bell 212A a 2225 Hz answer tone is answered with.
--
-- Bell 212A is the V.22 data pump and the V.22 handshake timings
-- (456 ms, 270 ms, 765 ms) with the 2225 Hz Bell answer tone in place of
-- unscrambled binary 1, no guard tone and no 2400 bit/s rate; V.22
-- §6.3.1.1 notes exactly this substitution.  Because the Bell answer tone
-- is also the Bell 103 answer mark, one probe serves both and the
-- caller's reply decides: an FSK originate carrier means Bell 103,
-- scrambled DPSK marks on the low channel mean Bell 212A.
--
-- Both state machines are pure and tick-driven: feed them one
-- 'ToneFrame' per hop (20 ms by default), plus the latest report from
-- a V.22 receiver listening on the other side's channel, and they return
-- what the transmitter should be doing and whether a data connection
-- has been established.
--
-- Answering side (ITU-T V.25 timing, then automode):
--
-- 1. Silence for the billing delay (1.8-2.5 s).
-- 2. ANS: 2100 Hz for 2.6-4.0 s.  A Bell 103 caller that answers it
--    with 1270 Hz is accepted straight away.
-- 3. Silence 75 +/- 20 ms.
-- 4. Probe standards in turn until the caller responds: V.22 (transmit
--    unscrambled binary 1 on the high channel, listen for scrambled 1 or 0
--    on the low channel for 270 ms, then scrambled ones for 765 ms and
--    data), V.21 (channel 2 mark, listen for channel 1 mark), Bell 103
--    (answer mark, listen for originate mark).  A Bell 103 caller that
--    mistakes unscrambled ones for the Bell answer tone is accepted
--    during the V.22 probe.  Fixing the standard skips the rotation.
--
-- Calling side:
--
-- 1. Listen.  Steady 2225 Hz is a Bell 103 answerer: reply with 1270 Hz.
--    2100 Hz is an ITU answer tone: wait for it to end.  Then whichever
--    signal follows picks the standard: V.22 unscrambled ones (155 ms,
--    then 456 ms of silence, then scrambled ones until the answerer's
--    scrambled ones have been seen for 270 ms, then 765 ms more),
--    1650 Hz (V.21, reply 980 Hz) or 2225 Hz (Bell 103).
-- 2. An FSK carrier must persist for the qualification time (300 ms,
--    within the 300-700 ms V.21 allows for circuit 109) before it counts.
--
-- V.8bis (when enabled, the default): after the billing delay the
-- answering station sends CRe (1375 + 2002 Hz for 400 ms, then 400 Hz
-- for 100 ms) and listens for up to 3 s.  A calling station that sees
-- CRe answers with ESr (1529 + 2225 Hz, then 1650 Hz which doubles as the
-- V.21 channel 2 preamble) and a CL message listing V.21, V.22 and
-- V.22bis; the answering station picks the best common mode and sends MS
-- on V.21 channel 1.  As V.8bis §9.9 requires, the station that received
-- MS then becomes the answering modem and the one that sent it the
-- calling modem, and the V.25 start-up follows with the roles reversed.
-- Without a response the classic start-up begins after 3 s.
--
-- V.22bis rate negotiation (§6.3.1.1): a 2400-capable caller sends the S1
-- pattern for 100 ms before its scrambled ones; an answerer that sees S1
-- turns circuit 112 on, answers with S1 and scrambled ones, and both sides
-- switch to scrambled ones at 2400 bit/s 600 ms after their own 112 ON,
-- with 16-way decisions from 450 ms.  32 consecutive received ones at
-- 2400 bit/s complete the connection.  Without S1 the connection stays at
-- 1200 bit/s.
--
-- Unscrambled binary 1 on the high channel is a tone at 2250 Hz, which a
-- 40 ms tone bank cannot separate from the 2225 Hz Bell answer tone, so
-- the V.22 receiver's phase-step quality decides: unscrambled ones give
-- exact 270 degree steps, 2225 Hz gives steps 15 degrees off.
module Modec.Handshake
  ( Standard (..)
  , allStandards
  , isV22Family
  , Role (..)
  , Link (..)
  , HsConfig (..)
  , defaultHsConfig
  , TxCmd (..)
  , V22Report (..)
  , HsOut (..)
  , HsStatus (..)
  , HsState
  , initialHandshake
  , HsIn (..)
  , noHsIn
  , handshakeStep
  , handshakeStage
  , linkFor
  , v22LinkAt
  ) where

import Modec.Detect
import Modec.DSP (fromDb)
import Modec.Hdlc (hdlcFrameBits)
import Modec.Standards
import Modec.Stream
import Modec.V22 (Rate (..), TxMode (..), V22Channel (..))
import Modec.V8
import Modec.V8bis
import Data.Word (Word8)

-- | A modulation the modem can negotiate.  'Bell212A' is the North
-- American 1200 bit/s DPSK standard: the same 600 baud data pump and
-- handshake timings as V.22, but announced with the 2225 Hz Bell answer
-- tone instead of unscrambled binary 1, without guard tones, and with no
-- 2400 bit/s rate.  'V22bis' is V.22 that negotiated 2400 bit/s.
data Standard = Bell103 | V21 | Bell212A | V22 | V22bis deriving (Eq, Show, Enum, Bounded)

-- | Every mode, best first; the default configuration.
allStandards :: [Standard]
allStandards = [V22bis, V22, Bell212A, V21, Bell103]

-- | Modes that use the V.22 data pump.
isV22Family :: Standard -> Bool
isV22Family s = s `elem` [Bell212A, V22, V22bis]

data Role = Originate | Answer deriving (Eq, Show)

-- | The channels of an established connection: our transmit side and
-- our receive side.
data Link
  = FskLink FskSpec FskSpec
  | V22Link V22Channel V22Channel Rate
  deriving (Eq, Show)

-- | The link a standard runs on, at its own rate.
linkFor :: Role -> Standard -> Link
linkFor Originate Bell103 = FskLink bell103Originate bell103Answer
linkFor Answer Bell103 = FskLink bell103Answer bell103Originate
linkFor Originate V21 = FskLink v21Channel1 v21Channel2
linkFor Answer V21 = FskLink v21Channel2 v21Channel1
linkFor role s = v22LinkAt role (if s == V22bis then R2400 else R1200)

v22LinkAt :: Role -> Rate -> Link
v22LinkAt Originate r = V22Link LowChannel HighChannel r
v22LinkAt Answer r = V22Link HighChannel LowChannel r

data HsConfig = HsConfig
  { hcRole        :: Role
  , hcModes       :: [Standard]       -- ^ modes we will negotiate, best first
  , hcBank        :: ToneBankConfig
  , hcSquelch     :: Double           -- ^ minimum tone amplitude
  , hcDomRatio    :: Double           -- ^ dominant tone must exceed others by this factor
  , hcBilling     :: Double           -- ^ answer: silence before ANS (1.8-2.5 s)
  , hcAnsDuration :: Double           -- ^ answer: ANS duration (2.6-4.0 s)
  , hcAnsGap      :: Double           -- ^ answer: silence after ANS (55-95 ms)
  , hcProbe       :: Double           -- ^ answer automode: time per standard before switching
  , hcQualify     :: Double           -- ^ FSK carrier must persist this long to count
  , hcDrop        :: Double           -- ^ carrier loss for this long drops the connection
  , hcTimeout     :: Double           -- ^ give up after this long
  , hcV8bis       :: Bool             -- ^ try a V.8bis capabilities exchange before the modem start-up
  , hcV8          :: Bool             -- ^ answer with ANSam and exchange V.8 CM/JM menus
  } deriving (Show)

defaultHsConfig :: Role -> HsConfig
defaultHsConfig role = HsConfig
  { hcRole = role, hcModes = allStandards, hcBank = defaultToneBank
  , hcSquelch = 3e-3, hcDomRatio = 1.5
  , hcBilling = 2.0, hcAnsDuration = 3.0, hcAnsGap = 0.075, hcProbe = 1.5
  , hcQualify = 0.3, hcDrop = 0.5, hcTimeout = 45, hcV8bis = True, hcV8 = False }

-- | What the transmitter should be doing right now.
data TxCmd
  = TxSilence
  | TxTone Double        -- ^ a single tone (answer tone)
  | TxMark FskSpec       -- ^ idle mark on this channel
  | TxData FskSpec       -- ^ data mode on this channel
  | TxV22 V22Channel Rate TxMode
  | TxDual Double Double Double   -- ^ two tones at the given amplitude factor (V.8bis segment 1)
  | TxBits FskSpec [Bool]         -- ^ queue these bits on the FSK channel, then idle mark
  | TxAnsam              -- ^ V.8 modified answer tone
  deriving (Eq, Show)

-- | What a V.22 receiver listening to the remote channel currently sees.
data V22Report = V22Report
  { vrEnergy   :: !Double
  , vrAngleErr :: !Double   -- ^ mean phase-step error in degrees
  , vrU11Run   :: !Int      -- ^ consecutive symbols of unscrambled ones
  , vrOnesRun  :: !Int      -- ^ consecutive descrambled ones
  , vrZerosRun :: !Int      -- ^ consecutive descrambled zeros
  , vrS1Run    :: !Int      -- ^ consecutive symbols of the S1 double-dibit pattern
  , vrOnes2400 :: !Int      -- ^ consecutive descrambled ones decided 16-way
  } deriving (Show)

-- | What the receivers found for the handshake this hop.  Everything
-- here arrives on the first tone frame of an audio block.
data HsIn = HsIn
  { hiFrames :: [[Word8]]    -- ^ HDLC frames (V.8bis)
  , hiV8     :: [V8Event]    -- ^ V.8 signals off the async V.21 receiver
  , hiAnsam  :: !Bool        -- ^ ANSam was confirmed in this block
  }

-- | Nothing was received.
noHsIn :: HsIn
noHsIn = HsIn [] [] False

-- | What the handshake wants from the modem this hop.
data HsOut = HsOut
  { hoTx     :: TxCmd
  , hoStatus :: HsStatus
  , hoRxRate :: Rate            -- ^ decision rate for the V.22 receiver
  , hoRole   :: Role            -- ^ effective modem role (V.8bis can reverse it)
  , hoHdlc   :: Maybe FskSpec   -- ^ run a synchronous V.21 receiver on this channel for HDLC frames
  , hoV8     :: Maybe FskSpec   -- ^ run a V.21 receiver on this channel for V.8 signals
  , hoV8Menu :: Maybe V8Menu    -- ^ the remote's menu, the hop it is confirmed
  } deriving (Show)

data HsStatus
  = HsBusy
  | HsConnected Standard Link
  | HsDropped
  | HsFailed String
  deriving (Eq, Show)

data Phase
  = ABilling
  | A8Dual                       -- ^ V.8bis: CRe segment 1 (dual tone)
  | A8Tone                       -- ^ V.8bis: CRe segment 2 (400 Hz)
  | A8Wait                       -- ^ V.8bis: waiting for ESr and the CL message
  | A8SendMS                     -- ^ V.8bis: MS message going out on V.21 channel 1
  | O8Dual                       -- ^ V.8bis: ESr segment 1
  | O8Tone                       -- ^ V.8bis: ESr segment 2 / message preamble (1650 Hz mark)
  | O8SendCL                     -- ^ V.8bis: CL message going out on V.21 channel 2
  | O8WaitMS                     -- ^ V.8bis: waiting for MS on V.21 channel 1
  | AV8Ansam                     -- ^ V.8: ANSam out, listening for CM
  | AV8JM                        -- ^ V.8: JM going out on V.21 channel 2
  | AV8Gap                       -- ^ V.8: 75 ms of silence before sigA
  | AAns
  | AGap
  | AProbe Standard              -- ^ answering: sending this standard's answer signal
  | AV22Ones                     -- ^ answering: scrambled ones for 765 ms (1200 bit/s)
  | AV22S1                       -- ^ answering: S1 seen (112 ON), sending S1 for 100 ms
  | AV22Ones1200                 -- ^ answering: scrambled ones at 1200 until 600 ms after 112 ON
  | AV22Ones2400                 -- ^ answering: scrambled ones at 2400, waiting for 32 of the remote's
  | OListen
  | OAnsEnding                   -- ^ ITU answer tone heard, waiting for it to end
  | OAfterAns                    -- ^ answer tone over, waiting for a carrier
  | OV8Wait                      -- ^ V.8: ANSam heard, silent for Te before CM
  | OV8CM                        -- ^ V.8: CM going out on V.21 channel 1
  | OV8CJ                        -- ^ V.8: CJ acknowledging JM
  | OV8Gap                       -- ^ V.8: 75 ms of silence before sigC
  | OReply Standard              -- ^ our FSK carrier is up, qualifying the remote
  | OV22Wait                     -- ^ unscrambled ones seen; 456 ms of silence
  | OV22S1                       -- ^ sending S1 for 100 ms (2400 capable)
  | OV22Ones                     -- ^ sending scrambled ones, waiting for the remote's (or its S1)
  | OV22Settle                   -- ^ remote scrambled ones seen; 765 ms more (1200 bit/s)
  | OV22Ones1200                 -- ^ remote S1 seen (112 ON); scrambled ones at 1200 until 600 ms
  | OV22Ones2400                 -- ^ scrambled ones at 2400, waiting for 32 of the remote's
  | Connected Standard Rate
  | V8NoMode                     -- ^ V.8 ran but found nothing in common
  | Done
  deriving (Eq, Show)

data HsState = HsState
  { hsPhase     :: !Phase
  , hsRole      :: !Role     -- ^ effective role (reversed by a V.8bis mode select)
  , hsSelected  :: !(Maybe Standard)   -- ^ standard selected by V.8bis
  , hsPairRun   :: !Double   -- ^ seconds the current V.8bis dual tone pair has been present
  , hsPairSeen  :: !(Maybe (Bool, Double))  -- ^ (initiating pair?, when it ended) for segment 2 matching
  , hsSig       :: !(Maybe Signal8)    -- ^ V.8bis signal detected this hop
  , hsLastRsp   :: !Bool     -- ^ the responding pair was on in the previous frame
  , hsPendingMs :: !(Maybe DataMode)   -- ^ mode select to transmit
  , hsFamily    :: !Standard  -- ^ which V.22-family standard is being negotiated
  , hsTried212  :: !Bool     -- ^ a Bell 212A attempt already failed; prefer Bell 103
  , hsPhaseAt   :: !Double   -- ^ time the phase was entered
  , hs112At     :: !Double   -- ^ time circuit 112 went ON (S1 exchanged)
  , hsToneSince :: !(Maybe (Double, Double))  -- ^ current dominant tone and when it started
  , hsLastTone  :: !Double   -- ^ last time any tone was dominant
  , hsAnsam     :: !Bool     -- ^ ANSam has been confirmed on this call
  , hsV8Last    :: !(Maybe V8Menu)  -- ^ the last V.8 sequence received
  , hsV8Reps    :: !Int      -- ^ how many times running it has been identical
  , hsV8Peer    :: !(Maybe V8Menu)  -- ^ the remote's menu, once confirmed
  , hsV8Mod     :: !(Maybe Modulation)  -- ^ what V.8 selected
  , hsT         :: !Double
  }

initialHandshake :: HsConfig -> HsState
initialHandshake cfg = HsState (case hcRole cfg of Answer -> ABilling; Originate -> OListen) (hcRole cfg) Nothing 0 Nothing Nothing False Nothing V22 False 0 0 Nothing (-1) False Nothing 0 Nothing Nothing 0

fskTx, fskRx :: Role -> Standard -> FskSpec
fskTx role s = case linkFor role s of
  FskLink t _ -> t
  V22Link {} -> error "fskTx: V.22"
fskRx role s = case linkFor role s of
  FskLink _ r -> r
  V22Link {} -> error "fskRx: V.22"

-- 1200 bit/s: 155 ms of unscrambled ones = 93 symbols; 270 ms = 324 bits;
-- S1 lasts 100 ms = 60 symbols, half of it is enough to recognise it
u11Symbols, scrambledBits, s1Symbols :: Int
u11Symbols = 93
scrambledBits = 324
s1Symbols = 30

-- | Advance the state machine by one tone frame and the latest V.22
-- receiver report (if a V.22 receiver is running).
handshakeStep :: HsConfig -> HsState -> ToneFrame -> Maybe V22Report -> HsIn -> (HsState, HsOut)
handshakeStep cfg st fr v22 inp = (st'', HsOut tx status rxRate (hsRole st'') hdlcListen v8Listen v8MenuOut)
  where
    t = tfTime fr
    dom = dominant (hcBank cfg) (hcSquelch cfg) (hcDomRatio cfg) fr
    role = hsRole st
    hop = tbHopSec (hcBank cfg)
    -- V.8bis dual tone pair detection: both tones above squelch and each at
    -- least half of the strongest tone in the bank; then the segment 2 tone
    amp f = toneAmp (hcBank cfg) fr f
    strongest = maximum (0 : [ amp f | f <- tbFreqs (hcBank cfg) ])
    pairOn (a, b) = amp a > hcSquelch cfg && amp b > hcSquelch cfg && amp a >= 0.5 * strongest && amp b >= 0.5 * strongest
    iniPair = pairOn (1375, 2002)
    rspPair = pairOn (1529, 2225)
    pairRun' | iniPair || rspPair = hsPairRun st + hop
             | otherwise = 0
    pairSeen' | (iniPair || rspPair) = hsPairSeen st
              | hsPairRun st >= 0.25 = Just (not rspPair && iniPairWas, t)
              | otherwise = case hsPairSeen st of
                  Just (ip, t0) | t - t0 <= 0.2 -> Just (ip, t0)
                  _ -> Nothing
      where iniPairWas = True
    -- the pair just ended: which pair it was is remembered from the last frame it was on
    pairKind = if rspPair then False else True
    pairSeen'' = case pairSeen' of
      Just (_, t0) | t0 == t -> Just (pairKind && not rspPairPrev, t0)
      other -> other
      where rspPairPrev = hsLastRsp st
    seg2 = case (pairSeen'', dom) of
      (Just (ini, _), Just f) ->
        case [ s | s <- [minBound .. maxBound], signalIsInitiating s == ini, snd (signalTones s) == f ] of
          (s : _) | heardFor f >= 0.05 -> Just s
          _ -> Nothing
      _ -> Nothing
    sig = seg2
    -- messages received this hop
    frames = hiFrames inp
    msgs = map decodeMessage frames
    clOffered = [ ms | CL ms <- msgs ]
    msSelected = [ m | MS m <- msgs ]
    -- V.8bis can only advertise ITU modes; Bell 212A has no codepoint
    modeToStandard m = case m of { ModeV21 -> V21; ModeV22 -> V22; ModeV22bis -> V22bis }
    standardToMode s = case s of
      V21 -> Just ModeV21
      V22 -> Just ModeV22
      V22bis -> Just ModeV22bis
      _ -> Nothing
    ourModes = [ m | s <- modes, Just m <- [standardToMode s] ]
    -- V.8 signals seen this hop.  Two identical sequences are required
    -- before either side acts on a menu (7.4, 8.1.2).
    v8Seqs = [ m | V8Sequence _ m <- hiV8 inp ]
    cjSeen = V8CJ `elem` hiV8 inp
    (v8Last', v8Reps') = foldl again (hsV8Last st, hsV8Reps st) v8Seqs
      where again (prev, k) m | prev == Just m = (Just m, k + 1)
                              | otherwise = (Just m, 1)
    v8Confirmed = if v8Reps' >= 2 then v8Last' else Nothing
    ansamSeen = hsAnsam st || hiAnsam inp
    -- only ITU modes have codepoints in Table 4, so a Bell-only
    -- configuration has nothing it can offer here
    ourV8Mods = [ MV22 | any (`elem` modes) [V22, V22bis] ] ++ [ MV21 | V21 `elem` modes ]
    v8Offer = emptyMenu { v8Call = Just CfData, v8Mods = ourV8Mods }
    -- JM lists what both have, and keeps the CM's octet count even when
    -- that is nothing at all (8.2.3)
    v8Reply peer = emptyMenu
      { v8Call = v8Call peer
      , v8Mods = [ m | m <- v8Mods peer, m `elem` ourV8Mods ]
      , v8ModOctets = v8ModOctets peer }
    pickMode offered = case [ m | m <- ourModes, m `elem` offered ] of
      (m : _) -> Just m
      [] -> Nothing
    toneSince = case (dom, hsToneSince st) of
      (Just f, Just (g, since)) | f == g -> Just (f, since)
      (Just f, _) -> Just (f, t)
      (Nothing, _) -> Nothing
    lastTone = if dom == Nothing then hsLastTone st else t
    heardFor f = case toneSince of
      Just (g, since) | g == f -> t - since
      _ -> 0
    quiet = t - lastTone
    -- how long a tone other than the ITU answer tone has been dominant;
    -- an answering modem that steps straight from the answer tone to the
    -- next rung of its fallback ladder leaves no silence between them
    sinceOtherTone = case toneSince of
      Just (f, since) | f /= 2100 -> t - since
      _ -> 0
    st' = st { hsToneSince = toneSince, hsLastTone = lastTone, hsT = t
             , hsAnsam = ansamSeen, hsV8Last = v8Last', hsV8Reps = v8Reps'
             , hsPairRun = pairRun', hsPairSeen = if sig /= Nothing then Nothing else pairSeen'', hsSig = sig, hsLastRsp = rspPair }
    inPhase = t - hsPhaseAt st
    -- the configured modes, narrowed to one by a V.8bis mode select
    modes = case hsSelected st of
      Just sel -> [sel]
      Nothing -> hcModes cfg
    allowed s = s `elem` modes
    v22Allowed = any (`elem` modes) [V22, V22bis]
    allow2400 = V22bis `elem` modes
    ituAllowed = any (`elem` modes) [V21, V22, V22bis]
    -- scrambled DPSK marks answering our 2225 Hz mean a 1200 bit/s link;
    -- V.22 modems do this too (V.22 §6.3.1.1 note), so accept either name
    bellDpsk = [ s | s <- [Bell212A, V22], allowed s ]
    -- With no V.22-family mode configured there is nothing to disturb by
    -- transmitting early, so follow V.25 and put our carrier up as soon as
    -- the answer tone ends rather than waiting to hear the answerer's.
    -- An answering modem that steps through a fallback ladder may hold
    -- each rung open for only a second or two.
    fskOnly = not v22Allowed && (allowed V21 || allowed Bell103)
    preferredFsk = case [ s | s <- modes, s `elem` [V21, Bell103] ] of
      (s : _) -> s
      [] -> V21
    -- which Bell mode a 2225 Hz answer tone should be answered with
    bellChoice = case [ s | s <- modes, s `elem` [Bell212A, Bell103], not (s == Bell212A && hsTried212 st) ] of
      (s : _) -> Just s
      [] -> Nothing
    enter p = st' { hsPhase = p, hsPhaseAt = t }
    justEntered = hsPhaseAt st == t
    -- V.22 signal detectors
    u11Seen = case v22 of
      Just r -> vrU11Run r >= u11Symbols && vrAngleErr r < 8
      Nothing -> False
    scrambledOnesSeen = case v22 of
      Just r -> vrOnesRun r >= scrambledBits
      Nothing -> False
    scrambledAnySeen = case v22 of
      Just r -> vrOnesRun r >= scrambledBits || vrZerosRun r >= scrambledBits
      Nothing -> False
    s1Seen = allow2400 && hsFamily st /= Bell212A && case v22 of
      Just r -> vrS1Run r >= s1Symbols
      Nothing -> False
    ones2400Seen = case v22 of
      Just r -> vrOnes2400 r >= 32
      Nothing -> False
    since112 = t - hs112At st
    enter112 p = (enter p) { hs112At = t }
    -- an FSK carrier counts once it has persisted; the Bell 103 answer
    -- mark must not be V.22 unscrambled ones in disguise
    qualified s | isV22Family s = False   -- qualified through the V.22 receiver, not tones
    qualified s = heardFor (fskMark (fskRx role s)) >= hcQualify cfg && not (s == Bell103 && role == Originate && u11Seen)
    -- V.22 §6.3.1.1 note: some answering modems emit 2225 Hz where the
    -- Recommendation has unscrambled binary 1; that is a Bell 212A answerer
    bell212Trigger = heardFor 2225 >= 0.155 && not u11Seen
    remoteAlive s = case linkFor role s of
      FskLink _ rx -> t - lastToneOf rx <= hcDrop cfg
      V22Link {} -> True
      where lastToneOf spec = case toneSince of
              Just (g, _) | g == fskMark spec || g == fskSpace spec -> t
              _ -> hsLastTone st
    -- one probe per family, in the traditional order, skipping families
    -- this modem is not configured for
    probeOrder = [ p | (p, needed) <- [ (V22, v22Allowed), (V21, allowed V21)
                                      , (Bell103, allowed Bell103 || allowed Bell212A) ], needed ]
    rotating = length probeOrder > 1
    nextProbe s = case dropWhile (/= s) probeOrder of
      (_ : n : _) -> n
      _ -> head probeOrder
    firstProbe = case probeOrder of
      (p : _) -> p
      [] -> Bell103

    st'' = case hsPhase st of
      _ | t > hcTimeout cfg && not (isConnected (hsPhase st)) && hsPhase st /= Done -> enter Done
      -- answering side
      ABilling
        | inPhase >= hcBilling cfg ->
            if hcV8 cfg && ituAllowed then enter AV8Ansam
            else if hcV8bis cfg && ituAllowed then enter A8Dual
            -- a Bell-only modem answers with 2225 Hz, never with the ITU tone
            else if ituAllowed then enter AAns else enter (AProbe Bell103)
      -- V.8bis, answering station initiating with CRe (transaction 2, no ACK requested)
      A8Dual
        | inPhase >= 0.4 -> enter A8Tone
      A8Tone
        | inPhase >= 0.1 -> enter A8Wait
      A8Wait
        | (m : _) <- [ pm | offered <- clOffered, Just pm <- [pickMode offered] ] ->
            (enter A8SendMS) { hsSelected = Just (modeToStandard m), hsPendingMs = Just m }
        | not (null clOffered) -> enter AAns             -- nothing in common: classic start-up
        | inPhase >= 3 -> enter AAns                     -- no V.8bis response
      A8SendMS
        -- 100 ms preamble plus about 15 octets at 300 bit/s; then we are the calling modem
        | inPhase >= 0.6 -> (enter OListen) { hsRole = Originate }
      -- 8.2.2: ANSam runs for 5 +/- 1 s if nothing takes it up
      AV8Ansam
        | Just peer <- v8Confirmed -> (enter AV8JM) { hsV8Peer = Just peer, hsV8Mod = v8Pick peer }
        | inPhase >= 5 -> enter AGap
      -- 8.2.3: JM continues until all three octets of CJ are in
      AV8JM
        | cjSeen -> enter AV8Gap
        | inPhase >= 3 -> enter AV8Gap
      AV8Gap
        | inPhase >= 0.075 -> case hsV8Mod st of
            Just MV22 -> enter (AProbe V22)
            Just MV21 -> enter (AProbe V21)
            _ -> enter V8NoMode
      AAns
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | inPhase >= hcAnsDuration cfg -> enter AGap
      AGap
        | inPhase >= hcAnsGap cfg -> enter (AProbe firstProbe)
      AProbe V22
        | s1Seen -> enter112 AV22S1
        | scrambledAnySeen -> (enter AV22Ones) { hsFamily = V22 }
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe V22))
        | otherwise -> st'
      -- the Bell probe transmits 2225 Hz, which serves Bell 103 and
      -- Bell 212A alike; the caller's reply says which it wanted
      AProbe Bell103
        | scrambledAnySeen, (fam : _) <- bellDpsk -> (enter AV22Ones) { hsFamily = fam }
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe Bell103))
        | otherwise -> st'
      AProbe s
        | qualified s -> enter (Connected s R1200)
        -- a Bell 103 caller transmits continuously, so accept it whatever
        -- we happen to be probing with
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe s))
      AV22Ones
        | inPhase >= 0.765 -> enter (Connected (hsFamily st) R1200)
      AV22S1
        | inPhase >= 0.1 -> enter AV22Ones1200
      AV22Ones1200
        | since112 >= 0.6 -> enter AV22Ones2400
      AV22Ones2400
        | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22bis R2400)
        | inPhase >= 6 -> enter Done
      -- V.8bis, calling station responding to CRe
      O8Dual
        | inPhase >= 0.4 -> enter O8Tone
      O8Tone
        | inPhase >= 0.1 -> enter O8SendCL
      O8SendCL
        | inPhase >= 0.6 -> enter O8WaitMS
      O8WaitMS
        | (m : _) <- msSelected, m `elem` ourModes ->
            (enter AAns) { hsRole = Answer, hsSelected = Just (modeToStandard m) }
        | inPhase >= 3 -> enter OListen
      -- calling side
      OListen
        | hcV8bis cfg && ituAllowed && sig == Just CRe && role == Originate -> enter O8Dual
        | v22Allowed && u11Seen -> (enter OV22Wait) { hsFamily = V22 }
        | bellChoice == Just Bell212A && bell212Trigger -> (enter OV22Wait) { hsFamily = Bell212A }
        | bellChoice == Just Bell103 && qualified Bell103 -> enter (OReply Bell103)
        | heardFor 2100 >= hcQualify cfg -> enter OAnsEnding
      OAnsEnding
        -- the answer tone is modulated: the far end speaks V.8 and is
        -- waiting to be told what we have (7.2, 8.1.1)
        | hcV8 cfg && ansamSeen && not (null ourV8Mods) -> enter OV8Wait
        | dom /= Just 2100 && (quiet >= 0.04 || sinceOtherTone >= 0.1) -> enter OAfterAns
      OAfterAns
        | v22Allowed && u11Seen -> (enter OV22Wait) { hsFamily = V22 }
        -- our carrier is already up in FSK-only mode, so the answerer's
        -- carrier is all that is still needed
        | allowed V21 && qualified V21 -> enter (if fskOnly then Connected V21 R1200 else OReply V21)
        | allowed Bell103 && qualified Bell103 -> enter (if fskOnly then Connected Bell103 R1200 else OReply Bell103)
      -- Te, the silence before CM: at least 0.5 s, and a full second
      -- when network echo cancellers are to be disabled (8.1.1)
      OV8Wait
        | inPhase >= 1.0 -> enter OV8CM
      OV8CM
        | Just peer <- v8Confirmed -> (enter OV8CJ) { hsV8Peer = Just peer, hsV8Mod = v8Pick peer }
        | inPhase >= 4 -> enter OAfterAns          -- no JM: back to the ladder
      OV8CJ
        -- three octets at 300 bit/s
        | inPhase >= 0.1 -> enter OV8Gap
      OV8Gap
        | inPhase >= 0.075 -> case hsV8Mod st of
            -- V.8 does not separate V.22 from V.22bis: the answerer now
            -- sends unscrambled binary 1 and the rate is settled by the
            -- usual S1 exchange
            Just MV22 -> enter OAfterAns
            Just MV21 -> enter (OReply V21)
            _ -> enter V8NoMode
      OReply s
        | inPhase >= hcQualify cfg && qualified s -> enter (Connected s R1200)
        | inPhase >= 5 -> enter OListen
      OV22Wait
        | inPhase >= 0.456 -> enter (if allow2400 && hsFamily st /= Bell212A then OV22S1 else OV22Ones)
      OV22S1
        | inPhase >= 0.1 -> enter OV22Ones
      OV22Ones
        | s1Seen -> enter112 OV22Ones1200
        | scrambledOnesSeen -> enter OV22Settle
        -- the far end did not take it up: fall back to the other Bell mode
        | inPhase >= 6 -> (enter OListen) { hsTried212 = hsTried212 st || hsFamily st == Bell212A }
      OV22Settle
        | s1Seen -> enter112 OV22Ones1200
        | inPhase >= 0.765 -> enter (Connected (hsFamily st) R1200)
      OV22Ones1200
        | since112 >= 0.6 -> enter OV22Ones2400
      OV22Ones2400
        | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22bis R2400)
        | inPhase >= 6 -> enter Done
      Connected s _
        | not (remoteAlive s) -> enter Done
      _ -> st'

    isConnected (Connected _ _) = True
    isConnected _ = False

    v8Level = fromDb (-12)
    clBits = replicate 30 True ++ hdlcFrameBits 3 2 (encodeMessage (CL ourModes))
    msBits m = replicate 30 True ++ hdlcFrameBits 3 2 (encodeMessage (MS m))
    tx = case hsPhase st'' of
      ABilling -> TxSilence
      A8Dual -> TxDual 1375 2002 v8Level
      A8Tone -> TxTone 400
      A8Wait -> TxSilence
      A8SendMS -> case hsPendingMs st'' of
        Just m | hsPhaseAt st'' == t -> TxBits v21Channel1 (msBits m)
        _ -> TxMark v21Channel1
      O8Dual -> TxDual 1529 2225 1
      O8Tone -> TxMark v21Channel2
      O8SendCL -> if hsPhaseAt st'' == t then TxBits v21Channel2 clBits else TxMark v21Channel2
      O8WaitMS -> TxSilence
      AV8Ansam -> TxAnsam
      AV8JM -> case hsV8Peer st'' of
        Just peer | hsPhaseAt st'' == t -> TxBits v21Channel2 (v8Repeat (sequenceBits SeqJM (v8Reply peer)))
        _ -> TxMark v21Channel2
      AV8Gap -> TxSilence
      AAns -> TxTone 2100
      AGap -> TxSilence
      AProbe V22 -> TxV22 HighChannel R1200 TxU11
      AProbe s -> TxMark (fskTx Answer s)
      AV22Ones -> TxV22 HighChannel R1200 TxScrambledOnes
      AV22S1 -> TxV22 HighChannel R1200 TxS1
      AV22Ones1200 -> TxV22 HighChannel R1200 TxScrambledOnes
      AV22Ones2400 -> TxV22 HighChannel R2400 TxScrambledOnes
      OListen -> TxSilence
      OAnsEnding -> TxSilence
      OV8Wait -> TxSilence
      OV8CM -> if hsPhaseAt st'' == t then TxBits v21Channel1 (v8Repeat (sequenceBits SeqCM v8Offer))
                                      else TxMark v21Channel1
      OV8CJ -> if hsPhaseAt st'' == t then TxBits v21Channel1 cjBits else TxMark v21Channel1
      OV8Gap -> TxSilence
      OAfterAns -> if fskOnly then TxMark (fskTx role preferredFsk) else TxSilence
      OReply s -> TxMark (fskTx Originate s)
      OV22Wait -> TxSilence
      OV22S1 -> TxV22 LowChannel R1200 TxS1
      OV22Ones -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Settle -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Ones1200 -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Ones2400 -> TxV22 LowChannel R2400 TxScrambledOnes
      Connected s r | isV22Family s -> case v22LinkAt (hsRole st'') r of
        V22Link txc _ _ -> TxV22 txc r TxScrambledData
        FskLink {} -> TxSilence
      Connected s _ -> TxData (fskTx (hsRole st'') s)
      Done -> TxSilence
      V8NoMode -> TxSilence
    -- where to listen for V.8bis messages: the answering (initiating) station
    -- receives on V.21 channel 2, the calling (responding) station on channel 1
    v8Pick peer = commonModulation v8Offer peer
    -- CM and JM repeat until answered; queue enough to cover the wait
    -- rather than re-arming the transmitter every hop
    v8Repeat bs = concat (replicate 12 bs)
    v8Listen = case hsPhase st'' of
      OV8Wait -> Just v21Channel2
      OV8CM -> Just v21Channel2
      AV8Ansam -> Just v21Channel1
      AV8JM -> Just v21Channel1
      _ -> Nothing
    v8MenuOut = if hsV8Peer st'' /= hsV8Peer st then hsV8Peer st'' else Nothing
    hdlcListen = case hsPhase st'' of
      A8Wait -> Just v21Channel2
      A8Tone -> Just v21Channel2
      O8SendCL -> Just v21Channel1
      O8WaitMS -> Just v21Channel1
      _ -> Nothing
    -- the receiver decides 16-way from 450 ms after circuit 112 went ON
    rxRate = case hsPhase st'' of
      AV22Ones1200 | since112 >= 0.45 -> R2400
      AV22Ones2400 -> R2400
      OV22Ones1200 | since112 >= 0.45 -> R2400
      OV22Ones2400 -> R2400
      Connected _ R2400 -> R2400
      _ -> R1200
    status = case (hsPhase st, hsPhase st'') of
      (Connected _ _, Done) -> HsDropped
      (_, Done) -> HsFailed "timeout"
      (_, V8NoMode) -> HsFailed "V.8: no modulation in common"
      (_, Connected s r) | isV22Family s -> HsConnected s (v22LinkAt (hsRole st'') r)
      (_, Connected s _) -> HsConnected s (linkFor (hsRole st'') s)
      _ -> HsBusy

-- | The handshake as a stage over tone frames (no V.22 receiver).
handshakeStage :: HsConfig -> Stage ToneFrame HsOut
handshakeStage cfg = Stage (initialHandshake cfg) (\s fr -> handshakeStep cfg s fr Nothing noHsIn)
