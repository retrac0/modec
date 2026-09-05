-- | Call establishment for Bell 103, V.21 and V.22.
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
import Modec.V8bis
import Data.Word (Word8)

data Standard = Bell103 | V21 | V22 deriving (Eq, Show)

data Role = Originate | Answer deriving (Eq, Show)

-- | The channels of an established connection: our transmit side and
-- our receive side.
data Link
  = FskLink FskSpec FskSpec
  | V22Link V22Channel V22Channel Rate
  deriving (Eq, Show)

-- | The link for a standard at its base rate (V.22 at 1200 bit/s).
linkFor :: Role -> Standard -> Link
linkFor Originate Bell103 = FskLink bell103Originate bell103Answer
linkFor Answer Bell103 = FskLink bell103Answer bell103Originate
linkFor Originate V21 = FskLink v21Channel1 v21Channel2
linkFor Answer V21 = FskLink v21Channel2 v21Channel1
linkFor Originate V22 = V22Link LowChannel HighChannel R1200
linkFor Answer V22 = V22Link HighChannel LowChannel R1200

v22LinkAt :: Role -> Rate -> Link
v22LinkAt Originate r = V22Link LowChannel HighChannel r
v22LinkAt Answer r = V22Link HighChannel LowChannel r

data HsConfig = HsConfig
  { hcRole        :: Role
  , hcStandard    :: Maybe Standard   -- ^ Nothing = automode
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
  , hcAllow2400   :: Bool             -- ^ V.22bis: negotiate 2400 bit/s with S1
  , hcV8bis       :: Bool             -- ^ try a V.8bis capabilities exchange before the modem start-up
  } deriving (Show)

defaultHsConfig :: Role -> HsConfig
defaultHsConfig role = HsConfig
  { hcRole = role, hcStandard = Nothing, hcBank = defaultToneBank
  , hcSquelch = 3e-3, hcDomRatio = 1.5
  , hcBilling = 2.0, hcAnsDuration = 3.0, hcAnsGap = 0.075, hcProbe = 1.5
  , hcQualify = 0.3, hcDrop = 0.5, hcTimeout = 45, hcAllow2400 = True, hcV8bis = True }

-- | What the transmitter should be doing right now.
data TxCmd
  = TxSilence
  | TxTone Double        -- ^ a single tone (answer tone)
  | TxMark FskSpec       -- ^ idle mark on this channel
  | TxData FskSpec       -- ^ data mode on this channel
  | TxV22 V22Channel Rate TxMode
  | TxDual Double Double Double   -- ^ two tones at the given amplitude factor (V.8bis segment 1)
  | TxBits FskSpec [Bool]         -- ^ queue these bits on the FSK channel, then idle mark
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

-- | What the handshake wants from the modem this hop.
data HsOut = HsOut
  { hoTx     :: TxCmd
  , hoStatus :: HsStatus
  , hoRxRate :: Rate            -- ^ decision rate for the V.22 receiver
  , hoRole   :: Role            -- ^ effective modem role (V.8bis can reverse it)
  , hoHdlc   :: Maybe FskSpec   -- ^ run a synchronous V.21 receiver on this channel for HDLC frames
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
  | OReply Standard              -- ^ our FSK carrier is up, qualifying the remote
  | OV22Wait                     -- ^ unscrambled ones seen; 456 ms of silence
  | OV22S1                       -- ^ sending S1 for 100 ms (2400 capable)
  | OV22Ones                     -- ^ sending scrambled ones, waiting for the remote's (or its S1)
  | OV22Settle                   -- ^ remote scrambled ones seen; 765 ms more (1200 bit/s)
  | OV22Ones1200                 -- ^ remote S1 seen (112 ON); scrambled ones at 1200 until 600 ms
  | OV22Ones2400                 -- ^ scrambled ones at 2400, waiting for 32 of the remote's
  | Connected Standard Rate
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
  , hsAllow2400Sel :: !Bool  -- ^ 2400 bit/s allowed by the V.8bis selection
  , hsPhaseAt   :: !Double   -- ^ time the phase was entered
  , hs112At     :: !Double   -- ^ time circuit 112 went ON (S1 exchanged)
  , hsToneSince :: !(Maybe (Double, Double))  -- ^ current dominant tone and when it started
  , hsLastTone  :: !Double   -- ^ last time any tone was dominant
  , hsT         :: !Double
  }

initialHandshake :: HsConfig -> HsState
initialHandshake cfg = HsState (case hcRole cfg of Answer -> ABilling; Originate -> OListen) (hcRole cfg) Nothing 0 Nothing Nothing False Nothing True 0 0 Nothing (-1) 0

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
handshakeStep :: HsConfig -> HsState -> ToneFrame -> Maybe V22Report -> [[Word8]] -> (HsState, HsOut)
handshakeStep cfg st fr v22 frames = (st'', HsOut tx status rxRate (hsRole st'') hdlcListen)
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
    msgs = map decodeMessage frames
    clOffered = [ ms | CL ms <- msgs ]
    msSelected = [ m | MS m <- msgs ]
    modeToStandard m = case m of { ModeV21 -> V21; ModeV22 -> V22; ModeV22bis -> V22 }
    ourModes = [ m | m <- [ModeV21, ModeV22, ModeV22bis], allowedMode m ]
    allowedMode m = case (m, hcStandard cfg) of
      (_, Nothing) -> m /= ModeV22bis || hcAllow2400 cfg
      (ModeV21, Just V21) -> True
      (ModeV22, Just V22) -> True
      (ModeV22bis, Just V22) -> hcAllow2400 cfg
      _ -> False
    pickMode offered = case [ m | m <- [ModeV22bis, ModeV22, ModeV21], m `elem` offered, m `elem` ourModes ] of
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
    st' = st { hsToneSince = toneSince, hsLastTone = lastTone, hsT = t
             , hsPairRun = pairRun', hsPairSeen = if sig /= Nothing then Nothing else pairSeen'', hsSig = sig, hsLastRsp = rspPair }
    inPhase = t - hsPhaseAt st
    allowed s = maybe True (== s) (case hsSelected st of { Just sel -> Just sel; Nothing -> hcStandard cfg })
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
    s1Seen = hcAllow2400 cfg && hsAllow2400Sel st && case v22 of
      Just r -> vrS1Run r >= s1Symbols
      Nothing -> False
    ones2400Seen = case v22 of
      Just r -> vrOnes2400 r >= 32
      Nothing -> False
    since112 = t - hs112At st
    enter112 p = (enter p) { hs112At = t }
    -- an FSK carrier counts once it has persisted; the Bell 103 answer
    -- mark must not be V.22 unscrambled ones in disguise
    qualified V22 = False   -- V.22 is qualified through the V.22 receiver, not tones
    qualified s = heardFor (fskMark (fskRx role s)) >= hcQualify cfg && not (s == Bell103 && role == Originate && u11Seen)
    remoteAlive s = case linkFor role s of
      FskLink _ rx -> t - lastToneOf rx <= hcDrop cfg
      V22Link {} -> True
      where lastToneOf spec = case toneSince of
              Just (g, _) | g == fskMark spec || g == fskSpace spec -> t
              _ -> hsLastTone st
    probeOrder = [V22, V21, Bell103]
    rotating = hcStandard cfg == Nothing && hsSelected st == Nothing
    nextProbe s = case dropWhile (/= s) probeOrder of
      (_ : n : _) -> n
      _ -> head probeOrder
    firstProbe = case hsSelected st of
      Just s -> s
      Nothing -> case hcStandard cfg of
        Just s -> s
        Nothing -> head probeOrder

    st'' = case hsPhase st of
      _ | t > hcTimeout cfg && not (isConnected (hsPhase st)) && hsPhase st /= Done -> enter Done
      -- answering side
      ABilling
        | inPhase >= hcBilling cfg ->
            if hcV8bis cfg then enter A8Dual
            else if hcStandard cfg == Just Bell103 then enter (AProbe Bell103) else enter AAns
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
      AAns
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | inPhase >= hcAnsDuration cfg -> enter AGap
      AGap
        | inPhase >= hcAnsGap cfg -> enter (AProbe firstProbe)
      AProbe V22
        | s1Seen -> enter112 AV22S1
        | scrambledAnySeen -> enter AV22Ones
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
        | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe V22))
        | otherwise -> st'
      AProbe s
        | qualified s -> enter (Connected s R1200)
        | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe s))
      AV22Ones
        | inPhase >= 0.765 -> enter (Connected V22 R1200)
      AV22S1
        | inPhase >= 0.1 -> enter AV22Ones1200
      AV22Ones1200
        | since112 >= 0.6 -> enter AV22Ones2400
      AV22Ones2400
        | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22 R2400)
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
            (enter AAns) { hsRole = Answer, hsSelected = Just (modeToStandard m), hsAllow2400Sel = m == ModeV22bis }
        | inPhase >= 3 -> enter OListen
      -- calling side
      OListen
        | hcV8bis cfg && sig == Just CRe && role == Originate -> enter O8Dual
        | allowed V22 && u11Seen -> enter OV22Wait
        | allowed Bell103 && qualified Bell103 -> enter (OReply Bell103)
        | heardFor 2100 >= hcQualify cfg -> enter OAnsEnding
      OAnsEnding
        | dom /= Just 2100 && quiet >= 0.04 -> enter OAfterAns
      OAfterAns
        | allowed V22 && u11Seen -> enter OV22Wait
        | allowed V21 && qualified V21 -> enter (OReply V21)
        | allowed Bell103 && qualified Bell103 -> enter (OReply Bell103)
      OReply s
        | inPhase >= hcQualify cfg && qualified s -> enter (Connected s R1200)
        | inPhase >= 5 -> enter OListen
      OV22Wait
        | inPhase >= 0.456 -> enter (if hcAllow2400 cfg && hsAllow2400Sel st then OV22S1 else OV22Ones)
      OV22S1
        | inPhase >= 0.1 -> enter OV22Ones
      OV22Ones
        | s1Seen -> enter112 OV22Ones1200
        | scrambledOnesSeen -> enter OV22Settle
        | inPhase >= 6 -> enter OListen
      OV22Settle
        | s1Seen -> enter112 OV22Ones1200
        | inPhase >= 0.765 -> enter (Connected V22 R1200)
      OV22Ones1200
        | since112 >= 0.6 -> enter OV22Ones2400
      OV22Ones2400
        | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22 R2400)
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
      OAfterAns -> TxSilence
      OReply s -> TxMark (fskTx Originate s)
      OV22Wait -> TxSilence
      OV22S1 -> TxV22 LowChannel R1200 TxS1
      OV22Ones -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Settle -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Ones1200 -> TxV22 LowChannel R1200 TxScrambledOnes
      OV22Ones2400 -> TxV22 LowChannel R2400 TxScrambledOnes
      Connected V22 r -> case v22LinkAt (hsRole st'') r of
        V22Link txc _ _ -> TxV22 txc r TxScrambledData
        FskLink {} -> TxSilence
      Connected s _ -> TxData (fskTx (hsRole st'') s)
      Done -> TxSilence
    -- where to listen for V.8bis messages: the answering (initiating) station
    -- receives on V.21 channel 2, the calling (responding) station on channel 1
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
      Connected V22 R2400 -> R2400
      _ -> R1200
    status = case (hsPhase st, hsPhase st'') of
      (Connected _ _, Done) -> HsDropped
      (_, Done) -> HsFailed "timeout"
      (_, Connected V22 r) -> HsConnected V22 (v22LinkAt (hsRole st'') r)
      (_, Connected s _) -> HsConnected s (linkFor (hsRole st'') s)
      _ -> HsBusy

-- | The handshake as a stage over tone frames (no V.22 receiver).
handshakeStage :: HsConfig -> Stage ToneFrame HsOut
handshakeStage cfg = Stage (initialHandshake cfg) (\s fr -> handshakeStep cfg s fr Nothing [])
