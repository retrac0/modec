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
  , HsStatus (..)
  , HsState
  , initialHandshake
  , handshakeStep
  , handshakeStage
  , linkFor
  ) where

import Modec.Detect
import Modec.Standards
import Modec.Stream
import Modec.V22 (TxMode (..), V22Channel (..))

data Standard = Bell103 | V21 | V22 deriving (Eq, Show)

data Role = Originate | Answer deriving (Eq, Show)

-- | The channels of an established connection: our transmit side and
-- our receive side.
data Link
  = FskLink FskSpec FskSpec
  | V22Link V22Channel V22Channel
  deriving (Eq, Show)

linkFor :: Role -> Standard -> Link
linkFor Originate Bell103 = FskLink bell103Originate bell103Answer
linkFor Answer Bell103 = FskLink bell103Answer bell103Originate
linkFor Originate V21 = FskLink v21Channel1 v21Channel2
linkFor Answer V21 = FskLink v21Channel2 v21Channel1
linkFor Originate V22 = V22Link LowChannel HighChannel
linkFor Answer V22 = V22Link HighChannel LowChannel

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
  } deriving (Show)

defaultHsConfig :: Role -> HsConfig
defaultHsConfig role = HsConfig
  { hcRole = role, hcStandard = Nothing, hcBank = defaultToneBank
  , hcSquelch = 3e-3, hcDomRatio = 1.5
  , hcBilling = 2.0, hcAnsDuration = 3.0, hcAnsGap = 0.075, hcProbe = 1.5
  , hcQualify = 0.3, hcDrop = 0.5, hcTimeout = 45 }

-- | What the transmitter should be doing right now.
data TxCmd
  = TxSilence
  | TxTone Double        -- ^ a single tone (answer tone)
  | TxMark FskSpec       -- ^ idle mark on this channel
  | TxData FskSpec       -- ^ data mode on this channel
  | TxV22 V22Channel TxMode
  deriving (Eq, Show)

-- | What a V.22 receiver listening to the remote channel currently sees.
data V22Report = V22Report
  { vrEnergy   :: !Double
  , vrAngleErr :: !Double   -- ^ mean phase-step error in degrees
  , vrU11Run   :: !Int      -- ^ consecutive symbols of unscrambled ones
  , vrOnesRun  :: !Int      -- ^ consecutive descrambled ones
  , vrZerosRun :: !Int      -- ^ consecutive descrambled zeros
  } deriving (Show)

data HsStatus
  = HsBusy
  | HsConnected Standard Link
  | HsDropped
  | HsFailed String
  deriving (Eq, Show)

data Phase
  = ABilling
  | AAns
  | AGap
  | AProbe Standard              -- ^ answering: sending this standard's answer signal
  | AV22Ones                     -- ^ answering: scrambled ones for 765 ms
  | OListen
  | OAnsEnding                   -- ^ ITU answer tone heard, waiting for it to end
  | OAfterAns                    -- ^ answer tone over, waiting for a carrier
  | OReply Standard              -- ^ our FSK carrier is up, qualifying the remote
  | OV22Wait                     -- ^ unscrambled ones seen; 456 ms of silence
  | OV22Ones                     -- ^ sending scrambled ones, waiting for the remote's
  | OV22Settle                   -- ^ remote scrambled ones seen; 765 ms more
  | Connected Standard
  | Done
  deriving (Eq, Show)

data HsState = HsState
  { hsPhase     :: !Phase
  , hsPhaseAt   :: !Double   -- ^ time the phase was entered
  , hsToneSince :: !(Maybe (Double, Double))  -- ^ current dominant tone and when it started
  , hsLastTone  :: !Double   -- ^ last time any tone was dominant
  , hsT         :: !Double
  }

initialHandshake :: HsConfig -> HsState
initialHandshake cfg = HsState (case hcRole cfg of Answer -> ABilling; Originate -> OListen) 0 Nothing (-1) 0

fskTx, fskRx :: Role -> Standard -> FskSpec
fskTx role s = case linkFor role s of
  FskLink t _ -> t
  V22Link {} -> error "fskTx: V.22"
fskRx role s = case linkFor role s of
  FskLink _ r -> r
  V22Link {} -> error "fskRx: V.22"

-- 1200 bit/s: 155 ms of unscrambled ones = 93 symbols; 270 ms = 324 bits
u11Symbols, scrambledBits :: Int
u11Symbols = 93
scrambledBits = 324

-- | Advance the state machine by one tone frame and the latest V.22
-- receiver report (if a V.22 receiver is running).
handshakeStep :: HsConfig -> HsState -> ToneFrame -> Maybe V22Report -> (HsState, (TxCmd, HsStatus))
handshakeStep cfg st fr v22 = (st'', (tx, status))
  where
    t = tfTime fr
    dom = dominant (hcBank cfg) (hcSquelch cfg) (hcDomRatio cfg) fr
    role = hcRole cfg
    toneSince = case (dom, hsToneSince st) of
      (Just f, Just (g, since)) | f == g -> Just (f, since)
      (Just f, _) -> Just (f, t)
      (Nothing, _) -> Nothing
    lastTone = if dom == Nothing then hsLastTone st else t
    heardFor f = case toneSince of
      Just (g, since) | g == f -> t - since
      _ -> 0
    quiet = t - lastTone
    st' = st { hsToneSince = toneSince, hsLastTone = lastTone, hsT = t }
    inPhase = t - hsPhaseAt st
    allowed s = maybe True (== s) (hcStandard cfg)
    enter p = st' { hsPhase = p, hsPhaseAt = t }
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
    nextProbe s = case dropWhile (/= s) probeOrder of
      (_ : n : _) -> n
      _ -> head probeOrder
    firstProbe = case hcStandard cfg of
      Just s -> s
      Nothing -> head probeOrder

    st'' = case hsPhase st of
      _ | t > hcTimeout cfg && not (isConnected (hsPhase st)) && hsPhase st /= Done -> enter Done
      -- answering side
      ABilling
        | inPhase >= hcBilling cfg -> if hcStandard cfg == Just Bell103 then enter (AProbe Bell103) else enter AAns
      AAns
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103)
        | inPhase >= hcAnsDuration cfg -> enter AGap
      AGap
        | inPhase >= hcAnsGap cfg -> enter (AProbe firstProbe)
      AProbe V22
        | scrambledAnySeen -> enter AV22Ones
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103)
        | hcStandard cfg == Nothing && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe V22))
        | otherwise -> st'
      AProbe s
        | qualified s -> enter (Connected s)
        | hcStandard cfg == Nothing && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe s))
      AV22Ones
        | inPhase >= 0.765 -> enter (Connected V22)
      -- calling side
      OListen
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
        | inPhase >= hcQualify cfg && qualified s -> enter (Connected s)
        | inPhase >= 5 -> enter OListen
      OV22Wait
        | inPhase >= 0.456 -> enter OV22Ones
      OV22Ones
        | scrambledOnesSeen -> enter OV22Settle
        | inPhase >= 6 -> enter OListen
      OV22Settle
        | inPhase >= 0.765 -> enter (Connected V22)
      Connected s
        | not (remoteAlive s) -> enter Done
      _ -> st'

    isConnected (Connected _) = True
    isConnected _ = False

    tx = case hsPhase st'' of
      ABilling -> TxSilence
      AAns -> TxTone 2100
      AGap -> TxSilence
      AProbe V22 -> TxV22 HighChannel TxU11
      AProbe s -> TxMark (fskTx Answer s)
      AV22Ones -> TxV22 HighChannel TxScrambledOnes
      OListen -> TxSilence
      OAnsEnding -> TxSilence
      OAfterAns -> TxSilence
      OReply s -> TxMark (fskTx Originate s)
      OV22Wait -> TxSilence
      OV22Ones -> TxV22 LowChannel TxScrambledOnes
      OV22Settle -> TxV22 LowChannel TxScrambledOnes
      Connected V22 -> case linkFor role V22 of
        V22Link txc _ -> TxV22 txc TxScrambledData
        FskLink {} -> TxSilence
      Connected s -> TxData (fskTx role s)
      Done -> TxSilence
    status = case (hsPhase st, hsPhase st'') of
      (Connected _, Done) -> HsDropped
      (_, Done) -> HsFailed "timeout"
      (_, Connected s) -> HsConnected s (linkFor role s)
      _ -> HsBusy

-- | The handshake as a stage over tone frames (no V.22 receiver).
handshakeStage :: HsConfig -> Stage ToneFrame (TxCmd, HsStatus)
handshakeStage cfg = Stage (initialHandshake cfg) (\s fr -> handshakeStep cfg s fr Nothing)
