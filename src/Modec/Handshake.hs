-- | Call establishment for the 300 bit/s FSK standards.
--
-- Both state machines are pure and tick-driven: feed them one
-- 'ToneFrame' per hop (20 ms by default) and they return what the
-- transmitter should be doing and whether a data connection has been
-- established.
--
-- Answering side (ITU-T V.25 timing):
--
-- 1. Silence for the billing delay (1.8-2.5 s).
-- 2. ANS: 2100 Hz for 2.6-4.0 s.  A Bell 103 caller that answers it
--    with 1270 Hz is accepted straight away.
-- 3. Silence 75 +/- 20 ms.
-- 4. Automode: transmit V.21 channel 2 mark (1650 Hz) and listen for
--    channel 1 mark (980 Hz); if nothing arrives, switch to Bell 103
--    answer mark (2225 Hz) and listen for 1270 Hz; alternate until the
--    caller responds or the overall timeout expires.  Fixing the
--    standard in the configuration skips the alternation.
--
-- Calling side:
--
-- 1. Listen.  Steady 2225 Hz is a Bell 103 answerer: reply with 1270 Hz.
--    2100 Hz is an ITU answer tone: wait for it to end, then whichever
--    carrier follows (1650 Hz = V.21, reply 980 Hz; 2225 Hz = Bell 103)
--    picks the standard.
-- 2. A carrier must persist for the qualification time (300 ms, within
--    the 300-700 ms V.21 allows for circuit 109) before it counts.
--
-- Once both carriers are up the connection is reported and the FSM
-- goes idle.  Loss of the remote carrier for the drop time afterwards
-- is reported as 'HsDropped'.
module Modec.Handshake
  ( Standard (..)
  , Role (..)
  , HsConfig (..)
  , defaultHsConfig
  , TxCmd (..)
  , HsStatus (..)
  , HsState
  , initialHandshake
  , handshakeStep
  , handshakeStage
  ) where

import Modec.Detect
import Modec.Standards
import Modec.Stream

data Standard = Bell103 | V21 deriving (Eq, Show)

data Role = Originate | Answer deriving (Eq, Show)

data HsConfig = HsConfig
  { hcRole        :: Role
  , hcStandard    :: Maybe Standard   -- ^ Nothing = automode
  , hcBank        :: ToneBankConfig
  , hcSquelch     :: Double           -- ^ minimum tone amplitude
  , hcDomRatio    :: Double           -- ^ dominant tone must exceed others by this factor
  , hcBilling     :: Double           -- ^ answer: silence before ANS (1.8-2.5 s)
  , hcAnsDuration :: Double           -- ^ answer: ANS duration (2.6-4.0 s)
  , hcAnsGap      :: Double           -- ^ answer: silence after ANS (55-95 ms)
  , hcProbe       :: Double           -- ^ answer automode: time per carrier before switching
  , hcQualify     :: Double           -- ^ carrier must persist this long to count
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
  deriving (Eq, Show)

data HsStatus
  = HsBusy
  | HsConnected Standard FskSpec FskSpec   -- ^ standard, our transmit spec, our receive spec
  | HsDropped
  | HsFailed String
  deriving (Eq, Show)

data Phase
  = ABilling
  | AAns
  | AGap
  | AProbe Standard              -- ^ answering: sending this standard's answer carrier
  | OListen
  | OAnsEnding                   -- ^ ITU answer tone heard, waiting for it to end
  | OAfterAns                    -- ^ answer tone over, waiting for a carrier
  | OReply Standard              -- ^ our carrier is up, qualifying the remote
  | Connected Standard
  | Done
  deriving (Eq, Show)

data HsState = HsState
  { hsPhase     :: !Phase
  , hsPhaseAt   :: !Double   -- ^ time the phase was entered
  , hsToneSince :: !(Maybe (Double, Double))  -- ^ current dominant tone and when it started
  , hsLastTone  :: !Double   -- ^ last time any tone was dominant
  , hsAnsSeen   :: !Double   -- ^ how long 2100 Hz has been heard
  , hsT         :: !Double
  }

initialHandshake :: HsConfig -> HsState
initialHandshake cfg = HsState (case hcRole cfg of Answer -> ABilling; Originate -> OListen) 0 Nothing (-1) 0 0

txSpec, rxSpec :: Role -> Standard -> FskSpec
txSpec Originate Bell103 = bell103Originate
txSpec Answer Bell103 = bell103Answer
txSpec Originate V21 = v21Channel1
txSpec Answer V21 = v21Channel2
rxSpec r s = txSpec (other r) s
  where other Originate = Answer
        other Answer = Originate

-- | Advance the state machine by one tone frame.
handshakeStep :: HsConfig -> HsState -> ToneFrame -> (HsState, (TxCmd, HsStatus))
handshakeStep cfg st fr = (st'' , (tx, status))
  where
    t = tfTime fr
    dom = dominant (hcBank cfg) (hcSquelch cfg) (hcDomRatio cfg) fr
    role = hcRole cfg
    -- track how long the current dominant tone has persisted
    toneSince = case (dom, hsToneSince st) of
      (Just f, Just (g, since)) | f == g -> Just (f, since)
      (Just f, _) -> Just (f, t)
      (Nothing, _) -> Nothing
    lastTone = if dom == Nothing then hsLastTone st else t
    heardFor f = case toneSince of
      Just (g, since) | g == f -> t - since
      _ -> 0
    quiet = t - lastTone   -- time since any tone
    ansSeen = if dom == Just 2100 then hsAnsSeen st + hopLen else hsAnsSeen st
    hopLen = tbHopSec (hcBank cfg)
    st' = st { hsToneSince = toneSince, hsLastTone = lastTone, hsAnsSeen = ansSeen, hsT = t }
    inPhase = t - hsPhaseAt st
    allowed s = maybe True (== s) (hcStandard cfg)
    enter p = st' { hsPhase = p, hsPhaseAt = t }
    qualified s = heardFor (fskMark (rxSpec role s)) >= hcQualify cfg
    remoteAlive s = t - lastToneOf (rxSpec role s) <= hcDrop cfg
      where lastToneOf spec = case toneSince of
              Just (g, _) | g == fskMark spec || g == fskSpace spec -> t
              _ -> hsLastTone st   -- approximation: any tone keeps it alive
    firstAllowed = case hcStandard cfg of
      Just s -> s
      Nothing -> V21

    st'' = case hsPhase st of
      _ | t > hcTimeout cfg && not (isConnected (hsPhase st)) && hsPhase st /= Done -> enter Done
      -- answering side
      ABilling
        | inPhase >= hcBilling cfg -> enter AAns
      AAns
        | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103)
        | inPhase >= hcAnsDuration cfg -> enter AGap
      AGap
        | inPhase >= hcAnsGap cfg -> enter (AProbe firstAllowed)
      AProbe s
        | qualified s -> enter (Connected s)
        | hcStandard cfg == Nothing && inPhase >= hcProbe cfg -> enter (AProbe (flipStd s))
      -- calling side
      OListen
        | allowed Bell103 && heardFor 2225 >= hcQualify cfg -> enter (OReply Bell103)
        | heardFor 2100 >= hcQualify cfg -> enter OAnsEnding
      OAnsEnding
        | dom /= Just 2100 && quiet >= 0.04 -> enter OAfterAns
      OAfterAns
        | allowed V21 && qualified V21 -> enter (OReply V21)
        | allowed Bell103 && qualified Bell103 -> enter (OReply Bell103)
      OReply s
        | inPhase >= hcQualify cfg && qualified s -> enter (Connected s)
        | inPhase >= 5 -> enter OListen   -- remote went away; start over
      Connected s
        | not (remoteAlive s) -> enter Done
      _ -> st'

    flipStd V21 = Bell103
    flipStd Bell103 = V21
    isConnected (Connected _) = True
    isConnected _ = False

    tx = case hsPhase st'' of
      ABilling -> TxSilence
      AAns -> TxTone 2100
      AGap -> TxSilence
      AProbe s -> TxMark (txSpec Answer s)
      OListen -> TxSilence
      OAnsEnding -> TxSilence
      OAfterAns -> TxSilence
      OReply s -> TxMark (txSpec Originate s)
      Connected s -> TxData (txSpec role s)
      Done -> TxSilence
    status = case (hsPhase st, hsPhase st'') of
      (Connected _, Done) -> HsDropped
      (_, Done) -> HsFailed "timeout"
      (_, Connected s) -> HsConnected s (txSpec role s) (rxSpec role s)
      _ -> HsBusy

-- | The handshake as a stage over tone frames.
handshakeStage :: HsConfig -> Stage ToneFrame (TxCmd, HsStatus)
handshakeStage cfg = Stage (initialHandshake cfg) (handshakeStep cfg)
