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
  ( HsConfig (..)
  , defaultHsConfig
  , withModes
  , TxCmd (..)
  , V22Report (..)
  , HsOut (..)
  , HsStatus (..)
  , HsState
  , hsPhaseName
  , initialHandshake
  , handshakeAfterV32
  , HsIn (..)
  , noHsIn
  , handshakeStep
  , handshakeStage
  ) where

import Modec.Detect
import Modec.Standards
import Modec.Stream
import Modec.Handshake.Cues
import Modec.Handshake.Modes
import Modec.Link
import Modec.V22 (TxMode (..), V22Report (..))
import Modec.V8


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
  , hcV32Offer    :: Double           -- ^ answer automode: seconds to hold the V.32 pair before falling back
  , hcDrop        :: Double           -- ^ carrier loss for this long drops the connection
  , hcTimeout     :: Double           -- ^ give up after this long
  , hcV8          :: Bool             -- ^ answer with ANSam and exchange V.8 CM/JM menus
  , hcV8OfferAll  :: Bool             -- ^ advertise every V.8 modulation, to read back a full menu
  } deriving (Show)

defaultHsConfig :: Role -> HsConfig
defaultHsConfig role = HsConfig
  { hcRole = role, hcModes = allStandards, hcBank = defaultToneBank
  , hcSquelch = 3e-3, hcDomRatio = 1.5
  , hcBilling = 2.0, hcAnsDuration = 3.0, hcAnsGap = 0.075, hcProbe = 1.5
  , hcQualify = 0.3, hcV32Offer = 2.0, hcDrop = 0.5, hcTimeout = 45, hcV8 = False, hcV8OfferAll = False }

-- | Apply a mode set, adjusting whatever the modes themselves imply.
--
-- Answering a V.23 call means hearing the caller's 390 Hz backward mark,
-- so the bank gains that pair and loses the V.8bis CRe tone at 400 Hz.
-- The two cannot share a bank: one bin apart at a 40 ms window, neither
-- would ever dominate the other and the answerer would sit through its
-- own timeout hearing nothing.  V.8bis goes off with its tone.  Calling a V.23 answerer needs neither change: the 1300 Hz
-- forward mark is in the bank already and nothing else claims it.
--
-- Set 'hcModes' directly and none of this happens, which is why a V.23
-- answerer configured that way waits out its timeout rather than
-- connecting: it is listening for a tone it never measures.
withModes :: [Standard] -> HsConfig -> HsConfig
withModes ms cfg
  | V23 `elem` ms && hcRole cfg == Answer =
      cfg { hcModes = ms
          , hcBank = (hcBank cfg)
              { tbFreqs = fskMark v23Backward : fskSpace v23Backward
                          : tbFreqs (hcBank cfg) } }
  | otherwise = cfg { hcModes = ms }

-- | What the transmitter should be doing right now.
data TxCmd
  = TxSilence
  | TxTone Double        -- ^ a single tone (answer tone)
  | TxMark FskSpec       -- ^ idle mark on this channel
  | TxData FskSpec       -- ^ data mode on this channel
  | TxV22 V22Channel Rate TxMode
  | TxBits FskSpec [Bool]         -- ^ queue these bits on the FSK channel, then idle mark
  | TxAnsam              -- ^ V.8 modified answer tone
  -- | V.32 makes its own audio from its own pump, so these two carry no
  -- signal description -- only which of the two things the pump should
  -- be doing.  The idle one matters: a far end's start-stop framer arms
  -- on a run of scrambled ones, and a modem that starts sending
  -- characters the instant it connects never gives it one.
  | TxV32Idle
  | TxV32Data
  deriving (Eq, Show)

-- | What the handshake wants from the modem this hop.
data HsOut = HsOut
  { hoTx     :: TxCmd
  , hoStatus :: HsStatus
  , hoRxRate :: Rate            -- ^ decision rate for the V.22 receiver
  , hoRole   :: Role            -- ^ effective modem role
  , hoV8     :: Maybe FskSpec   -- ^ run a V.21 receiver on this channel for V.8 signals
  , hoV8Menu :: Maybe V8Menu    -- ^ the remote's menu, the hop it is confirmed
  } deriving (Show)

data HsStatus
  = HsBusy
  | HsConnected Standard Link
  -- | V.32 was selected.  This is not a connection: V.32 has a start-up
  -- of its own, on the sample clock, so the modem hands the line to
  -- "Modec.V32Start" rather than straight to a data pump.
  | HsStartV32
  -- | ...and the same, speculatively, from an answering modem's own
  -- ladder rather than from an agreement.
  | HsOfferV32
  | HsDropped
  | HsFailed String
  deriving (Eq, Show)

data Phase
  = ABilling
  | AV8Ansam                     -- ^ V.8: ANSam out, listening for CM
  | AV8JM                        -- ^ V.8: JM going out on V.21 channel 2
  | AV8Gap                       -- ^ V.8: 75 ms of silence before sigA
  | AAns
  | AGap
  | AProbe Standard              -- ^ answering: sending this standard's answer signal
  | AV22Ones                     -- ^ answering: scrambled ones for 765 ms (1200 bit/s)
  | AV22S1                       -- ^ answering: S1 seen (112 ON), sending S1 for 100 ms
  | AV22U11                      -- ^ answering: unscrambled ones for 456 ms after S1
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
  | OV22U11                      -- ^ unscrambled ones for 456 ms after S1 (6.3.1.2)
  | OV22Ones                     -- ^ sending scrambled ones, waiting for the remote's (or its S1)
  | OV22Settle                   -- ^ remote scrambled ones seen; 765 ms more (1200 bit/s)
  | OV22Ones1200                 -- ^ remote S1 seen (112 ON); scrambled ones at 1200 until 600 ms
  | OV22Ones2400                 -- ^ scrambled ones at 2400, waiting for 32 of the remote's
  | Connected Standard Rate
  -- | V.8 chose V.32; the line is about to change hands.
  | V32Handover
  -- | The answering ladder is offering V.32 on spec, per A.2.2.  The
  -- line changes hands the same way, but the start-up is told it may
  -- give up quickly and come back here.
  | V32Offer
  | V8NoMode                     -- ^ V.8 ran but found nothing in common
  | Done
  deriving (Eq, Show)

data HsState = HsState
  { hsPhase     :: !Phase
  , hsRole      :: !Role     -- ^ effective modem role
  , hsFamily    :: !Standard  -- ^ which V.22-family standard is being negotiated
  , hsTried212  :: !Bool     -- ^ a Bell 212A attempt already failed; prefer Bell 103
  , hsTriedV32  :: !Bool     -- ^ V.32 has been offered once and not taken up
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

-- | Back to the answering ladder after a V.32 offer nothing took up.
--
-- Almost nothing needs restoring: the handshake was never torn down, it
-- was simply not stepped while the start-up had the line.  What it
-- cannot do is resume in 'V32Handover', which is a dead end -- so it
-- comes back on the rung after the one that made the offer, with the
-- offer marked spent.
--
-- The clock does not need rebasing either, and that is worth saying
-- because it looks as though it should: the handshake's time comes from
-- the tone bank, the tone bank is stepped only while the mode is
-- 'Modec.Modem.Handshaking', so time did not pass here at all.  A
-- fifteen-second detour costs the ladder nothing and hcTimeout is
-- measured against the same frozen clock.
handshakeAfterV32 :: HsConfig -> HsState -> HsState
handshakeAfterV32 cfg st = st
  { hsPhase = AProbe resume, hsPhaseAt = hsT st, hsTriedV32 = True }
  where
    order = probesFor (hcModes cfg)
    -- the rung after the one that made the offer, wrapping as the
    -- rotation does
    resume = case dropWhile (/= V22) order of
      (_ : n : _) -> n
      _ -> case order of { (p : _) -> p; [] -> Bell103 }

-- | The current phase, by name, for tracing.
hsPhaseName :: HsState -> String
hsPhaseName = show . hsPhase

initialHandshake :: HsConfig -> HsState
initialHandshake cfg = HsState (case hcRole cfg of Answer -> ABilling; Originate -> OListen) (hcRole cfg) V22 False False 0 0 Nothing (-1) False Nothing 0 Nothing Nothing 0

-- | The V.8 signals of this hop, and what this modem is prepared to
-- answer them with.
--
-- V.8 is a whole second protocol -- a menu exchange over V.21 -- carried
-- inside the handshake's own state machine, and its bindings used to sit
-- among the tone detectors and the ladder policy in one scope of forty
-- names.
data V8Seen = V8Seen
  { v8sConfirmed :: Maybe V8Menu          -- ^ the far end's menu, seen twice running
  , v8sCj        :: !Bool                 -- ^ all three octets of CJ are in
  , v8sAnsam     :: !Bool                 -- ^ ANSam confirmed on this call
  , v8sCanRun    :: Modulation -> Bool    -- ^ we could actually run this one
  , v8sOffer     :: V8Menu                -- ^ our CM
  , v8sReply     :: V8Menu -> V8Menu      -- ^ JM, given the far end's CM
  , v8sPick      :: V8Menu -> Maybe Modulation
  , v8sMods      :: [Modulation]          -- ^ what our CM lists
  }

-- | Read the V.8 traffic of this hop.  Two identical sequences are
-- required before either side acts on a menu (7.4, 8.1.2), which is what
-- the repeat count in 'HsState' is for.
v8Seen :: HsConfig -> Ladder -> HsState -> HsIn -> (V8Seen, Maybe V8Menu, Int, Bool)
v8Seen cfg lad st inp = (seen, last', reps', ansam)
  where
    seqs = [ m | V8Sequence _ m <- hiV8 inp ]
    (last', reps') = foldl again (hsV8Last st, hsV8Reps st) seqs
      where again (prev, k) m | prev == Just m = (Just m, k + 1)
                              | otherwise = (Just m, 1)
    ansam = hsAnsam st || hiAnsam inp
    modes = laModes lad
    -- Only ITU modes have codepoints in Table 4, so a Bell-only
    -- configuration has nothing it can offer here.
    ourMods
      -- JM is the intersection of CM with what the answerer has, so a CM
      -- listing only what we can run tells us only whether the far end
      -- has that.  Offering everything is how you get it to name its
      -- whole menu; nothing it then selects will be runnable, which is
      -- the price of asking.
      | hcV8OfferAll cfg = [minBound .. maxBound]
      | otherwise = [ MV32 | any isV32 modes ]
                    ++ [ MV22 | laV22 lad ]
                    ++ [ MV23Duplex | laV23 lad ]
                    ++ [ MV21 | laAllows lad V21 ]
    runnable m = case m of
      -- V.8 has one codepoint for the whole family (Table 4 item 3,
      -- printed as "V.32bis/V.32"); which rate is used is settled by the
      -- R1/R2/R3 exchange inside V.32's own start-up, exactly as the S1
      -- exchange settles 1200 against 2400 for V.22.
      MV32 -> any isV32 modes
      MV22 -> laV22 lad
      MV23Duplex -> laV23 lad
      MV21 -> laAllows lad V21
      _ -> False
    offer = emptyMenu { v8Call = Just CfData, v8Mods = ourMods }
    seen = V8Seen
      { v8sConfirmed = if reps' >= 2 then last' else Nothing
      , v8sCj = V8CJ `elem` hiV8 inp
      , v8sAnsam = ansam
      , v8sCanRun = runnable
      , v8sOffer = offer
      -- JM lists what both have, and keeps the CM's octet count even
      -- when that is nothing at all (8.2.3).
      , v8sReply = \peer -> emptyMenu
          { v8Call = v8Call peer
          , v8Mods = [ m | m <- v8Mods peer, m `elem` ourMods ]
          , v8ModOctets = v8ModOctets peer }
      , v8sPick = commonModulation offer
      , v8sMods = ourMods
      }

-- | Advance the state machine by one hop.
--
-- The whole timing ladder of V.25, V.22 §6.3 and V.8, in the order the
-- phases are declared.  Nothing here reads the line or decides what this
-- modem is willing to run -- 'Cues' and 'Ladder' have already done both
-- -- so every guard is a timing or a phase.
--
-- @st@ arrives with this hop's tone and V.8 bookkeeping already
-- recorded, so 'enter' only has to set the phase and stamp it.
advance :: HsConfig -> Ladder -> Cues -> V8Seen -> HsState -> HsState
advance cfg lad cu v8 st = case hsPhase st of
    _ | t > hcTimeout cfg && not (isConnected (hsPhase st)) && hsPhase st /= Done -> enter Done
    -- answering side
    ABilling
      | inPhase >= hcBilling cfg ->
          if hcV8 cfg && ituAllowed then enter AV8Ansam
          -- a Bell-only modem answers with 2225 Hz, never with the ITU tone
          else if ituAllowed then enter AAns else enter (AProbe firstProbe)
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
          Just MV32 | canRun MV32 -> enter V32Handover
          Just MV22 | canRun MV22 -> enter (AProbe V22)
          Just MV21 | canRun MV21 -> enter (AProbe V21)
          _ -> enter V8NoMode
    AAns
      -- A.2.2: "If signal AA is detected at any time during the
      -- transmission of the V.25 answer sequence, the modem shall
      -- continue as defined 5.4.2 at the second paragraph."  A calling
      -- V.32 modem holds carrier state A throughout our answer tone,
      -- so there is nothing to wait for once it has been heard.
      | offerV32 && v32Heard -> (enter V32Offer) { hsTriedV32 = True }
      | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
      | inPhase >= hcAnsDuration cfg -> enter AGap
    AGap
      | inPhase >= hcAnsGap cfg -> enter (AProbe firstProbe)
    AProbe V22
      | offerV32 && v32Heard -> (enter V32Offer) { hsTriedV32 = True }
      | s1Seen -> enter112 AV22S1
      | scrambledAnySeen, not otherFsk -> (enter AV22Ones) { hsFamily = V22 }
      | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
      -- 6.3.1.2: the answering modem answers the calling modem's
      -- unscrambled binary 1 with scrambled binary 1.  Keying off the
      -- caller's scrambled ones instead works against a caller that
      -- sends them early, and leaves a caller that follows the
      -- Recommendation -- holding unscrambled ones and waiting to be
      -- answered -- waiting for ever.  Bell 103 is qualified above, so
      -- an FSK caller has already been taken by then.
      -- ...but not when the line is carrying something else we can
      -- name.  A V.21 caller holds a steady mark carrier, and a V.22
      -- receiver pointed at the low channel reads a steady tone as
      -- unscrambled binary 1 -- so an answering modem probing V.22
      -- took a V.21 caller for a V.22 one and connected to it in a
      -- modulation it was not speaking.  V.8bis hid this for as long
      -- as it existed: the answerer never reached the V.22 probe.
      | u11Seen, not otherFsk -> (enter AV22Ones) { hsFamily = V22 }
      -- A.2.2 again, this time the fourth paragraph.  The answering
      -- automode ladder is ANS, then USB1 for Ta = 1500 +/- 50 ms
      -- while listening for S1 or SB1, and only then the alternating
      -- pair of 5.4.2.  This probe /is/ USB1 -- it transmits
      -- TxV22 HighChannel R1200 TxU11 -- and hcProbe is 1.5 s, so the
      -- rung goes here rather than straight after the answer tone.
      --
      -- Note 2 gives the reason for that order, and it is not
      -- cosmetic: sending the pair early risks "being received and
      -- possibly misinterpreted as a loss of carrier by some
      -- implementations of V.22 bis modems".
      | offerV32, inPhase >= hcProbe cfg -> (enter V32Offer) { hsTriedV32 = True }
      | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe V22))
      | otherwise -> st
    -- the Bell probe transmits 2225 Hz, which serves Bell 103 and
    -- Bell 212A alike; the caller's reply says which it wanted
    AProbe Bell103
      | scrambledAnySeen, (fam : _) <- bellDpsk -> (enter AV22Ones) { hsFamily = fam }
      | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
      | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe Bell103))
      | otherwise -> st
    AProbe s
      | qualified s -> enter (Connected s R1200)
      -- a Bell 103 caller transmits continuously, so accept it whatever
      -- we happen to be probing with
      | allowed Bell103 && qualified Bell103 -> enter (Connected Bell103 R1200)
      | rotating && inPhase >= hcProbe cfg -> enter (AProbe (nextProbe s))
    AV22Ones
      | inPhase >= 0.765 -> enter (Connected (hsFamily st) R1200)
    AV22S1
      | inPhase >= 0.1 -> enter AV22U11
    AV22U11
      | inPhase >= 0.456 -> enter AV22Ones1200
    AV22Ones1200
      | since112 >= 0.6 -> enter AV22Ones2400
    AV22Ones2400
      | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22bis R2400)
      | inPhase >= 6 -> enter Done
    -- calling side
    OListen
      | v22Allowed && u11Seen -> (enter OV22Wait) { hsFamily = V22 }
      | bellChoice == Just Bell212A && bell212Trigger -> (enter OV22Wait) { hsFamily = Bell212A }
      | bellChoice == Just Bell103 && qualified Bell103 -> enter (OReply Bell103)
      | heardFor answerToneItu >= hcQualify cfg -> enter OAnsEnding
    OAnsEnding
      -- the answer tone is modulated: the far end speaks V.8 and is
      -- waiting to be told what we have (7.2, 8.1.1)
      | hcV8 cfg && ansamSeen && not (null ourV8Mods) -> enter OV8Wait
      | dom /= Just answerToneItu && (quiet >= 0.04 || sinceOtherTone >= 0.1) -> enter OAfterAns
    -- No Bell 212A rung here, and that is not an oversight.  This
    -- ladder is entered on a 2100 Hz answer tone, which is an ITU
    -- answerer by definition -- a Bell 212A answering modem sends
    -- 2225 Hz and nothing else.  What follows a 2100 Hz tone is V.22's
    -- start-up: unscrambled binary 1, then S1 if the far end is
    -- 2400-capable, on the timings of 6.3.  Bell 212A has no such
    -- sequence to offer in reply.
    --
    -- The two are the same modem below the handshake.  A chip that
    -- implements both -- the Teridian 73K222BL, V.22 and Bell 212A on
    -- one die -- has one DPSK section producing "phase shifts as
    -- prescribed by the Bell 212A or V.22" standards, one scrambler,
    -- and bypasses that scrambler "in the Bell 103 or V.21 modes",
    -- which is to say in FSK and nowhere else.  Selecting Bell rather
    -- than CCITT changes the tones and only the tones: "In Bell 212A
    -- mode [it] employs a 2225 Hz answer tone.  [In] V.22 mode [it]
    -- produces either 550 or 1800 Hz guard tone, recognizes and
    -- generates a 2100 Hz answer tone".  Same 600 baud, same 1200 and
    -- 2400 Hz carriers, same dibit-to-quadrant table, same scrambler.
    --
    -- So a modem told to speak Bell 212A and dialled at an ITU
    -- answerer really does have nothing in common with it at 1200:
    -- not because the modulation differs but because neither end will
    -- run the other's call setup.  Bell 103 at 300 is what is left,
    -- and taking it is right.  A caller that wants both puts V22 in
    -- its modes and reaches 1200 through the rung below -- which is
    -- what the default modes do.
    --
    -- The same source settles the other half of it: "The CCITT V.22
    -- standard defines synchronous operation at 600 and 1200 bit/s.
    -- The Bell 212A standard defines synchronous operation only at
    -- 1200 bit/s."  There is no 2400 bit/s Bell modulation to select,
    -- which is why 's1Seen' refuses to fire on a Bell 212A call.
    OAfterAns
      -- 5.4.1: the answering V.32 modem's alternating pair at 600 and
      -- 3000 Hz.  V.32 is six years older than V.8 and does not need
      -- one -- without this rung a modem offering V.32 alongside
      -- anything else could only reach it through a CM/JM exchange,
      -- and fell back to V.22bis against every V.32 board that did not
      -- speak V.8.  Entered on hearing the pair rather than on hoping
      -- for it, so a far end that is not a V.32 answerer never costs
      -- us the rest of the ladder.
      | any isV32 modes && v32Heard -> enter V32Handover
      | v22Allowed && u11Seen -> (enter OV22Wait) { hsFamily = V22 }
      -- our carrier is already up in FSK-only mode, so the answerer's
      -- carrier is all that is still needed
      | allowed V21 && qualified V21 -> enter (if fskOnly then Connected V21 R1200 else OReply V21)
      | v23Allowed && qualified V23 -> enter (if fskOnly then Connected V23 R1200 else OReply V23)
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
          Just MV32 | canRun MV32 -> enter V32Handover
          Just MV22 | canRun MV22 -> enter OAfterAns
          Just MV23Duplex | canRun MV23Duplex -> enter (OReply V23)
          Just MV21 | canRun MV21 -> enter (OReply V21)
          _ -> enter V8NoMode
    OReply s
      | inPhase >= hcQualify cfg && qualified s -> enter (Connected s R1200)
      | inPhase >= 5 -> enter OListen
    OV22Wait
      | inPhase >= 0.456 -> enter (if allow2400 && hsFamily st /= Bell212A then OV22S1 else OV22U11)
    -- 100 ms is what 6.3.1.2 asks for and 150 is what survives a
    -- trunk: one lost 20 ms packet takes a fifth of the pattern, and
    -- an answerer that misses it offers 1200 and never mentions 2400
    -- again.  The answerer detects S1, it does not measure it, so the
    -- extra is free -- and it comes out of the unscrambled ones that
    -- follow, because the far end's timers do key off when those end.
    OV22S1
      | inPhase >= 0.15 -> enter OV22U11
    -- 6.3.1.2: S1 is followed by unscrambled binary 1, and only then by
    -- scrambled ones.  Leaving it out shortens everything after it by
    -- 456 ms, and the far end -- which changes rate on its own clock,
    -- not on ours -- then reads our 2400 bit/s as 1200 for the rest of
    -- the call while its own transmission stays perfectly readable.
    -- 6.3.1.2 has the answering modem send scrambled binary 1 only once
    -- it has detected /our/ unscrambled binary 1, so the calling modem
    -- holds it until that answer comes back rather than for a fixed
    -- time.  Leaving after 406 ms works against an answerer that keys
    -- off scrambled ones instead -- which is what this one used to do,
    -- so the two faults cancelled and modec talked to itself perfectly
    -- -- and fails against a modem that follows the Recommendation: on
    -- a VoIP trunk the burst arrives 150 ms late and is over before the
    -- far end has finished qualifying it.  One recorded call sat on
    -- unscrambled ones for three seconds and then gave up and offered
    -- V.21 instead.
    OV22U11
      | s1Seen -> enter112 OV22Ones1200
      | inPhase >= 0.406 && scrambledOnesSeen -> enter OV22Ones
      -- nothing came back: go on anyway rather than hold the line
      | inPhase >= 3 -> enter OV22Ones
    OV22Ones
      | s1Seen -> enter112 OV22Ones1200
      | scrambledOnesSeen -> enter OV22Settle
      -- the far end did not take it up: fall back to the other Bell mode
      | inPhase >= 6 -> (enter OListen) { hsTried212 = hsTried212 st || hsFamily st == Bell212A }
    OV22Settle
      | s1Seen -> enter112 OV22Ones1200
      | inPhase >= 0.765 -> enter (Connected (hsFamily st) R1200)
    -- 270 ms of scrambled ones at 1200 once the S1s have agreed 2400
    -- (6.3.1.3), then both ends change rate.  Holding longer than the
    -- far end does means arriving after its 200 ms of ones at the new
    -- rate has already been and gone.
    OV22Ones1200
      | since112 >= 0.27 -> enter OV22Ones2400
    OV22Ones2400
      | inPhase >= 0.2 && ones2400Seen -> enter (Connected V22bis R2400)
      -- the S1 exchange settled the rate; if the far end's ones at the
      -- new rate were missed, joining its data is better than sitting
      -- here until the call times out
      | inPhase >= 0.9 -> enter (Connected V22bis R2400)
    Connected s _
      | not (remoteAlive s) -> enter Done
    _ -> st

  where
    t = cuTime cu
    dom = cuDom cu
    heardFor = cuHeardFor cu
    quiet = cuQuiet cu
    sinceOtherTone = cuSinceOther cu
    u11Seen = cuU11 cu
    scrambledOnesSeen = cuScrOnes cu
    scrambledAnySeen = cuScrAny cu
    -- The S1 pattern is a cue; whether to act on it is policy, and a
    -- Bell 212A link has no 2400 rate to negotiate.
    s1Seen = la2400 lad && hsFamily st /= Bell212A && cuS1Pattern cu
    ones2400Seen = cuOnes2400 cu
    qualified = cuQualified cu
    bell212Trigger = cuBell212 cu
    otherFsk = cuOtherFsk cu
    remoteAlive = cuAlive cu
    v32Heard = cuV32Peer cu
    modes = laModes lad
    allowed = laAllows lad
    v22Allowed = laV22 lad
    allow2400 = la2400 lad
    ituAllowed = laItu lad
    v23Allowed = laV23 lad
    bellDpsk = laBellDpsk lad
    fskOnly = laFskOnly lad
    bellChoice = laBellChoice lad
    offerV32 = laOfferV32 lad
    rotating = laRotating lad
    nextProbe = laNext lad
    firstProbe = laFirst lad
    v8Confirmed = v8sConfirmed v8
    cjSeen = v8sCj v8
    ansamSeen = v8sAnsam v8
    ourV8Mods = v8sMods v8
    canRun = v8sCanRun v8
    v8Pick = v8sPick v8
    inPhase = t - hsPhaseAt st
    since112 = t - hs112At st
    enter p = st { hsPhase = p, hsPhaseAt = t }
    enter112 p = (enter p) { hs112At = t }
    isConnected (Connected _ _) = True
    isConnected _ = False

-- | What goes on the line in a phase.
--
-- The signal table, in the same order as the phase declaration and as
-- 'advance', so the two can be read side by side against the timing
-- diagram they both come from.
txFor :: Ladder -> V8Seen -> Double -> HsState -> TxCmd
txFor lad v8 t st = case hsPhase st of
    -- the V.32 start-up puts its own signals on the line from here
    V32Handover -> TxSilence
    V32Offer -> TxSilence
    ABilling -> TxSilence
    AV8Ansam -> TxAnsam
    AV8JM -> case hsV8Peer st of
      Just peer | hsPhaseAt st == t -> TxBits v21Channel2 (v8Repeat (sequenceBits SeqJM (v8Reply peer)))
      _ -> TxMark v21Channel2
    AV8Gap -> TxSilence
    AAns -> TxTone answerToneItu
    AGap -> TxSilence
    AProbe V22 -> TxV22 HighChannel R1200 TxU11
    AProbe s -> TxMark (fskTx Answer s)
    AV22Ones -> TxV22 HighChannel R1200 TxScrambledOnes
    AV22S1 -> TxV22 HighChannel R1200 TxS1
    AV22U11 -> TxV22 HighChannel R1200 TxU11
    AV22Ones1200 -> TxV22 HighChannel R1200 TxScrambledOnes
    AV22Ones2400 -> TxV22 HighChannel R2400 TxScrambledOnes
    OListen -> TxSilence
    OAnsEnding -> TxSilence
    OV8Wait -> TxSilence
    OV8CM -> if hsPhaseAt st == t then TxBits v21Channel1 (v8Repeat (sequenceBits SeqCM v8Offer))
                                    else TxMark v21Channel1
    OV8CJ -> if hsPhaseAt st == t then TxBits v21Channel1 cjBits else TxMark v21Channel1
    OV8Gap -> TxSilence
    -- 5.4.1: "The modem shall repetitively transmit carrier state A."
    -- A calling V.32 modem holds a steady 1800 Hz from here while it
    -- listens for the answerer's pair, and A.2.2 has the answering
    -- modem pre-empt its whole ladder on hearing it.  Sending silence
    -- instead meant no V.32 answerer could ever detect us, and two
    -- modecs that both offered V.32 settled on V.22bis because the
    -- answerer's USB1 probe arrived before either had said anything
    -- about V.32.
    --
    -- A.2.2 Note 1 knows this tone lands on top of the 1800 Hz V.22bis
    -- guard tone and says so; the Recommendation accepts the overlap.
    OAfterAns
      | any isV32 modes -> TxTone 1800
      | fskOnly -> TxMark (fskTx role preferredFsk)
      | otherwise -> TxSilence
    OReply s -> TxMark (fskTx Originate s)
    OV22Wait -> TxSilence
    OV22S1 -> TxV22 LowChannel R1200 TxS1
    OV22U11 -> TxV22 LowChannel R1200 TxU11
    OV22Ones -> TxV22 LowChannel R1200 TxScrambledOnes
    OV22Settle -> TxV22 LowChannel R1200 TxScrambledOnes
    OV22Ones1200 -> TxV22 LowChannel R1200 TxScrambledOnes
    OV22Ones2400 -> TxV22 LowChannel R2400 TxScrambledOnes
    -- V.32 never reaches here: once it is selected the modem hands the
    -- line to Modec.V32Start, which runs on the sample clock rather
    -- than on this machine's 20 ms tick.
    Connected s _ | isV32 s -> TxSilence
    Connected s r | isV22Family s -> case v22LinkAt (hsRole st) r of
      V22Link txc _ _ -> TxV22 txc r TxScrambledData
      _ -> TxSilence
    Connected s _ -> TxData (fskTx (hsRole st) s)
    Done -> TxSilence
    V8NoMode -> TxSilence
  -- where to listen for V.8bis messages: the answering (initiating) station
  -- receives on V.21 channel 2, the calling (responding) station on channel 1
  where
    modes = laModes lad
    fskOnly = laFskOnly lad
    preferredFsk = laPreferred lad
    role = hsRole st
    v8Offer = v8sOffer v8
    v8Reply = v8sReply v8
    -- CM and JM repeat until answered; queue enough to cover the wait
    -- rather than re-arming the transmitter every hop.
    v8Repeat bs = concat (replicate 12 bs)

-- | Where to listen for V.8 messages: the answering (initiating) station
-- receives on V.21 channel 2, the calling (responding) station on
-- channel 1.
v8ListenFor :: HsState -> Maybe FskSpec
v8ListenFor st = case hsPhase st of
    OV8Wait -> Just v21Channel2
    OV8CM -> Just v21Channel2
    AV8Ansam -> Just v21Channel1
    AV8JM -> Just v21Channel1
    _ -> Nothing

-- | The decision rate the V.22 receiver should run at.
--
-- §6.3.1.2 puts sixteen-way decisions at 450 ms after circuit 112 went
-- ON, and only the answering side reaches that inside a phase: it holds
-- 'AV22Ones1200' from 556 ms to 600 ms after its own 112, so the rule
-- governs there.
--
-- The calling side never does.  It leaves 'OV22Ones1200' at 270 ms
-- (§6.3.1.3 -- see the note on that arm) and goes sixteen-way through
-- the phase instead, which is earlier on purpose: the answering modem
-- starts sending at 2400 roughly 500 ms after the caller's 112, and a
-- receiver that arrives late has missed the 200 ms of ones it is
-- waiting for.  Decoding the far end's four points sixteen ways in the
-- meantime costs nothing -- they are four of the sixteen, so the
-- quadrant is still right -- and it is @ones2400Seen@ that actually
-- decides when the far end has switched, not the rate.
--
-- So an @OV22Ones1200 | since112 >= 0.45@ arm belongs here in symmetry
-- and cannot ever fire, and it used to be here.  It fired exactly once
-- per call, wrongly: this is passed the time since 112 measured against
-- the state the hop /ends/ in, and it used to be measured against the
-- state it began in.  On the one hop that turns 112 on, that stamp is
-- still the initial zero, so @since112@ was the whole call so far, the
-- guard passed, and the receiver was told to decide sixteen ways for
-- one block at the exact moment the far end was furthest from sending
-- sixteen points.
rxRateFor :: Double -> HsState -> Rate
rxRateFor since112 st = case hsPhase st of
    AV22Ones1200 | since112 >= 0.45 -> R2400
    AV22Ones2400 -> R2400
    OV22Ones2400 -> R2400
    Connected _ R2400 -> R2400
    _ -> R1200

-- | What the modem is told, from the phase this hop began in and the one
-- it ended in.
statusFor :: HsState -> HsState -> HsStatus
statusFor prev st = case (hsPhase prev, hsPhase st) of
    (Connected _ _, Done) -> HsDropped
    (_, Done) -> HsFailed "timeout"
    (_, V8NoMode) -> HsFailed (case hsV8Mod st of
      Just m -> "V.8: far end selected " ++ modName m ++ ", which this modem does not run"
      Nothing -> "V.8: no modulation in common")
    (_, V32Handover) -> HsStartV32
    (_, V32Offer) -> HsOfferV32
    (_, Connected s r) | isV22Family s -> HsConnected s (v22LinkAt (hsRole st) r)
    (_, Connected s _) -> HsConnected s (linkFor (hsRole st) s)
    _ -> HsBusy

-- | Advance the state machine by one tone frame and whatever the
-- receivers reported with it.
--
-- Four tables hang off this, and they are four questions the
-- Recommendation asks separately: what happens next ('advance'), what
-- goes on the line ('txFor'), where to listen ('v8ListenFor'), and what
-- the receiver's decision rate is ('rxRateFor').  They stay four,
-- ordered the same way, so each can be read against its own clause.
handshakeStep :: HsConfig -> HsState -> ToneFrame -> HsIn -> (HsState, HsOut)
handshakeStep cfg st fr inp = (st'', HsOut tx status rxRate (hsRole st'') v8Listen v8MenuOut)
  where
    lad = ladder (hcModes cfg) (hsRole st) (hsTried212 st) (hsTriedV32 st)
    cu = cues (cueConfig cfg) lad (hsRole st)
              (Tone (hsToneSince st) (hsLastTone st)) fr inp
    (v8, v8Last', v8Reps', ansamSeen) = v8Seen cfg lad st inp
    t = cuTime cu
    -- This hop's bookkeeping, before any transition: the tone the bank
    -- is on, the V.8 repeat count, the clock.
    st' = st { hsToneSince = tnSince (cuTone cu), hsLastTone = tnLast (cuTone cu)
             , hsT = t, hsAnsam = ansamSeen
             , hsV8Last = v8Last', hsV8Reps = v8Reps' }
    st'' = advance cfg lad cu v8 st'
    tx = txFor lad v8 t st''
    status = statusFor st st''
    rxRate = rxRateFor (t - hs112At st'') st''
    v8Listen = v8ListenFor st''
    v8MenuOut = if hsV8Peer st'' /= hsV8Peer st then hsV8Peer st'' else Nothing

-- | The thresholds 'Modec.Handshake.Cues' needs, out of the whole
-- configuration.
cueConfig :: HsConfig -> CueConfig
cueConfig cfg = CueConfig
  { ccSquelch = hcSquelch cfg, ccDomRatio = hcDomRatio cfg
  , ccQualify = hcQualify cfg, ccDrop = hcDrop cfg }

-- | The handshake as a stage over tone frames (no V.22 receiver).
handshakeStage :: HsConfig -> Stage ToneFrame HsOut
handshakeStage cfg = Stage (initialHandshake cfg) (\s fr -> handshakeStep cfg s fr noHsIn)
