-- | What is on the line, as the handshake reads it.
--
-- Everything here is a measurement or a threshold: how long a tone has
-- been dominant, whether the far end's carrier is unmodulated, how many
-- descrambled ones have gone by.  Nothing here decides anything.  It was
-- 150 lines of @where@ bindings inside 'handshakeStep', which is why the
-- numbers that decide when a V.22 handshake commits -- 93 symbols of
-- unscrambled ones, 324 scrambled bits, 8 degrees of phase-step error --
-- had never been tested against anything but a whole simulated call.
-- Given a 'V22Report' they can each be tested in a line.
module Modec.Handshake.Cues
  ( -- * What the receivers found
    HsIn (..)
  , noHsIn
    -- * The tone state carried between hops
  , Tone (..)
  , noTone
    -- * Reading the line
  , CueConfig (..)
  , Cues (..)
  , cues
    -- * The V.22 run lengths the timings are measured against
  , u11Symbols
  , scrambledBits
  , s1Symbols
  , u11Guard
  ) where

import Modec.Detect
import Modec.Handshake.Modes
import Modec.Link
import Modec.Standards
import Modec.V22 (V22Report (..))
import Modec.V8 (V8Event (..))

-- | What the receivers found for the handshake this hop.
--
-- 'hiV8' and 'hiAnsam' are per audio block, and so arrive on the first
-- tone frame of one; 'hiPump' is a running state rather than an event
-- and belongs on every frame, since the runs it counts are what the
-- timings are measured against.
data HsIn = HsIn
  { hiV8      :: [V8Event]       -- ^ V.8 signals off the async V.21 receiver
  , hiV32Peer :: !Bool           -- ^ the far end's V.32 opening signal is on the line
  , hiAnsam   :: !Bool           -- ^ ANSam was confirmed in this block
  , hiPump    :: Maybe V22Report -- ^ what the data pump's receiver sees
  }

-- | Nothing was received.
noHsIn :: HsIn
noHsIn = HsIn [] False False Nothing

-- | Which tone has been dominant, and since when.  Carried from hop to
-- hop, because every timing here is a duration.
data Tone = Tone
  { tnSince :: Maybe (Double, Double)  -- ^ the dominant tone and when it started
  , tnLast  :: !Double                 -- ^ the last time any tone was dominant
  } deriving (Eq, Show)

noTone :: Tone
noTone = Tone Nothing (-1)

-- | The thresholds reading the line depends on, and nothing else.
data CueConfig = CueConfig
  { ccSquelch  :: !Double  -- ^ minimum tone amplitude
  , ccDomRatio :: !Double  -- ^ a dominant tone must exceed the others by this
  , ccQualify  :: !Double  -- ^ an FSK carrier must persist this long to count
  , ccDrop     :: !Double  -- ^ carrier gone this long means the far end has
  }

-- | What this hop heard.
data Cues = Cues
  { cuTime       :: !Double
  , cuDom        :: Maybe Double        -- ^ the dominant tone, if there is one
  , cuTone       :: Tone                -- ^ the state to carry to the next hop
  , cuHeardFor   :: Double -> Double    -- ^ how long this tone has been dominant
  , cuQuiet      :: !Double             -- ^ how long no tone has been dominant
  , cuSinceOther :: !Double
    -- ^ how long a tone other than the ITU answer tone has been
    -- dominant.  An answering modem that steps straight from the answer
    -- tone to the next rung of its ladder leaves no silence between
    -- them, so 'cuQuiet' never rises and only this sees the change.
  , cuU11        :: !Bool               -- ^ the far end is sending unscrambled binary 1
  , cuScrOnes    :: !Bool               -- ^ ...and scrambled ones, which is a different thing
  , cuScrAny     :: !Bool               -- ^ scrambled ones or zeros: the carrier is modulated
  , cuS1Pattern  :: !Bool               -- ^ the S1 double dibit is on the line
  , cuOnes2400   :: !Bool               -- ^ 32 descrambled ones decided sixteen ways
  , cuQualified  :: Standard -> Bool    -- ^ this mode's carrier has persisted
  , cuBell212    :: !Bool               -- ^ 2225 Hz where the Recommendation has unscrambled ones
  , cuOtherFsk   :: !Bool               -- ^ an FSK mark we recognise is on the line right now
  , cuAlive      :: Standard -> Bool    -- ^ the far end is still there on this link
  , cuV32Peer    :: !Bool               -- ^ the far end's V.32 opening signal
  }

-- 1200 bit/s: 155 ms of unscrambled ones = 93 symbols; 270 ms = 324 bits;
-- S1 lasts 100 ms = 60 symbols, half of it is enough to recognise it.
u11Symbols, scrambledBits, s1Symbols, u11Guard :: Int
u11Symbols = 93
scrambledBits = 324
s1Symbols = 30
-- | A constant phase step held this long says the carrier is unmodulated
-- -- the answerer's unscrambled ones -- whatever the descrambler makes
-- of it.  Scrambled ones never hold one step for long.
u11Guard = 12

-- | Read one tone frame and whatever the receivers reported with it.
cues :: CueConfig -> Ladder -> Role -> Tone -> ToneFrame -> HsIn -> Cues
cues cfg lad role prev fr inp = Cues
  { cuTime = t
  , cuDom = dom
  , cuTone = Tone toneSince lastTone
  , cuHeardFor = heardFor
  , cuQuiet = t - lastTone
  , cuSinceOther = case toneSince of
      Just (f, since) | f /= answerToneItu -> t - since
      _ -> 0
  , cuU11 = u11Seen
  , cuScrOnes = pump $ \r -> vrOnesRun r >= scrambledBits && vrU11Run r < u11Guard
  , cuScrAny = pump $ \r -> vrOnesRun r >= scrambledBits || vrZerosRun r >= scrambledBits
  , cuS1Pattern = pump $ \r -> vrS1Run r >= s1Symbols
  , cuOnes2400 = pump $ \r -> vrOnes2400 r >= 32
  , cuQualified = qualified
  , cuBell212 = heardFor answerToneBell >= 0.155 && not u11Seen
  , cuOtherFsk = any (\s -> laAllows lad s && dom == Just (fskMark (fskRx role s)))
                     [V21, V23, Bell103]
  , cuAlive = alive
  , cuV32Peer = hiV32Peer inp
  }
  where
    t = tfTime fr
    dom = dominant (ccSquelch cfg) (ccDomRatio cfg) fr
    pump f = maybe False f (hiPump inp)

    toneSince = case (dom, tnSince prev) of
      (Just f, Just (g, since)) | f == g -> Just (f, since)
      (Just f, _)                        -> Just (f, t)
      (Nothing, _)                       -> Nothing
    lastTone = if dom == Nothing then tnLast prev else t
    heardFor f = case toneSince of
      Just (g, since) | g == f -> t - since
      _ -> 0

    -- Unscrambled binary 1 descrambles to ones as surely as scrambled
    -- binary 1 does -- a constant input to the descrambler is a constant
    -- output -- so a run of descrambled ones on its own does not say
    -- which of the two the far end is sending.  The carrier does: the
    -- answerer's unscrambled ones are one phase step repeated, while
    -- scrambled ones are whitened.  Without this the calling modem
    -- starts its 765 ms settle while the answerer is still in its
    -- unscrambled ones, and has already declared 1200 bit/s by the time
    -- the answerer's S1 arrives to agree on 2400.
    u11Seen = pump $ \r -> vrU11Run r >= u11Symbols && vrAngleErr r < 8

    -- An FSK carrier counts once it has persisted; the Bell 103 answer
    -- mark must not be V.22 unscrambled ones in disguise.
    --
    -- The V.22 family is qualified through its own receiver rather than
    -- through tones.  Answering a V.23 caller would mean detecting its
    -- 390 Hz backward mark, and 390 Hz cannot be told from the V.8bis
    -- CRe tone at 400 Hz by a bank whose 40 ms window resolves 25 Hz, so
    -- V.23 is offered on the calling side only.
    qualified s
      | isV22Family s = False
      | otherwise = heardFor (fskMark (fskRx role s)) >= ccQualify cfg
                    && not (s == Bell103 && role == Originate && u11Seen)

    alive s = case linkFor role s of
      FskLink _ rx -> t - lastToneOf rx <= ccDrop cfg
      V22Link {}   -> True
      V32Link {}   -> True
      where lastToneOf spec = case toneSince of
              Just (g, _) | g == fskMark spec || g == fskSpace spec -> t
              _ -> tnLast prev
