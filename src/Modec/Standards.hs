-- | The vocabulary the whole modem is written in: which modulations
-- exist, which end of the call we are, how a character is framed, and
-- the tone tables for the FSK standards.  Frequencies in Hz, rates in
-- baud.
--
-- The rule that keeps this module a leaf, and worth keeping: only types
-- with nothing underneath them live here.  'Link' does not -- it names a
-- V.22 channel and a V.32 rate -- so it lives in "Modec.Link", one layer
-- up.  Anything that needs a sample rate is further up still.
module Modec.Standards
  ( -- * Modulations
    Standard (..)
  , allStandards
  , isV22Family
  , isV32
  , standardName
  , standardNamed
    -- * Which end of the call
  , Role (..)
    -- * Character framing
  , Framing (..)
  , framing8N1
  , tddFraming
    -- * FSK tone tables
  , FskSpec (..)
  , bell103Originate
  , bell103Answer
  , v21Channel1
  , v21Channel2
  , v23Forward
  , v23Backward
  , tdd45
  , tdd50
  , fskStandards
  , answerToneItu
  , answerToneBell
  ) where

-- | A modulation the modem can negotiate.  'Bell212A' is the North
-- American 1200 bit/s DPSK standard: the same 600 baud data pump and
-- handshake timings as V.22, but announced with the 2225 Hz Bell answer
-- tone instead of unscrambled binary 1, without guard tones, and with no
-- 2400 bit/s rate.  'V22bis' is V.22 that negotiated 2400 bit/s.
-- 'V23' is V.23 duplex: 1200 bit/s from the answering modem, 75 bit/s
-- back from the calling one.  It is the only asymmetric mode here, and
-- the only one whose two directions run at different rates.
data Standard = Bell103 | V21 | V23 | Bell212A | V22 | V22bis | V32 | V32bis deriving (Eq, Show, Enum, Bounded)

-- | Every mode, best first; the default configuration.  V.23 is not in
-- it: 75 bit/s upstream is worse than V.21 for anything but viewdata, so
-- it is a mode to ask for rather than one to fall into.
allStandards :: [Standard]
allStandards = [V22bis, V22, Bell212A, V21, Bell103]

-- | Modes that use the V.22 data pump.
isV22Family :: Standard -> Bool
isV22Family s = s `elem` [Bell212A, V22, V22bis]

-- | V.32 is the one mode here that does not take turns by frequency.
-- Both directions share the whole band on an 1800 Hz carrier, its
-- start-up is the sample-accurate exchange of "Modec.V32Start" rather
-- than anything this tick-driven machine can drive, and it is the only
-- mode that needs an echo canceller.
isV32 :: Standard -> Bool
isV32 s = s == V32 || s == V32bis

-- | Which end of the call we are.  The calling modem originates; the
-- answering modem answers.  Every asymmetry in the whole modem -- which
-- FSK band we transmit in, which V.22 channel we own, which V.32
-- scrambler polynomial whitens our data -- comes back to this one bit.
data Role = Originate | Answer deriving (Eq, Show)

-- | The spelling a mode goes by on the command line, in a @.call@
-- fixture and in the call log.  One table, because the three that used
-- to do this -- the @--mode@ reader, the fixture minter and the corpus
-- parser -- could drift apart without anything failing to compile, and
-- the corpus would then break at test time rather than at build time.
standardName :: Standard -> String
standardName s = case s of
  Bell103 -> "bell103"; V21 -> "v21"; V23 -> "v23"; Bell212A -> "bell212a"
  V22 -> "v22"; V22bis -> "v22bis"; V32 -> "v32"; V32bis -> "v32bis"

-- | The inverse.  @bell212@ is accepted for 'Bell212A' because the
-- command line has always taken it.
standardNamed :: String -> Maybe Standard
standardNamed "bell212" = Just Bell212A
standardNamed n = lookup n [(standardName s, s) | s <- [minBound .. maxBound]]

-- | Asynchronous character framing.  Start bit is always one space;
-- data bits are sent LSB first; no parity support yet.
--
-- The stop period is measured in bit times and need not be whole: the
-- 5-bit text telephone code specifies a minimum of one and a half.
-- "Minimum" is load bearing -- after the stop bits the line simply
-- idles in mark until the next start bit, for anything from nothing to
-- a second, so a receiver must resynchronise on every start edge rather
-- than assume a fixed character period.  Only 'frameKeyed' can express
-- the fraction; 'frameBits', which frames whole bits for the
-- continuous-carrier modes, rounds up.
data Framing = Framing
  { frDataBits :: !Int
  , frStopBits :: !Double
  } deriving (Eq, Show)

framing8N1 :: Framing
framing8N1 = Framing 8 1

-- | The 5-bit text telephone character (V.18 A.4 / ANSI TIA-825).
--
-- Two stop bits, where the Recommendation asks for a minimum of one and
-- a half.  The minimum is what a receiver may require; it is not what a
-- transmitter should send.  Asterisk sends exactly 1.5 and minimodem's
-- @tdd@ preset requires 2.0, so 1.5 on the line loses characters to
-- minimodem -- 32 of 35, with the clock read 3.9 % fast -- while 2.0 is
-- decoded perfectly by both it and us.  Ultratec's Turbo Code uses two
-- stop bits for the same reason, to give a tone detector enough mark to
-- lock to.  The cost is 11 ms a character, six per cent of a line that
-- is slow anyway.
--
-- The receiver deliberately does not enforce this: it wants a mark stop
-- bit and nothing more, because the stop period is idle mark of
-- unbounded length and a far end sending the 1.5 minimum is correct.
tddFraming :: Framing
tddFraming = Framing 5 2

data FskSpec = FskSpec
  { fskName  :: String
  , fskMark  :: Double
  , fskSpace :: Double
  , fskBaud  :: Double
  } deriving (Eq, Show)

-- | Bell 103 calling (originate) modem transmits in the low band.
bell103Originate :: FskSpec
bell103Originate = FskSpec "bell103-originate" 1270 1070 300

-- | Bell 103 answering modem transmits in the high band; its mark tone
-- (2225 Hz) doubles as the Bell answer tone.
bell103Answer :: FskSpec
bell103Answer = FskSpec "bell103-answer" 2225 2025 300

-- | V.21 channel 1 (calling modem transmits).  Note mark is the lower tone.
v21Channel1 :: FskSpec
v21Channel1 = FskSpec "v21-ch1" 980 1180 300

-- | V.21 channel 2 (answering modem transmits).  Also the V.8/V.8bis
-- message channel and the fax T.30 handshake channel.
v21Channel2 :: FskSpec
v21Channel2 = FskSpec "v21-ch2" 1650 1850 300

-- | V.23 forward channel, 1200 bit/s.  Mark is the lower tone again, and
-- the space tone sits on 2100 Hz, which is also the V.25 answer tone: a
-- carrier detector for this channel has to key on the mark alone.
v23Forward :: FskSpec
v23Forward = FskSpec "v23-forward" 1300 2100 1200

-- | V.23 backward channel, 75 bit/s.  The pair is far below the forward
-- channel so the two run simultaneously in the same direction-pair; this
-- is what makes V.23 duplex asymmetric rather than half-duplex.
v23Backward :: FskSpec
v23Backward = FskSpec "v23-backward" 390 450 75

-- | The 5-bit text telephone (TTY/TDD) line, ITU-T V.18 Annex A, whose
-- normative definition is ANSI/TIA-825.  Mark is the lower tone and the
-- tolerance on both is +/-5 %.
--
-- One tone pair serves both directions, so the mode is half duplex and
-- a station hears its own transmitter through the hybrid; and no
-- carrier at all is present between characters, so this is the only
-- line here whose receiver cannot hunt for a start bit in idle mark.
-- Both facts are the receiver's problem, not the spec's: see
-- 'Modec.FSK.fskBurstDeframer'.
tdd45 :: FskSpec
tdd45 = FskSpec "tdd-45" 1400 1800 45.45

-- | The same line at 50 baud, as sold outside North America.  It is a
-- separate mode rather than a tolerance: 50 into a 45.45 receiver is a
-- 10 % clock error, and this family of receivers gives up around 3 %.
tdd50 :: FskSpec
tdd50 = FskSpec "tdd-50" 1400 1800 50

-- | The channels an offline classifier can tell apart by their tones.
--
-- 'v23Forward' is deliberately absent.  A tone bank integrates over a
-- window, and at 1200 bit/s any window wide enough to separate its
-- 1300 Hz mark from the Bell 103 originate mark 30 Hz away spans dozens
-- of bits, across which a continuous-phase carrier does not add up.  The
-- forward channel is identified by demodulating it, not by looking at
-- it; the backward channel at 75 bit/s has no such problem.
--
-- 'tdd50' is absent for the opposite reason: it is not that its tones
-- cannot be measured but that they are the same two tones as 'tdd45'.
-- Nothing in a tone bank separates two modes that differ only in baud,
-- so the classifier names the tone pair and the rate is settled by
-- demodulating at each and seeing which one frames characters.
fskStandards :: [FskSpec]
fskStandards =
  [bell103Originate, bell103Answer, v21Channel1, v21Channel2, v23Backward, tdd45]

-- | V.25 answer tone.
answerToneItu :: Double
answerToneItu = 2100

-- | Bell 103 / 212A answer tone.
answerToneBell :: Double
answerToneBell = 2225
