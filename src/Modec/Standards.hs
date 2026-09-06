-- | Tone tables for the FSK standards.  Frequencies in Hz, rates in baud.
module Modec.Standards
  ( FskSpec (..)
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
