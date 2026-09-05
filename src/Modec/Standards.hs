-- | Tone tables for the FSK standards.  Frequencies in Hz, rates in baud.
module Modec.Standards
  ( FskSpec (..)
  , bell103Originate
  , bell103Answer
  , v21Channel1
  , v21Channel2
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

fskStandards :: [FskSpec]
fskStandards = [bell103Originate, bell103Answer, v21Channel1, v21Channel2]

-- | V.25 answer tone.
answerToneItu :: Double
answerToneItu = 2100

-- | Bell 103 / 212A answer tone.
answerToneBell :: Double
answerToneBell = 2225
