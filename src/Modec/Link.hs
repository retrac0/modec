-- | What a connection is: which modulation carries it, which way round
-- it runs, and how fast.
--
-- These types sat in the modules that own their data pumps -- 'Rate' and
-- 'V22Channel' in "Modec.V22", 'V32Rate' in "Modec.V32", 'Link' itself
-- in "Modec.Handshake" -- so naming what a call agreed on meant
-- importing a modem, and the handshake could not be split up without
-- taking the vocabulary with it.  A link is not a modem; it is what two
-- modems settled on.
--
-- One layer above "Modec.Standards" and below everything that puts a
-- sample on a line.  What lives here is the enumerations and the
-- properties that are arithmetic on them.  What does not is anything
-- measured off a constellation: 'Modec.V32.rateMargin' is derived from
-- the point set and stays with the point set.
module Modec.Link
  ( -- * V.22 family
    V22Channel (..)
  , carrierOf
  , Rate (..)
    -- * V.32 family
  , V32Rate (..)
  , allV32Rates
  , rateBitsPerSymbol
  , rateTrellis
  , rateUncoded
  , rateBitRate
    -- * An established connection
  , Link (..)
  , linkFor
  , v22LinkAt
  , linkBitRate
  , linkSyncable
  , linkRateName
  , linkChannels
  ) where

import Modec.Standards

-- | Which half of the band a V.22 modem transmits in.  The calling modem
-- has the low channel, the answering modem the high one, and each hears
-- the other.
data V22Channel = LowChannel | HighChannel deriving (Eq, Show)

carrierOf :: V22Channel -> Double
carrierOf LowChannel = 1200
carrierOf HighChannel = 2400

-- | The two V.22 rates.  Both run at 600 baud; 2400 bit\/s is the same
-- pump deciding among sixteen points instead of four.
data Rate = R1200 | R2400 deriving (Eq, Show)

-- | The rates this implementation offers.  2400 bit\/s is "for further
-- study" in §2.4.3 and does not exist in any real modem, so it is not
-- here; the rate signal can still advertise it as unavailable.
data V32Rate
  = V32R4800    -- ^ 4800 bit\/s, four states, no trellis (V.32 §2.4.2)
  | V32R7200    -- ^ 7200 bit\/s, 16-point trellis coded (V.32bis §2.3.4)
  | V32R9600    -- ^ 9600 bit\/s, 16-point non-redundant (V.32 §2.4.1.1)
  | V32R9600T   -- ^ 9600 bit\/s, 32-point trellis coded (V.32 §2.4.1.2)
  | V32R12000   -- ^ 12000 bit\/s, 64-point trellis coded (V.32bis §2.3.2)
  | V32R14400   -- ^ 14400 bit\/s, 128-point trellis coded (V.32bis §2.3.1)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The V.32bis rates, best first.  4800 and 9600 are V.32's and are
-- reached by a V.32bis modem talking to a V.32 one; Table 5\/V.32bis
-- Note 1 says as much, by making the bits for those two rates
-- permanently set.
allV32Rates :: [V32Rate]
allV32Rates = [V32R14400, V32R12000, V32R9600T, V32R9600, V32R7200, V32R4800]

-- | Data bits carried per symbol.  A trellis rate's redundant bit is not
-- among them: it is the constellation that grows, not the payload.
rateBitsPerSymbol :: V32Rate -> Int
rateBitsPerSymbol V32R4800 = 2
rateBitsPerSymbol V32R7200 = 3
rateBitsPerSymbol V32R9600 = 4
rateBitsPerSymbol V32R9600T = 4
rateBitsPerSymbol V32R12000 = 5
rateBitsPerSymbol V32R14400 = 6

-- | Whether this rate runs through the convolutional encoder.
rateTrellis :: V32Rate -> Bool
rateTrellis r = r `elem` [V32R7200, V32R9600T, V32R12000, V32R14400]

-- | Bits that bypass the coding entirely (Q3 onwards), choosing between
-- the points of one trellis subset.
rateUncoded :: V32Rate -> Int
rateUncoded r
  | rateTrellis r = rateBitsPerSymbol r - 2
  | otherwise = 0

rateBitRate :: V32Rate -> Int
rateBitRate r = 2400 * rateBitsPerSymbol r

-- | The channels of an established connection: our transmit side and
-- our receive side.
data Link
  = FskLink FskSpec FskSpec
  | V22Link V22Channel V22Channel Rate
  -- | V.32 is symmetric -- one carrier, one rate, the same both ways --
  -- so a link is just which end we are and what was settled on.
  | V32Link Role V32Rate
  deriving (Eq, Show)

-- | The link a standard runs on, at its own rate.
linkFor :: Role -> Standard -> Link
linkFor Originate Bell103 = FskLink bell103Originate bell103Answer
linkFor Answer Bell103 = FskLink bell103Answer bell103Originate
linkFor Originate V21 = FskLink v21Channel1 v21Channel2
linkFor Answer V21 = FskLink v21Channel2 v21Channel1
-- The caller has the 75 bit/s backward channel and listens to the 1200
-- bit/s forward one; the answerer, which is the end with something to
-- say, has it the other way round.
linkFor Originate V23 = FskLink v23Backward v23Forward
linkFor Answer V23 = FskLink v23Forward v23Backward
linkFor role Bell212A = v22LinkAt role R1200
linkFor role V22 = v22LinkAt role R1200
linkFor role V22bis = v22LinkAt role R2400
linkFor role V32 = V32Link role V32R9600T
linkFor role V32bis = V32Link role V32R9600T

v22LinkAt :: Role -> Rate -> Link
v22LinkAt Originate r = V22Link LowChannel HighChannel r
v22LinkAt Answer r = V22Link HighChannel LowChannel r

-- | The line rate of an established link, for the protocol layer's timers.
-- On an asymmetric link the slow direction is the one that governs: an
-- acknowledgement crawling back at 75 bit/s is what a timeout has to
-- wait for, whatever the other direction manages.
linkBitRate :: Link -> Double
linkBitRate (FskLink tx rx) = min (fskBaud tx) (fskBaud rx)
linkBitRate (V22Link _ _ R1200) = 1200
linkBitRate (V22Link _ _ R2400) = 2400
linkBitRate (V32Link _ r) = fromIntegral (rateBitRate r)

-- | Whether the link can carry bit-oriented framing.  Only the V.22 data
-- pump can: dropping the start and stop bits at 300 bit/s would buy 20 %
-- of thirty characters a second, and the FSK link is noise limited rather
-- than framing limited anyway.
linkSyncable :: Link -> Bool
linkSyncable V22Link {} = True
linkSyncable V32Link {} = True
linkSyncable FskLink {} = False

-- | What to call the speed of a link.  An asymmetric link has two, and
-- naming only one of them would be a lie by omission.
--
-- This and 'linkChannels' are the two things the call log and the
-- CONNECT line say about a connection.  They were written out three
-- times in the executable, once per place that had to print one.
linkRateName :: Link -> String
linkRateName link = case link of
  FskLink tx rx
    | fskBaud tx == fskBaud rx -> rounded (fskBaud tx) ++ " bit/s"
    | otherwise -> rounded (fskBaud rx) ++ "/" ++ rounded (fskBaud tx) ++ " bit/s"
  V22Link _ _ R1200 -> "1200 bit/s"
  V22Link _ _ R2400 -> "2400 bit/s"
  V32Link _ r -> show (rateBitRate r) ++ " bit/s"
  where rounded b = show (round b :: Int)

-- | Which way round the link runs, in the terms the standard uses.
linkChannels :: Link -> String
linkChannels link = case link of
  FskLink tx rx -> "sending " ++ fskName tx ++ ", hearing " ++ fskName rx
  V22Link tx rx _ -> "sending " ++ show tx ++ ", hearing " ++ show rx
  -- V.32 has one carrier and both ends on it, which is the whole reason
  -- it needs an echo canceller and the others do not.
  V32Link role r -> "1800 Hz both ways, " ++ show role ++ ", " ++ v32RateName r
  where
    v32RateName r = case r of
      V32R4800 -> "4800 bit/s"
      V32R7200 -> "7200 bit/s, trellis coded"
      V32R9600 -> "9600 bit/s, 16 point"
      V32R9600T -> "9600 bit/s, trellis coded"
      V32R12000 -> "12000 bit/s, trellis coded"
      V32R14400 -> "14400 bit/s, trellis coded"
