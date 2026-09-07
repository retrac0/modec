-- | ITU-T V.8bis: signals and messages for identifying and selecting a
-- common mode of operation before the modem start-up.
--
-- Signals (7.1): an initiating station sends a 1375 + 2002 Hz dual tone,
-- a responding station 1529 + 2225 Hz, both for 400 ms, followed by a
-- 100 ms single tone naming the signal (MRe 650, MRd 1150, CRe 400,
-- CRd 1900, ESi 980, ESr 1650 Hz).
--
-- Messages (7.2, 8): HDLC frames over V.21, channel 1 from the
-- initiating station and channel 2 from the responding station, each
-- preceded by 100 ms of mark.  This module encodes and decodes the
-- subset needed to negotiate a data mode: CL (capabilities list), MS
-- (mode select) and ACK(1)/NAK, with the identification field's
-- revision 2 and the standard information field's Data capabilities
-- (V.21, V.22, V.22bis, transparent data, V.14).
module Modec.V8bis
  ( Signal8 (..)
  , signalTones
  , signalIsInitiating
  , v8bisToneFreqs
  , DataMode (..)
  , Message (..)
  , encodeMessage
  , decodeMessage
  , dataModeStandardBit
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.List (foldl')
import Data.Word (Word8)

data Signal8 = MRe | MRd | CRe | CRd | ESi | ESr deriving (Eq, Show, Enum, Bounded)

-- | (segment 1 dual tone pair, segment 2 tone) in Hz.
signalTones :: Signal8 -> ((Double, Double), Double)
signalTones s = case s of
  MRe -> (ini, 650)
  MRd -> (ini, 1150)
  CRe -> (ini, 400)
  CRd -> (rsp, 1900)
  ESi -> (ini, 980)
  ESr -> (rsp, 1650)
  where
    ini = (1375, 2002)
    rsp = (1529, 2225)

-- | Signals sent by an initiating station use the initiating tone pair.
signalIsInitiating :: Signal8 -> Bool
signalIsInitiating s = s `elem` [MRe, MRd, CRe, ESi]

-- | Frequencies a tone bank needs to recognise every V.8bis signal.
v8bisToneFreqs :: [Double]
v8bisToneFreqs = [1375, 2002, 1529, 2225, 650, 1150, 400, 1900, 980, 1650]

-- | Data modes this implementation can offer or select (Table 6-3c).
data DataMode = ModeV21 | ModeV22 | ModeV22bis deriving (Eq, Show, Ord, Enum, Bounded)

-- | Bit (1-based, in Data NPar(2) octet 3) for a mode.
dataModeStandardBit :: DataMode -> Int
dataModeStandardBit ModeV22bis = 2
dataModeStandardBit ModeV22 = 3
dataModeStandardBit ModeV21 = 4

data Message
  = CL [DataMode]          -- ^ capabilities list: the data modes offered
  | MS DataMode            -- ^ mode select: the data mode requested (V.25 start-up follows)
  | Ack Int                -- ^ ACK(1) or ACK(2)
  | Nak Int                -- ^ NAK(1..4)
  | Other Word8 [Word8]    -- ^ any other message type with its raw information field
  deriving (Eq, Show)

revision :: Word8
revision = 2

typeMS, typeCL, typeACK1, typeNAK1 :: Word8
typeMS = 1
typeCL = 2
typeACK1 = 4
typeNAK1 = 8

-- | Information field octets of a message.
--
-- Identification field: type and revision, then the NPar(1) block (one
-- octet, delimiter bit 8 set, no V.8 start-up so V.25 start-up follows),
-- then the SPar(1) block (one octet, delimiter set, no network type).
-- Standard information field (CL and MS only): NPar(1) block (none),
-- SPar(1) block with the Data capability, then the Data Par(2) block:
-- NPar(2) octet 1 (transparent data, V.14), octet 2 (none), octet 3
-- (the modes) with bits 7 and 8 set to close both the NPar(2) block and
-- the Par(2) block.
encodeMessage :: Message -> [Word8]
encodeMessage msg = case msg of
  CL modes -> ident typeCL ++ standard modes
  MS mode -> ident typeMS ++ standard [mode]
  Ack n -> ident (typeACK1 + fromIntegral (max 0 (min 1 (n - 1))))
  Nak n -> ident (typeNAK1 + fromIntegral (max 0 (min 3 (n - 1))))
  Other t raw -> t : raw
  where
    ident t = [t .&. 0x0F .|. (revision `shiftL` 4), 0x80, 0x80]
    standard modes =
      [ 0x80                                   -- S NPar(1): none, last octet
      , 0x81                                   -- S SPar(1): Data, last octet
      , 0x09                                   -- Data NPar(2) octet 1: transparent data, V.14
      , 0x00                                   -- Data NPar(2) octet 2: none
      , 0xC0 .|. foldl' (\acc m -> acc .|. (1 `shiftL` (dataModeStandardBit m - 1))) 0 modes ]

-- | Parse an information field.  Unknown or malformed content becomes
-- 'Other' (with the raw field) rather than an error, as 8.3.1 asks
-- receivers to ignore what they do not understand.
decodeMessage :: [Word8] -> Message
decodeMessage [] = Other 0 []
decodeMessage field@(first : rest) =
  let t = first .&. 0x0F
  in case t of
       _ | t == typeACK1 -> Ack 1
         | t == typeACK1 + 1 -> Ack 2
         | t >= typeNAK1 && t <= typeNAK1 + 3 -> Nak (fromIntegral (t - typeNAK1) + 1)
         | t == typeCL || t == typeMS ->
             case dataModes rest of
               Just ms | t == typeCL -> CL ms
               Just (m : _) | t == typeMS -> MS m
               _ -> Other t rest
         | otherwise -> Other t rest
  where
    -- skip a block delimited by the given bit (1-based), returning the remainder
    skipBlock bit os = case os of
      [] -> []
      (o : more) | testBit o (bit - 1) -> more
                 | otherwise -> skipBlock bit more
    dataModes os =
      let afterI = skipBlock 8 (skipBlock 8 os)        -- I: NPar(1) block, SPar(1) block (no Par(2) blocks assumed)
          afterSN = skipBlock 8 afterI                  -- S: NPar(1) block
      in case afterSN of
           (spar : par2) | testBit spar 0 ->             -- Data capability present in SPar(1) octet 1
             let npar2 = takeBlock 7 (skipBlock 8 (dropSparRest spar par2))
             in case npar2 of
                  (_ : _ : o3 : _) -> Just [ m | m <- [minBound .. maxBound], testBit o3 (dataModeStandardBit m - 1) ]
                  _ -> Just []
           _ -> Nothing
    -- the SPar(1) block may span octets; skip its remaining octets (bit 8 delimits)
    dropSparRest spar par2 = if testBit spar 7 then 0x80 : par2 else skipToDelim par2
      where skipToDelim [] = [0x80]
            skipToDelim (o : more) | testBit o 7 = 0x80 : more
                                   | otherwise = skipToDelim more
    takeBlock bit os = case os of
      [] -> []
      (o : more) | testBit o (bit - 1) -> [o]
                 | otherwise -> o : takeBlock bit more
    _ = field
    _ = shiftR (0 :: Word8) 0
