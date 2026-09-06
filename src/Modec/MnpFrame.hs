{-# LANGUAGE BangPatterns #-}
-- | MNP frame structure: the two framing modes and the coding of the
-- frame header, from ITU-T V.42 (10/96) Annex A, "Operation of the error
-- control function -- alternative procedure".
--
-- That annex is MNP classes 2 to 4 with the trade name filed off; it was
-- deleted again in the 03/2002 revision, so the current V.42 does not
-- describe any of this.  The frame type codes, the constant parameter
-- @1, 6, 1, 0, 0, 0, 0, 255@ and the retransmission limit of 12 all match
-- Microcom's own public-domain release of 1987 exactly, which is how the
-- identification is confirmed.
--
-- Framing mode 2 (A.3, "start-stop, octet oriented") runs over the
-- ordinary 8N1 characters the FSK and V.22 data pumps already carry:
--
-- > SYN DLE STX <header and information, DLE stuffed> DLE ETX FCS_lo FCS_hi
--
-- Framing mode 3 (A.4, "bit oriented") is ISO 3309 HDLC and reuses
-- "Modec.Hdlc" unchanged; the MNP header sits directly after the opening
-- flag, with no address or control field in front of it.
--
-- The two modes use /different/ frame check sequences, which is the
-- easiest thing in the whole protocol to get wrong.  Mode 3 uses the
-- CCITT FCS of "Modec.Hdlc": x^16 + x^12 + x^5 + 1, register preset to
-- ones, remainder complemented, most significant bit first.  Mode 2 uses
-- CRC-16/ARC: x^16 + x^15 + x^2 + 1, register preset to /zero/, /not/
-- complemented, least significant bit first, and it covers the unstuffed
-- header and information plus the ETX octet of the closing flag.  The two
-- live in separate modules and are written in deliberately different
-- styles so that neither can be mistaken for the other.
--
-- Nothing here knows about sequence numbers, timers or the state of a
-- link; that is "Modec.Mnp".  This module is only the wire format.
module Modec.MnpFrame
  ( -- * CRC-16\/ARC
    crc16arc
  , crc16arcStep
    -- * Framing mode 2: start-stop, octet oriented
  , mode2Encode
  , MnpRxError (..)
  , Mode2Rx
  , mode2RxInit
  , mode2RxOctets
  , mode2Junk
    -- * Framing mode 3: synchronous, bit oriented
  , mode3Encode
    -- * Frames
  , MnpFrame (..)
  , MnpLr (..)
  , defaultLr
  , encodeFrame
  , decodeFrame
  , negotiateLr
  , lrOctets
    -- * Frame type codes (Table A.1)
  , tLR, tLD, tLT, tLA, tLN, tLNA
  ) where

import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl')
import Data.Word (Word8, Word16)

import Modec.Hdlc (hdlcFrameBits)

-- | @SYN@, the mode 2 lead-in.
syn :: Word8
syn = 0x16

-- | @DLE@, the escape that makes mode 2 transparent.
dle :: Word8
dle = 0x10

-- | @STX@, the second octet of the opening flag.
stx :: Word8
stx = 0x02

-- | @ETX@, the second octet of the closing flag.  It is covered by the FCS.
etx :: Word8
etx = 0x03

-- Frame types, Table A.1.  3 is unassigned.

tLR, tLD, tLT, tLA, tLN, tLNA :: Word8
tLR = 1   -- ^ link request
tLD = 2   -- ^ link disconnect
tLT = 4   -- ^ link transfer
tLA = 5   -- ^ link acknowledgement
tLN = 6   -- ^ link attention
tLNA = 7  -- ^ link attention acknowledgement

-- | Feed one octet into the CRC-16\/ARC register (A.3.6).  The polynomial
-- is 0xA001, the reflection of x^16 + x^15 + x^2 + 1, and the register is
-- neither preset nor complemented.
--
-- This is not the frame check sequence of "Modec.Hdlc": different
-- polynomial, different preset, opposite bit order, no final complement.
crc16arcStep :: Word16 -> Word8 -> Word16
crc16arcStep reg o = go (8 :: Int) (reg `xor` fromIntegral o)
  where
    go 0 !r = r
    go k !r = go (k - 1) (if testBit r 0 then (r `shiftR` 1) `xor` 0xA001 else r `shiftR` 1)

-- | CRC-16\/ARC of a sequence of octets, starting from a zero register.
-- The check value for the octets of @\"123456789\"@ is 0xBB3D.
crc16arc :: [Word8] -> Word16
crc16arc = foldl' crc16arcStep 0

-- | A frame body (header and information) as mode 2 line octets.
--
-- Two @SYN@s rather than one: the receiver hunts for @DLE STX@ and
-- ignores what comes before it, so the extra octet costs nothing and
-- gives the far end's start-stop framer a second character to lock its
-- start bit on if it was in the middle of noise.
--
-- The FCS covers the unstuffed body and the ETX; it is transmitted after
-- the closing flag, low octet first, and is /not/ itself stuffed, so a
-- receiver must take exactly two raw octets there.
mode2Encode :: [Word8] -> [Word8]
mode2Encode body =
  [syn, syn, dle, stx] ++ concatMap esc body ++ [dle, etx, lo, hi]
  where
    esc b | b == dle = [dle, dle]
          | otherwise = [b]
    f = crc16arc (body ++ [etx])
    lo = fromIntegral (f .&. 0xFF)
    hi = fromIntegral (f `shiftR` 8)

-- | Why a frame was thrown away.  The distinction matters at the end of
-- link establishment: A.7.2.2 says a timeout with damaged frames seen
-- disconnects with an LD, while a timeout with nothing received at all
-- falls through silently to an unprotected connection.
data MnpRxError
  = BadFcs        -- ^ a complete frame whose check sequence did not match
  | BadStuffing   -- ^ a DLE followed by neither DLE nor ETX, or an over-long frame
  deriving (Eq, Show)

-- | Hunting for @DLE STX@ (the flag holds a pending DLE), inside the
-- body, just past a DLE in the body, or collecting the two FCS octets.
data Mode2Phase
  = PHunt !Bool
  | PBody
  | PBodyDle
  | PFcs1
  | PFcs2 !Word8
  deriving (Eq, Show)

-- | Receiver state for framing mode 2.  Body and junk are held newest
-- first and reversed on the way out.
data Mode2Rx = Mode2Rx
  { mrPhase :: !Mode2Phase
  , mrBody  :: [Word8]
  , mrN     :: !Int
  , mrJunk  :: [Word8]
  , mrJunkN :: !Int
  }

-- | Longest body accepted before giving up and resynchronising: the
-- largest header plus the 256-octet information field class 4 allows,
-- with room to spare.
maxBody :: Int
maxBody = 600

-- | How much unframed data is remembered for 'mode2Junk'.
maxJunk :: Int
maxJunk = 512

mode2RxInit :: Mode2Rx
mode2RxInit = Mode2Rx (PHunt False) [] 0 [] 0

-- | Octets seen outside any frame, oldest first.  When a link falls
-- through to unprotected operation because the far end never answered,
-- these were the far end's data all along -- typically the opening lines
-- of a BBS banner -- so they are handed to the terminal rather than
-- dropped.  Cleared whenever a frame decodes, since a working link means
-- they were only lead-in.
mode2Junk :: Mode2Rx -> [Word8]
mode2Junk = reverse . mrJunk

-- | Feed received line octets; returns frame bodies in order, or the
-- errors that replaced them.
mode2RxOctets :: Mode2Rx -> [Word8] -> (Mode2Rx, [Either MnpRxError [Word8]])
mode2RxOctets st0 os = go st0 os []
  where
    go st [] acc = (st, reverse acc)
    go st (o : rest) acc = case mrPhase st of
      PHunt pending
        | pending && o == stx -> go st { mrPhase = PBody, mrBody = [], mrN = 0 } rest acc
        | pending && o == dle -> go (junk st dle) rest acc          -- stay pending
        | pending             -> go (junk (junk st dle) o) { mrPhase = PHunt False } rest acc
        | o == dle            -> go st { mrPhase = PHunt True } rest acc
        | otherwise           -> go (junk st o) rest acc
      PBody
        | o == dle -> go st { mrPhase = PBodyDle } rest acc
        | mrN st >= maxBody -> go (reset st) rest (Left BadStuffing : acc)
        | otherwise -> go st { mrBody = o : mrBody st, mrN = mrN st + 1 } rest acc
      PBodyDle
        -- a stuffed DLE: one of the pair belongs to the body
        | o == dle -> go st { mrPhase = PBody, mrBody = dle : mrBody st, mrN = mrN st + 1 } rest acc
        -- the closing flag
        | o == etx -> go st { mrPhase = PFcs1 } rest acc
        -- an opening flag inside a frame: the frame was truncated and a
        -- new one starts here, so report the loss and resynchronise
        | o == stx -> go st { mrPhase = PBody, mrBody = [], mrN = 0 } rest (Left BadStuffing : acc)
        | otherwise -> go (reset st) rest (Left BadStuffing : acc)
      PFcs1 -> go st { mrPhase = PFcs2 o } rest acc
      PFcs2 lo ->
        let body = reverse (mrBody st)
            want = crc16arc (body ++ [etx])
            got = fromIntegral lo .|. (fromIntegral o `shiftL` 8) :: Word16
        in if want == got
             then go (reset st) { mrJunk = [], mrJunkN = 0 } rest (Right body : acc)
             else go (reset st) rest (Left BadFcs : acc)

    reset st = st { mrPhase = PHunt False, mrBody = [], mrN = 0 }
    junk st o
      | mrJunkN st >= maxJunk = st
      | otherwise = st { mrJunk = o : mrJunk st, mrJunkN = mrJunkN st + 1 }

-- | A frame body as mode 3 line bits, with one opening and one closing
-- flag.  Interframe fill is the caller's business: an idle synchronous
-- transmitter sends 'Modec.Hdlc.hdlcFlagBits'.
mode3Encode :: [Word8] -> [Bool]
mode3Encode = hdlcFrameBits 1 1

-- | A decoded frame header, with the information field of an LT.
data MnpFrame
  = FrLR MnpLr
  | FrLD !Word8 (Maybe Word8)   -- ^ reason code, and the user code when the reason is 255
  | FrLT !Word8 [Word8]         -- ^ N(S) and the information field, 1 to N401 octets
  | FrLA !Word8 !Word8          -- ^ N(R) and N(k), the receive credit
  | FrLN !Word8 !Word8          -- ^ N(SA) and the attention type
  | FrLNA !Word8                -- ^ N(RA)
  | FrOther !Word8 [Word8]     -- ^ an unknown type, kept whole so it can be traced and ignored
  deriving (Eq, Show)

-- | The link request, Table A.2.  Every field after the type octet is a
-- variable parameter except 'lrConst1'; the eight octets that V.42 prints
-- as "constant parameter 2" are themselves a well formed parameter of
-- type 1 and length 6, which is why they are held here as a value.
data MnpLr = MnpLr
  { lrConst1  :: !Word8               -- ^ constant parameter 1; 2, or the link is refused
  , lrConst2  :: [Word8]              -- ^ variable parameter 1's value, echoed verbatim
  , lrFraming :: !Word8               -- ^ variable parameter 2: 1 half duplex, 2 octet, 3 bit oriented
  , lrK       :: !Word8               -- ^ variable parameter 3: outstanding LT frames
  , lrN401    :: !Int                 -- ^ variable parameter 4: information field limit, two octets
  , lrDpo     :: !Word8               -- ^ variable parameter 8; 0 when the parameter is absent
  , lrOther   :: [(Word8, [Word8])]   -- ^ parameters we do not know, kept so they can be traced
  } deriving (Eq, Show)

-- | What this modem offers: framing mode 2, eight outstanding frames, a
-- 64-octet information field, no data phase optimization.  A.7.5 says an
-- implementation should support exactly these.
defaultLr :: MnpLr
defaultLr = MnpLr
  { lrConst1 = 2
  , lrConst2 = [0x01, 0x00, 0x00, 0x00, 0x00, 0xFF]
  , lrFraming = 2
  , lrK = 8
  , lrN401 = 64
  , lrDpo = 0
  , lrOther = []
  }

-- | The 21 header octets of the reference initiator LR (A.6.4.1), kept as
-- a golden vector: a decoder that agrees with a real MNP modem has to
-- produce exactly these from 'defaultLr'.
lrOctets :: [Word8]
lrOctets =
  [ 0x14, 0x01, 0x02
  , 0x01, 0x06, 0x01, 0x00, 0x00, 0x00, 0x00, 0xFF
  , 0x02, 0x01, 0x02
  , 0x03, 0x01, 0x08
  , 0x04, 0x02, 0x40, 0x00
  ]

tlv :: Word8 -> [Word8] -> [Word8]
tlv t v = t : fromIntegral (length v) : v

-- | Prefix the length indication, which counts the header from the type
-- octet onwards and excludes both itself and any information field.
withLi :: [Word8] -> [Word8]
withLi hdr = fromIntegral (length hdr) : hdr

-- | Encode a frame body.  The flag is the data phase optimization of
-- class 4, which shortens LT and LA by promoting their sequence numbers
-- from variable to fixed parameters (Tables A.3b and A.6b); it never
-- applies to the LR exchange, which is always sent in the long form.
encodeFrame :: Bool -> MnpFrame -> [Word8]
encodeFrame dpo frame = case frame of
  FrLR lr -> withLi
    ( tLR : lrConst1 lr
    : tlv 1 (lrConst2 lr)
   ++ tlv 2 [lrFraming lr]
   ++ tlv 3 [lrK lr]
   ++ tlv 4 [fromIntegral (lrN401 lr .&. 0xFF), fromIntegral ((lrN401 lr `shiftR` 8) .&. 0xFF)]
   ++ (if lrDpo lr /= 0 then tlv 8 [lrDpo lr] else [])
   ++ concatMap (uncurry tlv) (lrOther lr) )
  FrLD reason muser -> withLi (tLD : tlv 1 [reason] ++ maybe [] (\u -> tlv 2 [u]) muser)
  FrLT ns info
    | dpo -> withLi [tLT, ns] ++ info
    | otherwise -> withLi (tLT : tlv 1 [ns]) ++ info
  FrLA nr nk
    | dpo -> withLi [tLA, nr, nk]
    | otherwise -> withLi (tLA : tlv 1 [nr] ++ tlv 2 [nk])
  FrLN nsa atype -> withLi (tLN : tlv 1 [nsa] ++ tlv 2 [atype])
  FrLNA nra -> withLi (tLNA : tlv 1 [nra])
  FrOther t rest -> withLi (t : rest)

-- | Decode a frame body.
--
-- LT and LA are dispatched on the length indication (4 or 2, 7 or 3)
-- rather than on the optimization negotiated for the connection.  The two
-- forms are distinguishable on sight, so a receiver never has to agree
-- with the far end about which is in force, and the one way the two ends
-- could fall out of step is closed off.
decodeFrame :: [Word8] -> Either String MnpFrame
decodeFrame body = do
  (li, hdr, info) <- split body
  case hdr of
    [] -> Left "empty frame header"
    (t : rest)
      | t == tLR -> FrLR <$> decodeLr rest
      | t == tLD -> case params rest of
          Right [(1, [r])] -> Right (FrLD r Nothing)
          Right [(1, [r]), (2, [u])] -> Right (FrLD r (Just u))
          _ -> Left "malformed LD"
      | t == tLT -> case (li, rest) of
          (2, [ns]) -> ltWith ns
          (4, [1, 1, ns]) -> ltWith ns
          _ -> Left "malformed LT"
      | t == tLA -> case (li, rest) of
          (3, [nr, nk]) -> Right (FrLA nr nk)
          (7, [1, 1, nr, 2, 1, nk]) -> Right (FrLA nr nk)
          _ -> Left "malformed LA"
      | t == tLN -> case params rest of
          Right [(1, [nsa]), (2, [a])] -> Right (FrLN nsa a)
          _ -> Left "malformed LN"
      | t == tLNA -> case params rest of
          Right [(1, [nra])] -> Right (FrLNA nra)
          _ -> Left "malformed LNA"
      | otherwise -> Right (FrOther t rest)
      where
        ltWith ns
          | null info = Left "LT with an empty information field"
          | otherwise = Right (FrLT ns info)
  where
    -- A length indication of 255 escapes to a 16-bit count.  V.42 does not
    -- give its octet order; we read it low octet first, as parameter 4 of
    -- the LR is written, and never generate one.
    split (0xFF : a : b : rest) =
      let n = fromIntegral a + 256 * fromIntegral b
      in if length rest < n then Left "truncated extended frame"
         else Right (n, take n rest, drop n rest)
    split (li : rest)
      | length rest < fromIntegral li = Left "truncated frame"
      | otherwise = Right (fromIntegral li, take (fromIntegral li) rest, drop (fromIntegral li) rest)
    split [] = Left "empty frame"

-- | Split a run of type-length-value parameters.
params :: [Word8] -> Either String [(Word8, [Word8])]
params [] = Right []
params (t : n : rest)
  | length rest < fromIntegral n = Left "truncated parameter"
  | otherwise = ((t, take (fromIntegral n) rest) :) <$> params (drop (fromIntegral n) rest)
params _ = Left "trailing octet in parameter list"

decodeLr :: [Word8] -> Either String MnpLr
decodeLr [] = Left "LR without constant parameter 1"
decodeLr (c1 : rest) = do
  ps <- params rest
  let known = [1, 2, 3, 4, 8] :: [Word8]
  n401 <- case lookup 4 ps of
    Just [a, b] -> Right (fromIntegral a + 256 * fromIntegral b)
    Just _ -> Left "LR parameter 4 is not two octets"
    Nothing -> Right (lrN401 defaultLr)
  Right MnpLr
    { lrConst1 = c1
    , lrConst2 = maybe (lrConst2 defaultLr) id (lookup 1 ps)
    , lrFraming = one 2 (lrFraming defaultLr) ps
    , lrK = one 3 (lrK defaultLr) ps
    , lrN401 = n401
    , lrDpo = one 8 0 ps
    , lrOther = [ p | p@(t, _) <- ps, t `notElem` known ]
    }
  where
    one t d ps = case lookup t ps of { Just [v] -> v; _ -> d }

-- | Apply the negotiation rules of A.7.1 to our own link request and the
-- one that came back.  The responder's answer is authoritative for the
-- data phase, and every negotiable value is the smaller of the two, so a
-- station that cannot carry synchronous framing simply offers mode 2 and
-- the minimum settles it.  'Left' carries the reason code for the LD to
-- send instead.
--
-- Note that V.42 takes the smaller N401 while Microcom's 1987 code takes
-- the larger.  That is a bug in the reference implementation, not a
-- dialect: the smaller is the only value both ends are known to accept.
negotiateLr :: MnpLr -> MnpLr -> Either Word8 MnpLr
negotiateLr ours theirs
  | lrConst1 theirs /= 2 = Left 2
  | lrFraming theirs < 1 || lrFraming theirs > 3 = Left 3
  | lrK theirs == 0 || lrN401 theirs == 0 = Left 3
  | otherwise = Right ours
      { lrFraming = min (lrFraming ours) (lrFraming theirs)
      , lrK = min (lrK ours) (lrK theirs)
      , lrN401 = min (lrN401 ours) (lrN401 theirs)
      , lrDpo = lrDpo ours .&. lrDpo theirs
      , lrConst2 = lrConst2 theirs
      , lrOther = []
      }
