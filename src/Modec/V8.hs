-- | ITU-T V.8 (11/2000): starting a session by exchanging menus of
-- capabilities before any modem trains.
--
-- The answering modem announces that it speaks V.8 by sending ANSam
-- instead of a plain answer tone: 2100 Hz amplitude-modulated by a 15 Hz
-- sine so the envelope swings between 0.8 and 1.2 of its average (7.2).
-- The calling modem, which must not send CM until it has heard ANSam,
-- then goes quiet for Te and offers a call menu CM on V.21(L); the
-- answerer replies with the joint menu JM on V.21(H) listing what the
-- two have in common; the caller acknowledges with CJ, both fall silent
-- for 75 ms, and the selected modulation starts (8.1, 8.2).
--
-- What that buys a modem that would otherwise climb a fallback ladder is
-- a definite answer: the far end says what it has, rather than being
-- probed one modulation at a time.  The catch is that only ITU modes
-- have codepoints, so Bell 103 and Bell 212A cannot be offered here at
-- all, and V.8 does not separate V.22 from V.22bis -- item 4 covers
-- both, and the rate is still settled by the V.22 S1 exchange.
--
-- Coding format (5): a signal is a repeated sequence of ten ONEs, ten
-- synchronization bits, then information octets, each with a start bit
-- (ZERO) and a stop bit (ONE), sent b0 first.  A category octet carries
-- a 4-bit tag in b0-b3, ZERO in b4, and three option bits; an extension
-- octet carries ONE in b4 and five more option bits for the category it
-- follows.  The fixed bits are what keep an HDLC flag from ever
-- appearing in the stream, so that a T.30 receiver listening to the same
-- V.21(H) channel is not confused by JM.
module Modec.V8
  ( -- * Menus
    CallFunction (..)
  , callFunctionCode
  , Modulation (..)
  , modItem
  , modName
  , V8Menu (..)
  , emptyMenu
  , describeMenu
  , commonModulation
    -- * Signals
  , SeqKind (..)
  , sequenceBits
  , cjBits
  , ciBits
    -- * Receiving
  , V8Rx
  , v8RxInit
  , V8Event (..)
  , v8RxBits
    -- * ANSam
  , ansamSignal
  , Ansam
  , ansamInit
  , ansamBlock
  ) where

import Data.Maybe (listToMaybe)
import Data.Bits (setBit, shiftL, testBit, (.&.))
import Data.List (foldl', intercalate, sort)
import Data.Word (Word8)
import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal)

-- | Call functions (Table 3), given by the three option bits b5-b7 of
-- the call function octet.  A data call is the only one this modem makes.
data CallFunction
  = CfMultimedia          -- ^ PSTN multimedia terminal (H.324)
  | CfTextphone           -- ^ textphone (V.18)
  | CfVideotext           -- ^ videotext (T.101)
  | CfFaxFromCaller       -- ^ transmit facsimile from the call terminal (T.30)
  | CfFaxToCaller         -- ^ receive facsimile at the call terminal (T.30)
  | CfData                -- ^ data, unspecified application
  deriving (Eq, Show, Enum, Bounded)

-- | (b5, b6, b7) for a call function.
callFunctionCode :: CallFunction -> (Bool, Bool, Bool)
callFunctionCode cf = case cf of
  CfMultimedia    -> (True,  False, False)
  CfTextphone     -> (False, True,  False)
  CfVideotext     -> (True,  True,  False)
  CfFaxFromCaller -> (False, False, True)
  CfFaxToCaller   -> (True,  False, True)
  CfData          -> (False, True,  True)

callFunctionOf :: (Bool, Bool, Bool) -> Maybe CallFunction
callFunctionOf c = listToMaybe [ f | f <- [minBound .. maxBound], callFunctionCode f == c ]

-- | Modulation modes (Table 4), in item-number order.  The item number
-- decides the outcome: "the indicated modulation category modulation
-- mode with the lowest item number shall be used" (7.4).
data Modulation
  = MV34Duplex | MV34Half | MV32 | MV22 | MV17 | MV29Half | MV27ter
  | MV26ter | MV26bis | MV23Duplex | MV23Half | MV21
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Item number from Table 4.  Item 0 is not a modulation: it flags the
-- presence of the PCM modem availability category.
modItem :: Modulation -> Int
modItem m = case m of
  MV34Duplex -> 1;  MV34Half -> 2;  MV32 -> 3;  MV22 -> 4
  MV17 -> 5;        MV29Half -> 6;  MV27ter -> 7
  MV26ter -> 8;     MV26bis -> 9;   MV23Duplex -> 10
  MV23Half -> 11;   MV21 -> 12

modName :: Modulation -> String
modName m = case m of
  MV34Duplex -> "V.34 duplex"; MV34Half -> "V.34 half-duplex"
  MV32 -> "V.32bis/V.32";      MV22 -> "V.22bis/V.22"
  MV17 -> "V.17";              MV29Half -> "V.29 half-duplex"
  MV27ter -> "V.27ter";        MV26ter -> "V.26ter"
  MV26bis -> "V.26bis";        MV23Duplex -> "V.23 duplex"
  MV23Half -> "V.23 half-duplex"; MV21 -> "V.21"

-- | Which octet of the modulation category a mode lives in (0, 1 or 2)
-- and which bit position within it.
modSlot :: Modulation -> (Int, Int)
modSlot m = case m of
  MV34Duplex -> (0, 6); MV34Half -> (0, 7)
  MV32 -> (1, 0); MV22 -> (1, 1); MV17 -> (1, 2); MV29Half -> (1, 6); MV27ter -> (1, 7)
  MV26ter -> (2, 0); MV26bis -> (2, 1); MV23Duplex -> (2, 2); MV23Half -> (2, 6); MV21 -> (2, 7)

-- | A decoded or to-be-encoded CM or JM.
data V8Menu = V8Menu
  { v8Call   :: Maybe CallFunction
  , v8Mods   :: [Modulation]
  , v8ModOctets :: Int                    -- ^ how many modulation octets were sent
  , v8Lapm   :: Bool                      -- ^ the protocol category asked for V.42 LAPM
  , v8Pcm    :: Maybe (Bool, Bool, Bool)  -- ^ V.90/V.92 analogue, digital, V.91
  , v8Access :: Maybe (Bool, Bool, Bool)  -- ^ call cellular, answer cellular, digital network
  , v8Octets :: [Word8]                   -- ^ every octet as received, for the record
  } deriving (Eq, Show)

emptyMenu :: V8Menu
emptyMenu = V8Menu Nothing [] 0 False Nothing Nothing []

-- | One line naming everything the menu claims.
describeMenu :: V8Menu -> String
describeMenu m = intercalate ", " (fn ++ mods ++ extras)
  where
    fn = case v8Call m of
      Just CfData -> ["data"]
      Just c -> [show c]
      Nothing -> ["no call function"]
    mods = case sort (v8Mods m) of
      [] -> ["no modulations"]
      ms -> map modName ms
    extras =
      [ "LAPM" | v8Lapm m ] ++
      [ pcmName a d n | Just (a, d, n) <- [v8Pcm m] ] ++
      [ "cellular (call)" | Just (True, _, _) <- [v8Access m] ] ++
      [ "cellular (answer)" | Just (_, True, _) <- [v8Access m] ] ++
      [ "digital network" | Just (_, _, True) <- [v8Access m] ]
    pcmName a d n = intercalate "+"
      ([ "V.90/V.92 analogue" | a ] ++ [ "V.90/V.92 digital" | d ] ++ [ "V.91" | n ])

-- | The mode both menus offer with the lowest item number (7.4).
commonModulation :: V8Menu -> V8Menu -> Maybe Modulation
commonModulation a b = listToMaybe [ m | m <- [minBound .. maxBound], m `elem` v8Mods a, m `elem` v8Mods b ]

-- | Which signal a sequence is; they differ only in the synchronization
-- bits (Table 1).
data SeqKind = SeqCI | SeqCM | SeqJM deriving (Eq, Show)

syncBits :: SeqKind -> [Bool]
syncBits SeqCI = map (== '1') "0000000001"
syncBits _     = map (== '1') "0000001111"

-- | Ten ONEs then the synchronization pattern.
preambleBits :: SeqKind -> [Bool]
preambleBits k = replicate 10 True ++ syncBits k

-- | An octet with its start and stop bits, b0 first.
frameOctet :: Word8 -> [Bool]
frameOctet w = False : [ testBit w i | i <- [0 .. 7] ] ++ [True]

octetOf :: [Bool] -> Word8
octetOf bs = foldl' (\w (i, b) -> if b then setBit w i else w) 0 (zip [0 :: Int ..] bs)

-- Category tags from Table 2, as the value of b0-b3 with b0 least
-- significant.
tagCall, tagMod, tagProt, tagAccess, tagPcm :: Word8
tagCall = 1; tagMod = 5; tagProt = 10; tagAccess = 13; tagPcm = 7

-- | A category octet: tag in b0-b3, ZERO in b4, option bits in b5-b7.
categoryOctet :: Word8 -> (Bool, Bool, Bool) -> Word8
categoryOctet tag (b5, b6, b7) =
  (tag .&. 0x0f) + bit 5 b5 + bit 6 b6 + bit 7 b7
  where bit i b = if b then 1 `shiftL` i else 0

-- | An extension octet: ZERO in b3, ONE in b4, ZERO in b5, five option
-- bits in b0-b2 and b6-b7.
extensionOctet :: (Bool, Bool, Bool, Bool, Bool) -> Word8
extensionOctet (b0, b1, b2, b6, b7) =
  bit 0 b0 + bit 1 b1 + bit 2 b2 + (1 `shiftL` 4) + bit 6 b6 + bit 7 b7
  where bit i b = if b then 1 `shiftL` i else 0

-- | The octets of a menu: call function first (5), then the modulation
-- octets, then whatever else was asked for.
menuOctets :: V8Menu -> [Word8]
menuOctets m = callO ++ modO ++ protO ++ accessO ++ pcmO
  where
    callO = case v8Call m of
      Nothing -> []
      Just cf -> [categoryOctet tagCall (callFunctionCode cf)]
    -- enough octets to carry the highest item offered, and never fewer
    -- than asked for: a JM saying "nothing in common" must still match
    -- the CM's octet count (8.2.3)
    need = maximum (v8ModOctets m : 1 : [ o + 1 | md <- v8Mods m, let (o, _) = modSlot md ])
    modO = [ octet i | i <- [0 .. need - 1] ]
    set i p = or [ True | md <- v8Mods m, modSlot md == (i, p) ]
    octet 0 = categoryOctet tagMod (v8Pcm m /= Nothing, set 0 6, set 0 7)
    octet i = extensionOctet (set i 0, set i 1, set i 2, set i 6, set i 7)
    protO = [ categoryOctet tagProt (True, False, False) | v8Lapm m ]
    accessO = [ categoryOctet tagAccess a | Just a <- [v8Access m] ]
    pcmO = [ categoryOctet tagPcm p | Just p <- [v8Pcm m] ]

-- | One whole CM or JM sequence: preamble then framed octets.
sequenceBits :: SeqKind -> V8Menu -> [Bool]
sequenceBits k m = preambleBits k ++ concatMap frameOctet (menuOctets m)

-- | A CI sequence: preamble and the call function octet alone (7.1).
ciBits :: CallFunction -> [Bool]
ciBits cf = preambleBits SeqCI ++ frameOctet (categoryOctet tagCall (callFunctionCode cf))

-- | CJ: three consecutive octets of all ZEROs with start and stop bits
-- (3.5).
cjBits :: [Bool]
cjBits = concat (replicate 3 (frameOctet 0))

-- | Receiver for a V.21 channel carrying V.8 signals.
--
-- It hunts for a preamble, then reads framed octets until the framing
-- stops holding, which is what the ten ONEs of the next sequence look
-- like from here.  CJ is recognised directly from the octets rather than
-- as a menu, since it has no preamble of its own.
data V8Rx = V8Rx
  { vrHist  :: ![Bool]        -- ^ recent bits, newest first, for preamble hunting
  , vrIn    :: !(Maybe (SeqKind, [Word8], [Bool]))  -- ^ sequence being read
  , vrZeros :: !Int           -- ^ consecutive all-zero octets seen (CJ is three)
  }

v8RxInit :: V8Rx
v8RxInit = V8Rx [] Nothing 0

data V8Event
  = V8Sequence SeqKind V8Menu     -- ^ a complete CM or JM
  | V8CJ                          -- ^ three zero octets: the caller is done
  deriving (Eq, Show)

-- | Feed demodulated bits in, get whole signals out.
v8RxBits :: V8Rx -> [Bool] -> (V8Rx, [V8Event])
v8RxBits st0 bs = let (st', evs) = foldl' step (st0, []) bs in (st', reverse evs)
  where
    step (st, acc) b = case vrIn st of
      Nothing ->
        let h = take 20 (b : vrHist st)
        in case matchPreamble h of
             Just k -> (st { vrHist = [], vrIn = Just (k, [], []) }, acc)
             Nothing -> (st { vrHist = h }, acc)
      Just (k, os, partial) ->
        let bits = partial ++ [b]
        in if length bits < 10
             then (st { vrIn = Just (k, os, bits) }, acc)
             else
               let start = head bits
                   stop = last bits
                   o = octetOf (take 8 (drop 1 bits))
               in if start || not stop
                    -- framing lost: the sequence has ended.  Re-examine
                    -- these bits for a preamble rather than dropping
                    -- them, because the next sequence starts here.
                    then let st1 = st { vrIn = Nothing, vrHist = reverse bits, vrZeros = 0 }
                         in (st1, emit k os acc)
                    else
                      let z = if o == 0 then vrZeros st + 1 else 0
                          acc' = if z == 3 then V8CJ : acc else acc
                      in (st { vrIn = Just (k, os ++ [o], []), vrZeros = z }, acc')
    emit k os acc
      | null os = acc
      | otherwise = V8Sequence k (decodeOctets os) : acc
    matchPreamble h
      | length h < 20 = Nothing
      | otherwise =
          let seen = reverse h                 -- oldest first
              ones = take 10 seen
              sync = drop 10 seen
          in if and ones
               then if sync == syncBits SeqCM then Just SeqCM
                    else if sync == syncBits SeqCI then Just SeqCI
                    else Nothing
               else Nothing

-- | Turn a sequence's octets into a menu.  Extension octets belong to
-- the category octet they follow (5.2); anything reserved is ignored, as
-- a receiver is required to do (6).
decodeOctets :: [Word8] -> V8Menu
decodeOctets os = go os Nothing 0 emptyMenu { v8Octets = os }
  where
    go [] _ _ m = m
    go (o : rest) cat idx m
      | not (testBit o 4) =                       -- category octet
          let tag = o .&. 0x0f
              opts = (testBit o 5, testBit o 6, testBit o 7)
              m' | tag == tagCall = m { v8Call = callFunctionOf opts }
                 | tag == tagMod = (modBits 0 o m) { v8ModOctets = 1 }
                 | tag == tagProt = m { v8Lapm = opts == (True, False, False) }
                 | tag == tagAccess = m { v8Access = Just opts }
                 | tag == tagPcm = m { v8Pcm = Just opts }
                 | otherwise = m
          in go rest (Just tag) 1 m'
      | otherwise = case cat of                   -- extension octet
          Just tag | tag == tagMod ->
            go rest cat (idx + 1) ((modBits idx o m) { v8ModOctets = idx + 1 })
          _ -> go rest cat (idx + 1) m
    modBits i o m = m { v8Mods = v8Mods m ++
      [ md | md <- [minBound .. maxBound]
           , let (oi, p) = modSlot md, oi == i, testBit o p ] }

-- | An ANSam signal: 2100 Hz, amplitude modulated by 15 Hz so the
-- envelope runs between 0.8 and 1.2 of average, with a phase reversal
-- every 450 ms (7.2).  Phase reversals are only for disabling network
-- echo cancellers; over a VoIP trunk there are none to disable, so they
-- are optional here.
ansamSignal :: Double -> Double -> Double -> Bool -> Signal
ansamSignal fs amp secs reversals = VS.generate n sample
  where
    n = round (fs * secs)
    sample i =
      let t = fromIntegral i / fs
          env = 1 + 0.2 * sin (2 * pi * 15 * t)
          flips = if reversals then fromIntegral (floor (t / 0.45) :: Int) * pi else 0
      in amp * env * sin (2 * pi * 2100 * t + flips)

-- | Detector for ANSam.
--
-- A plain answer tone and ANSam are the same 2100 Hz to a tone bank, so
-- the only thing that separates them is the 15 Hz on the envelope.  The
-- signal is mixed down to baseband around 2100 Hz, low-pass filtered to
-- keep the carrier and its two sidebands, and the envelope's 15 Hz
-- component is compared with its mean over a window long enough to
-- resolve it.  A ratio near 0.2 is ANSam; a bare tone gives almost zero.
data Ansam = Ansam
  { asPhase :: !Double
  , asI     :: !Double        -- ^ low-passed in-phase
  , asQ     :: !Double
  , asSumE  :: !Double        -- ^ envelope accumulators for this window
  , asCos   :: !Double
  , asSin   :: !Double
  , asSumSq :: !Double        -- ^ total signal power, to check 2100 Hz dominates
  , asCount :: !Int
  , asFs    :: !Double
  }

ansamInit :: Double -> Ansam
ansamInit fs = Ansam 0 0 0 0 0 0 0 0 fs

-- | Feed a block; 'True' means ANSam was confirmed in a window that
-- ended inside this block.
ansamBlock :: Ansam -> Signal -> (Ansam, Bool)
ansamBlock st0 xs = VS.foldl' step (st0, False) xs
  where
    fs = asFs st0
    -- one-pole low-pass at about 40 Hz keeps the 2100 Hz carrier and the
    -- 15 Hz sidebands and rejects everything else in the voice band
    k = 1 - exp (-2 * pi * 40 / fs)
    -- 0.4 s of envelope: three bin widths of separation at 15 Hz
    win = round (0.4 * fs) :: Int
    step (st, hit) x =
      let ph = asPhase st + 2 * pi * 2100 / fs
          ph' = if ph > 2 * pi then ph - 2 * pi else ph
          i' = asI st + k * (x * cos ph - asI st)
          q' = asQ st + k * (negate x * sin ph - asQ st)
          env = sqrt (i' * i' + q' * q')
          tw = fromIntegral (asCount st) / fs
          st1 = st { asPhase = ph', asI = i', asQ = q'
                   , asSumE = asSumE st + env
                   , asCos = asCos st + env * cos (2 * pi * 15 * tw)
                   , asSin = asSin st + env * sin (2 * pi * 15 * tw)
                   , asSumSq = asSumSq st + x * x
                   , asCount = asCount st + 1 }
      in if asCount st1 < win
           then (st1, hit)
           else
             let n = fromIntegral win
                 mean = asSumE st1 / n
                 depth = 2 * sqrt (asCos st1 ** 2 + asSin st1 ** 2) / n / max 1e-9 mean
                 -- the envelope of anything carries some 15 Hz, so this
                 -- only means ANSam when 2100 Hz is what is on the line:
                 -- a modem's data carrier is rejected by the filter and
                 -- leaves the envelope far too small for its power
                 -- a quadrature mix halves the amplitude, so the tone
                 -- on the line is twice the envelope
                 carrier = 2 * mean * mean
                 total = asSumSq st1 / n
                 -- 0.2 nominal; allow for a filtered, noisy trunk, but
                 -- stay well clear of the near-zero a bare tone gives
                 ok = mean > 5e-3 && depth > 0.08 && depth < 0.6
                      && carrier > 0.5 * total
             in (st1 { asSumE = 0, asCos = 0, asSin = 0, asSumSq = 0, asCount = 0 }, hit || ok)
