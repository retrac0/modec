-- | The 5-bit text telephone character code (ITU-T V.18 Annex A, whose
-- normative definition since Amendment 1 is ANSI/TIA-825).
--
-- Two 32-entry alphabets share the code space, and a shift character
-- selects between them: LTRS (11111) for letters, FIGS (11011) for
-- digits and punctuation.  Everything here is the character layer only
-- -- the line, its tones and its keying live in "Modec.FSK".
--
-- The table is US-TTY ITA2 with four changes, and these four positions
-- are exactly where a decoder ported from an RTTY program produces
-- garbage: 00101 (S) has no figure at all rather than BELL, 11010 (G)
-- is @+@ rather than @&@, 10100 (H) is @=@ rather than @#@, and 00000
-- is BACKSPACE in both shifts rather than NUL.
--
-- Shift discipline (A.4), all of it load bearing on a real line:
--
-- * both ends start in LTRS, and the transmitter sends LTRS before the
--   first translated character;
-- * a shift goes out whenever the alphabet changes;
-- * and a shift goes out every 72 characters even when the alphabet has
--   /not/ changed.  A lost shift character otherwise corrupts every
--   character after it until the next alphabet change, which on a line
--   of digits can be the rest of the call; the periodic reshift is the
--   only thing that bounds that.  Asterisk implements it as
--   @charnum % 72 == 0@ over data characters, counted independently of
--   the shifts an alphabet change emits, and this module matches it.
--
-- There is deliberately no unshift-on-space.  That is the RTTY
-- convention, it appears in neither V.18 nor TIA-825, and neither
-- Asterisk nor spandsp does it -- but minimodem's @tdd@ mode enables it
-- by default, so a cross-check against minimodem has to pass @-u 0@ or
-- every figure after a space comes back as a letter.
module Modec.Baudot
  ( Shift (..)
  , ltrsCode
  , figsCode
    -- * Tables
  , baudotTable
  , baudotChar
  , baudotCode
  , foldToBaudot
  , reshiftEvery
    -- * Transmit
  , BaudotTx (..)
  , baudotTxInit
  , baudotEncode
    -- * Receive
  , BaudotRx (..)
  , baudotRxInit
  , baudotDecode
  ) where

import Data.Bits ((.&.))
import Data.Char (toUpper)
import Data.Word (Word8)

data Shift = Ltrs | Figs deriving (Eq, Show)

ltrsCode, figsCode :: Word8
ltrsCode = 0x1F
figsCode = 0x1B

-- | How often the shift in force is re-sent even though it has not
-- changed (V.18 A.4).
reshiftEvery :: Int
reshiftEvery = 72

-- | V.18 Table A.1: code, letters case, figures case.  The code is
-- written as the integer whose least significant bit is code element 1,
-- which is the element transmitted first.  'Nothing' is a position that
-- carries no character: the two shifts themselves, and the unassigned
-- figure at 00101.
baudotTable :: [(Word8, Maybe Char, Maybe Char)]
baudotTable =
  [ (0x00, Just '\b', Just '\b')   -- BACKSPACE both shifts; ITA2 has NUL here
  , (0x01, Just 'E',  Just '3')
  , (0x02, Just '\n', Just '\n')
  , (0x03, Just 'A',  Just '-')
  , (0x04, Just ' ',  Just ' ')
  , (0x05, Just 'S',  Nothing)     -- US TTY has BELL here, V.18 nothing
  , (0x06, Just 'I',  Just '8')
  , (0x07, Just 'U',  Just '7')
  , (0x08, Just '\r', Just '\r')
  , (0x09, Just 'D',  Just '$')
  , (0x0A, Just 'R',  Just '4')
  , (0x0B, Just 'J',  Just '\'')
  , (0x0C, Just 'N',  Just ',')
  , (0x0D, Just 'F',  Just '!')
  , (0x0E, Just 'C',  Just ':')
  , (0x0F, Just 'K',  Just '(')
  , (0x10, Just 'T',  Just '5')
  , (0x11, Just 'Z',  Just '"')
  , (0x12, Just 'L',  Just ')')
  , (0x13, Just 'W',  Just '2')
  , (0x14, Just 'H',  Just '=')    -- US TTY has '#'
  , (0x15, Just 'Y',  Just '6')
  , (0x16, Just 'P',  Just '0')
  , (0x17, Just 'Q',  Just '1')
  , (0x18, Just 'O',  Just '9')
  , (0x19, Just 'B',  Just '?')
  , (0x1A, Just 'G',  Just '+')    -- US TTY has '&'
  , (0x1B, Nothing,   Nothing)     -- FIGS
  , (0x1C, Just 'M',  Just '.')
  , (0x1D, Just 'X',  Just '/')
  , (0x1E, Just 'V',  Just ';')
  , (0x1F, Nothing,   Nothing)     -- LTRS
  ]

-- | The character a code stands for in a shift, if any.
baudotChar :: Shift -> Word8 -> Maybe Char
baudotChar sh c = case [ e | e@(k, _, _) <- baudotTable, k == c ] of
  ((_, l, f) : _) -> case sh of { Ltrs -> l; Figs -> f }
  [] -> Nothing

-- | The code for a character, and the shift it has to be sent in.
-- 'Nothing' for the shift means the character is in both alphabets, so
-- it costs nothing and disturbs nothing wherever it is sent.
baudotCode :: Char -> Maybe (Word8, Maybe Shift)
baudotCode ch = case [ (k, l, f) | (k, l, f) <- baudotTable, l == Just ch || f == Just ch ] of
  ((k, l, f) : _)
    | l == Just ch && f == Just ch -> Just (k, Nothing)
    | l == Just ch -> Just (k, Just Ltrs)
    | otherwise -> Just (k, Just Figs)
  [] -> Nothing

-- | V.18 Table A.2: fold a T.50 character onto one the 5-bit alphabet
-- can carry, or drop it.  Everything outside the alphabet would
-- otherwise be silently lost, and a wrong-looking character reads
-- better than a missing one.
foldToBaudot :: Char -> Maybe Char
foldToBaudot c = case toUpper c of
  '#' -> Just '$'
  '%' -> Just '/'
  '&' -> Just '+'
  '*' -> Just '.'
  '<' -> Just '('
  '>' -> Just ')'
  '[' -> Just '('
  ']' -> Just ')'
  '{' -> Just '('
  '}' -> Just ')'
  '\\' -> Just '/'
  '^' -> Just '\''
  '`' -> Just '\''
  '_' -> Just ' '
  '~' -> Just ' '
  '@' -> Just 'X'
  '\t' -> Just ' '
  '\v' -> Just '\n'
  '\f' -> Just '\n'
  '\SUB' -> Just '?'
  u | u `elem` ['\FS', '\GS', '\RS'] -> Just '\n'
    | otherwise -> if any (\(_, l, f) -> l == Just u || f == Just u) baudotTable
                     then Just u
                     else Nothing

-- | Shift in force at the far end, and characters sent since a shift
-- last went out.  The count starts at zero so that the first character
-- of a call is preceded by LTRS, which is the same rule as the periodic
-- reshift and needs no separate case.
data BaudotTx = BaudotTx
  { btShift :: !Shift
  , btSince :: !Int
  } deriving (Eq, Show)

baudotTxInit :: BaudotTx
baudotTxInit = BaudotTx Ltrs 0

-- | T.50 bytes in, 5-bit codes out.
--
-- T.50 DEL from the keyboard is not a character but a resynchronisation
-- request: it puts the receiving translator back into LTRS (A.4), which
-- is what a user who is looking at a screenful of figures presses.
baudotEncode :: BaudotTx -> [Word8] -> (BaudotTx, [Word8])
baudotEncode st0 = go st0 []
  where
    go st acc [] = (st, reverse acc)
    go st acc (b : bs)
      | b == 0x7F = go (BaudotTx Ltrs 0) (ltrsCode : acc) bs
      | otherwise = case foldToBaudot (toEnum (fromIntegral b)) >>= baudotCode of
          Nothing -> go st acc bs
          Just (code, needed) ->
            let (st1, pre) = shiftFor st needed
            in go st1 { btSince = btSince st1 + 1 } (code : pre ++ acc) bs
    -- the shift characters that have to precede this one: an alphabet
    -- change, or the periodic reshift, never both
    shiftFor st needed = case needed of
      Just sh | sh /= btShift st -> (BaudotTx sh 0, [codeOf sh])
      _ | btSince st `mod` reshiftEvery == 0 -> (st { btSince = 0 }, [codeOf (btShift st)])
        | otherwise -> (st, [])
    codeOf Ltrs = ltrsCode
    codeOf Figs = figsCode

-- | Shift the incoming codes are being read in.  It starts in LTRS
-- because the far end is required to start there too.
newtype BaudotRx = BaudotRx { brShift :: Shift } deriving (Eq, Show)

baudotRxInit :: BaudotRx
baudotRxInit = BaudotRx Ltrs

-- | 5-bit codes in, T.50 bytes out.  A shift while already in that
-- shift is a no-op rather than a character, which is what makes the
-- periodic reshift free.
baudotDecode :: BaudotRx -> [Word8] -> (BaudotRx, [Word8])
baudotDecode st0 = go st0 []
  where
    go st acc [] = (st, reverse acc)
    go st acc (c0 : cs)
      | c == ltrsCode = go (BaudotRx Ltrs) acc cs
      | c == figsCode = go (BaudotRx Figs) acc cs
      | otherwise = case baudotChar (brShift st) c of
          Just ch -> go st (fromIntegral (fromEnum ch) : acc) cs
          Nothing -> go st acc cs
      where c = c0 .&. 0x1F   -- the deframer delivers five bits; be defensive
