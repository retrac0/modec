{-# LANGUAGE OverloadedStrings #-}
-- | The DTE side of a voice-mode modem: ITU-T V.253, née TIA IS-101.
--
-- A Conexant-class USB modem with @AT+FCLASS=8@ is a telephone-line
-- sound card.  Samples go both ways over the serial stream, in
-- whichever format @+VSM@ selected, and the stream carries two things
-- besides samples: events the modem noticed on the line (a ring, a
-- busy tone, its own buffer running dry) and the end of the stream
-- itself.  Both are carried in band, shielded by @<DLE>@ (0x10): a
-- sample byte of 0x10 is sent twice, @<DLE><ETX>@ ends the stream, and
-- @<DLE>@ followed by anything else is an event.
--
-- The shielding is on bytes, not samples.  In a two-byte format a 0x10
-- can be either half of a sample, so this runs before bytes are paired
-- into samples, and an event is removed as a whole two-byte unit so the
-- payload's byte parity is what the modem sent.  That is the one thing
-- here that would be easy to get wrong, and the reason this module has
-- a test that splits the stream at every byte.
--
-- The set-up dialogue is data rather than code, because the order and
-- the exact spelling are what bring-up on real hardware adjusts.
module Modec.Voice
  ( -- * DLE shielding
    DleState
  , dleInit
  , Dle (..)
  , dleDecode
  , dleEncode
  , dleEtx
  , dleLeaveDuplex
  , describeVoiceEvent
    -- * The AT dialogue
  , AtStep (..)
  , vsmCode
  , voiceSetup
  , voiceTeardown
  , atFinal
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import Modec.Sample (SampleFormat (..))
import Modec.Standards (Role (..))

dle, etx :: Word8
dle = 0x10
etx = 0x03

-- | Whether the last byte of the previous block was an unpaired @<DLE>@.
newtype DleState = DleState Bool

dleInit :: DleState
dleInit = DleState False

-- | What one block of the stream held.
data Dle = Dle
  { dlPayload :: B.ByteString   -- ^ sample bytes, unshielded
  , dlEvents  :: [Word8]        -- ^ event codes, in order
  , dlRest    :: Maybe B.ByteString
    -- ^ 'Just' what followed @<DLE><ETX>@: the stream has ended and the
    -- modem is back to talking in result codes, which these are the
    -- start of.
  } deriving (Eq, Show)

-- | Unshield a block, carrying a trailing @<DLE>@ over to the next.
dleDecode :: DleState -> B.ByteString -> (DleState, Dle)
dleDecode (DleState pending) bs = go pending 0 mempty []
  where
    n = B.length bs
    go p i out evs
      | i >= n = (DleState p, Dle (build out) (reverse evs) Nothing)
      | p =
          let c = B.index bs i
          in if c == dle then go False (i + 1) (out <> BB.word8 dle) evs
             else if c == etx then (DleState False, Dle (build out) (reverse evs) (Just (B.drop (i + 1) bs)))
             else go False (i + 1) out (c : evs)
      | otherwise =
          -- the run up to the next DLE is payload as it stands
          let (plain, rest) = B.break (== dle) (B.drop i bs)
              i' = i + B.length plain
          in if B.null rest then go False n (out <> BB.byteString plain) evs
             else go True (i' + 1) (out <> BB.byteString plain) evs
    build = BL.toStrict . BB.toLazyByteString

-- | Shield a block of sample bytes for the modem.
dleEncode :: B.ByteString -> B.ByteString
dleEncode bs
  | B.notElem dle bs = bs
  | otherwise = B.concatMap (\c -> if c == dle then B.pack [dle, dle] else B.singleton c) bs

-- | End of the DTE's stream.
dleEtx :: B.ByteString
dleEtx = B.pack [dle, etx]

-- | Leave the duplex state, from the DTE side: @<DLE>^@.
dleLeaveDuplex :: B.ByteString
dleLeaveDuplex = B.pack [dle, 0x5E]

-- | An event code, in words.  V.253 Table 14 has more; these are the
-- ones a modem call meets.
describeVoiceEvent :: Word8 -> String
describeVoiceEvent c = case toEnum (fromIntegral c) :: Char of
  'b' -> "busy tone"
  'd' -> "dial tone"
  'r' -> "ringback"
  'R' -> "ring"
  's' -> "silence after voice"
  'q' -> "quiet"
  'c' -> "fax calling tone"
  'e' -> "data calling tone"
  'a' -> "answer tone"
  'h' -> "local handset on hook"
  'H' -> "local handset off hook"
  'l' -> "loop current interrupted"
  'L' -> "loop current reversed"
  'u' -> "buffer underrun"
  'o' -> "buffer overrun"
  'T' -> "timing mark"
  'i' -> "invalid voice format"
  '/' -> "DTMF starts"
  '~' -> "DTMF ends"
  ch | ch `elem` ("0123456789*#ABCD" :: String) -> "DTMF " ++ [ch]
  _ -> "event 0x" ++ hex c
  where
    hex v = [digit (v `div` 16), digit (v `mod` 16)]
    digit d = "0123456789abcdef" !! fromIntegral d

-- | One step of a dialogue with the modem.
data AtStep
  = Send B.ByteString [B.ByteString]
    -- ^ a command (without the CR) and the final results that count as
    -- success
  | WaitFor B.ByteString
    -- ^ an unsolicited result to wait for before going on
  deriving (Eq, Show)

-- | The @+VSM@ compression method for a format, where the modem has
-- one.  The numbers are the ones a CX93010 reports to @+VSM=?@.
vsmCode :: SampleFormat -> Maybe Int
vsmCode f = case f of
  S8 -> Just 0
  U8 -> Just 1
  Ulaw -> Just 131
  Alaw -> Just 132
  Pcm14 -> Just 133
  _ -> Nothing

-- | From a modem that has just answered @AT@ to one streaming samples.
--
-- Silence detection and the inactivity timer both end the stream on
-- their own, which a handshake's pauses would trigger.  @+VPR=0@ is
-- autobaud, which over CDC-ACM means nothing is renegotiated.  The
-- answering side goes off hook only once the line has rung.
voiceSetup :: Int -> Role -> [AtStep]
voiceSetup code role =
  [ Send "AT" ["OK"]
  , Send "AT+FCLASS=8" ["OK"]
  , Send (BC.pack ("AT+VSM=" ++ show code ++ ",8000")) ["OK"]
  , Send "AT+VSD=0,0" ["OK"]
  , Send "AT+VIT=0" ["OK"]
  , Send "AT+VPR=0" ["OK"]
  ]
  ++ [ WaitFor "RING" | role == Answer ]
  ++ [ Send "AT+VLS=1" ["OK", "VCON"]
     , Send "AT+VTR" ["VCON", "CONNECT"]
     ]

-- | Back on hook.  The stream is left first, with 'dleLeaveDuplex'.
voiceTeardown :: [AtStep]
voiceTeardown =
  [ Send "AT+VLS=0" ["OK", "VCON"]
  , Send "ATH" ["OK"]
  ]

-- | A line from the modem that is a final result, stripped of its
-- line ends; anything else is information text or echo.
atFinal :: B.ByteString -> Maybe B.ByteString
atFinal raw
  | line `elem` finals = Just line
  | "CONNECT" `B.isPrefixOf` line = Just "CONNECT"
  | otherwise = Nothing
  where
    line = BC.strip raw
    finals = ["OK", "ERROR", "VCON", "NO CARRIER", "BUSY", "NO DIALTONE", "NO ANSWER", "RING"]
