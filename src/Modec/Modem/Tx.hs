{-# LANGUAGE BangPatterns #-}
-- | The transmitter: phase-continuous tones, FSK with a byte queue and
-- per-channel band limiting, the V.8 modified answer tone, and the V.22
-- modulator behind a queue of its own.
--
-- Pure DSP, and it was living inside the modem's state machine, which
-- is a different kind of thing.  Nothing here knows what phase the call
-- is in: it is handed a 'TxCmd' and a block length and makes samples.
module Modec.Modem.Tx
  ( TxLine (..)
  , TxState
  , txInit
  , txBlock
  , txPending
  , txOctetsPending
    -- * What is waiting to go out
  , txQueueOctets
  , txQueueBits
  , txClearSync
  , txDrop
  , txLineBits
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.DSP (Signal, firStream, wrapTwoPi)
import Modec.FSK (frameBits, txFilterKernel)
import Modec.Handshake (TxCmd (..))
import Modec.Standards
import Modec.V22 (TxMode (..), V22TxState, v22TxBlock, v22TxInit, v22TxQueued, withBits)

-- | What the transmitter needs to know about the line it is driving,
-- which does not change for the life of a call.  'txBlock' used to take
-- these as four leading positional arguments, unpacked from the modem
-- configuration at each of its two call sites.
data TxLine = TxLine
  { tlFs      :: !Double   -- ^ sample rate
  , tlAmp     :: !Double   -- ^ transmit amplitude
  , tlFraming :: !Framing  -- ^ how a byte is framed as a character
  , tlGuard   :: !Bool     -- ^ V.22 guard tone on the high channel
  }

-- | Streaming transmitter: phase-continuous tones, FSK with a byte
-- queue (band limited per channel), and the V.22 modulator.
data TxState = TxState
  { txPhase   :: !Double
  , txBitPos  :: !Double
  , txCurBit  :: !Bool
  , txBits    :: [Bool]
  , txQueue   :: [Word8]
  , txFir     :: Maybe (String, VS.Vector Double, Signal)   -- ^ spec name, reversed kernel, history
  , txSync    :: [Bool]             -- ^ synchronous line bits waiting (MNP framing mode 3)
  , txV22     :: V22TxState
  , txPhase2  :: !Double            -- ^ second tone phase (dual tones)
  }

txInit :: TxState
txInit = TxState 0 0 True [] [] Nothing [] v22TxInit 0

-- | Generate @n@ samples for a transmit command, consuming queued bytes
-- only in data modes.
txBlock :: TxLine -> TxCmd -> Int -> TxState -> (TxState, Signal)
txBlock (TxLine fs amp fr guard) cmd n st = case cmd of
  TxSilence -> (st { txFir = Nothing }, VS.replicate n 0)
  TxTone f ->
    let w = 2 * pi * f / fs
        sig = VS.generate n (\i -> amp * sin (txPhase st + w * fromIntegral i))
    in (st { txPhase = wrap (txPhase st + w * fromIntegral n), txFir = Nothing }, sig)
  TxMark spec -> fsk spec False
  TxData spec -> fsk spec True
  TxBits spec bits -> fskWith spec False bits
  -- ANSam: 2100 Hz with a 15 Hz envelope between 0.8 and 1.2 of average
  -- (7.2/V.8).  No phase reversals: those exist only to disable network
  -- echo cancellers, and the Recommendation says not to send them when
  -- that is not wanted.
  TxAnsam ->
    let w = 2 * pi * answerToneItu / fs
        wm = 2 * pi * 15 / fs
        sig = VS.generate n (\i ->
          let k = fromIntegral i
          in amp * (1 + 0.2 * sin (txPhase2 st + wm * k)) * sin (txPhase st + w * k))
    in (st { txPhase = wrap (txPhase st + w * fromIntegral n)
           , txPhase2 = wrap (txPhase2 st + wm * fromIntegral n)
           , txFir = Nothing }, sig)
  TxV22 ch rate mode ->
    let (bytes, st1) = case mode of
          TxScrambledData -> (txQueue st, st { txQueue = [] })
          _ -> ([], st)
        v0 = case mode of
          TxSyncData -> withBits (txV22 st1) (txSync st1)
          _ -> txV22 st1
        (v', sig) = v22TxBlock fs ch fr amp guard rate mode bytes n v0
        st2 = case mode of { TxSyncData -> st1 { txSync = [] }; _ -> st1 }
    in (st2 { txV22 = v', txFir = Nothing }, sig)
  where
    twoPi = 2 * pi
    wrap = wrapTwoPi
    -- queue raw bits before generating (used once per TxBits command)
    fskWith spec allowData bits = fskFrom (st { txBits = txBits st ++ bits }) spec allowData
    fsk spec allowData = fskFrom st spec allowData
    fskFrom st0 spec allowData =
      let st = st0 in
      let spb = fs / fskBaud spec
          step (!ph, !pos, !cur, bits, queue) =
            let (pos', cur', bits', queue')
                  | pos >= spb = nextBit (pos - spb) bits queue
                  | otherwise = (pos, cur, bits, queue)
                nextBit p bs q = case bs of
                  (b : rest) -> (p, b, rest, q)
                  [] | allowData, (byte : q') <- q, (b : rest) <- frameBits fr [byte] -> (p, b, rest, q')
                     | otherwise -> (p, True, [], q)
                f = if cur' then fskMark spec else fskSpace spec
                v = amp * sin ph
            in Just (v, (wrap (ph + twoPi * f / fs), pos' + 1, cur', bits', queue'))
          (raw, (ph1, pos1, cur1, bits1, queue1)) = unfoldExactN n step (txPhase st, txBitPos st, txCurBit st, txBits st, txQueue st)
          (hrev, hist) = case txFir st of
            Just (name, h, hs) | name == fskName spec -> (h, hs)
            _ -> let h = VS.reverse (txFilterKernel fs spec) in (h, VS.replicate (VS.length h - 1) 0)
          (out, hist') = firStream hrev hist raw
      in (st { txPhase = ph1, txBitPos = pos1, txCurBit = cur1, txBits = bits1, txQueue = queue1
             , txFir = Just (fskName spec, hrev, hist') }, out)

-- | Like 'VS.unfoldrN' but also returns the final state.
unfoldExactN :: Int -> (s -> Maybe (Double, s)) -> s -> (Signal, s)
unfoldExactN n f s0 = go 0 s0 []
  where
    go !i s acc
      | i >= n = (VS.fromListN n (reverse acc), s)
      | otherwise = case f s of
          Just (v, s') -> go (i + 1) s' (v : acc)
          Nothing -> (VS.fromListN n (reverse acc), s)

-- | Octets still waiting to go on the line, across both transmitters.
txPending :: TxState -> Int
txPending st =
  txOctetsPending st + (length (txSync st) + 7) `div` 8

-- | Bytes still waiting to be framed as start-stop characters.  The
-- framing switch waits on this: the acknowledgement that closes
-- establishment is octet framed, and a synchronous transmitter does not
-- drain the byte queue, so switching while it is still going out would
-- strand it on the way to a far end waiting for exactly that frame.
--
-- The bits of the character already being shifted out are deliberately
-- not counted.  The synchronous mode appends its frame bits behind them,
-- so the ordering holds either way -- and in that mode those bits /are/
-- the frames, so counting them would hold the switch off for ever and
-- leave the line filling between frames with mark instead of flags.
txOctetsPending :: TxState -> Int
txOctetsPending st = length (txQueue st) + v22TxQueued (txV22 st)

-- | Bytes for the line, to be framed as start-stop characters in a data
-- mode.  Queued, not sent: the transmitter drains them at the line's
-- own pace.
txQueueOctets :: [Word8] -> TxState -> TxState
txQueueOctets bs st = st { txQueue = txQueue st ++ bs }

-- | Line bits for the synchronous transmitter (MNP framing mode 3),
-- behind whatever is already waiting.
txQueueBits :: [Bool] -> TxState -> TxState
txQueueBits bs st = st { txSync = txSync st ++ bs }

-- | Forget any synchronous bits waiting.  The protocol layer does this
-- when it hands over octets instead: the two queues are not meant to
-- interleave.
txClearSync :: TxState -> TxState
txClearSync st = st { txSync = [] }

-- | Forget everything waiting, both queues.  What a retrain does with
-- what the DTE typed while the line was being renegotiated.
txDrop :: TxState -> TxState
txDrop st = st { txQueue = [], txSync = [] }

-- | Everything waiting, as bits for a synchronous line: the octet queue
-- framed as start-stop characters, then the synchronous bits behind it.
-- This is what the V.32 pump takes, which makes its own audio and so
-- never sees a 'TxCmd'.
txLineBits :: Framing -> TxState -> [Bool]
txLineBits fr st = concatMap (\b -> frameBits fr [b]) (txQueue st) ++ txSync st
