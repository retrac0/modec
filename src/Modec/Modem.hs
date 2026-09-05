{-# LANGUAGE BangPatterns #-}
-- | The complete modem as a pure, block-driven state machine: call
-- establishment (see "Modec.Handshake") followed by full-duplex data
-- transfer over the negotiated FSK standard.  The executable feeds it
-- audio blocks and bytes; the tests connect two of them through the
-- channel simulator.
module Modec.Modem
  ( ModemConfig (..)
  , defaultModemConfig
  , ModemState
  , ModemEvent (..)
  , modemInit
  , modemStep
  , modemStatus
  , modemConnected
    -- * Transmitter
  , TxState
  , txInit
  , txBlock
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.Detect
import Modec.DSP
import Modec.FSK
import Modec.Handshake
import Modec.Standards
import Modec.Stream

data ModemConfig = ModemConfig
  { mcRate      :: Double
  , mcHandshake :: HsConfig
  , mcNoHandshake :: Bool        -- ^ skip call establishment; requires a fixed standard
  , mcDemod     :: DemodParams
  , mcFraming   :: Framing
  , mcTxAmp     :: Double
  , mcSettle    :: Double        -- ^ seconds of idle mark after CONNECT before data flows
  } deriving (Show)

defaultModemConfig :: Double -> Role -> Maybe Standard -> ModemConfig
defaultModemConfig fs role std = ModemConfig
  { mcRate = fs
  , mcHandshake = (defaultHsConfig role) { hcStandard = std }
  , mcNoHandshake = False
  , mcDemod = defaultDemodParams
  , mcFraming = framing8N1
  , mcTxAmp = 0.5
  , mcSettle = 0.3
  }

data ModemEvent
  = EvConnected Standard FskSpec FskSpec   -- ^ standard, our tx spec, our rx spec
  | EvDropped
  | EvFailed String
  deriving (Eq, Show)

-- | Streaming transmitter: phase-continuous tones and FSK with a byte
-- queue, band limited per channel.
data TxState = TxState
  { txPhase   :: !Double
  , txBitPos  :: !Double
  , txCurBit  :: !Bool
  , txBits    :: [Bool]
  , txQueue   :: [Word8]
  , txFir     :: Maybe (String, VS.Vector Double, Signal)   -- ^ spec name, reversed kernel, history
  }

txInit :: TxState
txInit = TxState 0 0 True [] [] Nothing

-- | Generate @n@ samples for a transmit command, consuming queued bytes
-- only in data mode.
txBlock :: Double -> Double -> Framing -> TxCmd -> Int -> TxState -> (TxState, Signal)
txBlock fs amp fr cmd n st = case cmd of
  TxSilence -> (st { txFir = Nothing }, VS.replicate n 0)
  TxTone f ->
    let w = 2 * pi * f / fs
        sig = VS.generate n (\i -> amp * sin (txPhase st + w * fromIntegral i))
    in (st { txPhase = wrap (txPhase st + w * fromIntegral n), txFir = Nothing }, sig)
  TxMark spec -> fsk spec False
  TxData spec -> fsk spec True
  where
    twoPi = 2 * pi
    wrap p = p - twoPi * fromIntegral (floor (p / twoPi) :: Int)
    fsk spec allowData =
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
      in (TxState ph1 pos1 cur1 bits1 queue1 (Just (fskName spec, hrev, hist')), out)

-- | Like 'VS.unfoldrN' but also returns the final state.
unfoldExactN :: Int -> (s -> Maybe (Double, s)) -> s -> (Signal, s)
unfoldExactN n f s0 = go 0 s0 []
  where
    go !i s acc
      | i >= n = (VS.fromListN n (reverse acc), s)
      | otherwise = case f s of
          Just (v, s') -> go (i + 1) s' (v : acc)
          Nothing -> (VS.fromListN n (reverse acc), s)

data Mode
  = Handshaking
  | Data Standard FskSpec FskSpec (Stage Signal Discriminated) (Stage Discriminated [Word8])
  | Finished

data ModemState = ModemState
  { msMode    :: Mode
  , msBank    :: Stage Signal [ToneFrame]
  , msHs      :: HsState
  , msTx      :: TxState
  , msTxCmd   :: TxCmd
  , msLost    :: !Double     -- ^ seconds of missing carrier in data mode
  , msSettled :: !Double     -- ^ seconds spent in data mode so far
  , msStatus  :: HsStatus
  }

modemInit :: ModemConfig -> ModemState
modemInit cfg
  | mcNoHandshake cfg, Just s <- hcStandard hs =
      let (tx, rx) = specsFor (hcRole hs) s
      in base { msMode = dataMode s tx rx, msTxCmd = TxData tx, msStatus = HsConnected s tx rx }
  | otherwise = base
  where
    hs = mcHandshake cfg
    fs = mcRate cfg
    base = ModemState Handshaking (toneBank fs (hcBank hs)) (initialHandshake hs) txInit TxSilence 0 0 HsBusy
    dataMode s tx rx = Data s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))

specsFor :: Role -> Standard -> (FskSpec, FskSpec)
specsFor Originate Bell103 = (bell103Originate, bell103Answer)
specsFor Answer Bell103 = (bell103Answer, bell103Originate)
specsFor Originate V21 = (v21Channel1, v21Channel2)
specsFor Answer V21 = (v21Channel2, v21Channel1)

modemStatus :: ModemState -> HsStatus
modemStatus = msStatus

modemConnected :: ModemState -> Bool
modemConnected st = case msMode st of
  Data {} -> True
  _ -> False

-- | Process one block of received audio and newly queued bytes.  Returns
-- the audio to transmit (same length), received bytes and events.
modemStep :: ModemConfig -> ModemState -> Signal -> [Word8] -> (ModemState, Signal, [Word8], [ModemEvent])
modemStep cfg st0 rxBlock newBytes =
  let st = st0 { msTx = (msTx st0) { txQueue = txQueue (msTx st0) ++ newBytes } }
      fs = mcRate cfg
      n = VS.length rxBlock
      blockSec = fromIntegral n / fs
      hs = mcHandshake cfg
  in case msMode st of
    Finished ->
      (st, VS.replicate n 0, [], [])
    Handshaking ->
      let (bank', frames) = stepStage (msBank st) rxBlock
          (hsState', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep hs h fr in (h', acc ++ [o])) (msHs st, []) frames
          (cmd, status) = if null outs then (msTxCmd st, HsBusy) else last outs
          st1 = st { msBank = bank', msHs = hsState', msTxCmd = cmd }
      in case status of
           HsConnected s tx rx ->
             let disc = fskDiscriminator fs rx (mcDemod cfg)
                 framer = fskDeframer fs rx (mcFraming cfg) (mcDemod cfg)
                 st2 = st1 { msMode = Data s tx rx disc framer, msTxCmd = TxData tx, msStatus = status, msSettled = 0 }
                 (txSt, audio) = txBlock fs (mcTxAmp cfg) (mcFraming cfg) (TxMark tx) n (msTx st2)
             in (st2 { msTx = txSt }, audio, [], [EvConnected s tx rx])
           HsFailed why ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], [EvFailed why])
           HsDropped ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], [EvDropped])
           HsBusy ->
             let (txSt, audio) = txBlock fs (mcTxAmp cfg) (mcFraming cfg) cmd n (msTx st1)
             in (st1 { msTx = txSt }, audio, [], [])
    Data s tx rx disc framer ->
      let (disc', d) = stepStage disc rxBlock
          (framer', bytes) = stepStage framer d
          presence = if n == 0 then 1 else VS.sum (dPresent d) / fromIntegral n
          lost = if presence < 0.5 then msLost st + blockSec else 0
          settled = msSettled st + blockSec
          -- idle mark for a moment after CONNECT so both receivers can settle
          cmd = if settled < mcSettle cfg then TxMark tx else TxData tx
          (txSt, audio) = txBlock fs (mcTxAmp cfg) (mcFraming cfg) cmd n (msTx st)
          st1 = st { msMode = Data s tx rx disc' framer', msTx = txSt, msLost = lost, msSettled = settled }
      in if lost > hcDrop hs
           then (st1 { msMode = Finished, msStatus = HsDropped, msTxCmd = TxSilence }, VS.replicate n 0, bytes, [EvDropped])
           else (st1, audio, bytes, [])
