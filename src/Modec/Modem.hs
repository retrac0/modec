{-# LANGUAGE BangPatterns #-}
-- | The complete modem as a pure, block-driven state machine: call
-- establishment (see "Modec.Handshake") followed by full-duplex data
-- transfer over the negotiated standard (Bell 103, V.21 or V.22).  The
-- executable feeds it audio blocks and bytes; the tests connect two of
-- them through the channel simulator.
module Modec.Modem
  ( ModemConfig (..)
  , defaultModemConfig
  , ModemState
  , ModemEvent (..)
  , modemInit
  , modemStep
  , modemStatus
  , modemConnected
  , modemTxCmd
  , modemV22Rx
    -- * Transmitter
  , TxState
  , txInit
  , txBlock
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.Async
import Modec.Detect
import Modec.DSP
import Modec.FSK
import Modec.Handshake
import Modec.Standards
import Modec.Stream
import Modec.V22
import Modec.Hdlc

data ModemConfig = ModemConfig
  { mcRate      :: Double
  , mcHandshake :: HsConfig
  , mcNoHandshake :: Bool        -- ^ skip call establishment; requires a fixed standard
  , mcDemod     :: DemodParams
  , mcFraming   :: Framing
  , mcTxAmp     :: Double
  , mcSettle    :: Double        -- ^ seconds of idle mark after CONNECT before data flows
  , mcGuardTone :: Bool          -- ^ V.22 high channel 1800 Hz guard tone
  } deriving (Show)

-- | @modes@ lists the standards the modem may negotiate, best first.
defaultModemConfig :: Double -> Role -> [Standard] -> ModemConfig
defaultModemConfig fs role modes = ModemConfig
  { mcRate = fs
  , mcHandshake = (defaultHsConfig role) { hcModes = modes }
  , mcNoHandshake = False
  , mcDemod = defaultDemodParams
  , mcFraming = framing8N1
  , mcTxAmp = 0.5
  , mcSettle = 0.6
  , mcGuardTone = False
  }

data ModemEvent
  = EvConnected Standard Link
  | EvDropped
  | EvFailed String
  deriving (Eq, Show)

-- | Streaming transmitter: phase-continuous tones, FSK with a byte
-- queue (band limited per channel), and the V.22 modulator.
data TxState = TxState
  { txPhase   :: !Double
  , txBitPos  :: !Double
  , txCurBit  :: !Bool
  , txBits    :: [Bool]
  , txQueue   :: [Word8]
  , txFir     :: Maybe (String, VS.Vector Double, Signal)   -- ^ spec name, reversed kernel, history
  , txV22     :: V22TxState
  , txPhase2  :: !Double            -- ^ second tone phase (dual tones)
  }

txInit :: TxState
txInit = TxState 0 0 True [] [] Nothing v22TxInit 0

-- | Generate @n@ samples for a transmit command, consuming queued bytes
-- only in data modes.
txBlock :: Double -> Double -> Framing -> Bool -> TxCmd -> Int -> TxState -> (TxState, Signal)
txBlock fs amp fr guard cmd n st = case cmd of
  TxSilence -> (st { txFir = Nothing }, VS.replicate n 0)
  TxTone f ->
    let w = 2 * pi * f / fs
        sig = VS.generate n (\i -> amp * sin (txPhase st + w * fromIntegral i))
    in (st { txPhase = wrap (txPhase st + w * fromIntegral n), txFir = Nothing }, sig)
  TxMark spec -> fsk spec False
  TxData spec -> fsk spec True
  TxBits spec bits -> fsk spec False `withQueuedBits` bits
  TxDual f1 f2 g ->
    let w1 = 2 * pi * f1 / fs
        w2 = 2 * pi * f2 / fs
        sig = VS.generate n (\i -> 0.5 * amp * g * (sin (txPhase st + w1 * fromIntegral i) + sin (txPhase2 st + w2 * fromIntegral i)))
    in (st { txPhase = wrap (txPhase st + w1 * fromIntegral n), txPhase2 = wrap (txPhase2 st + w2 * fromIntegral n), txFir = Nothing }, sig)
  TxV22 ch rate mode ->
    let (bytes, st1) = case mode of
          TxScrambledData -> (txQueue st, st { txQueue = [] })
          _ -> ([], st)
        (v', sig) = v22TxBlock fs ch fr amp guard rate mode bytes n (txV22 st1)
    in (st1 { txV22 = v', txFir = Nothing }, sig)
  where
    twoPi = 2 * pi
    wrap p = p - twoPi * fromIntegral (floor (p / twoPi) :: Int)
    -- queue raw bits before generating (used once per TxBits command)
    withQueuedBits f bits = let _ = f in fskWith spec' False bits
      where spec' = case cmd of { TxBits s _ -> s; _ -> error "withQueuedBits" }
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

data Mode
  = Handshaking
  | DataFsk Standard FskSpec FskSpec (Stage Signal Discriminated) (Stage Discriminated [Word8])
  | DataV22 V22Channel V22Channel Rate AsyncRx Bool   -- ^ the Bool: framer armed (idle mark seen after lock)
  | Finished

data ModemState = ModemState
  { msMode    :: Mode
  , msBank    :: Stage Signal [ToneFrame]
  , msHs      :: HsState
  , msTx      :: TxState
  , msTxCmd   :: TxCmd
  , msV22Rx   :: Maybe (V22Channel, V22RxState)   -- ^ receiver on the remote's V.22 channel
  , msRole    :: Role        -- ^ effective role the V.22 receiver channel follows
  , msHdlc    :: Maybe (FskSpec, Stage Signal Discriminated, Stage Discriminated [Bool], HdlcRx)
  , msRxRate  :: Rate        -- ^ decision rate currently set on that receiver
  , msZeros   :: !Int        -- ^ consecutive descrambled zeros seen (for the handshake)
  , msLost    :: !Double     -- ^ seconds of missing carrier in data mode
  , msSettled :: !Double     -- ^ seconds spent in data mode so far
  , msStatus  :: HsStatus
  }

modemInit :: ModemConfig -> ModemState
modemInit cfg
  | mcNoHandshake cfg, [s] <- hcModes hs =
      let link = linkFor (hcRole hs) s
      in base { msMode = dataMode s link, msTxCmd = dataCmd link, msStatus = HsConnected s link }
  | otherwise = base
  where
    hs = mcHandshake cfg
    fs = mcRate cfg
    listenCh = case hcRole hs of { Originate -> HighChannel; Answer -> LowChannel }
    base = ModemState Handshaking (toneBank fs (hcBank hs)) (initialHandshake hs) txInit TxSilence
             (Just (listenCh, v22RxInit fs)) (hcRole hs) Nothing R1200 0 0 0 HsBusy
    dataMode s link = case link of
      FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
      V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False

dataCmd :: Link -> TxCmd
dataCmd (FskLink tx _) = TxData tx
dataCmd (V22Link tx _ r) = TxV22 tx r TxScrambledData

-- | Idle-mark command for a link (used while settling after CONNECT).
markCmd :: Link -> TxCmd
markCmd (FskLink tx _) = TxMark tx
markCmd (V22Link tx _ r) = TxV22 tx r TxScrambledOnes

modemStatus :: ModemState -> HsStatus
modemStatus = msStatus

-- | Current transmit command (for tracing).
modemTxCmd :: ModemState -> TxCmd
modemTxCmd = msTxCmd

-- | The V.22 receiver state and its decision rate (for tracing).
modemV22Rx :: ModemState -> (Maybe (V22Channel, V22RxState), Rate)
modemV22Rx st = (msV22Rx st, msRxRate st)

modemConnected :: ModemState -> Bool
modemConnected st = case msMode st of
  DataFsk {} -> True
  DataV22 {} -> True
  _ -> False

-- | Process one block of received audio and newly queued bytes.  Returns
-- the audio to transmit (same length), received bytes and events.
modemStep :: ModemConfig -> ModemState -> Signal -> [Word8] -> (ModemState, Signal, [Word8], [ModemEvent])
modemStep cfg st0 rxBlock newBytes =
  let st = st0 { msTx = (msTx st0) { txQueue = txQueue (msTx st0) ++ newBytes } }
      fs = mcRate cfg
      n = VS.length rxBlock
      hs = mcHandshake cfg
      transmit cmd s = txBlock fs (mcTxAmp cfg) (mcFraming cfg) (mcGuardTone cfg) cmd n (msTx s)
  in case msMode st of
    Finished ->
      (st, VS.replicate n 0, [], [])
    Handshaking ->
      let (bank', frames) = stepStage (msBank st) rxBlock
          -- synchronous V.21 receiver for V.8bis messages, when the handshake asks for one
          (hdlc', hdlcFrames) = case msHdlc st of
            Nothing -> (Nothing, [])
            Just (spec, disc, sync, hrx) ->
              let (disc', d) = stepStage disc rxBlock
                  (sync', bits) = stepStage sync d
                  (hrx', fsOut) = hdlcRxBits hrx bits
              in (Just (spec, disc', sync', hrx'), fsOut)
          -- V.22 receiver on the remote channel, reported to the handshake
          (v22', report, zeros') = case msV22Rx st of
            Nothing -> (Nothing, Nothing, 0)
            Just (ch, rxSt) ->
              let (rxSt', o) = v22RxBlock fs ch rxBlock rxSt
                  z = foldl (\acc b -> if b then 0 else acc + 1) (msZeros st) (roBits o)
              in (Just (ch, rxSt'), Just (V22Report (roEnergy o) (roAngleErr o) (roU11Run o) (roOnesRun o) z (roS1Run o) (roOnes2400 o)), z)
          (hsState', outs) = foldl (\(h, acc) (i, fr) -> let (h', o) = handshakeStep hs h fr report (if i == 0 then hdlcFrames else []) in (h', acc ++ [o])) (msHs st, []) (zip [0 :: Int ..] frames)
          (cmd, status, rxRate, role', hdlcWant) =
            if null outs then (msTxCmd st, HsBusy, msRxRate st, msRole st, fmap (\(s, _, _, _) -> s) (msHdlc st))
            else let o = last outs in (hoTx o, hoStatus o, hoRxRate o, hoRole o, hoHdlc o)
          -- switch the receiver's decision rate when the handshake says so
          v22a = if rxRate /= msRxRate st then fmap (\(c, r) -> (c, v22RxSetRate rxRate r)) v22' else v22'
          -- a V.8bis mode select reverses the roles: listen on the other channel
          v22'' = if role' /= msRole st
                    then Just (case role' of { Originate -> HighChannel; Answer -> LowChannel }, v22RxInit fs)
                    else v22a
          hdlc'' = case (hdlcWant, hdlc') of
            (Nothing, _) -> Nothing
            (Just spec, Just cur@(s, _, _, _)) | fskName s == fskName spec -> Just cur
            (Just spec, _) -> Just (spec, fskDiscriminator fs spec (mcDemod cfg), fskSyncBits fs spec (mcDemod cfg), hdlcRxInit)
          st1 = st { msBank = bank', msHs = hsState', msTxCmd = cmd, msV22Rx = v22'', msRole = role', msHdlc = hdlc'', msRxRate = rxRate, msZeros = zeros' }
      in case status of
           HsConnected s link ->
             let mode = case link of
                   FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
                   V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False
                 st2 = st1 { msMode = mode, msTxCmd = dataCmd link, msStatus = status, msSettled = 0 }
                 (txSt, audio) = transmit (markCmd link) st2
             in (st2 { msTx = txSt }, audio, [], [EvConnected s link])
           HsFailed why ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], [EvFailed why])
           HsDropped ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], [EvDropped])
           HsBusy ->
             let (txSt, audio) = transmit cmd st1
             in (st1 { msTx = txSt }, audio, [], [])
    DataFsk s tx rx disc framer ->
      let (disc', d) = stepStage disc rxBlock
          (framer', bytes) = stepStage framer d
          presence = if n == 0 then 1 else VS.sum (dPresent d) / fromIntegral n
      in finishData st (DataFsk s tx rx disc' framer') (FskLink tx rx) (presence >= 0.5) bytes
    DataV22 tx rx rate framer armed ->
      let (rxSt', o) = case msV22Rx st of
            Just (_, r) -> v22RxBlock fs rx rxBlock (if msRxRate st == rate then r else v22RxSetRate rate r)
            Nothing -> v22RxBlock fs rx rxBlock (v22RxSetRate rate (v22RxInit fs))
          -- frame nothing until the receiver has locked and seen idle mark,
          -- otherwise the start-up bits produce junk characters
          armed' = armed || roOnesRun o >= 16
          (framer', bytes) = if armed then asyncRxBits framer (roBits o) else (framer, [])
          st1 = st { msV22Rx = Just (rx, rxSt'), msRxRate = rate }
      in finishData st1 (DataV22 tx rx rate framer' armed') (V22Link tx rx rate) (roEnergy o > 1e-5) bytes
  where
    -- common tail of the data modes: carrier watchdog, settle time, transmit
    finishData st mode link present bytes =
      let fs = mcRate cfg
          n = VS.length rxBlock
          blockSec = fromIntegral n / fs
          hs = mcHandshake cfg
          lost = if present then 0 else msLost st + blockSec
          settled = msSettled st + blockSec
          cmd = if settled < mcSettle cfg then markCmd link else dataCmd link
          (txSt, audio) = txBlock fs (mcTxAmp cfg) (mcFraming cfg) (mcGuardTone cfg) cmd n (msTx st)
          st1 = st { msMode = mode, msTx = txSt, msLost = lost, msSettled = settled }
      in if lost > hcDrop hs
           then (st1 { msMode = Finished, msStatus = HsDropped, msTxCmd = TxSilence }, VS.replicate n 0, bytes, [EvDropped])
           else (st1, audio, bytes, [])
