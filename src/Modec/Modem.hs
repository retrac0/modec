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
  , modemMnp
    -- * Link properties, for the error-correcting protocol
  , linkBitRate
  , linkSyncable
  , mnpRoleOf
    -- * Transmitter
  , TxState
  , txInit
  , txBlock
  ) where

import qualified Data.Vector.Storable as VS
import Data.Maybe (isJust)
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
import Modec.Mnp
import Modec.V8

data ModemConfig = ModemConfig
  { mcRate      :: Double
  , mcHandshake :: HsConfig
  , mcNoHandshake :: Bool        -- ^ skip call establishment; requires a fixed standard
  , mcDemod     :: DemodParams
  , mcFraming   :: Framing
  , mcTxAmp     :: Double
  , mcSettle    :: Double        -- ^ seconds of idle mark after CONNECT before data flows
  , mcGuardTone :: Bool          -- ^ V.22 high channel 1800 Hz guard tone
  , mcMnp       :: Maybe MnpConfig  -- ^ MNP error correction; 'Nothing' passes bytes straight through
  , mcMaxEvm    :: Double        -- ^ stop handing bytes to the DTE above this decision error
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
  , mcMnp = Nothing
  , mcMaxEvm = 1.0
  }

data ModemEvent
  = EvConnected Standard Link
  | EvDropped
  | EvFailed String
  | EvV8Menu V8Menu        -- ^ the far end's V.8 capabilities
  | EvMnp MnpEvent         -- ^ the error-correcting protocol's state
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
  , txSync    :: [Bool]             -- ^ synchronous line bits waiting (MNP framing mode 3)
  , txV22     :: V22TxState
  , txPhase2  :: !Double            -- ^ second tone phase (dual tones)
  }

txInit :: TxState
txInit = TxState 0 0 True [] [] Nothing [] v22TxInit 0

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
  -- ANSam: 2100 Hz with a 15 Hz envelope between 0.8 and 1.2 of average
  -- (7.2/V.8).  No phase reversals: those exist only to disable network
  -- echo cancellers, and the Recommendation says not to send them when
  -- that is not wanted.
  TxAnsam ->
    let w = 2 * pi * 2100 / fs
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
  , msV8      :: Maybe (FskSpec, Stage Signal Discriminated, Stage Discriminated [Bool], V8Rx)
  , msAnsam   :: Ansam
  , msRxRate  :: Rate        -- ^ decision rate currently set on that receiver
  , msZeros   :: !Int        -- ^ consecutive descrambled zeros seen (for the handshake)
  , msLost    :: !Double     -- ^ seconds of missing carrier in data mode
  , msSettled :: !Double     -- ^ seconds spent in data mode so far
  , msStatus  :: HsStatus
  , msDte     :: [Word8]     -- ^ terminal bytes the protocol layer has not taken yet
  , msMnp     :: Maybe MnpState
  }

modemInit :: ModemConfig -> ModemState
modemInit cfg
  | mcNoHandshake cfg, [s] <- hcModes hs =
      let link = linkFor (hcRole hs) s
      in base { msMode = dataMode s link, msTxCmd = dataCmd link, msStatus = HsConnected s link
              , msMnp = mnpFor cfg link }
  | otherwise = base
  where
    hs = mcHandshake cfg
    fs = mcRate cfg
    listenCh = case hcRole hs of { Originate -> HighChannel; Answer -> LowChannel }
    base = ModemState Handshaking (toneBank fs (hcBank hs)) (initialHandshake hs) txInit TxSilence
             (Just (listenCh, v22RxInit fs)) (hcRole hs) Nothing Nothing (ansamInit fs) R1200 0 0 0 HsBusy
             [] Nothing
    dataMode s link = case link of
      FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
      V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False

-- | The line rate of an established link, for the protocol layer's timers.
linkBitRate :: Link -> Double
linkBitRate (FskLink tx _) = fskBaud tx
linkBitRate (V22Link _ _ R1200) = 1200
linkBitRate (V22Link _ _ R2400) = 2400

-- | Whether the link can carry bit-oriented framing.  Only the V.22 data
-- pump can: dropping the start and stop bits at 300 bit/s would buy 20 %
-- of thirty characters a second, and the FSK link is noise limited rather
-- than framing limited anyway.
linkSyncable :: Link -> Bool
linkSyncable V22Link {} = True
linkSyncable FskLink {} = False

-- | Which end starts the protocol.  This reads the established link, not
-- the configured role: a V.8bis mode select reverses the two before the
-- data pump ever starts, and the station transmitting on the calling
-- side's channel is the one that sends the first link request.
mnpRoleOf :: Link -> MnpRole
mnpRoleOf (V22Link LowChannel _ _) = MnpInitiator
mnpRoleOf (V22Link HighChannel _ _) = MnpResponder
mnpRoleOf (FskLink tx _)
  | fskName tx `elem` [fskName bell103Originate, fskName v21Channel1] = MnpInitiator
  | otherwise = MnpResponder

-- | Start the protocol layer for a link, if the configuration asks for it.
-- The rate and whether synchronous framing is possible are properties of
-- the link, so they are filled in here rather than by the caller.
mnpFor :: ModemConfig -> Link -> Maybe MnpState
mnpFor cfg link = case mcMnp cfg of
  Nothing -> Nothing
  Just _ -> Just (mnpInit (mnpConfFor cfg link) (mnpRoleOf link))

-- | The configuration the protocol layer runs under on this link.
mnpConfFor :: ModemConfig -> Link -> MnpConfig
mnpConfFor cfg link = case mcMnp cfg of
  Nothing -> defaultMnpConfig (linkBitRate link) False
  Just tmpl -> tmpl { mnBitRate = linkBitRate link
                    , mnSyncable = mnSyncable tmpl && linkSyncable link }

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


dataCmd :: Link -> TxCmd
dataCmd (FskLink tx _) = TxData tx
dataCmd (V22Link tx _ r) = TxV22 tx r TxScrambledData

-- | The same link carrying bit-oriented framing.  Only the V.22 data pump
-- has a synchronous mode; 'linkSyncable' keeps the FSK links from ever
-- negotiating one, so the fallback here is never reached.
syncCmd :: Link -> TxCmd
syncCmd (V22Link tx _ r) = TxV22 tx r TxSyncData
syncCmd l = dataCmd l

-- | Idle-mark command for a link (used while settling after CONNECT).
markCmd :: Link -> TxCmd
markCmd (FskLink tx _) = TxMark tx
markCmd (V22Link tx _ r) = TxV22 tx r TxScrambledOnes

modemStatus :: ModemState -> HsStatus
modemStatus = msStatus

-- | Current transmit command (for tracing).
modemTxCmd :: ModemState -> TxCmd
modemTxCmd = msTxCmd

-- | The error-correcting protocol's state, for tracing.
modemMnp :: ModemState -> Maybe MnpState
modemMnp = msMnp

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
  let st = case mcMnp cfg of
        Nothing -> st0 { msTx = (msTx st0) { txQueue = txQueue (msTx st0) ++ newBytes } }
        -- with error correction the terminal's bytes belong to the
        -- protocol layer, which decides when they go on the line
        Just _ -> st0 { msDte = msDte st0 ++ newBytes }
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
          -- the same arrangement for V.8's CM, JM and CJ, which are
          -- async octets rather than HDLC frames
          (v8rx', v8Evs) = case msV8 st of
            Nothing -> (Nothing, [])
            Just (spec, disc, sync, vrx) ->
              let (disc', d) = stepStage disc rxBlock
                  (sync', bits) = stepStage sync d
                  (vrx', evs) = v8RxBits vrx bits
              in (Just (spec, disc', sync', vrx'), evs)
          -- ANSam runs on the raw block: it is the envelope of the answer
          -- tone, which a tone bank cannot see
          (ansam', ansamHit) = ansamBlock (msAnsam st) rxBlock
          -- V.22 receiver on the remote channel, reported to the handshake
          (v22', report, zeros') = case msV22Rx st of
            Nothing -> (Nothing, Nothing, 0)
            Just (ch, rxSt) ->
              let (rxSt', o) = v22RxBlock fs ch rxBlock rxSt
                  z = foldl (\acc b -> if b then 0 else acc + 1) (msZeros st) (roBits o)
              in (Just (ch, rxSt'), Just (V22Report (roEnergy o) (roAngleErr o) (roU11Run o) (roOnesRun o) z (roS1Run o) (roOnes2400 o)), z)
          (hsState', outs) = foldl (\(h, acc) (i, fr) -> let (h', o) = handshakeStep hs h fr report (if i == 0 then HsIn hdlcFrames v8Evs ansamHit else noHsIn) in (h', acc ++ [o])) (msHs st, []) (zip [0 :: Int ..] frames)
          (cmd, status, rxRate, role', hdlcWant) =
            if null outs then (msTxCmd st, HsBusy, msRxRate st, msRole st, fmap (\(s, _, _, _) -> s) (msHdlc st))
            else let o = last outs in (hoTx o, hoStatus o, hoRxRate o, hoRole o, hoHdlc o)
          v8Want = if null outs then fmap (\(s, _, _, _) -> s) (msV8 st) else hoV8 (last outs)
          v8Menus = [ EvV8Menu m | o <- outs, Just m <- [hoV8Menu o] ]
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
          v8'' = case (v8Want, v8rx') of
            (Nothing, _) -> Nothing
            (Just spec, Just cur@(sp, _, _, _)) | fskName sp == fskName spec -> Just cur
            (Just spec, _) -> Just (spec, fskDiscriminator fs spec (mcDemod cfg), fskSyncBits fs spec (mcDemod cfg), v8RxInit)
          st1 = st { msBank = bank', msHs = hsState', msTxCmd = cmd, msV22Rx = v22'', msRole = role', msHdlc = hdlc'', msV8 = v8'', msAnsam = ansam', msRxRate = rxRate, msZeros = zeros' }
      in case status of
           HsConnected s link ->
             let mode = case link of
                   FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
                   V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False
                 st2 = st1 { msMode = mode, msTxCmd = dataCmd link, msStatus = status, msSettled = 0
                           , msMnp = mnpFor cfg link }
                 (txSt, audio) = transmit (markCmd link) st2
             in (st2 { msTx = txSt }, audio, [], v8Menus ++ [EvConnected s link])
           HsFailed why ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], v8Menus ++ [EvFailed why])
           HsDropped ->
             (st1 { msMode = Finished, msStatus = status, msTxCmd = TxSilence }, VS.replicate n 0, [], v8Menus ++ [EvDropped])
           HsBusy ->
             let (txSt, audio) = transmit cmd st1
             in (st1 { msTx = txSt }, audio, [], v8Menus)
    DataFsk s tx rx disc framer ->
      let (disc', d) = stepStage disc rxBlock
          (framer', bytes) = stepStage framer d
          presence = if n == 0 then 1 else VS.sum (dPresent d) / fromIntegral n
      in finishData st (DataFsk s tx rx disc' framer') (FskLink tx rx) (presence >= 0.5) (LineOctets bytes)
    DataV22 tx rx rate framer armed ->
      let (rxSt', o) = case msV22Rx st of
            Just (_, r) -> v22RxBlock fs rx rxBlock (if msRxRate st == rate then r else v22RxSetRate rate r)
            Nothing -> v22RxBlock fs rx rxBlock (v22RxSetRate rate (v22RxInit fs))
          -- frame nothing until the receiver has locked and seen idle mark,
          -- otherwise the start-up bits produce junk characters
          -- Locking once is not enough.  The receiver's own error measure
          -- sits near 0.01 on a good link and still only 0.35 at 20 dB
          -- SNR, where 97% of the text comes through; but while it is
          -- still converging after CONNECT, and again while a carrier is
          -- collapsing because the far end hung up, it runs from 1 to
          -- over 100, and every bit handed over in that state is noise.
          -- A modem that passes on characters its own receiver knows are
          -- worthless is worse than one that passes none, because nothing
          -- downstream can tell the difference.
          trust = rxEvmEstimate rxSt' < mcMaxEvm cfg
          armed' = armed || (roOnesRun o >= 16 && trust)
          -- Once the protocol layer has switched to bit-oriented framing
          -- the start-stop framer is out of the way entirely: HDLC finds
          -- its own frames from the flags, so there is nothing to arm and
          -- no character boundary to keep.
          sync = case msMnp st of
            Just m -> mnpFraming m == FramingBit
            Nothing -> False
          (framer', line)
            | sync = (framer, LineBits (if trust then roBits o else []))
            | otherwise =
                let (f', bs) = if armed && trust then asyncRxBits framer (roBits o) else (framer, [])
                in (f', LineOctets bs)
          st1 = st { msV22Rx = Just (rx, rxSt'), msRxRate = rate }
      in finishData st1 (DataV22 tx rx rate framer' armed') (V22Link tx rx rate) (roEnergy o > 1e-5) line
  where
    -- common tail of the data modes: carrier watchdog, settle time, transmit
    finishData st mode link present line =
      let fs = mcRate cfg
          n = VS.length rxBlock
          blockSec = fromIntegral n / fs
          hs = mcHandshake cfg
          lost = if present then 0 else msLost st + blockSec
          settled = msSettled st + blockSec

          -- The protocol layer runs only after the settle window.  Before
          -- it the transmitter is still sending idle mark, which would
          -- swallow a link request, and the far end's start-stop framer
          -- may not have armed yet.
          ran = settled >= mcSettle cfg
          (mnp', toLine, toDte, mnpEvs) = case msMnp st of
            Just m | ran ->
              let (m', o) = mnpStep (mnpConfFor cfg link) m MnpIn
                    { miDt = blockSec, miLine = line, miDte = msDte st
                    , miTxPending = txPending (msTx st), miDteReady = maxBound }
              in (Just m', moLine o, moDte o, moEvents o)
            Just m -> (Just m, OutOctets [], [], [])
            Nothing -> (Nothing, OutOctets [], lineOctets line, [])

          tx0 = msTx st
          tx1 = case toLine of
            -- back in octet framing after a switch was undone: whatever
            -- synchronous bits were still queued are frames the far end
            -- was never going to read, so they go
            OutOctets bs -> tx0 { txQueue = txQueue tx0 ++ bs, txSync = [] }
            OutBits bs -> tx0 { txSync = txSync tx0 ++ bs }

          -- The command has to match what the protocol layer just encoded,
          -- not the framing it will use next: the frame closing
          -- establishment is octet framed and the switch lands after it.
          -- An empty block still matters, because a synchronous
          -- transmitter idles on flags rather than on mark.
          --
          -- The switch also waits for the octet queue to empty.  That
          -- frame takes several blocks to reach the line at 1200 bit/s,
          -- and the synchronous mode would not carry the rest of it.
          wantSync = case toLine of { OutBits _ -> True; OutOctets _ -> False }
          cmd | settled < mcSettle cfg = markCmd link
              | wantSync && txOctetsPending tx1 == 0 = syncCmd link
              | otherwise = dataCmd link
          (txSt, audio) = txBlock fs (mcTxAmp cfg) (mcFraming cfg) (mcGuardTone cfg) cmd n tx1
          st1 = st { msMode = mode, msTx = txSt, msTxCmd = cmd, msLost = lost, msSettled = settled
                   , msMnp = mnp'
                   , msDte = if isJust (msMnp st) && ran then [] else msDte st }
          evs = map EvMnp mnpEvs
          -- a disconnected error-correcting link is a dead data path, so
          -- it ends the call the same way a lost carrier does
          fatal = any (\e -> case e of { MnpDown _ -> True; _ -> False }) mnpEvs
      in if lost > hcDrop hs || fatal
           then (st1 { msMode = Finished, msStatus = HsDropped, msTxCmd = TxSilence }, VS.replicate n 0, toDte, evs ++ [EvDropped])
           else (st1, audio, toDte, evs)

    lineOctets (LineOctets os) = os
    lineOctets (LineBits _) = []
