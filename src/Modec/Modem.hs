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
  , modemV32Evm
  , modemEchoErle
  , modemEchoDelay
  , modemPhase
  , modemV32Phase
  , modemV32Bits
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
import Modec.V32 (Direction (..), V32Rate (..), RateSeq (..), rateBitRate, rateMargin, v32Rates, v32bisRates)
import Modec.V32Pump (V32Data, v32DataInit, v32DataFrom, v32DataRx, v32DataTx, v32DataEvm, v32DataPower)
import Modec.V32Start
import Modec.Echo
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
  , mcV32Rates  :: Maybe RateSeq -- ^ rates to offer; 'Nothing' takes them from the modes
  , mcEcho      :: EchoConfig    -- ^ echo canceller tuning, for the modes that need one
  , mcMaxEvmV32 :: Double  -- ^ stop passing bytes when the decision error exceeds this much of the constellation's own margin
  , mcMinPowerV32 :: Double  -- ^ below this received power the V.32 carrier is gone
  , mcMaxEvm    :: Double        -- ^ stop handing bytes to the DTE above this decision error
  } deriving (Show)

-- | @modes@ lists the standards the modem may negotiate, best first.
defaultModemConfig :: Double -> Role -> [Standard] -> ModemConfig
defaultModemConfig fs role modes = ModemConfig
  { mcRate = fs
  , mcHandshake = withModes modes (defaultHsConfig role)
  , mcNoHandshake = False
  , mcDemod = defaultDemodParams
  , mcFraming = framing8N1
  , mcTxAmp = 0.5
  , mcSettle = 0.6
  , mcGuardTone = False
  , mcMnp = Nothing
  -- V.22 measures its decision error in grid units where the mean
  -- power is 10; V.32's constellations are normalised to unit mean
  -- power, so the same fraction of the signal is a tenth of the number.
  , mcV32Rates = Nothing
  , mcEcho = defaultEchoConfig
  , mcMaxEvmV32 = 0.5
  -- Measured: a connected V.32 receiver sits around 4e-3 of this and a
  -- silent line at 1e-9.  Anywhere between is a threshold; this is two
  -- orders below the one and far above the other.
  , mcMinPowerV32 = 1e-5
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
    wrap = wrapTwoPi
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
  -- | The V.32 start-up of Figure 4, which runs on the sample clock and
  -- so cannot be driven by the handshake's 20 ms tick.
  | Starting32 V32Start
  | DataFsk Standard FskSpec FskSpec (Stage Signal Discriminated) (Stage Discriminated [Word8])
  | DataV22 V22Channel V22Channel Rate AsyncRx Bool   -- ^ the Bool: framer armed (idle mark seen after lock)
  | DataV32 Role V32Rate V32Data AsyncRx Bool
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
  , msEcho    :: Maybe EchoState  -- ^ echo canceller, for the modes that share a band
  , msZeros   :: !Int        -- ^ consecutive descrambled zeros seen (for the handshake)
  , msLost    :: !Double     -- ^ seconds of missing carrier in data mode
  , msSettled :: !Double     -- ^ seconds spent in data mode so far
  , msStatus  :: HsStatus
  , msDte     :: [Word8]     -- ^ terminal bytes the protocol layer has not taken yet
  , msMnp     :: Maybe MnpState
  }

modemInit :: ModemConfig -> ModemState
modemInit cfg
  -- V.32 has a start-up of its own and no probe rotation to join: an
  -- answering V.32 modem sends the answer tone and then its alternating
  -- AC, which no other mode here would make sense of.  So a modem
  -- configured for V.32 goes straight into Figure 4, and reaches it
  -- otherwise only when V.8 or V.8bis has picked it.
  | [s] <- hcModes hs, isV32 s =
      base { msMode = Starting32 (v32StartInit fs (dirOfRole (hcRole hs)) (v32Offer cfg)) }
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
             (Just (listenCh, v22RxInit fs)) (hcRole hs) Nothing Nothing (ansamInit fs) R1200
             echo0 0 0 0 HsBusy [] Nothing
    -- Only V.32 shares a band with the far end, so only V.32 needs its
    -- own signal taken back out of what returns.
    echo0 = if any isV32 (hcModes hs) then Just (echoInit (mcEcho cfg)) else Nothing
    dataMode s link = case link of
      FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
      V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False
      V32Link role r -> DataV32 role r (v32DataInit fs r) (asyncRxInit (mcFraming cfg)) False

-- | Which rates we will accept.  If the far end is V.32 only, its rate
-- signal says so (Table 5\/V.32 bis Note 1) and the exchange settles on
-- 9600 or below without either end being told which Recommendation to
-- speak.
-- Which rates go out is the difference between the two modes, so it
-- follows the modes unless a rate was asked for by name.  V.32bis is
-- announced by B4 and B8 together, and a V.32 call has to leave B4
-- clear or a V.32bis modem on the other end will read Note 1 the other
-- way and offer 14400 to a modem that cannot take it.
v32Offer :: ModemConfig -> RateSeq
v32Offer cfg = case mcV32Rates cfg of
  Just r -> r
  Nothing
    | V32bis `elem` modes -> v32bisRates
    | otherwise -> v32Rates
  where modes = hcModes (mcHandshake cfg)

dirOfRole :: Role -> Direction
dirOfRole Originate = Calling
dirOfRole Answer = Answering

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

-- | Which end starts the protocol.  This reads the established link, not
-- the configured role: a V.8bis mode select reverses the two before the
-- data pump ever starts, and the station transmitting on the calling
-- side's channel is the one that sends the first link request.
mnpRoleOf :: Link -> MnpRole
mnpRoleOf (V22Link LowChannel _ _) = MnpInitiator
mnpRoleOf (V22Link HighChannel _ _) = MnpResponder
mnpRoleOf (V32Link Originate _) = MnpInitiator
mnpRoleOf (V32Link Answer _) = MnpResponder
mnpRoleOf (FskLink tx _)
  | fskName tx `elem` [fskName bell103Originate, fskName v21Channel1, fskName v23Backward] = MnpInitiator
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
-- V.32 makes its own audio from Modec.V32Pump; these say which of the
-- two things it should be doing, not what signal to make.
dataCmd (V32Link _ _) = TxV32Data
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
markCmd (V32Link _ _) = TxV32Idle
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

-- | Where the modem is, by name, for tracing a call back.
modemPhase :: ModemState -> String
modemPhase st = case msMode st of
  Handshaking -> hsPhaseName (msHs st)
  Starting32 s32 -> "Starting32 " ++ show (v32Phase s32)
  DataFsk s _ _ _ _ -> "Data " ++ show s
  DataV22 _ _ r _ _ -> "Data V22 " ++ show r
  DataV32 _ r _ _ _ -> "Data V32 " ++ show r
  Finished -> "Finished"

-- | The echo canceller's return loss enhancement, for tracing: how much
-- of what arrived it is taking out.  'Nothing' when no canceller is
-- running, which is every mode but V.32.
-- | Where the canceller found the echo, in samples, for tracing.
modemEchoDelay :: ModemState -> Maybe Int
modemEchoDelay st = msEcho st >>= echoDelay

modemEchoErle :: ModemState -> Maybe Double
modemEchoErle = fmap echoErle . msEcho

-- | Where the V.32 start-up has got to, for tracing.  'Nothing' once it
-- is over, or if it never ran.
modemV32Phase :: ModemState -> Maybe (V32Phase, Bool)
modemV32Phase st = case msMode st of
  Starting32 s32 -> Just (v32Phase s32, v32EchoAdapt s32)
  _ -> Nothing

-- | The V.32 receiver's decision error, for tracing.
-- | What the start-up receiver has decoded lately, most recent bit
-- first, for tracing what the far end is actually sending during
-- Figure 4.  'Nothing' outside the V.32 start-up.
modemV32Bits :: ModemState -> Maybe [Bool]
modemV32Bits st = case msMode st of
  Starting32 s32 -> Just (v32Bits s32)
  _ -> Nothing

modemV32Evm :: ModemState -> Maybe Double
modemV32Evm st = case msMode st of
  DataV32 _ _ pump _ armed -> Just (if armed then negate (v32DataEvm pump) else v32DataEvm pump)
  _ -> Nothing

modemConnected :: ModemState -> Bool
modemConnected st = case msMode st of
  DataFsk {} -> True
  DataV22 {} -> True
  DataV32 {} -> True
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
          -- the block's events go to the first frame of the block only;
          -- the pump report is a running state and goes to every frame
          hsInFor i = if i == 0 then HsIn hdlcFrames v8Evs ansamHit report
                                else noHsIn { hiPump = report }
          (hsState', outs) = foldl (\(h, acc) (i, fr) -> let (h', o) = handshakeStep hs h fr (hsInFor i) in (h', acc ++ [o])) (msHs st, []) (zip [0 :: Int ..] frames)
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
                   V32Link role r -> DataV32 role r (v32DataInit fs r) (asyncRxInit (mcFraming cfg)) False
                 st2 = st1 { msMode = mode, msTxCmd = dataCmd link, msStatus = status, msSettled = 0
                           , msMnp = mnpFor cfg link }
                 (txSt, audio) = transmit (markCmd link) st2
             in (st2 { msTx = txSt }, audio, [], v8Menus ++ [EvConnected s link])
           -- V.8 chose V.32.  The answer tone has already been sent -- that
           -- is what ANSam was -- so the start-up begins at the answering
           -- modem's alternating AC rather than repeating it.
           HsStartV32 ->
             let s32 = v32StartAfterAnswerTone fs (dirOfRole (hcRole hs)) (v32Offer cfg)
                 st2 = st1 { msMode = Starting32 s32
                           , msEcho = Just (echoInit (mcEcho cfg)) }
             in (st2, VS.replicate n 0, [], v8Menus)
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
    Starting32 s32 ->
      let (echo', rxClean) = cancelEcho (v32EchoAdapt s32) st n rxBlock
          (s32', audio, status) = v32StartStep s32 rxClean
          st1 = st { msEcho = pushEcho audio echo' }
      in case status of
           V32Busy -> (st1 { msMode = Starting32 s32' }, audio, [], [])
           V32Connected r ->
             let link = V32Link (hcRole hs) r
                 std = if v32Bis s32' then V32bis else V32
                 -- carry the receiver the start-up trained and the
                 -- transmitter's symbol clock, rather than restarting both
                 -- mid-signal
                 pump = v32DataFrom (v32StartRx s32') (v32StartTx s32') (v32StartCoder s32')
                                    (v32DataInit fs r)
                 st2 = st1 { msMode = DataV32 (hcRole hs) r pump (asyncRxInit (mcFraming cfg)) False
                           , msStatus = HsConnected std link, msSettled = 0
                           , msMnp = mnpFor cfg link }
             in (st2, audio, [], [EvConnected std link])
           V32Failed why ->
             (st1 { msMode = Finished, msStatus = HsFailed why }, audio, [], [EvFailed why])
    DataV32 role rate pump framer armed ->
      let (echo', rxClean) = cancelEcho False st n rxBlock
          -- receive only; what goes on the line is decided further down,
          -- once the protocol layer has had its say
          (pump', gotBits) = v32DataRx fs (dirOfRole role) rate pump rxClean
          -- Scaled by how far this constellation's points are from the
          -- wrong answer, because the same decision error means
          -- different things at 4800 and at 14400 -- 0.71 of margin
          -- against 0.11.  A fixed ceiling is generous enough at the
          -- bottom of the range to pass a receiver that is already
          -- making errors at the top, and a marginal 14400 line then
          -- spends a whole call handing the terminal noise between the
          -- bytes it gets right.
          trust = decisionError < mcMaxEvmV32 cfg * rateMargin rate
          decisionError = sqrt (v32DataEvm pump')
          -- Arm on a *run* of descrambled ones -- the idle both ends send
          -- between characters -- and not on a count of them, since noise
          -- is half ones and counting arms the framer on nothing at all.
          -- The run has to outlast the descrambler coming into step as
          -- well: for its first 23 bits it emits whatever it likes, and a
          -- zero followed by ones in there is a start bit and a character
          -- as far as the framer can tell.  A settle window of scrambled
          -- ones makes 64 free.
          --
          -- 64 is not enough here, though, and the reason is the
          -- handover rather than the descrambler.  The receiver arrives
          -- in data mode with the equaliser the start-up trained on four
          -- points and a decision error around 0.28, and takes about
          -- 160 ms to converge on the thirty-two it now has to read.  A
          -- fixed ceiling lets it through at 0.065 -- comfortably inside
          -- 'mcMaxEvmV32', and thirteen times the 0.005 it settles at --
          -- so the framer armed on a receiver that was still acquiring
          -- and handed the terminal a page of noise.  Asking for a run
          -- four times as long asks the right question instead of a
          -- better-tuned version of the wrong one: a run of ones this
          -- long is itself the evidence the line is being read
          -- correctly, since a receiver that is still converging puts a
          -- zero in and starts the count again.  §5.4.2's B1 is 128
          -- symbol intervals of scrambled ones, which is 512 bits at
          -- 9600, so there is room for it.
          --
          -- 64 is enough for the descrambler and not for the handover.
          -- The receiver arrives in data mode with the equaliser the
          -- start-up trained on four points, a decision error around
          -- 0.28, and about 160 ms of converging to do on the thirty-two
          -- points it now has to read.  A fixed ceiling lets it through
          -- at 0.065 -- comfortably inside 'mcMaxEvmV32', and thirteen
          -- times the 0.005 it settles at -- so the framer armed on a
          -- receiver that was still acquiring and handed the terminal a
          -- page of noise before the first real byte.
          --
          -- So arming asks for a converged receiver and not merely a
          -- usable one, which is the ordinary split between acquiring a
          -- signal and tracking it: hard to catch, easy to keep.  The
          -- half second is for the line that never gets that good, where
          -- passing bits with errors in them still beats passing none.
          acquired = decisionError < mcMaxEvmV32 cfg * rateMargin rate / 4 || msSettled st > 0.5
          onesRun' = foldl (\acc b -> if b then acc + 1 else 0) (msZeros st) gotBits
          armed' = armed || (onesRun' >= 64 && trust && acquired)
          sync = case msMnp st of
            Just m -> mnpFraming m == FramingBit
            Nothing -> False
          (framer', line)
            | sync = (framer, LineBits (if trust then gotBits else []))
            | otherwise =
                let (f', bs) = if armed && trust then asyncRxBits framer gotBits else (framer, [])
                in (f', LineOctets bs)
          st1 = st { msEcho = echo', msZeros = onesRun' }
      in finishDataWith (v32Audio role rate pump') st1
           (DataV32 role rate pump' framer' armed') (V32Link role rate)
           (v32DataPower pump' > mcMinPowerV32 cfg) line
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
    -- common tail of the data modes: carrier watchdog, settle time,
    -- error correction, transmit.  How the audio is made is a parameter
    -- because V.32 does not make it from a TxCmd: its pump carries the
    -- bits itself, on a carrier shared with the far end.
    finishData = finishDataWith cmdAudio
    cmdAudio n tx1 cmd mode =
      let (txSt, audio) = txBlock (mcRate cfg) (mcTxAmp cfg) (mcFraming cfg) (mcGuardTone cfg) cmd n tx1
      in (txSt, audio, mode)
    finishDataWith mkAudio st mode link present line =
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
          (txSt, audio, mode') = mkAudio n tx1 cmd mode
          st1 = st { msMode = mode', msTx = txSt, msTxCmd = cmd, msLost = lost, msSettled = settled
                   , msMnp = mnp'
                   -- Remember what we just put on the line.  The canceller
                   -- stops adapting at the handover -- both ends are
                   -- talking from here -- but it still has to be fed, or
                   -- the reference runs out from under the taps the
                   -- start-up trained and it quietly cancels nothing.
                   -- 'msEcho' is Nothing in every other mode, so this is
                   -- the identity there.
                   , msEcho = pushEcho audio (msEcho st)
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

    -- V.32's transmitter: the octets the protocol layer queued, framed
    -- and handed to the pump as bits.  Whatever will not fit in this
    -- block stays in the coder, so nothing has to be pushed back.
    v32Audio role rate pump0 n' tx1 cmd' mode =
      let -- during the settle window send nothing but the scrambled ones
          -- the pump idles on, so the far end's framer has something to
          -- arm on before the first character arrives
          bits | cmd' == TxV32Idle = []
               | otherwise = concatMap (\b -> frameBits (mcFraming cfg) [b]) (txQueue tx1) ++ txSync tx1
          (pump', audio) = v32DataTx (mcRate cfg) (dirOfRole role) rate (mcTxAmp cfg) n' bits pump0
          mode' = case mode of
            DataV32 r rt _ fr ar -> DataV32 r rt pump' fr ar
            other -> other
      in (if cmd' == TxV32Idle then tx1 else tx1 { txQueue = [], txSync = [] }, audio, mode')

    -- Take our own signal back out of what arrived, and remember what we
    -- sent so the next block can be cleaned too.  Only V.32 needs this;
    -- for every other mode msEcho is Nothing and this is the identity.
    -- @adapt@ is the far end's silence, and it is the only thing that
    -- lets the taps move: with both ends transmitting, the far signal
    -- lands in the error term and drives the filter away from the echo
    -- path.  Figure 4/V.32 is built out of half-duplex periods precisely
    -- so this never has to be guessed at, and 'v32EchoAdapt' is the
    -- start-up saying which of them we are in.
    cancelEcho adapt st' n' blk = case msEcho st' of
      Nothing -> (Nothing, blk)
      Just e -> let (e', clean) = echoBlock (mcEcho cfg) adapt blk (aimed e)
                in (Just e', if n' == 0 then blk else clean)
      where
        -- Look for the echo while the far end is quiet, which is the
        -- one time what arrives /is/ the echo.  Once, and only until
        -- something is found: aiming drops the taps, so doing it again
        -- half way through a training window throws away the training.
        aimed e
          | not adapt = e
          | Just _ <- echoDelay e = e
          | Just (l, _) <- echoSearch (mcEcho cfg) e = echoAim (mcEcho cfg) l e
          | otherwise = e

    pushEcho audio = fmap (echoPush (mcEcho cfg) audio)

    -- 'echoSetFar' is deliberately not called from here, and the round
    -- trip the start-up measures is not what aims the filter.  The
    -- obvious reading -- that NT and MT say where the echo lives --
    -- does not survive being tried: they time the far modem's
    -- turnaround, which is its processing delay as much as the line's,
    -- and retargeting on them drops the taps in the middle of the one
    -- window that was training them.  Measured, it took the calling
    -- modem from 17 dB of return loss to none.
    --
    -- Nor does a bulk delay chosen in advance.  That is what was here,
    -- and it spans 20 to 52 ms; dialling the voip.ms echo test, which
    -- returns everything it is sent, put our own signal back at 116 ms.
    -- The filter was aimed at empty line for the whole of every VoIP
    -- call this modem has ever made.  'echoSearch' measures where the
    -- reflection actually is instead.
