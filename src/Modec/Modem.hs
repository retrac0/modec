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
  , RetrainCause (..)
  , V32Entry (..)
  , modemConnected
  , modemV32Evm
  , modemV32Line
  , modemEchoErle
  , modemEchoData
  , modemEchoDelay
  , modemPhase
  , modemV32Phase
  , modemV32Bits
  , modemTxCmd
  , modemV22Rx
  , modemMnp
    -- * Link properties, for the error-correcting protocol
  , mnpRoleOf
    -- * Transmitter
  , TxLine (..)
  , txLineOf
  , TxState
  , txInit
  , txBlock
  ) where

import Numeric (showFFloat)
import qualified Data.Vector.Storable as VS
import Data.Maybe (isJust)
import Data.Word (Word8)

import Modec.Link
import Modec.Modem.Tx
import Modec.Async
import Modec.Detect
import Modec.DSP
import Modec.FSK
import Modec.Handshake
import Modec.Standards
import Modec.Stream
import Modec.V22
import Modec.V32 (RateSeq (..), rateDecisionMargin, ratesBelow, rateSeqCleardown, v32Rates, v32bisRates)
import Modec.V32Pump (V32Data, v32DataInit, v32DataFrom, v32DataResume, v32DataRx, v32DataTx, v32DataEvm, v32DataSps, v32DataPower, v32DataRxState, v32DataTxState)
import Modec.V32Start
import Modec.Echo
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
  , mcRetrainMax :: Int    -- ^ how many retrains one call may spend before giving up
  , mcProbe    :: Bool     -- ^ measure an echo path instead of placing a call
  , mcAnsReversals :: Bool -- ^ V.25 phase reversals on the answer tone; see 'v32AnsReversals'
  , mcAidB1     :: Bool    -- ^ train the V.32 receiver on B1's known symbols ('aidB1').  Off: it predicts B1 exactly and does not reliably help; see the measurement in docs/reference-modem.md
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
  -- A line bad enough to want a fifth retrain is not going to be fixed
  -- by one; past this the call is over.
  , mcRetrainMax = 4
  , mcProbe = False
  , mcAnsReversals = True
  , mcAidB1 = False
  , mcMaxEvm = 1.0
  }

data ModemEvent
  = EvConnected Standard Link
  | EvDropped
  | EvFailed String
  | EvV8Menu V8Menu        -- ^ the far end's V.8 capabilities
  | EvMnp MnpEvent         -- ^ the error-correcting protocol's state
  | EvRetrain RetrainCause -- ^ 5.5: the link is being trained again
  | EvRate V32Rate         -- ^ a retrain settled on a different rate
  deriving (Eq, Show)

data Mode
  = Handshaking
  -- | The V.32 start-up of Figure 4, which runs on the sample clock and
  -- so cannot be driven by the handshake's 20 ms tick.
  | Starting32 V32Start V32Entry
  | DataFsk Standard FskSpec FskSpec (Stage Signal Discriminated) (Stage Discriminated [Word8])
  | DataV22 V22Channel V22Channel Rate AsyncRx Bool   -- ^ the Bool: framer armed (idle mark seen after lock)
  | DataV32 Role V32Rate V32Data AsyncRx Bool
  -- | 5.5: back in Figure 4 mid-call, with everything the session needs
  -- held aside until it comes out the other side.
  | Retrain32 V32Start Held
  -- | Not a call: a continuous V.32 carrier with the echo canceller
  -- adapting, for measuring an echo path against something that returns
  -- what it is sent.  The start-up cannot do this job -- a calling modem
  -- waits in OListen for an answering modem, and an echo service is not
  -- one, so it sends a second of V.8bis and then nothing at all.
  | Probe32 V32Data
  | Finished

-- | What a retrain has to give back when it finishes.  The call has not
-- ended -- the terminal must not see it end -- so the framer, the pump
-- and the count of how many of these we have already spent all wait
-- here.  The pump is kept for the bits it is holding: 'vdBits' is data
-- the terminal handed over that has not reached the line yet, and
-- dropping it would lose bytes the far end never had a chance to see.
data Held = Held
  { hdRole   :: !Role
  , hdRate   :: !V32Rate
  , hdPump   :: !V32Data
  , hdFramer :: !AsyncRx
  , hdCount  :: !Int
  , hdWhy    :: !RetrainCause
  }

-- | Why a retrain started, for tracing and for deciding when to stop
-- trying.
data RetrainCause
  = RetrainLocal      -- ^ our receiver stopped being able to read the line
  | RetrainFarEnd     -- ^ the far end started the signal that opens one
  deriving (Eq, Show)

-- | Whether the V.32 start-up may retreat if nothing answers it.
--
-- V.8 choosing V.32, or V.32 being the only mode configured, is definite
-- evidence and there is nowhere to retreat to.  An answering modem
-- offering the alternating pair on spec is guessing, and A.2.2 has a
-- ladder waiting for it when the guess is wrong.
data V32Entry = V32Committed | V32Offered deriving (Eq, Show)

data ModemState = ModemState
  { msMode    :: Mode
  , msBank    :: Stage Signal [ToneFrame]
  , msHs      :: HsState
  , msTx      :: TxState
  , msTxCmd   :: TxCmd
  , msV22Rx   :: Maybe (V22Channel, V22RxState)   -- ^ receiver on the remote's V.22 channel
  , msRole    :: Role        -- ^ effective role the V.22 receiver channel follows
  , msV8      :: Maybe (FskSpec, Stage Signal Discriminated, Stage Discriminated [Bool], V8Rx)
  , msAnsam   :: Ansam
  , msRxRate  :: Rate        -- ^ decision rate currently set on that receiver
  , msEcho    :: Maybe EchoState
  , msListen  :: Maybe V32Listen  -- ^ watching for the far end to retrain
  , msBad     :: !Int      -- ^ consecutive blocks the V.32 receiver could not be trusted
  , msRetrains :: !Int     -- ^ how many retrains this call has spent  -- ^ echo canceller, for the modes that share a band
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
  | mcProbe cfg = base { msMode = Probe32 (v32DataInit fs V32R9600T) }
  -- ...unless V.8 or V.8bis is on, in which case the negotiation comes
  -- first and picks V.32 itself.  Taking the shortcut regardless meant
  -- --mode v32 --v8 never sent a CM at all: the modem went straight
  -- into Figure 4 as the calling side and waited in OListen for an
  -- alternating AC that the far end, still expecting a V.8 exchange,
  -- was never going to send.
  -- V.8 only, and not V.8bis: V.8bis has no codepoint for V.32 at all,
  -- so leaving the shortcut to it would mean a V.32-only modem sat
  -- through a capabilities exchange that cannot name the one thing it
  -- can do.
  | [s] <- hcModes hs, isV32 s, not (hcV8 hs) =
      base { msMode = Starting32 (v32AidB1 (mcAidB1 cfg) (v32AnsReversals (mcAnsReversals cfg) (v32StartInit fs ((hcRole hs)) (v32Offer cfg)))) V32Committed }
  | mcNoHandshake cfg, [s] <- hcModes hs =
      let link = linkFor (hcRole hs) s
      in base { msMode = dataModeFor cfg s link, msTxCmd = dataCmd link, msStatus = HsConnected s link
              , msMnp = mnpFor cfg link }
  | otherwise = base
  where
    hs = mcHandshake cfg
    fs = mcRate cfg
    base = ModemState Handshaking (toneBank fs (hcBank hs)) (initialHandshake hs) txInit TxSilence
             (Just (listenChannel (hcRole hs), v22RxInit fs)) (hcRole hs) Nothing (ansamInit fs) R1200
             echo0 listen0 0 0 0 0 0 HsBusy [] Nothing
    -- Only V.32 shares a band with the far end, so only V.32 needs its
    -- own signal taken back out of what returns.
    echo0 = if any isV32 (hcModes hs) then Just (echoInit (mcEcho cfg)) else Nothing
    listen0 = if any isV32 (hcModes hs)
                then Just (v32ListenInit fs ((hcRole hs))) else Nothing

-- | What the transmitter needs to know about the line, out of the
-- whole configuration.
txLineOf :: ModemConfig -> TxLine
txLineOf cfg = TxLine (mcRate cfg) (mcTxAmp cfg) (mcFraming cfg) (mcGuardTone cfg)

-- | The V.22 channel this end listens on: the other one.
listenChannel :: Role -> V22Channel
listenChannel Originate = HighChannel
listenChannel Answer = LowChannel

-- | The data mode a link runs in, with its receiver and framer fresh.
-- Written out twice before -- once for @--no-handshake@, once for a
-- handshake that connected -- and a third site that looked the same
-- was not: a V.32 retrain resumes the pump and framer it held.
dataModeFor :: ModemConfig -> Standard -> Link -> Mode
dataModeFor cfg s link = case link of
  FskLink tx rx -> DataFsk s tx rx (fskDiscriminator fs rx (mcDemod cfg)) (fskDeframer fs rx (mcFraming cfg) (mcDemod cfg))
  V22Link tx rx r -> DataV22 tx rx r (asyncRxInit (mcFraming cfg)) False
  V32Link role r -> DataV32 role r (v32DataInit fs r) (asyncRxInit (mcFraming cfg)) False
  where fs = mcRate cfg

-- | Whether the start-stop framer may run yet, and what the line hands
-- the protocol layer this block.
--
-- The framer arms on a run of 64 descrambled ones from a receiver that
-- is both trusted -- its decision error inside the rate's gate -- and
-- acquired, which is a stricter bar: converged, not merely usable.  The
-- two data pumps measure those in their own units, so they arrive here
-- decided.  Once armed it stays armed; and until it is armed nothing
-- reaches the framer at all, since a start-stop framer given a
-- descrambler's warm-up will find a start bit in it.
--
-- Once the protocol layer has switched to bit-oriented framing the
-- start-stop framer is out of the way entirely: HDLC finds its own
-- frames from the flags, so there is nothing to arm and no character
-- boundary to keep.
--
-- This was written out twice, once per pump, and the two had already
-- begun to differ in their comments.
armFramer :: Maybe MnpState -> Bool -> Bool -> Bool -> Int -> [Bool] -> AsyncRx
          -> (AsyncRx, MnpLineIn, Bool)
armFramer mnp armed trust acquired onesRun bits framer
  | sync = (framer, LineBits (if trust then bits else []), armed')
  | otherwise =
      let (f', bs) = if armed && trust then asyncRxBits framer bits else (framer, [])
      in (f', LineOctets bs, armed')
  where
    armed' = armed || (onesRun >= 64 && trust && acquired)
    sync = case mnp of
      Just m -> mnpFraming m == FramingBit
      Nothing -> False

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
  Starting32 s32 _ -> "Starting32 " ++ show (v32Phase s32) ++ lineErr s32
  DataFsk s _ _ _ _ -> "Data " ++ show s
  DataV22 _ _ r _ _ -> "Data V22 " ++ show r
  DataV32 _ r _ _ _ -> "Data V32 " ++ show r
  Retrain32 s32 h -> "Retraining V32 from " ++ show (hdRate h) ++ ", " ++ show (hdWhy h) ++ ", " ++ show (v32Phase s32)
  Probe32 _ -> "Probing the echo path"
  Finished -> "Finished"
  where
    -- The rate is chosen out of this number, so a trace that shows the
    -- rate signals without it shows the answer and not the reason.
    lineErr s32
      | v32Phase s32 `elem` [OR2, ACond2] =
          "  line " ++ showFFloat (Just 4) (v32LineError s32) ""
      | otherwise = ""

-- | The echo canceller's return loss enhancement, for tracing: how much
-- of what arrived it is taking out.  'Nothing' when no canceller is
-- running, which is every mode but V.32.
-- | Where the canceller found the echo, in samples, for tracing.
modemEchoDelay :: ModemState -> Maybe Int
modemEchoDelay st = msEcho st >>= echoDelay

modemEchoErle :: ModemState -> Maybe Double
modemEchoErle = fmap echoErle . msEcho

-- | The data-mode canceller's state: on or off, and the share of the
-- line its filter predicts.  Nothing until it has aimed.
modemEchoData :: ModemState -> Maybe (Bool, Double)
modemEchoData st = msEcho st >>= echoDataState

-- | Where the V.32 start-up has got to, for tracing.  'Nothing' once it
-- is over, or if it never ran.
modemV32Phase :: ModemState -> Maybe (V32Phase, Bool)
modemV32Phase st = case msMode st of
  Starting32 s32 _ -> Just (v32Phase s32, v32EchoAdapt s32)
  _ -> Nothing

-- | The V.32 receiver's decision error, for tracing.
-- | What the start-up receiver has decoded lately, most recent bit
-- first, for tracing what the far end is actually sending during
-- Figure 4.  'Nothing' outside the V.32 start-up.
modemV32Bits :: ModemState -> Maybe [Bool]
modemV32Bits st = case msMode st of
  Starting32 s32 _ -> Just (v32Bits s32)
  _ -> Nothing

modemV32Evm :: ModemState -> Maybe Double
modemV32Evm st = case msMode st of
  DataV32 _ _ pump _ armed -> Just (if armed then negate (v32DataEvm pump) else v32DataEvm pump)
  _ -> Nothing

-- | The V.32 receiver's decision error and samples per symbol, for a
-- line trace: the same 'decisionError' the byte gate and the retrain
-- timer are reading, so a trace and a retrain cannot disagree.
modemV32Line :: ModemState -> Maybe (Double, Double)
modemV32Line st = case msMode st of
  DataV32 _ _ pump _ _ -> Just (sqrt (v32DataEvm pump), v32DataSps pump)
  _ -> Nothing

modemConnected :: ModemState -> Bool
modemConnected st = case msMode st of
  DataFsk {} -> True
  DataV22 {} -> True
  DataV32 {} -> True
  -- A retrain is not a disconnection.  The call is up, the terminal has
  -- not been told anything, and saying otherwise here would have the
  -- Hayes layer report NO CARRIER in the middle of 5.5 working.
  Retrain32 {} -> True
  _ -> False

-- | Process one block of received audio and newly queued bytes.  Returns
-- the audio to transmit (same length), received bytes and events.
modemStep :: ModemConfig -> ModemState -> Signal -> [Word8] -> (ModemState, Signal, [Word8], [ModemEvent])
modemStep cfg st0 rxBlock newBytes =
  let st = case mcMnp cfg of
        Nothing -> st0 { msTx = txQueueOctets newBytes (msTx st0) }
        -- with error correction the terminal's bytes belong to the
        -- protocol layer, which decides when they go on the line
        Just _ -> st0 { msDte = msDte st0 ++ newBytes }
      fs = mcRate cfg
      n = VS.length rxBlock
      hs = mcHandshake cfg
      transmit cmd s = txBlock (txLineOf cfg) cmd n (msTx s)
  in case msMode st of
    Finished ->
      (st, VS.replicate n 0, [], [])
    Handshaking ->
      let (bank', frames) = stepStage (msBank st) rxBlock
          -- a V.21 receiver for V.8's CM, JM and CJ, which are async
          -- octets on whichever channel the handshake names
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
          -- The calling modem's AA, off the same 1800 Hz tracker the
          -- data-mode retrain listener uses.  It cannot come off the
          -- tone bank: 1800 sits two bins from V.21 channel 2's space at
          -- 1850 in a 40 ms window, which Detect says outright are "not
          -- separable at all" there, and it is the TTY space tone as
          -- well.  The tracker measures phase, holds the pair apart, and
          -- already counts a duration rather than an instant -- which is
          -- what 5.4.2 asks for.
          listen' = fmap (v32ListenBlock rxBlock) (msListen st)
          v32Peer = maybe False (v32ListenRetrain ((hcRole hs))) listen'
          -- the block's events go to the first frame of the block only;
          -- the pump report is a running state and goes to every frame
          hsInFor i = if i == 0 then HsIn v8Evs v32Peer ansamHit report
                                else noHsIn { hiPump = report }
          (hsState', outs) = foldl (\(h, acc) (i, fr) -> let (h', o) = handshakeStep hs h fr (hsInFor i) in (h', acc ++ [o])) (msHs st, []) (zip [0 :: Int ..] frames)
          (cmd, status, rxRate, role') =
            if null outs then (msTxCmd st, HsBusy, msRxRate st, msRole st)
            else let o = last outs in (hoTx o, hoStatus o, hoRxRate o, hoRole o)
          v8Want = if null outs then fmap (\(s, _, _, _) -> s) (msV8 st) else hoV8 (last outs)
          v8Menus = [ EvV8Menu m | o <- outs, Just m <- [hoV8Menu o] ]
          -- switch the receiver's decision rate when the handshake says so
          v22a = if rxRate /= msRxRate st then fmap (\(c, r) -> (c, v22RxSetRate rxRate r)) v22' else v22'
          v22'' = v22a
          v8'' = case (v8Want, v8rx') of
            (Nothing, _) -> Nothing
            (Just spec, Just cur@(sp, _, _, _)) | fskName sp == fskName spec -> Just cur
            (Just spec, _) -> Just (spec, fskDiscriminator fs spec (mcDemod cfg), fskSyncBits fs spec (mcDemod cfg), v8RxInit)
          st1 = st { msBank = bank', msHs = hsState', msTxCmd = cmd, msV22Rx = v22'', msRole = role', msV8 = v8'', msAnsam = ansam', msRxRate = rxRate, msZeros = zeros', msListen = listen' }
      in case status of
           HsConnected s link ->
             let mode = dataModeFor cfg s link
                 st2 = st1 { msMode = mode, msTxCmd = dataCmd link, msStatus = status, msSettled = 0
                           , msMnp = mnpFor cfg link }
                 (txSt, audio) = transmit (markCmd link) st2
             in (st2 { msTx = txSt }, audio, [], v8Menus ++ [EvConnected s link])
           -- V.8 chose V.32.  The answer tone has already been sent -- that
           -- is what ANSam was -- so the start-up begins at the answering
           -- modem's alternating AC rather than repeating it.
           HsStartV32 ->
             -- An answering modem that reached here from its own ladder
             -- rather than from a V.8 agreement is guessing, and has
             -- somewhere to go back to.  It also cannot afford the
             -- twenty-five seconds the start-up would otherwise spend
             -- on the alternating pair: that whole time it is
             -- transmitting 600 and 3000 Hz, which no V.22, V.21 or Bell
             -- caller understands.
             let s32 = v32AidB1 (mcAidB1 cfg) (v32AnsReversals (mcAnsReversals cfg) (v32StartAfterAnswerTone fs ((hcRole hs)) (v32Offer cfg)))
                 st2 = st1 { msMode = Starting32 s32 V32Committed
                           , msEcho = Just (echoInit (mcEcho cfg)) }
             in (st2, VS.replicate n 0, [], v8Menus)
           -- A.2.2: the answering ladder offering the pair on spec.  The
           -- same handoff, bounded, and with somewhere to go back to.
           HsOfferV32 ->
             let s32 = v32AidB1 (mcAidB1 cfg) (v32AnsReversals (mcAnsReversals cfg) (v32StartOffer fs ((hcRole hs)) (v32Offer cfg) (hcV32Offer hs)))
                 st2 = st1 { msMode = Starting32 s32 V32Offered
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
    Starting32 s32 entry ->
      let (echo', rxClean) = cancelEcho (v32EchoAdapt s32) st n rxBlock
          (s32', audio, status) = v32StartStep s32 rxClean
          st1 = st { msEcho = pushEcho audio echo' }
      in case status of
           V32Busy -> (st1 { msMode = Starting32 s32' entry }, audio, [], [])
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
           -- Nothing took the offer up, and there is a ladder waiting.
           --
           -- The handshake was never torn down -- it was simply not
           -- stepped while the start-up had the line -- so coming back
           -- is a matter of moving it off V32Handover, which is a dead
           -- end, and marking the offer spent.  The clock does not need
           -- rebasing: the tone bank is stepped only in this branch's
           -- Handshaking sibling, so no handshake time passed at all
           -- and hcTimeout is measured against the same frozen clock.
           --
           -- The V.22 receiver does get restarted.  It was not stepped
           -- either, and its carrier state is however many seconds
           -- stale; handing that to the probe that follows would have it
           -- deciding about a tone that stopped before the offer began.
           V32Failed _ | entry == V32Offered ->
             let listenCh = listenChannel (hcRole hs)
             in ( st1 { msMode = Handshaking
                      , msHs = handshakeAfterV32 hs (msHs st1)
                      , msV22Rx = Just (listenCh, v22RxInit fs)
                      , msEcho = Nothing }
                , audio, [], [] )
           V32Failed why ->
             (st1 { msMode = Finished, msStatus = HsFailed why }, audio, [], [EvFailed why])
    DataV32 role rate pump framer armed ->
      let (echo', rxClean) = cancelEchoData st n rxBlock
          -- receive only; what goes on the line is decided further down,
          -- once the protocol layer has had its say
          (pump', gotBits) = v32DataRx fs (role) rate pump rxClean
          -- Scaled by how far this constellation's points are from the
          -- wrong answer, because the same decision error means
          -- different things at 4800 and at 14400 -- 0.71 of margin
          -- against 0.11.  A fixed ceiling is generous enough at the
          -- bottom of the range to pass a receiver that is already
          -- making errors at the top, and a marginal 14400 line then
          -- spends a whole call handing the terminal noise between the
          -- bytes it gets right.
          trust = decisionError < mcMaxEvmV32 cfg * rateDecisionMargin rate
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
          -- So arming asks for a converged receiver and not merely a
          -- usable one, which is the ordinary split between acquiring a
          -- signal and tracking it: hard to catch, easy to keep.  The
          -- half second is for the line that never gets that good, where
          -- passing bits with errors in them still beats passing none.
          acquired = decisionError < mcMaxEvmV32 cfg * rateDecisionMargin rate / 4 || msSettled st > 0.5
          onesRun' = foldl (\acc b -> if b then acc + 1 else 0) (msZeros st) gotBits
          (framer', line, armed') = armFramer (msMnp st) armed trust acquired onesRun' gotBits framer
          listen' = fmap (v32ListenBlock rxBlock) (msListen st)
          present = v32DataPower pump' > mcMinPowerV32 cfg
          -- How long the receiver has been unable to read a line that is
          -- still carrying something.  Not the same question as the
          -- carrier watchdog's: that one asks whether the far end is
          -- still there, this one whether we can still understand it.
          bad' = if present && not trust then msBad st + 1 else 0
          held = Held role rate pump' framer' (msRetrains st) RetrainLocal
          -- 5.5.  Either end may ask, and the far end asking is a tone
          -- where a data signal never puts one.  Ours is a receiver that
          -- has spent a second unable to read a line that is plainly
          -- still live -- long enough that it is the link and not a
          -- burst, and short enough to be worth doing something about.
          wantRetrain
            -- Nothing at all on the line is a hang-up, and belongs to
            -- the carrier watchdog.  A retrain is for a line that is
            -- still there and has stopped being readable; asking for one
            -- when the far end has simply gone takes the call round
            -- Figure 4 instead of ending it, and the watchdog never gets
            -- its turn because the retrain suspends it.
            | not present = Nothing
            | maybe False (v32ListenRetrain (role)) listen' = Just RetrainFarEnd
            | bad' >= round (1.0 / blockSecs) = Just RetrainLocal
            | otherwise = Nothing
          blockSecs = fromIntegral (max 1 n) / fs
          -- A rate change, which is a retrain that narrows what it will
          -- accept on the way in.  Retraining at the rate that had just
          -- stopped working negotiates its way straight back to it, and
          -- a line that cannot hold 14400 will not hold it any better
          -- for having been asked twice.  So a retrain we asked for
          -- drops a rate; one the far end asked for does not, because
          -- it is the far end's judgement of its own receiver and it
          -- narrows its own offer if it means to.
          offer' = case wantRetrain of
            Just RetrainLocal -> ratesBelow rate (v32Offer cfg)
            _ -> v32Offer cfg
          st1 = st { msEcho = echo', msZeros = onesRun', msListen = listen', msBad = bad' }
      in case wantRetrain of
           Just why | hdCount held < mcRetrainMax cfg, not (rateSeqCleardown offer') ->
             let s32 = v32RetrainInit fs (role) offer'
                         (why == RetrainLocal)
                         (v32DataRxState pump') (v32DataTxState pump') Nothing
                 (txSt, audio) = transmit TxSilence st1
             in ( st1 { msMode = Retrain32 s32 held { hdWhy = why, hdCount = hdCount held + 1 }
                      , msTx = txSt, msBad = 0, msRetrains = msRetrains st + 1 }
                , audio, [], [EvRetrain why] )
           _ -> finishDataWith (v32Audio role rate pump') st1
                  (DataV32 role rate pump' framer' armed') (V32Link role rate)
                  present line
    -- 5.5, running.  Figure 4 all over again, with the session held
    -- aside: the terminal is not told, MNP is not stepped, and the
    -- carrier watchdog does not run -- the start-up contains silences
    -- longer than hcDrop by design, so a watchdog left on would drop
    -- every retrain it was there to make possible.
    Retrain32 s32 held ->
      let (echo', rxClean) = cancelEcho (v32EchoAdapt s32) st n rxBlock
          (s32', audio, status) = v32StartStep s32 rxClean
          st1 = st { msEcho = pushEcho audio echo' }
      in case status of
           V32Busy -> (st1 { msMode = Retrain32 s32' held }, audio, [], [])
           V32Connected r ->
             let role = hdRole held
                 link = V32Link role r
                 pump = v32DataResume fs r (v32StartRx s32') (v32StartTx s32')
                          (v32StartCoder s32') (hdPump held)
                 st2 = st1 { msMode = DataV32 role r pump (hdFramer held) False
                           , msStatus = HsConnected (if v32Bis s32' then V32bis else V32) link
                           , msSettled = 0, msBad = 0 }
             in (st2, audio, [], [EvRate r | r /= hdRate held])
           V32Failed why ->
             (st1 { msMode = Finished, msStatus = HsDropped, msTxCmd = TxSilence }
             , audio, [], [EvFailed why, EvDropped])
    Probe32 pump ->
      -- Adapting throughout: what comes back from a reflection /is/ the
      -- echo, which is the condition the half-duplex windows exist to
      -- create and the one this arranges directly.
      let (echo', _) = cancelEcho True st n rxBlock
          (pump', audio) = v32DataTx fs Originate V32R9600T (mcTxAmp cfg) n [] pump
          st1 = st { msEcho = pushEcho audio echo', msMode = Probe32 pump' }
      in (st1, audio, [], [])
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
          -- Sixteen was shorter than the descrambler.  V.22's is
          -- 1 + x^-14 + x^-17, so for its first seventeen bits it emits
          -- whatever its register happened to hold, and a zero followed
          -- by ones in there is a start bit and a character as far as
          -- the framer can tell.  A run four times the register's length
          -- is itself the evidence that the descrambler is in step,
          -- because one that is not puts a zero in and starts the count
          -- again.  There is room for it: the far end sends scrambled
          -- ones at 1200 and again at 2400 before any data, and every
          -- fixture in the corpus decodes byte for byte either way.
          -- This is the same bar Modec.V32's framer already sets, for
          -- the same reason and against a longer descrambler.
          --
          -- And a converged receiver rather than merely a usable one,
          -- which is the ordinary split between acquiring a signal and
          -- tracking it.  'mcMaxEvm' is where the decisions stop being
          -- worth passing on at all; a quarter of the rate's own
          -- decision margin is where the receiver is reading the line
          -- properly.  The half second is for the line that never gets
          -- that good, where passing bits with errors in them still
          -- beats passing none.
          acquired = rxEvmEstimate rxSt' < acqLimit || msSettled st > 0.5
          acqLimit = (decisionMargin rate / 4) ^ (2 :: Int)
          (framer', line, armed') = armFramer (msMnp st) armed trust acquired (roOnesRun o) (roBits o) framer
          st1 = st { msV22Rx = Just (rx, rxSt'), msRxRate = rate }
      in finishData st1 (DataV22 tx rx rate framer' armed') (V22Link tx rx rate) (roEnergy o > 1e-5) line
  where
    -- common tail of the data modes: carrier watchdog, settle time,
    -- error correction, transmit.  How the audio is made is a parameter
    -- because V.32 does not make it from a TxCmd: its pump carries the
    -- bits itself, on a carrier shared with the far end.
    finishData = finishDataWith cmdAudio
    cmdAudio n tx1 cmd mode =
      let (txSt, audio) = txBlock (txLineOf cfg) cmd n tx1
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
            OutOctets bs -> txQueueOctets bs (txClearSync tx0)
            OutBits bs -> txQueueBits bs tx0

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
               | otherwise = txLineBits (mcFraming cfg) tx1
          (pump', audio) = v32DataTx (mcRate cfg) (role) rate (mcTxAmp cfg) n' bits pump0
          mode' = case mode of
            DataV32 r rt _ fr ar -> DataV32 r rt pump' fr ar
            other -> other
      in (if cmd' == TxV32Idle then tx1 else txDrop tx1, audio, mode')

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
        --
        -- Tried and withdrawn: looking in every phase.  Through an ATA
        -- and a softphone the reflection of our conditioning signal
        -- comes back 1159 ms after it was sent, by which time the far
        -- end has been talking for half a second and this modem has
        -- stopped adapting to train on it, so the quiet windows only
        -- ever hold the echo of a signal that repeats every two
        -- symbols, which the search rightly refuses.  Searching while
        -- the far end talked found a peak at 85 ms instead, aimed there,
        -- and a filter adapting at the wrong delay against a signal it
        -- cannot predict took 9600t's decision error from 0.015 to
        -- 0.033.  On a path that late the reflection of the aperiodic
        -- TRN never arrives inside a quiet window at all, so the
        -- canceller would have to be aimed and trained in data mode,
        -- decision-directed, which this does not do; and the ATA's own
        -- canceller sits at the hybrid where the reflection is born --
        -- and was standing down for every call because the answer
        -- tone's V.25 reversals told it to.  'v32AnsReversals' is the
        -- fix, and it took 12000 from nothing to clean.
        -- See docs/reference-modem.md.
        aimed e
          | not adapt = e
          | Just _ <- echoDelay e = e
          | Just (l, _) <- echoSearch (mcEcho cfg) e = echoAim (mcEcho cfg) l e
          | otherwise = e

    -- Data mode: the far end is talking for the rest of the call, and
    -- the canceller has its own way of working under that; see
    -- 'echoBlockData'.
    cancelEchoData st' n' blk = case msEcho st' of
      Nothing -> (Nothing, blk)
      Just e -> let (e', clean) = echoBlockData (mcEcho cfg) blk e
                in (Just e', if n' == 0 then blk else clean)

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
