{-# LANGUAGE BangPatterns #-}
-- | MNP classes 2 to 4: the alternative error-correcting protocol of
-- ITU-T V.42 (10/96) Annex A.  The frame structure lives in
-- "Modec.MnpFrame"; this module is the protocol on top of it -- link
-- establishment (A.7.1), the go-back-N data phase with its credit window
-- (A.7.3), the timers of A.7.5 and the two ways a link can fail to come
-- up at all.
--
-- Pure and tick driven, in the shape of "Modec.Handshake": a
-- configuration, a state, one input record and one output record,
-- advanced once per audio block.  Nothing here touches audio; the caller
-- hands it whatever the receiver produced and whatever the terminal
-- typed, and it says what to put on the line and what to give back to the
-- terminal.
--
-- Three conventions in the Recommendation are worth stating, because all
-- three are the opposite of what HDLC teaches:
--
-- * V(S), V(R), V(SA) and V(RA) all start at 1, not 0.
--
-- * N(R) in an acknowledgement names the /last frame correctly received/,
--   which is V(R) - 1, rather than the next one expected.  That is why
--   the acknowledgement confirming establishment carries zero.
--
-- * An acknowledgement repeating the previous N(R) is a negative
--   acknowledgement: it is the signal to retransmit, and it arrives long
--   before the timer would.
--
-- The one place a receiver must stay quiet is A.7.3.2.2: the /first/ LT
-- frame arriving with N(S) = V(R) - 1 is ignored outright and draws no
-- acknowledgement.  Without that, one duplicated frame sets off an
-- exchange of acknowledgements that never settles.
module Modec.Mnp
  ( -- * Configuration
    MnpConfig (..)
  , defaultMnpConfig
  , MnpRole (..)
  , MnpFraming (..)
  , offerLr
  , mnpT401
    -- * State
  , MnpState
  , mnpInit
  , MnpPhase (..)
  , mnpPhase
  , mnpRole
  , mnpFraming
  , mnpNegotiated
  , mnpTrace
    -- * Stepping
  , MnpLineIn (..)
  , MnpLineOut (..)
  , MnpIn (..)
  , MnpOut (..)
  , MnpEvent (..)
  , mnpStep
    -- * Sequence arithmetic
  , seqDist
  ) where

import Data.Bits ((.&.), (.|.))
import Data.Word (Word8)

import Modec.Async (AsyncRx, asyncRxInit, asyncRxBits)
import Modec.FSK (framing8N1)
import Modec.Hdlc (HdlcRx, hdlcRxInit, hdlcRxBits)
import Modec.MnpFrame

-- | Which end started the protocol.  This follows the established link,
-- not the modem's configured role: a V.8bis mode select can reverse the
-- two before the data pump ever starts.
data MnpRole = MnpInitiator | MnpResponder deriving (Eq, Show)

-- | Which framing the line is carrying at this instant.  The whole
-- establishment exchange is octet framed whatever gets negotiated (A.6.4),
-- and the negotiated framing only takes effect once it is over.
data MnpFraming = FramingOctet | FramingBit deriving (Eq, Show)

data MnpConfig = MnpConfig
  { mnClass      :: !Int      -- ^ highest class to offer: 2, 3 or 4
  , mnK          :: !Word8    -- ^ outstanding LT frames we will allow (A.7.5: support 8)
  , mnN401       :: !Int      -- ^ largest information field we will accept
  , mnBitRate    :: !Double   -- ^ line rate, for the retransmission timer
  , mnSyncable   :: !Bool     -- ^ the link can carry bit-oriented framing (V.22 family only)
  , mnT401       :: Maybe Double  -- ^ override the computed retransmission timer
  , mnT401Lr     :: !Double   -- ^ establishment timer (A.7.1 allows 0.5 to 9 s)
  , mnLrTries    :: !Int      -- ^ link requests sent before giving up
  , mnT403       :: Maybe Double  -- ^ inactivity timer, 59 s or more when enabled
  , mnRxWindow   :: !Int      -- ^ receive buffer in frames; the source of credit
  , mnDataDetect :: !Bool     -- ^ fall through as soon as the far end sends plain data
  } deriving (Show)

-- | Offer everything: class 4, eight outstanding frames, the 256-octet
-- information field the data phase optimization permits.
defaultMnpConfig :: Double -> Bool -> MnpConfig
defaultMnpConfig bitRate syncable = MnpConfig
  { mnClass = 4
  , mnK = 8
  , mnN401 = 256
  , mnBitRate = bitRate
  , mnSyncable = syncable
  , mnT401 = Nothing
  , mnT401Lr = 3
  , mnLrTries = 2
  , mnT403 = Nothing
  , mnRxWindow = 8
  , mnDataDetect = True
  }

-- | The link request this configuration offers.  A station whose data
-- pump cannot carry synchronous framing simply offers octet framing; the
-- negotiated minimum then settles it without anyone having to refuse.
offerLr :: MnpConfig -> MnpLr
offerLr c = defaultLr
  { lrFraming = if mnClass c >= 3 && mnSyncable c then 3 else 2
  , lrK = max 1 (mnK c)
  , lrN401 = n401
  , lrDpo = (if big then 1 else 0) .|. (if mnClass c >= 4 then 2 else 0)
  }
  where
    big = mnClass c >= 4 && mnN401 c > 64
    n401 = if big then min 256 (mnN401 c) else min 64 (max 1 (mnN401 c))

-- | The data phase retransmission timer (A.7.5.1):
--
-- > T401 >= 2 * ((k/2)*Lb*(Lf + Llt + N401) + Lb*(Lf + Lla)) / bitrate + Trt
--
-- with Lb the bits per octet (10 when octet framed, 8 when bit framed),
-- Lf the framing overhead (7 octets against 4), and Llt and Lla the LT
-- and LA header lengths for the optimization in force.  At 1200 bit\/s
-- with k = 8 and N401 = 64 this gives about six seconds, which is what
-- Table A.9 prints.
mnpT401 :: MnpConfig -> MnpFraming -> Bool -> Word8 -> Int -> Double
mnpT401 c fr dpo k n401 =
    2 * (kh * lb * (lf + llt + fromIntegral n401) + lb * (lf + lla)) / rate + trt
  where
    lb = if fr == FramingOctet then 10 else 8
    lf = if fr == FramingOctet then 7 else 4
    llt = if dpo then 3 else 5
    lla = if dpo then 4 else 8
    kh = fromIntegral (max 1 (k `div` 2)) :: Double
    rate = max 1 (mnBitRate c)
    trt = 0.5   -- round trip, including the far end's processing

data MnpPhase
  = MnpEstablish
  | MnpData
  | MnpTransparent   -- ^ nothing answered: the line carries plain data
  | MnpClosed
  deriving (Eq, Show)

data MnpEvent
  = MnpUp !Int !Word8 !Int    -- ^ data phase open: class, k, N401
  | MnpTransparentFallback    -- ^ no protocol at the far end; no error correction on this call
  | MnpDown String            -- ^ disconnected, with the reason
  deriving (Eq, Show)

-- | What the receiver produced this block, in whichever framing is in
-- force.  Bit framing hands over the raw descrambled bit stream, not
-- start-stop characters.
data MnpLineIn = LineOctets [Word8] | LineBits [Bool] deriving (Eq, Show)

data MnpLineOut = OutOctets [Word8] | OutBits [Bool] deriving (Eq, Show)

data MnpIn = MnpIn
  { miDt        :: !Double   -- ^ seconds since the previous step
  , miLine      :: MnpLineIn
  , miDte       :: [Word8]   -- ^ new bytes from the terminal
  , miTxPending :: !Int      -- ^ octets still queued in the transmitter
  , miDteReady  :: !Int      -- ^ octets the terminal will take this block
  }

data MnpOut = MnpOut
  { moLine   :: MnpLineOut
  , moDte    :: [Word8]
  , moEvents :: [MnpEvent]
  , moBusy   :: !Bool        -- ^ no credit: the terminal should be held off
  }

-- | An LT frame that has been given a sequence number.
type Pending = (Word8, [Word8])

data MnpState = MnpState
  { msPhase   :: !MnpPhase
  , msRole    :: !MnpRole
  , msT       :: !Double
  , msFraming :: !MnpFraming     -- ^ in force now
  , msNextFraming :: !MnpFraming -- ^ in force from the next block
  , msNeg     :: MnpLr           -- ^ what was agreed for the data phase
  , msDpo     :: !Bool           -- ^ encode LT and LA in the short form
  , msRx2     :: Mode2Rx
  , msRx3     :: HdlcRx
  , msRxGrace :: Maybe (AsyncRx, Mode2Rx)  -- ^ octet decoder kept alive across the switch
  , msGraceAt :: !Double
  , msVS      :: !Word8
  , msVR      :: !Word8
  , msLastNR  :: Maybe Word8
  , msPeerNk  :: !Word8
  , msLastNk  :: !Word8
  , msOutCtrl :: [MnpFrame]      -- ^ control frames, not subject to the window
  , msOutQ    :: [Pending]       -- ^ numbered, not yet transmitted
  , msUnacked :: [Pending]       -- ^ transmitted, awaiting acknowledgement
  , msDupSeen :: !Bool           -- ^ the one free duplicate of A.7.3.2.2 is spent
  , msTxBuf   :: [Word8]
  , msRxBuf   :: [Word8]
  , msAckRx   :: !Int            -- ^ accepted LTs not yet acknowledged
  , msAckForce :: !Bool          -- ^ an acknowledgement is owed whatever the count
  , msRetries :: !Int            -- ^ N400
  , msLrTries :: !Int
  , msT401At  :: !Double
  , msT402At  :: !Double
  , msT404At  :: !Double
  , msT403At  :: !Double
  , msSawBad  :: !Bool
  , msSawGood :: !Bool
  }

never :: Double
never = 1 / 0

mnpInit :: MnpConfig -> MnpRole -> MnpState
mnpInit c role = MnpState
  { msPhase = MnpEstablish
  , msRole = role
  , msT = 0
  , msFraming = FramingOctet
  , msNextFraming = FramingOctet
  , msNeg = offerLr c
  , msDpo = False
  , msRx2 = mode2RxInit
  , msRx3 = hdlcRxInit
  , msRxGrace = Nothing
  , msGraceAt = never
  , msVS = 1, msVR = 1
  , msLastNR = Nothing
  , msPeerNk = mnK c
  , msLastNk = mnK c
  , msOutCtrl = []
  , msOutQ = [], msUnacked = []
  , msDupSeen = False
  , msTxBuf = [], msRxBuf = []
  , msAckRx = 0
  , msAckForce = False
  , msRetries = 0
  , msLrTries = 0
  , msT401At = case role of
      -- the initiator speaks first, on its very next step; the responder
      -- only listens, so it needs a deadline of its own or a caller with
      -- no protocol at all would hold it in establishment for ever
      MnpInitiator -> never
      MnpResponder -> mnT401Lr c * fromIntegral (max 1 (mnLrTries c))
  , msT402At = never, msT404At = never, msT403At = never
  , msSawBad = False, msSawGood = False
  }

mnpPhase :: MnpState -> MnpPhase
mnpPhase = msPhase

-- | Which end of the protocol this station is.
mnpRole :: MnpState -> MnpRole
mnpRole = msRole

mnpFraming :: MnpState -> MnpFraming
mnpFraming = msFraming

-- | What was agreed for the data phase, once establishment is over.
mnpNegotiated :: MnpState -> MnpLr
mnpNegotiated = msNeg

-- | A one-line summary of the link, for tracing a real call.
mnpTrace :: MnpState -> String
mnpTrace st = unwords
  [ show (msPhase st), show (msFraming st)
  , "V(S)=" ++ show (msVS st), "V(R)=" ++ show (msVR st)
  , "unacked=" ++ show (length (msUnacked st))
  , "outq=" ++ show (length (msOutQ st))
  , "retries=" ++ show (msRetries st)
  , "t=" ++ show (msT st)
  , "t401=" ++ show (msT401At st)
  ]

-- | Distance from @b@ forward to @a@ in the modulo-256 sequence space.
-- Every comparison of sequence numbers goes through this, so the
-- wraparound at 255 is handled once rather than at each use.
seqDist :: Word8 -> Word8 -> Int
seqDist a b = fromIntegral (a - b)

-- | Is a frame numbered @ns@ covered by an acknowledgement naming @nr@?
acked :: Word8 -> Word8 -> Bool
acked nr ns = seqDist nr ns < 128

-- | The class implied by what was negotiated.
negClass :: MnpLr -> Int
negClass n
  | lrDpo n /= 0 = 4
  | lrFraming n == 3 = 3
  | otherwise = 2

mnpStep :: MnpConfig -> MnpState -> MnpIn -> (MnpState, MnpOut)
mnpStep c st0 inp =
  let t = msT st0 + miDt inp
      st1 = st0 { msT = t, msTxBuf = msTxBuf st0 ++ miDte inp }
  in case msPhase st1 of
       MnpClosed -> closed st1
       MnpTransparent -> transparent st1 (plainOf (miLine inp))
       _ ->
         let (st2, frames, bad, plain) = decodeLine st1 (miLine inp)
             st3 = st2 { msSawBad = msSawBad st2 || bad }
             (st4, evs0) = foldl (handleFrame c) (st3, []) frames
             (st5, evs1) = timers c st4 plain
         in if msPhase st5 == MnpTransparent || msPhase st5 == MnpClosed
              then let (st6, out) = if msPhase st5 == MnpTransparent
                                      then transparent st5 []
                                      else closed st5
                   in (st6, out { moEvents = evs0 ++ evs1 ++ moEvents out })
              else let (st6, out) = emit c st5 inp
                   in (st6, out { moEvents = evs0 ++ evs1 ++ moEvents out })

plainOf :: MnpLineIn -> [Word8]
plainOf (LineOctets os) = os
plainOf (LineBits _) = []

-- | Shutting down.  Whatever was queued still goes out: a disconnect is
-- worth nothing if it never reaches the line.
closed :: MnpState -> (MnpState, MnpOut)
closed st =
  ( st { msOutCtrl = [] }
  , MnpOut (encodeAll (msFraming st) (msDpo st) (msOutCtrl st)) [] [] False )

-- | Once the link has fallen through, the protocol is out of the way
-- entirely: whatever arrives goes to the terminal and whatever the
-- terminal typed goes on the line, once each.
transparent :: MnpState -> [Word8] -> (MnpState, MnpOut)
transparent st os =
  ( st { msTxBuf = [], msRxBuf = [] }
  , MnpOut (OutOctets (msTxBuf st)) (msRxBuf st ++ os) [] False )

-- | Feed the line into whichever decoder the current framing calls for.
-- Returns the frames, whether anything was damaged, and how many octets
-- arrived outside any frame (which is how a far end with no protocol at
-- all announces itself).
decodeLine :: MnpState -> MnpLineIn -> (MnpState, [MnpFrame], Bool, [Word8])
decodeLine st line = case (msFraming st, line) of
  (FramingOctet, LineOctets os) ->
    let (rx', out) = mode2RxOctets (msRx2 st) os
        st' = st { msRx2 = rx' }
    in (st', bodies out, any isBad out, mode2Junk rx')
  (FramingBit, LineBits bs) ->
    let (rx', frs) = hdlcRxBits (msRx3 st) bs
        -- while the switch settles, keep decoding the same bits as
        -- start-stop characters too: if the acknowledgement that closed
        -- establishment was lost, the far end is still octet framed and
        -- will be repeating its link request
        (grace', graceFrames)
          | msT st >= msGraceAt st = (Nothing, [])
          | otherwise = case msRxGrace st of
              Nothing -> (Nothing, [])
              Just (ar, m2) ->
                let (ar', os) = asyncRxBits ar bs
                    (m2', out) = mode2RxOctets m2 os
                in (Just (ar', m2'), bodies out)
        st' = st { msRx3 = rx', msRxGrace = grace' }
    in (st', decodeAll frs ++ graceFrames, False, [])
  -- the caller handed us the wrong shape; nothing to do but wait
  _ -> (st, [], False, [])
  where
    isBad = either (const True) (const False)
    bodies out = decodeAll [ b | Right b <- out ]
    decodeAll = concatMap (\b -> case decodeFrame b of { Right f -> [f]; Left _ -> [] })

handleFrame :: MnpConfig -> (MnpState, [MnpEvent]) -> MnpFrame -> (MnpState, [MnpEvent])
handleFrame c (st, evs) frame = case (msPhase st, frame) of
  (_, FrLD reason _) ->
    (st { msPhase = MnpClosed }, evs ++ [MnpDown ("link disconnect, reason " ++ show reason)])

  -- establishment
  (MnpEstablish, FrLR theirs) -> case negotiateLr (offerLr c) theirs of
    Left reason ->
      ( (sendCtrl st (FrLD reason Nothing)) { msPhase = MnpClosed }
      , evs ++ [MnpDown ("link request refused, reason " ++ show reason)] )
    Right n -> case msRole st of
      -- the initiator answers with the acknowledgement that opens the
      -- data phase; A.7.1 fixes its N(R) at zero, which is exactly
      -- V(R) - 1 with V(R) starting at one
      MnpInitiator ->
        let st' = sendCtrl st { msSawGood = True } (FrLA 0 (mnK c))
        in enterData c st' n evs
      -- the responder answers with its own link request and waits to be
      -- acknowledged; that answer is what both ends then obey
      MnpResponder ->
        let st' = (sendCtrl st { msSawGood = True, msNeg = n } (FrLR n))
                    { msLrTries = msLrTries st + 1
                    , msT401At = msT st + mnT401Lr c }
        in (st', evs)
  (MnpEstablish, FrLA _ _) -> case msRole st of
    MnpResponder | msSawGood st -> enterData c st { msSawGood = True } (msNeg st) evs
    _ -> (st, evs)

  -- A link request arriving in the data phase means the acknowledgement
  -- that closed establishment never got there: the far end is still octet
  -- framed and repeating itself.  Go back to the framing it can read, say
  -- so again, and re-arm the switch.  Without this the two ends are stuck
  -- for good -- one talking flags, the other listening for characters --
  -- and it is exactly the frame most likely to be lost that causes it,
  -- since nothing is retransmitting it.
  (MnpData, FrLR _) | msRole st == MnpInitiator ->
    let st' = sendCtrl st { msFraming = FramingOctet } (FrLA 0 (mnK c))
    in ( st' { msNextFraming = framingOf (msNeg st)
             , msRxGrace = if framingOf (msNeg st) == FramingBit
                             then Just (asyncRxInit framing8N1, mode2RxInit)
                             else Nothing
             , msGraceAt = if framingOf (msNeg st) == FramingBit
                             then msT st + 2 * t401Of c (msNeg st)
                             else never
             }
       , evs )

  -- data phase
  (MnpData, FrLT ns info) -> (receiveLt c st ns info, evs)
  (MnpData, FrLA nr nk) -> (receiveLa st nr nk, evs)
  -- an attention is acknowledged but never originated: modec has no
  -- break signal to carry, and a link that stalled on one would be worse
  -- than one that ignores it
  (MnpData, FrLN nsa _) -> (sendCtrl st (FrLNA nsa), evs)
  _ -> (st, evs)

-- | Adopt what was negotiated and open the data phase.
enterData :: MnpConfig -> MnpState -> MnpLr -> [MnpEvent] -> (MnpState, [MnpEvent])
enterData c st n evs =
  ( st { msPhase = MnpData
       , msNeg = n
       , msDpo = lrDpo n .&. 2 /= 0
       , msPeerNk = lrK n
       , msLastNk = lrK n
       , msT401At = never
       , msT402At = never
       , msT404At = msT st + t404 c
       , msT403At = maybe never (msT st +) (mnT403 c)
       -- A.7.1: the acknowledgement that closes establishment is itself
       -- octet framed, and the negotiated framing starts after it.  The
       -- switch therefore lands one block late, once 'emit' has encoded
       -- and sent that frame in the framing the far end is still reading.
       , msNextFraming = framingOf n
       , msRxGrace = grace
       , msGraceAt = if lrFraming n == 3 then msT st + 2 * t401Of c n else never
       }
  , evs ++ [MnpUp (negClass n) (lrK n) (lrN401 n)] )
  where
    grace | lrFraming n == 3 = Just (asyncRxInit framing8N1, mode2RxInit)
          | otherwise = Nothing

-- | Whether a run of octets reads as text a terminal would have been
-- shown, rather than as the wreckage of frames that did not survive the
-- line.  Nearly all of it must be printable ASCII or ordinary whitespace.
looksLikeText :: [Word8] -> Bool
looksLikeText os =
  not (null os) && 5 * length [ () | o <- os, printable o ] >= 4 * length os
  where printable o = o == 9 || o == 10 || o == 13 || (o >= 32 && o <= 126)

-- | The framing a negotiated link request calls for.
framingOf :: MnpLr -> MnpFraming
framingOf x = if lrFraming x == 3 then FramingBit else FramingOctet

t401Of :: MnpConfig -> MnpLr -> Double
t401Of c n = case mnT401 c of
  Just v -> v
  Nothing -> mnpT401 c (if lrFraming n == 3 then FramingBit else FramingOctet)
                       (lrDpo n .&. 2 /= 0) (lrK n) (lrN401 n)

-- | The forced acknowledgement timer (A.7.5.4): seven seconds at
-- 1200 bit\/s, three above it.
t404 :: MnpConfig -> Double
t404 c = if mnBitRate c < 2400 then 7 else 3

receiveLt :: MnpConfig -> MnpState -> Word8 -> [Word8] -> MnpState
receiveLt c st ns info
  | ns == msVR st =
      ackSoon c st { msVR = msVR st + 1
                   , msRxBuf = msRxBuf st ++ info
                   , msAckRx = msAckRx st + 1
                   , msDupSeen = False
                   , msT403At = maybe never (msT st +) (mnT403 c) }
  -- A.7.3.2.2: the first repeat of the frame we last took is ignored
  -- outright, and draws no acknowledgement.  Answering it is what turns
  -- one duplicate into an unending exchange.
  | ns == msVR st - 1 && not (msDupSeen st) = st { msDupSeen = True }
  -- anything else is out of sequence: the information is dropped and the
  -- far end is told at once where we actually are
  | otherwise = forceAck c st

receiveLa :: MnpState -> Word8 -> Word8 -> MnpState
receiveLa st nr nk
  -- A.7.3.5(a): an acknowledgement repeating the previous N(R) is an
  -- implicit negative acknowledgement.  It only means that when something
  -- is actually outstanding: an idle far end repeats N(R) every time its
  -- forced acknowledgement timer expires, and reading those as losses
  -- would walk the retransmission count up to N400 on a healthy link.
  | Just nr == msLastNR st && not (null (msUnacked st)) = retransmit st { msPeerNk = nk }
  | otherwise =
      let unacked' = [ f | f@(ns, _) <- msUnacked st, not (acked nr ns) ]
          st' = st { msUnacked = unacked', msPeerNk = nk, msLastNR = Just nr
                   , msRetries = 0 }
      in st' { msT401At = if null unacked' && null (msOutQ st') then never else msT401At st' }

-- | Go-back-N: everything not yet acknowledged goes back on the queue, in
-- order, ahead of anything new.  There is no selective reject.
retransmit :: MnpState -> MnpState
retransmit st = st
  { msOutQ = msUnacked st ++ msOutQ st
  , msUnacked = []
  , msRetries = if null (msUnacked st) then msRetries st else msRetries st + 1
  , msT401At = never
  }

-- | Note that an acknowledgement is owed, and start the delay within
-- which it must go out.
ackSoon :: MnpConfig -> MnpState -> MnpState
ackSoon c st
  | msT402At st < never = st
  | otherwise = st { msT402At = msT st + 0.5 * t401Of c (msNeg st) }

-- | An acknowledgement is owed now, whatever the count of frames taken.
-- An out-of-sequence frame increments nothing, so without this the far
-- end would hear nothing back until the forced timer expired seconds
-- later, by which time its window has filled and it has stopped sending.
forceAck :: MnpConfig -> MnpState -> MnpState
forceAck _ st = st { msAckForce = True }

-- | Queue a frame that is not subject to the transmit window.
sendCtrl :: MnpState -> MnpFrame -> MnpState
sendCtrl st f = st { msOutCtrl = msOutCtrl st ++ [f] }

-- | Everything the clock decides: the two ways establishment can end, the
-- retransmission timer, the acknowledgement timers and the inactivity
-- timer.
timers :: MnpConfig -> MnpState -> [Word8] -> (MnpState, [MnpEvent])
timers c st plain = case msPhase st of
  MnpEstablish
    -- The far end is sending ordinary characters rather than link
    -- requests, so there is no protocol over there to wait for.
    --
    -- What it sends has to actually look like characters.  On a noisy
    -- line a far end that does speak the protocol produces nothing but
    -- junk -- its link requests arrive too damaged to frame -- and
    -- counting octets alone would read that as a modem with no error
    -- correction and give up on it precisely when it is needed most.  A
    -- damaged frame is likewise evidence of a protocol, not of its
    -- absence.
    | mnDataDetect c && not (msSawGood st) && not (msSawBad st)
    , length plain > 16, looksLikeText plain -> fallThrough st
    | msRole st == MnpInitiator && msLrTries st == 0 ->
        ( (sendCtrl st (FrLR (offerLr c)))
            { msLrTries = 1, msT401At = msT st + mnT401Lr c }, [] )
    | msT st >= msT401At st ->
        if msLrTries st > 0 && msLrTries st < mnLrTries c
          then ( (sendCtrl st (FrLR (if msRole st == MnpResponder then msNeg st else offerLr c)))
                   { msLrTries = msLrTries st + 1
                   , msT401At = msT st + mnT401Lr c }, [] )
          -- A.7.2.2: damaged frames were seen, so there is a protocol over
          -- there and it is worth saying goodbye; silence means there
          -- never was one, and then no disconnect may be sent at all
          else if msSawBad st || msSawGood st
            then ( (sendCtrl st (FrLD 1 Nothing)) { msPhase = MnpClosed }
                 , [MnpDown "no reply to the link request"] )
            else fallThrough st
    | otherwise -> (st, [])
  MnpData
    | msT st >= msT403At st ->
        ( (sendCtrl st (FrLD 5 Nothing)) { msPhase = MnpClosed }
        , [MnpDown "inactivity timer expired"] )
    | msRetries st > 12 ->
        ( (sendCtrl st (FrLD 4 Nothing)) { msPhase = MnpClosed }
        , [MnpDown "retransmission limit reached"] )
    | msT st >= msT401At st && not (null (msUnacked st)) -> (retransmit st, [])
    | otherwise -> (st, [])
  _ -> (st, [])
  where
    -- the far end has no error-correcting protocol.  A.7.2.2 is explicit
    -- that nothing is transmitted here: the connection simply carries on
    -- unprotected, and everything heard so far was data all along.
    fallThrough s =
      ( s { msPhase = MnpTransparent
          , msRxBuf = msRxBuf s ++ mode2Junk (msRx2 s)
          , msRx2 = mode2RxInit }
      , [MnpTransparentFallback] )

-- | Decide what actually goes on the line this block, hand the terminal
-- what has been reassembled, and account for the credit in both
-- directions.
emit :: MnpConfig -> MnpState -> MnpIn -> (MnpState, MnpOut)
emit c st inp =
  let n401 = max 1 (lrN401 (msNeg st))
      k = max 1 (lrK (msNeg st))
      window = fromIntegral (min k (msPeerNk st)) :: Int

      -- cut new information fields out of what the terminal typed, as far
      -- as the window allows.  A zero-length field is not a legal LT.
      (queued, txBuf', vs') = fill (msOutQ st) (msTxBuf st) (msVS st)
      fill q buf v
        | msPhase st /= MnpData = (q, buf, v)
        | length q + length (msUnacked st) >= window = (q, buf, v)
        | null buf = (q, buf, v)
        | otherwise =
            let (chunk, rest) = splitAt n401 buf
            in fill (q ++ [(v, chunk)]) rest (v + 1)

      -- one information frame per block, and only once the transmitter has
      -- drained.  That keeps the retransmission timer honest: it is meant
      -- to measure the far end's silence, not our own backlog.
      (sendLt, outQ') = case queued of
        (f : rest) | miTxPending inp == 0 && msPhase st == MnpData -> (Just f, rest)
        _ -> (Nothing, queued)

      unacked' = msUnacked st ++ maybe [] (: []) sendLt
      -- A.7.3.1: the timer starts when transmission of an information
      -- frame begins.  Re-arming it on every block instead would slide
      -- the deadline forward for as long as anything is outstanding, so
      -- it could never expire and nothing would ever be retransmitted.
      t401'
        | null unacked' = never
        | Just _ <- sendLt = msT st + t401Of c (msNeg st)
        | otherwise = msT401At st

      -- what the terminal will take, and therefore what credit we can
      -- advertise back to the far end
      (toDte, rxBuf') = splitAt (max 0 (miDteReady inp)) (msRxBuf st)
      buffered = (length rxBuf' + n401 - 1) `div` n401
      credit = fromIntegral (max 0 (min (fromIntegral k) (mnRxWindow c - buffered))) :: Word8

      -- an acknowledgement is owed when the far end is waiting on one, when
      -- half the window has gone unacknowledged, when the forced timer
      -- expires, or when credit has just come back after reaching zero.
      -- That last case is the one that would otherwise deadlock: with no
      -- credit the far end sends nothing, so nothing prompts the
      -- acknowledgement that would restore it.
      reopened = credit > 0 && msLastNk st == 0
      ackNow = msPhase st == MnpData &&
        ( msAckForce st
       || reopened
       || (msAckRx st > 0 && msT st >= msT402At st)
       || (msAckRx st > 0 && msAckRx st >= max 1 (fromIntegral k `div` 2))
       || (msAckRx st > 0 && null txBuf' && null outQ')
       || msT st >= msT404At st
       || credit == 0 && msLastNk st > 0 )

      ackFrames = [ FrLA (msVR st - 1) credit | ackNow ]
      ctrl = msOutCtrl st ++ ackFrames

      frames = ctrl ++ maybe [] (\(ns, info) -> [FrLT ns info]) sendLt
      lineOut = encodeAll (msFraming st) (msDpo st) frames

      st' = st
        { msOutCtrl = []
        , msOutQ = outQ'
        , msUnacked = unacked'
        , msTxBuf = txBuf'
        , msVS = vs'
        , msRxBuf = rxBuf'
        , msFraming = msNextFraming st
        , msT401At = if msPhase st == MnpData then t401' else msT401At st
        , msAckRx = if ackNow then 0 else msAckRx st
        , msAckForce = if ackNow then False else msAckForce st
        , msT402At = if ackNow then never else msT402At st
        , msT404At = if ackNow then msT st + t404 c else msT404At st
        , msLastNk = if ackNow then credit else msLastNk st
        }
  in (st', MnpOut lineOut toDte [] (length unacked' >= window))

encodeAll :: MnpFraming -> Bool -> [MnpFrame] -> MnpLineOut
encodeAll FramingOctet dpo fs = OutOctets (concatMap (mode2Encode . encodeFrame dpo) fs)
encodeAll FramingBit dpo fs = OutBits (concatMap (mode3Encode . encodeFrame dpo) fs)
