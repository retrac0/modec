{-# LANGUAGE BangPatterns #-}
-- | V.32 on the line: the coding layer of "Modec.V32" carried by the
-- pump of "Modec.QAM", at 2400 baud on an 1800 Hz carrier.
--
-- Kept apart from "Modec.V32" so that module stays free of any sample
-- rate.  Everything the Recommendation states as a table can then be
-- tested with no DSP underneath it, which is where most of the ways to
-- get V.32 wrong live.
module Modec.V32Pump
  ( -- * Line parameters
    v32Params
  , v32RxCfg
  , v32RollOff
    -- * Coding to and from symbols
  , TxCoder
  , txCoderInit
  , txCoderFrom
  , encodeSymbols
  , RxCoder
  , rxCoderInit
  , decodeSymbols
  , Coded
  , codedQuads
  , decodeQuads
    -- * Start-up signalling
  , conditioningSymbols
  , stateSymbols
  , modulatePointsFor
    -- * A duplex data pump
  , V32Data
  , v32DataInit
  , v32DataFrom
  , v32DataRx
  , v32DataTx
  , v32DataEvm
    -- * Offline helpers
  , v32Modulate
  , v32ModulateTrained
  , v32Demodulate
  , v32DemodulateWith
  , v32DemodulateTrained
  , v32DemodulateTrainedWith
  ) where

import Modec.DSP (Signal, chunksOf)
import Modec.QAM
import Modec.Stream (concatStage)
import Modec.V32

import qualified Data.Vector.Storable as VS

-- | V.32 does not name a shaping filter; §2.2 constrains the transmitted
-- spectrum instead, requiring the energy density at 600 and 3000 Hz --
-- the edges of the 2400 baud Nyquist band around 1800 Hz -- to be down
-- 4.5 +/- 2.5 dB.  A root raised cosine is 3 dB down there whatever its
-- roll-off, so the roll-off is ours to choose; 0.25 puts the signal in
-- 300 to 3300 Hz, which is the whole telephone band and no more.
v32RollOff :: Double
v32RollOff = 0.25

-- The pulse is truncated at 12 symbols each side rather than V.22's 6.
-- A root raised cosine with a 0.25 roll-off has tails that fall away
-- like 1\/t^2, and cutting them at 6 symbols leaves enough intersymbol
-- interference to cost 13 dB of implementation signal to noise ratio --
-- invisible at 4800 bit\/s, where the four points are far apart, and
-- fatal at 9600.  V.22 gets away with 6 because its 0.75 roll-off decays
-- far faster, which is exactly the kind of constant that does not
-- survive being carried between modes.
v32Params :: Double -> QamParams
v32Params fs = QamParams
  { qpFs = fs, qpBaud = 2400, qpCarrier = 1800
  , qpRollOff = v32RollOff, qpSpan = 12 }

-- | The receiver's view of a rate.  On the trellis alternative the
-- slicer here is the immediate one: it feeds the carrier and timing
-- loops, which cannot wait for a traceback, while the Viterbi decoder
-- runs behind it on the same symbols.
v32RxCfg :: V32Rate -> QamRxCfg
v32RxCfg r
  | rateTrellis r = base { qrThKp = 0.03 }
  | otherwise = base
  where
    base = defaultRxCfg (slicePoint r) (constellation r)
    -- Two carrier loops, one per stage.  The four-point receiver that
    -- runs the start-up acquires with the fast gain, which reaches +/-18
    -- Hz and follows timing wander; the trellis rates then inherit that
    -- receiver already locked, and track with a gain small enough that
    -- the Viterbi decoder is not shown the loop's own jitter.  Neither
    -- gain does both jobs: the fast one costs the trellis symbols at
    -- 30 dB, the slow one cannot acquire a 1 % clock offset at all.

-- | Scrambler, differential encoder and (on the trellis alternative)
-- convolutional encoder, with any bits left over from the last block.
data TxCoder = TxCoder
  { tcScr  :: !Scrambler
  , tcPrev :: !(Bool, Bool)
  , tcConv :: !ConvState
  , tcBits :: [Bool]
  }

txCoderInit :: TxCoder
txCoderInit = TxCoder scramblerInit (False, False) convInit []

-- | A data coder that carries on the scrambler the start-up was already
-- running.  §5.4.1 changes the coding at signal E but not the scrambler,
-- and sets the convolutional encoder's delay elements to zero -- so this
-- takes the one and resets the other.
txCoderFrom :: Scrambler -> (Bool, Bool) -> TxCoder
txCoderFrom sc q = TxCoder sc q convInit []

-- | Scramble and code as many whole symbols as the given bits allow,
-- keeping the remainder for next time.
encodeSymbols :: Direction -> V32Rate -> [Bool] -> TxCoder -> (TxCoder, [Point])
encodeSymbols dir r newBits st0 = go st0 { tcBits = [] } (tcBits st0 ++ scrambled) []
  where
    (scr', scrambled) = scrambleRun dir (tcScr st0) newBits
    k = rateBitsPerSymbol r
    go st bits acc
      | length bits < k = (st { tcScr = scr', tcBits = bits }, reverse acc)
      | otherwise =
          let (grp, rest) = splitAt k bits
              (st', p) = codeSymbol r st grp
          in go st' rest (p : acc)

-- | One symbol.  Every rate does the same three things in the same
-- order -- differentially encode the first two bits, run them through
-- the convolutional encoder if this rate is trellis coded, and map the
-- result with whatever bits are left over -- so the only differences are
-- which differential table (Table 1 without trellis coding, Table 2
-- with) and how many bits ride above.
codeSymbol :: V32Rate -> TxCoder -> [Bool] -> (TxCoder, Point)
codeSymbol r st (q1 : q2 : rest)
  | not (rateTrellis r) =
      let y@(y1, y2) = diffEncode1 (q1, q2) (tcPrev st)
      in (st { tcPrev = y }, constellation r (bitsToInt ([y1, y2] ++ rest)))
  | otherwise =
      let y@(y1, y2) = diffEncode2 (q1, q2) (tcPrev st)
          (cv, y0) = convStep (tcConv st) y
      in (st { tcPrev = y, tcConv = cv }, constellation r (bitsToInt ([y0, y1, y2] ++ rest)))
codeSymbol _ _ _ = error "Modec.V32Pump.codeSymbol: short group"

-- | One symbol's coded content: the differentially encoded pair, then
-- whatever uncoded bits the rate carries above it.
type Coded = (Bool, Bool, [Bool])

data RxCoder = RxCoder
  { rcDescr :: !Scrambler
  , rcPrev  :: !(Bool, Bool)
  }

rxCoderInit :: RxCoder
rxCoderInit = RxCoder scramblerInit (False, False)

-- | Turn decided symbols into data bits.  The trellis alternative is
-- decoded from the equalised points rather than the immediate
-- decisions, which is the whole point of it.
--
-- The far end scrambles with the polynomial of /its/ direction, so the
-- descrambler here is given the other one.
decodeSymbols :: Direction -> V32Rate -> [QamSym] -> RxCoder -> (RxCoder, [Bool])
decodeSymbols dir r syms st = decodeQuads dir r (codedQuads r syms) st

-- | Decided symbols to the coded bits they carry, before any of the
-- undoing: on the trellis alternative that is the Viterbi decoder's job
-- and needs a run of symbols, on the others it is one slicing per
-- symbol.
--
-- This is separate from 'decodeQuads' because the two have opposite
-- needs at a block boundary.  A trellis decoder wants the previous
-- block's last symbols back, to keep its path metrics continuous; a
-- descrambler must see every line bit exactly once, and showing it the
-- overlap a second time puts it out of step with the far end -- which
-- looks like a receiver that locks perfectly and then decodes noise.
codedQuads :: V32Rate -> [QamSym] -> [Coded]
codedQuads r syms
  | rateTrellis r = viterbiDecode r viterbiDepth (map qsPoint syms)
  | otherwise =
      [ let i = qsIndex sym
            k = rateBitsPerSymbol r
            bits = [ odd (i `div` (2 ^ b)) | b <- reverse [0 .. k - 1] ]
        in case bits of
             (y1 : y2 : rest) -> (y1, y2, rest)
             _ -> (False, False, [])
      | sym <- syms ]

-- | Coded bits to data bits: undo the differential encoding, then the
-- scrambler.  Stateful, and advanced exactly once per symbol.
--
-- The far end scrambles with the polynomial of /its/ direction, so the
-- descrambler here is given the other one.
decodeQuads :: Direction -> V32Rate -> [Coded] -> RxCoder -> (RxCoder, [Bool])
decodeQuads dir r quads st0 = (st1 { rcDescr = descr' }, dataBits)
  where
    far = case dir of { Calling -> Answering; Answering -> Calling }
    diff = if rateTrellis r then diffDecode2 else diffDecode1
    (st1, coded) = foldlAcc (\st (y1, y2, rest) ->
      let (q1, q2) = diff (y1, y2) (rcPrev st)
      in (st { rcPrev = (y1, y2) }, q1 : q2 : rest)) st0 quads
    (descr', dataBits) = descrambleRun far (rcDescr st0) coded

foldlAcc :: (s -> a -> (s, [b])) -> s -> [a] -> (s, [b])
foldlAcc f = go []
  where
    go acc st [] = (st, concat (reverse acc))
    go acc st (x : xs) = let (st', bs) = f st x in go (bs : acc) st' xs

-- | A run of one training state, as points.
stateSymbols :: Int -> TrainState -> [Point]
stateSymbols n st = replicate n (statePoint st)

-- | The receiver conditioning signal of §5.2, in full: segment 1 is 256
-- symbols alternating A and B, segment 2 is 16 alternating C and D --
-- the transition between them is the only sharply defined instant in
-- the whole start-up, and is what the far end takes its timing
-- reference from -- and segment 3 is @trn@ symbols of scrambled ones.
--
-- Segment 3 is the one that does the work: §5.2.3 requires at least
-- 1280 symbols of it, and says plainly what they are for -- training the
-- adaptive equaliser at the far end and the echo canceller at this one.
-- A receiver judged on data it was handed cold is being asked to do
-- something the Recommendation never asks of it.
conditioningSymbols :: Direction -> Int -> [Point]
conditioningSymbols dir trn =
  take 256 (cycle [statePoint StA, statePoint StB])
  ++ take 16 (cycle [statePoint StC, statePoint StD])
  ++ map statePoint (trnStates dir trn)

-- | Modulate data behind a full receiver conditioning signal, the way a
-- V.32 modem actually puts data on a line.  Returns the signal and how
-- many symbols precede the data.
v32ModulateTrained :: Double -> Direction -> V32Rate -> Double -> Int -> [Bool] -> (Signal, Int)
v32ModulateTrained fs dir r amp trn bits = (modulatePoints fs amp (pre ++ pts), length pre)
  where
    pre = conditioningSymbols dir trn
    (_, pts) = encodeSymbols dir r bits txCoderInit

-- | Points on the line at 8 kHz and the usual level, for tests and
-- for generating the start-up signals offline.
modulatePointsFor :: [Point] -> Signal
modulatePointsFor = modulatePoints 8000 0.5

modulatePoints :: Double -> Double -> [Point] -> Signal
modulatePoints fs amp pts0 = go qamTxInit pts0 []
  where
    p = v32Params fs
    go _ [] acc = VS.concat (reverse acc)
    go st syms acc =
      let (st', sig, rest) = qamTxBlock p amp 160 syms st
      in if length rest == length syms
           then VS.concat (reverse (sig : acc))
           else go st' rest (sig : acc)

-- | Modulate data bits into a signal.  Offline: the whole thing at once.
v32Modulate :: Double -> Direction -> V32Rate -> Double -> [Bool] -> Signal
v32Modulate fs dir r amp bits = modulatePoints fs amp (snd (encodeSymbols dir r bits txCoderInit))

-- | Demodulate a whole signal back to data bits.
v32Demodulate :: Double -> Direction -> V32Rate -> Signal -> [Bool]
v32Demodulate fs dir r = v32DemodulateWith fs dir r 160

v32DemodulateWith :: Double -> Direction -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateWith fs dir r blk sig = snd (decodeSymbols dir r syms rxCoderInit)
  where
    p = v32Params fs
    syms = concatStage (qamReceiver p (v32RxCfg r)) (chunksOf blk sig)

-- | Demodulate a signal whose first @preSyms@ symbols are the receiver
-- conditioning signal, as 'v32ModulateTrained' produces and as the
-- start-up of §5.4 sends.
--
-- The slicer changes when the data does.  While the conditioning signal
-- is arriving the line carries only the four states A, B, C and D, so
-- the receiver must decide against those four and not against the data
-- constellation -- a 16- or 32-point slicer fed a four-point signal
-- reports errors that are not there, and an equaliser adapting on them
-- diverges rather than converges.  This is why a V.32 receiver has to
-- know where it is in the start-up, and cannot simply be pointed at the
-- line.
v32DemodulateTrained :: Double -> Direction -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateTrained = v32DemodulateTrainedWith id

-- | The same, with the receiver's tuning adjusted -- for finding out
-- what the tuning should be.
v32DemodulateTrainedWith :: (QamRxCfg -> QamRxCfg) -> Double -> Direction -> V32Rate -> Int -> Signal -> [Bool]
v32DemodulateTrainedWith tune fs dir r preSyms sig = snd (decodeSymbols dir r syms rxCoderInit)
  where
    p = v32Params fs
    trainCfg = tune (v32RxCfg V32R4800)
    dataCfg = tune (v32RxCfg r)
    preN = ceiling (fromIntegral preSyms * samplesPerSymbol p)
    (pre, dat) = VS.splitAt preN sig
    stAfter = snd (runBlocks p trainCfg pre (qamRxInit p trainCfg))
    syms = fst (runBlocks p dataCfg dat stAfter)

-- | Drive the receiver over a whole signal, returning the state it ends
-- in as well as the symbols.  The trained start-up needs that state to
-- hand to a second pass under a different slicer, which is the one thing
-- a 'Stage' cannot give back, so this stays a plain fold.
runBlocks :: QamParams -> QamRxCfg -> Signal -> QamRxState -> ([QamSym], QamRxState)
runBlocks p cfg sig st0 = go st0 (chunksOf 160 sig)
  where
    go st [] = ([], st)
    go st (c : cs) =
      let (st', out) = qamRxBlock p cfg c st
          (more, stF) = go st' cs
      in (out ++ more, stF)

-- | A V.32 data pump for the connected state: a block of audio in, a
-- block out, bits both ways.
data V32Data = V32Data
  { vdTx    :: !QamTxState
  , vdCode  :: !TxCoder
  , vdRx    :: !QamRxState
  , vdDec   :: !RxCoder
  , vdPend  :: [QamSym]   -- ^ symbols held back for the decoder's traceback
  , vdWait  :: [QamSym]   -- ^ symbols decoded but held back for traceback
  , vdBits  :: [Bool]     -- ^ data bits not yet coded onto symbols
  }

v32DataInit :: Double -> V32Rate -> V32Data
v32DataInit fs r = V32Data
  { vdTx = qamTxInit, vdCode = txCoderInit
  , vdRx = qamRxInit (v32Params fs) (v32RxCfg r), vdDec = rxCoderInit
  , vdPend = [], vdWait = [], vdBits = [] }

-- | A data pump that inherits a receiver and transmitter the start-up
-- has already brought into lock.
v32DataFrom :: QamRxState -> QamTxState -> TxCoder -> V32Data -> V32Data
v32DataFrom rx tx code st = st { vdRx = rx, vdTx = tx, vdCode = code }

v32DataEvm :: V32Data -> Double
v32DataEvm = qamRxEvm . vdRx

-- | How many symbols of context the decoder needs behind it.  A trellis
-- decoder judges a sequence, so restarting it at every block boundary
-- would throw away the very thing it is for; carrying the last few
-- symbols forward and decoding them again keeps the path metrics
-- continuous across a seam the far end knows nothing about.
vdOverlap :: Int
vdOverlap = 40

-- | Traceback depth, in symbols.  The same number the offline decoder
-- uses, and the number of symbols the block decoder must hold back.
viterbiDepth :: Int
viterbiDepth = 16

-- | Receive one block.
--
-- Transmit and receive are separate calls on purpose.  A modem does both
-- every block, but not at the same point in the block: the receiver runs
-- at the top, on audio that has arrived, and the transmitter at the
-- bottom, on bits the protocol layer has by then decided to send.  One
-- function doing both has to be handed a dummy for whichever half the
-- caller does not mean, and it then advances that half's state anyway --
-- which is silent, and costs every byte on the link.
v32DataRx :: Double -> Direction -> V32Rate -> V32Data -> Signal -> (V32Data, [Bool])
v32DataRx fs dir r st rx = (st', out)
  where
    p = v32Params fs
    (rx', syms) = qamRxBlock p (v32RxCfg r) rx (vdRx st)
    -- A Viterbi decoder is only sure of a symbol once it has seen the
    -- traceback's worth of symbols after it.  Emitting a block's newest
    -- symbols the moment they arrive therefore hands out precisely the
    -- decisions the decoder has not finished making -- and then, next
    -- block, re-decodes them properly and throws the good answer away.
    -- At 9600 the difference rarely shows; at 12000 and 14400 it is the
    -- difference between a working link and a stream of noise.
    depth = if rateTrellis r then viterbiDepth else 0
    stream = vdPend st ++ vdWait st ++ syms
    nPend = length (vdPend st)
    emitTo = max nPend (length stream - depth)
    fresh = take (emitTo - nPend) (drop nPend (codedQuads r stream))
    (dec', out) = decodeQuads dir r fresh (vdDec st)
    st' = st { vdRx = rx', vdDec = dec'
             , vdWait = drop emitTo stream
             , vdPend = lastN vdOverlap (take emitTo stream) }

-- | Transmit @n@ samples, carrying as many of @bits@ as will fit.  What
-- does not fit stays in the coder, so nothing has to be handed back.
v32DataTx :: Double -> Direction -> V32Rate -> Double -> Int -> [Bool] -> V32Data
          -> (V32Data, Signal)
v32DataTx fs dir r amp n bits st = (st { vdTx = tx', vdCode = code', vdBits = keep }, audio)
  where
    p = v32Params fs
    want = qamTxSymbolsFor p n (vdTx st)
    needed = want * rateBitsPerSymbol r
    -- Code exactly the symbols this block will carry, and no more.
    -- encodeSymbols will happily turn every bit it is given into a
    -- symbol, and qamTxBlock takes only the ones it has room for and
    -- hands the surplus back -- so a burst larger than one block, which
    -- is any burst at all at 9600 bit/s, is coded and then dropped on
    -- the floor.  Holding the bits here instead means the queue drains
    -- over as many blocks as it takes.
    pendingAll = vdBits st ++ bits
    (send, keep) = splitAt needed pendingAll
    -- Idle on scrambled ones when there is nothing to say: that is what
    -- keeps the far end's carrier, timing and equaliser alive between
    -- characters.
    idle = replicate (max 0 (needed - length send)) True
    (code', pts) = encodeSymbols dir r (send ++ idle) (vdCode st)
    (tx', audio, _) = qamTxBlock p amp n pts (vdTx st)

lastN :: Int -> [a] -> [a]
lastN k xs = drop (max 0 (length xs - k)) xs
