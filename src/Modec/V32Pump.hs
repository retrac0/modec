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
  , encodeSymbols
  , RxCoder
  , rxCoderInit
  , decodeSymbols
    -- * Start-up signalling
  , conditioningSymbols
  , stateSymbols
    -- * Offline helpers
  , v32Modulate
  , v32ModulateTrained
  , v32Demodulate
  , v32DemodulateWith
  , v32DemodulateTrained
  ) where

import Modec.DSP (Signal)
import Modec.QAM
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
v32RxCfg r = defaultRxCfg (slicePoint r) (constellation r)

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

codeSymbol :: V32Rate -> TxCoder -> [Bool] -> (TxCoder, Point)
codeSymbol V32R4800 st [q1, q2] =
  let y = diffEncode1 (q1, q2) (tcPrev st)
  in (st { tcPrev = y }, statePoint (stateOfDibit y))
codeSymbol V32R9600 st [q1, q2, q3, q4] =
  let y@(y1, y2) = diffEncode1 (q1, q2) (tcPrev st)
  in (st { tcPrev = y }, constellation V32R9600 (packBits [y1, y2, q3, q4]))
codeSymbol V32R9600T st [q1, q2, q3, q4] =
  let y@(y1, y2) = diffEncode2 (q1, q2) (tcPrev st)
      (cv, y0) = convStep (tcConv st) y
  in (st { tcPrev = y, tcConv = cv }, constellation V32R9600T (packBits [y0, y1, y2, q3, q4]))
codeSymbol _ _ _ = error "Modec.V32Pump.codeSymbol: wrong group size"

packBits :: [Bool] -> Int
packBits = foldl (\acc b -> acc * 2 + (if b then 1 else 0)) 0

scrambleRun :: Direction -> Scrambler -> [Bool] -> (Scrambler, [Bool])
scrambleRun dir = go []
  where
    go acc sc [] = (sc, reverse acc)
    go acc sc (b : bs) = let (sc', o) = scrambleBit dir sc b in go (o : acc) sc' bs

descrambleRun :: Direction -> Scrambler -> [Bool] -> (Scrambler, [Bool])
descrambleRun dir = go []
  where
    go acc sc [] = (sc, reverse acc)
    go acc sc (b : bs) = let (sc', o) = descrambleBit dir sc b in go (o : acc) sc' bs

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
decodeSymbols dir r syms st0 = (st1 { rcDescr = descr' }, dataBits)
  where
    far = case dir of { Calling -> Answering; Answering -> Calling }
    (st1, coded) = case r of
      V32R9600T ->
        let quads = viterbiDecode 16 (map qsPoint syms)
        in foldlAcc (\st (y1, y2, q3, q4) ->
             let (q1, q2) = diffDecode2 (y1, y2) (rcPrev st)
             in (st { rcPrev = (y1, y2) }, [q1, q2, q3, q4])) st0 quads
      V32R9600 ->
        foldlAcc (\st sym ->
          let i = qsIndex sym
              y = unpair (i `div` 4)
              (q3, q4) = unpair (i `mod` 4)
              (q1, q2) = diffDecode1 y (rcPrev st)
          in (st { rcPrev = y }, [q1, q2, q3, q4])) st0 syms
      V32R4800 ->
        foldlAcc (\st sym ->
          let y = dibitOfState (toEnum (qsIndex sym))
              (q1, q2) = diffDecode1 y (rcPrev st)
          in (st { rcPrev = y }, [q1, q2])) st0 syms
    (descr', dataBits) = descrambleRun far (rcDescr st0) coded

unpair :: Int -> (Bool, Bool)
unpair i = (odd (i `div` 2), odd i)

dibitOfState :: TrainState -> (Bool, Bool)
dibitOfState s = case s of
  StA -> (False, False)
  StB -> (False, True)
  StC -> (True, True)
  StD -> (True, False)

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
    cfg = v32RxCfg r
    syms = run (qamRxInit p cfg) sig
    run st s
      | VS.null s = []
      | otherwise =
          let (chunk, rest) = VS.splitAt blk s
              (st', out) = qamRxBlock p cfg chunk st
          in out ++ run st' rest

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
v32DemodulateTrained fs dir r preSyms sig = snd (decodeSymbols dir r syms rxCoderInit)
  where
    p = v32Params fs
    trainCfg = v32RxCfg V32R4800
    dataCfg = v32RxCfg r
    preN = ceiling (fromIntegral preSyms * samplesPerSymbol p)
    (pre, dat) = VS.splitAt preN sig
    stAfter = snd (runBlocks p trainCfg pre (qamRxInit p trainCfg))
    syms = fst (runBlocks p dataCfg dat stAfter)

runBlocks :: QamParams -> QamRxCfg -> Signal -> QamRxState -> ([QamSym], QamRxState)
runBlocks p cfg = go
  where
    go s st
      | VS.null s = ([], st)
      | otherwise =
          let (chunk, rest) = VS.splitAt 160 s
              (st', out) = qamRxBlock p cfg chunk st
              (more, stF) = go rest st'
          in (out ++ more, stF)
