-- | Calls that never touch a line: two state machines connected by
-- audio, in blocks, through the channel simulator.
module Harness (Side (..), simulateCall, callerAgainstU11, runLength, modemDuplex, grace, modemDuplexCut, modemDuplexFor, modemDuplexDisturb, modemDuplexEcho, modemDuplexStream) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Modec.Detect
import Modec.Handshake
import Modec.DSP
import Modec.Modem
import Modec.V22
import Modec.Stream
import Modec.Standards

-- | Duplex call simulation: two handshake state machines connected by
-- attenuated, noisy audio in 20 ms blocks.
data Side = Side { sdPhase :: Double, sdBank :: Stage Signal [ToneFrame], sdHs :: HsState, sdTrace :: [(Double, TxCmd)], sdStatus :: HsStatus, sdTx :: TxCmd }

simulateCall :: HsConfig -> HsConfig -> Double -> Double -> (Side, Side)
simulateCall cfgO cfgA snr maxT = go 0 (side cfgO) (side cfgA)
  where
    fs = 8000
    blk = 160 :: Int
    side cfg = Side 0 (toneBank fs (hcBank cfg)) (initialHandshake cfg) [] HsBusy TxSilence
    go t o a
      | t >= maxT = (o, a)
      | connected (sdStatus o) && connected (sdStatus a) && t > stopAfter = (o, a)
      | otherwise =
          let (audioO, o1) = gen o
              (audioA, a1) = gen a
              o2 = recv cfgO t o1 (impair 1 t audioA)
              a2 = recv cfgA t a1 (impair 2 t audioO)
          in go (t + fromIntegral blk / fs) o2 a2
      where stopAfter = maybe maxT (+ 1) (connectTime a)
    connected (HsConnected {}) = True
    connected _ = False
    connectTime s = case [ tm | (tm, TxData _) <- sdTrace s ] of
      [] -> Nothing
      ts -> Just (minimum ts)
    -- 20 dB loss and additive noise, deterministic per block
    -- 20 dB loss, then noise for the requested SNR relative to a 0.5 amplitude tone
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    gen s =
      let f = case sdTx s of
            TxSilence -> 0
            TxTone x -> x
            TxMark sp -> fskMark sp
            TxData sp -> fskMark sp
            TxV22 ch _ _ -> if ch == HighChannel then 2250 else 1050   -- unscrambled ones, as a tone
            TxDual f1 _ _ -> f1
            TxBits sp _ -> fskMark sp
          w = 2 * pi * f / fs
          sig = VS.generate blk (\i -> if f == 0 then 0 else 0.5 * sin (sdPhase s + w * fromIntegral i))
          ph = sdPhase s + w * fromIntegral blk
      in (sig, s { sdPhase = ph - 2 * pi * fromIntegral (floor (ph / (2 * pi)) :: Int) })
    recv cfg t s audio =
      case sdBank s of
       Stage bst bstep ->
        let (bst', frames) = bstep bst audio
            (hs', outs) = foldl (\(h, acc) fr -> let (h', o) = handshakeStep cfg h fr noHsIn in (h', acc ++ [(hoTx o, hoStatus o)])) (sdHs s, []) frames
            s' = s { sdBank = Stage bst' bstep, sdHs = hs' }
        in case outs of
             [] -> s'
             _ -> let (tx, st) = last outs
                  in s' { sdTx = tx, sdStatus = if st == HsBusy then sdStatus s else st, sdTrace = sdTrace s ++ [ (t, tx) | tx /= sdTx s ] }

-- | Drive the calling side against a synthetic answering modem that
-- follows 6.3.1.2: it sends unscrambled binary 1 and waits to be
-- answered in kind, and never sends scrambled ones.  That is what the
-- recorded BBS in docs/recordings/v32 does.
--
-- Returns the transmit command timeline.
callerAgainstU11 :: [Standard] -> Double -> [(Double, TxCmd)]
callerAgainstU11 modes maxT = go 0 (toneBank fs (hcBank cfg)) (initialHandshake cfg) TxSilence []
  where
    fs = 8000
    blk = 160 :: Int
    cfg = (withModes modes (defaultHsConfig Originate)) { hcV8bis = False }
    -- 2100 Hz answer tone for three seconds, then the answerer's
    -- unscrambled binary 1 on the high channel, for ever
    audioAt t
      | t < 3.0 = tone 2100 t
      | otherwise = tone 2250 t
    tone f t = VS.generate blk (\i -> 0.35 * sin (2 * pi * f * (t + fromIntegral i / fs)))
    -- what the V.22 receiver makes of it: unscrambled ones once the
    -- answer tone is over, and never a scrambled one
    pumpAt t
      | t < 3.0 = V22Report 0.02 90 0 0 0 0 0
      | otherwise = V22Report 0.02 1 400 0 0 0 0
    go t bank hs lastTx acc
      | t >= maxT = acc
      | otherwise = case bank of
          Stage bst bstep ->
            let (bst', frames) = bstep bst (audioAt t)
                inp = noHsIn { hiPump = Just (pumpAt t) }
                (hs', outs) = foldl (\(h, o) fr -> let (h2, o2) = handshakeStep cfg h fr inp in (h2, o ++ [hoTx o2])) (hs, []) frames
                tx = case outs of { [] -> lastTx; _ -> last outs }
                acc' = acc ++ [ (t, tx) | tx /= lastTx ]
            in go (t + fromIntegral blk / fs) (Stage bst' bstep) hs' tx acc'

-- | Duration of the first run of a given transmit command in a trace.
runLength :: (TxCmd -> Bool) -> [(Double, TxCmd)] -> Maybe Double
runLength p tr = case dropWhile (not . p . snd) tr of
  ((t0, _) : rest) -> case dropWhile (p . snd) rest of
    ((t1, _) : _) -> Just (t1 - t0)
    [] -> Nothing
  [] -> Nothing

-- | Two complete modems talking through attenuated, noisy audio in
-- 20 ms blocks; text is queued on both sides once connected.
-- | @maxT@ is a budget, not a duration: a call that has carried both
-- texts has shown what it was asked to show, and the remaining thirty
-- seconds of a V.32 call cost more than everything else in this suite
-- put together.  'grace' keeps simulating for a while after the last
-- expected byte, so trailing rubbish still has somewhere to appear.
modemDuplex :: ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8] -> Double -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplex cfgO cfgA snr textO textA maxT = modemDuplexFor (Just grace) cfgO cfgA snr textO textA maxT (maxT * 2)

-- | Seconds of call to keep simulating after both directions have
-- delivered everything they were given.
grace :: Double
grace = 2

-- | As 'modemDuplex', but the answerer's audio stops at @cutAt@, the way
-- a far end that hangs up stops.  This one runs the whole budget: what
-- it is looking for is what arrives /after/ the text, so it cannot stop
-- when the text is complete.
modemDuplexCut :: ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8] -> Double -> Double -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexCut = modemDuplexFor Nothing

modemDuplexFor :: Maybe Double -> ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8] -> Double -> Double -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexFor stop cfgO cfgA snr textO textA maxT cutAt =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0) False False Nothing [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    go t so sa fromA fromO sentO sentA fullAt rxO rxA evO evA
      | t >= maxT = out
      | Just g <- stop, Just t0 <- fullAt, t - t0 >= g = out
      | otherwise =
          let queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              heard = if t >= cutAt then VS.replicate (VS.length fromA) 0 else fromA
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t heard) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t fromO) queueA
              rxO' = reverse bytesO ++ rxO
              rxA' = reverse bytesA ++ rxA
              sentO' = sentO || not (null queueO)
              sentA' = sentA || not (null queueA)
              full = sentO' && sentA'
                     && length rxO' >= length textA && length rxA' >= length textO
              fullAt' = case fullAt of
                          Just _ -> fullAt
                          Nothing -> if full then Just t else Nothing
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO sentO' sentA' fullAt'
                rxO' rxA' (reverse eO ++ evO) (reverse eA ++ evA)
      where out = (reverse rxO, reverse rxA, reverse evO, reverse evA)

-- | A call that is wrecked for a while and has to put itself back
-- together: 5.5's retrain, provoked.
--
-- Text goes both ways twice, once before the disturbance and once after,
-- so the test can tell a link that survived from one that merely
-- reported something.  The disturbance is noise loud enough that no
-- receiver could read through it, applied to one direction only, which
-- is what a burst of impulse noise or a route change looks like.
modemDuplexDisturb :: ModemConfig -> ModemConfig -> Double -> [Word8] -> [Word8]
                   -> Bool -> Double -> Double
                   -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexDisturb cfgO cfgA snr first second atCaller from until_ =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0)
     False False False False [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    maxT = 90
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    wreck t x
      | t >= from && t < until_ = addNoise (round (t * 977)) 0.5 x
      | otherwise = x
    go t so sa fromA fromO s1o s1a s2o s2a rxO rxA evO evA
      | t >= maxT = out
      | otherwise =
          let up o = modemConnected o
              -- the first text as soon as we are up, the second once the
              -- disturbance is over and done with
              qO | up so, not s1o = first
                 | up so, s1o, not s2o, t >= until_ + 3 = second
                 | otherwise = []
              qA | up sa, not s1a = first
                 | up sa, s1a, not s2a, t >= until_ + 3 = second
                 | otherwise = []
              -- which end is made deaf, and so which end asks for the
              -- retrain: the other has to notice from the tone alone
              (so', audioO, bytesO, eO) =
                modemStep cfgO so (side atCaller (impair 1 t fromA)) qO
              (sa', audioA, bytesA, eA) =
                modemStep cfgA sa (side (not atCaller) (impair 2 t fromO)) qA
              side yes x = if yes then wreck t x else x
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO
                (s1o || not (null qO)) (s1a || not (null qA))
                (s2o || (s1o && not (null qO))) (s2a || (s1a && not (null qA)))
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA)
                (reverse eO ++ evO) (reverse eA ++ evA)
      where out = (reverse rxO, reverse rxA, reverse evO, reverse evA)

-- | As 'modemDuplex', but each side also hears its own transmit coming
-- back at it.  That is far-end echo: our signal reflected by the hybrid
-- at the other end of the line, which nothing at either end removes --
-- the far modem's own canceller subtracts its /own/ transmit from its
-- /own/ receiver, and our reflection happens outside that loop.  It is
-- the echo Modec.Echo exists for, and the one a V.32 call meets on any
-- real line, because both directions share the 1800 Hz carrier.
--
-- The taps are delays in samples and linear gains, fractional delays
-- included, because a hybrid returns a dispersive smear rather than one
-- clean reflection.
modemDuplexEcho :: [(Double, Double)] -> ModemConfig -> ModemConfig -> Double
                -> [Word8] -> [Word8] -> Double
                -> ([Word8], [Word8], [ModemEvent], [ModemEvent], Double, Double)
modemDuplexEcho taps cfgO cfgA snr textO textA maxT =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0)
     (replicate hist quiet) (replicate hist quiet) False False Nothing [] [] [] [] 0 0
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    quiet = VS.replicate blk 0
    -- enough transmit history behind us to cover the longest tap
    hist = 4 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    -- the echo of our own earlier transmits arriving during this block;
    -- index n0 is the first sample of the block being received now, so
    -- every tap reaches back into the history
    echoNow h =
      let ext = VS.concat h
          n0 = VS.length ext
      in VS.generate blk $ \i ->
           sum [ g * sampleAt ext (fromIntegral (n0 + i) - d) | (d, g) <- taps ]
    -- the same budget-not-duration rule as 'modemDuplex', and for the
    -- same reason: these are the most expensive calls in the suite
    go t so sa fromA fromO hO hA sentO sentA fullAt rxO rxA evO evA erO erA
      | t >= maxT = out
      | Just t0 <- fullAt, t - t0 >= grace = out
      | otherwise =
          let queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              heardO = VS.zipWith (+) fromA (echoNow hO)
              heardA = VS.zipWith (+) fromO (echoNow hA)
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t heardO) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t heardA) queueA
              rxO' = reverse bytesO ++ rxO
              rxA' = reverse bytesA ++ rxA
              sentO' = sentO || not (null queueO)
              sentA' = sentA || not (null queueA)
              full = sentO' && sentA'
                     && length rxO' >= length textA && length rxA' >= length textO
              fullAt' = case fullAt of
                          Just _ -> fullAt
                          Nothing -> if full then Just t else Nothing
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO
                (drop 1 hO ++ [audioO]) (drop 1 hA ++ [audioA])
                sentO' sentA' fullAt' rxO' rxA' (reverse eO ++ evO) (reverse eA ++ evA)
                (maybe erO (max erO) (modemEchoErle so')) (maybe erA (max erA) (modemEchoErle sa'))
      where out = (reverse rxO, reverse rxA, reverse evO, reverse evA, erO, erA)

-- | Like 'modemDuplex', but each side is offered @perBlock@ bytes of its
-- text on every block once connected, rather than the whole of it in one
-- burst.  A protocol layer needs a continuous stream to exercise its
-- window, and a burst larger than the window would simply sit in a buffer.
modemDuplexStream :: ModemConfig -> ModemConfig -> Double -> Int
                  -> [Word8] -> [Word8] -> Double
                  -> ([Word8], [Word8], [ModemEvent], [ModemEvent])
modemDuplexStream cfgO cfgA snr perBlock textO textA maxT =
  go 0 (modemInit cfgO) (modemInit cfgA) (VS.replicate blk 0) (VS.replicate blk 0)
     textO textA [] [] [] []
  where
    fs = mcRate cfgO
    blk = 160 :: Int
    impair k t x = addNoise (k * 100003 + round (t * 1000)) (0.05 * 0.707 / fromDb snr) (VS.map (* 0.1) x)
    go t so sa fromA fromO bufO bufA rxO rxA evO evA
      | t >= maxT = (reverse rxO, reverse rxA, reverse evO, reverse evA)
      | otherwise =
          let (queueO, bufO') = if modemConnected so then splitAt perBlock bufO else ([], bufO)
              (queueA, bufA') = if modemConnected sa then splitAt perBlock bufA else ([], bufA)
              (so', audioO, bytesO, eO) = modemStep cfgO so (impair 1 t fromA) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (impair 2 t fromO) queueA
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO bufO' bufA'
                (reverse bytesO ++ rxO) (reverse bytesA ++ rxA)
                (reverse eO ++ evO) (reverse eA ++ evA)
