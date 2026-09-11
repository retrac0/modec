{-# LANGUAGE BangPatterns #-}
-- | The channel simulator, a block at a time.
--
-- 'Modec.Channel.applyChannel' takes a whole recording: every stage sees
-- the signal from beginning to end, which is what lets a band-pass be
-- centred, a resampler know its length, and one seed lay the noise down
-- once.  A live call has none of that.  It has twenty milliseconds at a
-- time, and a band-pass restarted at every block splatters at every
-- seam, a seed reused at every block repeats the same noise for ever,
-- and a delay that reads ahead of the block has nothing to read.
--
-- So this is the same 'Channel' -- the same record, the same profiles,
-- the same @--impair K=V@ -- run with its state carried from block to
-- block: filter histories, delay lines, the random walk's last value,
-- the loss chain's state and the last good frame, an impulse's
-- ring-down into the next block, the running level the noise is set
-- against.  The stages come in 'applyChannel''s order and each is the
-- identity when its field is unset, so a live call with nothing asked
-- of the line is bit for bit the call it was.
--
-- What cannot come over unchanged is time itself.  A filter that was
-- centred is causal here and arrives late by half its length; a
-- resampler cannot make samples before they exist, so a clock offset
-- is a delay that starts at a second and drifts from there; and a
-- buffer slip's splices accumulate rather than being laid out in
-- advance.  'liveLatency' says how late the fixed part is.  None of it
-- matters to a modem, which has no idea when the block began.
module Modec.Channel.Live
  ( LiveChannel
  , liveInit
  , liveStep
  , liveLatency
  ) where

import qualified Data.Vector.Storable as VS
import Data.Bits (shiftL, xor)
import Modec.DSP
import Modec.G711
import Modec.Channel

data LiveChannel = LiveChannel
  { lcN       :: !Int      -- ^ global index of the next input sample
  , lcBlock   :: !Int      -- ^ blocks so far, which moves the seeds
  , lcEchoIn  :: !Signal   -- ^ what reached the hybrid, for the reflections
  , lcSing    :: !Signal   -- ^ the last outputs of the singing loop
  , lcBp      :: !Signal   -- ^ band-pass history
  , lcDd      :: !Signal   -- ^ delay distortion history
  , lcHil     :: !Signal   -- ^ Hilbert history, for the carrier stages
  , lcInPh    :: !Signal   -- ^ the in-phase path, delayed to meet it
  , lcWarpIn  :: !Signal   -- ^ what the variable delay reads from
  , lcWalk    :: !Double   -- ^ the random-walk jitter's position
  , lcLossBad :: !Bool     -- ^ the loss chain is in its bad state
  , lcLossLast :: !Signal  -- ^ the last frame that arrived whole
  , lcLossCur :: !Signal   -- ^ the frame arriving now
  , lcImpTail :: !Signal   -- ^ an impulse still ringing into this block
  , lcRms     :: !Double   -- ^ running level the noise is measured against
  , lcDc      :: !Double   -- ^ running mean the polynomial takes back out
  }

-- | Filter lengths, as 'applyChannel' chooses them.
bpTaps, ddTaps, hilTaps :: Double -> Int
bpTaps fs = 2 * round (fs / 40) + 1
ddTaps fs = 2 * round (fs * 0.02) + 1
hilTaps fs = 2 * round (fs / 125) + 1

-- | How much history the variable delay keeps: two seconds, which is
-- the reach of a clock offset before it stops drifting (see 'warp').
warpLen :: Double -> Int
warpLen fs = round (2 * fs) + 64

-- | The constant delay a clock offset starts from, so that a fast far
-- clock -- which reads ahead -- has somewhere to read from.
rateBias :: Double -> Double
rateBias fs = fs

-- | The constant delay every warp of time is biased by, so that the
-- interpolator always has samples on both sides of where it reads.
--
-- A bias and not a clamp.  Sine jitter swings its delay down to zero,
-- and a floor put under it there does not delay the signal -- it flattens
-- the bottom of every cycle into a different waveform, which is a
-- distortion the caller did not ask for and the whole-signal version
-- does not have.  Biased, the live line is the same warp arriving late,
-- which is what 'liveLatency' is for.
warpBias :: Double
warpBias = 8

liveInit :: Double -> Channel -> LiveChannel
liveInit fs ch = LiveChannel
  { lcN = 0, lcBlock = 0
  , lcEchoIn = VS.replicate (echoReach fs ch) 0
  , lcSing = VS.replicate (singReach fs ch) 0
  , lcBp = VS.replicate (bpTaps fs - 1) 0
  , lcDd = VS.replicate (ddTaps fs - 1) 0
  , lcHil = VS.replicate (hilTaps fs - 1) 0
  , lcInPh = VS.replicate ((hilTaps fs - 1) `div` 2) 0
  , lcWarpIn = VS.replicate (warpLen fs) 0
  , lcWalk = 0
  , lcLossBad = False, lcLossLast = VS.empty, lcLossCur = VS.empty
  , lcImpTail = VS.empty
  , lcRms = 0, lcDc = 0 }

echoReach :: Double -> Channel -> Int
echoReach fs ch = 8 + maximum (0 : [ round (s * fs) | Just (s, _) <- [chEcho ch] ]
                                 ++ [ ceiling (s * fs) + 4 | (s, _) <- chEchoTaps ch ])

singReach :: Double -> Channel -> Int
singReach fs ch = case chSing ch of
  Just (secs, _) -> max 1 (round (secs * fs))
  Nothing -> 0

-- | The fixed delay the stages that are on add, in samples: what a
-- whole-signal run would have taken back out and a live one cannot.
liveLatency :: Double -> Channel -> Int
liveLatency fs ch = sum
  [ if chBandpass ch /= Nothing then (bpTaps fs - 1) `div` 2 else 0
  , if chDelayDist ch /= 0 then (ddTaps fs - 1) `div` 2 else 0
  , if chFreqOffsetHz ch /= 0 || chWobble ch /= Nothing || chPhaseJitter ch /= Nothing
      then (hilTaps fs - 1) `div` 2 else 0
  , if chRateOffset ch /= 0 then round (rateBias fs) else 0
  , if warpingIn ch then round warpBias else 0 ]

-- | Whether any stage warps time, and so reads through the delay line.
warpingIn :: Channel -> Bool
warpingIn ch = (case chJitter ch of { NoJitter -> False; _ -> True })
               || chRateOffset ch /= 0
               || maybe False ((/= 0) . snd) (chSlip ch)

-- | One block through the line.  The 'Channel' may change between
-- blocks in its numbers -- a noise schedule sets 'chSnrDb' as it goes
-- -- but not in which stages are on, which sized the state.
liveStep :: Double -> Channel -> LiveChannel -> Signal -> (LiveChannel, Signal)
liveStep fs ch st0 x0 = (stF { lcN = lcN st0 + n, lcBlock = lcBlock st0 + 1 }, y)
  where
    n = VS.length x0
    n0 = lcN st0
    -- per-impairment seeds as applyChannel numbers them, moved along
    -- with the block so no two blocks share a realisation
    seed k = chSeed ch * 7919 + k + 1000003 * lcBlock st0

    -- 'applyChannel''s order, first stage first.
    x1 = gainDc x0
    (stA, x2) = nonlin st0 x1
    (stB, x3) = echo stA x2
    (stC, x4) = sing stB x3
    (stD, x5) = bandpass stC x4
    (stE, x6) = delayDist stD x5
    (stG, x7) = carrier stE x6            -- freqOff and wobble share the Hilbert
    (stH, x8) = warp stG x7               -- jitter, clock offset and slips share the delay line
    (stI, x9) = span_ stH x8
    x10 = hits x9
    x11 = dropout x10
    (stJ, x12) = impulse stI x11
    x13 = clip x12
    x14 = hum x13
    (stF, y) = noise stJ x14

    gainDc = VS.map (\v -> v * chGain ch + chDcOffset ch)

    nonlin st x = case chNonlin ch of
      Nothing -> (st, x)
      Just (SoftClip d)
        | d <= 0 -> (st, x)
        | otherwise -> (st, VS.map (\v -> tanh (d * v) / d) x)
      Just (Polynomial a2 a30) ->
        -- the whole-signal stage takes the mean back out; here it is a
        -- running one, since the mean of a call is not known yet
        let a3 = min (1 / 3) a30
            raw = VS.map (\v -> v + a2 * v * v - a3 * v * v * v) x
            m = if n == 0 then lcDc st else VS.sum raw / fromIntegral n
            dc = 0.98 * lcDc st + 0.02 * m
        in (st { lcDc = dc }, VS.map (subtract dc) raw)

    echo st x = case (chEcho ch, chEchoTaps ch) of
      (Nothing, []) -> (st, x)
      (e, taps) ->
        let hist = lcEchoIn st VS.++ x
            base = VS.length hist - n
            at j = if j < 0 || j >= VS.length hist then 0 else VS.unsafeIndex hist j
            one i = case e of
              Nothing -> 0
              Just (secs, g) -> g * at (base + i - round (secs * fs))
            many i = sum [ g * cubicAt hist (fromIntegral (base + i) - s * fs) | (s, g) <- taps ]
            y' = VS.generate n (\i -> VS.unsafeIndex x i + one i + many i)
        in (st { lcEchoIn = VS.drop n hist }, y')

    sing st x = case chSing ch of
      Nothing -> (st, x)
      Just (_, g0) ->
        let d = VS.length (lcSing st)
            g = max (-0.95) (min 0.95 g0)
            acc = VS.constructN (d + n) $ \a ->
              let i = VS.length a
              in if i < d then VS.unsafeIndex (lcSing st) i
                 else VS.unsafeIndex x (i - d) + g * VS.unsafeIndex a (i - d)
        in (st { lcSing = VS.drop n acc }, VS.drop d acc)

    bandpass st x = case chBandpass ch of
      Nothing -> (st, x)
      Just (f1, f2) ->
        let (y', h') = firStream (VS.reverse (firBandpass fs f1 f2 (bpTaps fs))) (lcBp st) x
        in (st { lcBp = h' }, y')

    delayDist st x
      | chDelayDist ch == 0 = (st, x)
      | otherwise =
          let (y', h') = firStream (VS.reverse (delayDistortionKernel fs (chDelayDist ch) (ddTaps fs))) (lcDd st) x
          in (st { lcDd = h' }, y')

    -- Frequency offset, frequency wobble and phase jitter are one
    -- stage: a phase, applied through a Hilbert pair.  The in-phase
    -- path is delayed by the transformer's own group delay so the two
    -- meet, which the centred whole-signal version did by looking ahead.
    carrier st x
      | chFreqOffsetHz ch == 0 && chWobble ch == Nothing && chPhaseJitter ch == Nothing = (st, x)
      | otherwise =
          let (xq, hh') = firStream (VS.reverse (firHilbert (hilTaps fs))) (lcHil st) x
              ext = lcInPh st VS.++ x
              xi = VS.take n ext
              w = 2 * pi * chFreqOffsetHz ch / fs
              ph i = let t = fromIntegral (n0 + i) / fs
                         fm = case chWobble ch of
                           Just (dev, rate) | rate > 0 -> (dev / rate) * (1 - cos (2 * pi * rate * t))
                           _ -> 0
                         pm = case chPhaseJitter ch of
                           Just (deg, rate) -> (deg * pi / 360) * sin (2 * pi * rate * t)
                           _ -> 0
                     in w * fromIntegral (n0 + i) + fm + pm
              y' = VS.izipWith (\i v q -> let p = ph i in v * cos p - q * sin p) xi xq
          in (st { lcHil = hh', lcInPh = VS.drop n ext }, y')

    -- Every warping of time reads the same history at a delay that
    -- moves: jitter as a sine, a walk or slips; wow and flutter; a
    -- clock offset as a delay that starts at a second and drifts; a
    -- buffer slip as one that jumps a frame at a time.  A delay can
    -- only ever look back, so each is biased to stay positive, and the
    -- drifting ones stop at the edge of the history rather than run
    -- off it -- two seconds, which is a 1 % clock for three minutes.
    warp st x
      | not warping = (st, x)
      | otherwise =
          let hist = lcWarpIn st VS.++ x
              len = VS.length hist
              base = len - n
              (walk', walkAt) = case chJitter ch of
                WalkJitter stepS mx ->
                  let steps = gaussianNoise (seed 2) n stepS
                      w = VS.scanl' (\d s -> max (-mx) (min mx (d + s))) (lcWalk st) steps
                  in (VS.last w, \i -> mx + VS.unsafeIndex w (i + 1))
                _ -> (lcWalk st, const 0)
              jit i = case chJitter ch of
                NoJitter -> 0
                SineJitter a f -> a + a * sin (2 * pi * f * fromIntegral (n0 + i) / fs)
                WalkJitter _ _ -> walkAt i
                Slips every s ->
                  let per = max 1 (round (every * fs)) :: Int
                  in s * fromIntegral (((n0 + i) `div` per) `mod` 2)
                WowFlutter comps ->
                  let ms = [ (a * fs / (2 * pi * f), f) | (a, f) <- comps, f > 0, a /= 0 ]
                      d0 = 2 * sum (map fst ms) + 6
                      t = fromIntegral (n0 + i) / fs
                  in if null ms then 0 else d0 - sum [ m * (1 - cos (2 * pi * f * t)) | (m, f) <- ms ]
              rateD i | chRateOffset ch == 0 = 0
                      | otherwise = rateBias fs - chRateOffset ch * fromIntegral (n0 + i)
              slipD i = case chSlip ch of
                Just (every, k) | k /= 0 ->
                  let blk = max 1 (round (0.02 * fs)) :: Int
                      per = max 1 (round (every * fs)) :: Int
                  in fromIntegral (((n0 + i) `div` per) * k * blk)
                _ -> 0
              total i = max 3 (min (fromIntegral (len - 4))
                                   (warpBias + jit i + rateD i + slipD i))
              -- 'sampleAt', which is what 'variableDelay' uses: a
              -- cheaper interpolator here would be a second difference
              -- between the live line and the one every offline number
              -- in this project was measured on.
              y' = VS.generate n (\i -> sampleAt hist (fromIntegral (base + i) - total i))
          in (st { lcWarpIn = VS.drop n hist, lcWalk = walk' }, y')
    warping = warpingIn ch

    -- The digital span, at the code level as applyChannel does it.
    span_ st x
      | not spanOn = (st, x)
      | otherwise =
          let codes = VS.map enc x
              u = uniformNoise (seed 5) (2 * n)
              damaged = VS.imap (\i c ->
                let c1 = case chBitError ch of
                      Just p | VS.unsafeIndex u (2 * i) < p ->
                        let b = min (bits - 1) (floor (VS.unsafeIndex u (2 * i + 1) * fromIntegral bits))
                        in c `xor` (1 `shiftL` b)
                      _ -> c
                in case chStuck ch of
                     Just (Stuck every for code) ->
                       let per = max 1 (round (every * fs)) :: Int
                           l = max 1 (round (for * fs)) :: Int
                       in if (n0 + i) `mod` per < l then fromIntegral code else c1
                     Nothing -> c1) codes
              decoded = VS.map dec damaged
          in losses st decoded
    spanOn = case (chCodec ch, chBitError ch, chStuck ch, chLoss ch) of
      (Nothing, Nothing, Nothing, Nothing) -> False
      _ -> True
    (enc, dec, bits) = case maybe Ulaw id (chCodec ch) of
      Ulaw -> (fromIntegral . ulawEncode, ulawDecode . fromIntegral, 8 :: Int)
      Alaw -> (fromIntegral . alawEncode, alawDecode . fromIntegral, 8)
      LinearBits b ->
        let half = 2 ^^ (b - 1) :: Double
            lo = negate (round half) :: Int
            hi = round half - 1
        in (\v -> max lo (min hi (round (v * half))), \k -> fromIntegral k / half, b)

    -- Gilbert-Elliott over frames, the chain's state and the last
    -- whole frame carried across blocks.  Frames are counted from the
    -- start of the call, so a block boundary is not a frame boundary
    -- unless the block is a frame.
    losses st y = case chLoss ch of
      Nothing -> (st, y)
      Just (Loss frameSec toBad toGood how) ->
        let blk = max 1 (round (frameSec * fs)) :: Int
            go !i !bad !lastF !cur !acc
              | i >= n = (bad, lastF, cur, reverse acc)
              | otherwise =
                  let g = n0 + i
                      atFrameStart = g `mod` blk == 0
                      -- one draw per frame, from the frame's own number
                      u = VS.head (uniformNoise (seed 6 + g `div` blk) 1)
                      bad' | not atFrameStart = bad
                           | bad = not (u < toGood)
                           | otherwise = u < toBad
                      lastF' | atFrameStart && not bad && VS.length cur == blk = cur
                             | otherwise = lastF
                      cur' | atFrameStart = VS.empty
                           | otherwise = cur
                      v = VS.unsafeIndex y i
                      out | not bad' = v
                          | otherwise = case how of
                              Silence -> 0
                              HoldLast -> if VS.null lastF' then 0 else VS.last lastF'
                              RepeatFrame -> if VS.length lastF' == blk then VS.unsafeIndex lastF' (g `mod` blk) else 0
                      cur'' = if bad' then cur' else VS.snoc cur' v
                  in go (i + 1) bad' lastF' cur'' (out : acc)
            (badF, lastFF, curF, outs) = go 0 (lcLossBad st) (lcLossLast st) (lcLossCur st) []
        in (st { lcLossBad = badF, lcLossLast = lastFF, lcLossCur = curF }, VS.fromList outs)

    -- One draw per slot, taken once for the block.  Drawn inside the
    -- map it was a vector allocated per sample, in the audio path.
    slotDraws base blk = VS.generate (span_slots) (\j -> VS.head (uniformNoise (base + first + j) 1))
      where first = n0 `div` blk
            span_slots = ((n0 + n - 1) `div` blk) - first + 1
    hits x = case chHits ch of
      Nothing -> x
      Just (Hits perSec forSec gainDb) ->
        let blk = max 1 (round (forSec * fs)) :: Int
            p = perSec * forSec
            g = fromDb gainDb
            us = slotDraws (seed 8) blk
            first = n0 `div` blk
        in VS.imap (\i v -> if VS.unsafeIndex us ((n0 + i) `div` blk - first) < p then g * v else v) x

    dropout x = case chDropout ch of
      Nothing -> x
      Just (secs, prob) ->
        let blk = max 1 (round (secs * fs)) :: Int
            first = n0 `div` blk
            nslots = ((n0 + n - 1) `div` blk) - first + 1
            qs = VS.generate nslots $ \j ->
                   let u = VS.head (gaussianNoise (seed 3 + first + j) 1 1)
                   in 0.5 + 0.5 * erfApprox (u / sqrt 2)
        in VS.imap (\i v -> if VS.unsafeIndex qs ((n0 + i) `div` blk - first) < prob then 0 else v) x

    -- Poisson arrivals, each a ring-down longer than a block: the part
    -- that runs past the block is kept and added to the next one.
    impulse st x = case chImpulse ch of
      Just (Impulse perSec amp ringHz decay) | perSec > 0 && amp /= 0 ->
        let u = uniformNoise (seed 7) n
            p = perSec / fs
            deltas = VS.generate n (\i -> if VS.unsafeIndex u i < p then 1 else 0)
            taps = max 1 (round (6 * decay * fs)) :: Int
            raw = VS.generate taps $ \k ->
                    let t = fromIntegral k / fs
                    in exp (negate t / decay) * sin (2 * pi * ringHz * t)
            peak = max 1e-12 (VS.maximum (VS.map abs raw))
            kern = VS.map (\v -> amp * v / peak) raw
            -- full convolution: n + taps - 1 samples
            full = VS.generate (n + taps - 1) $ \i ->
                     sum [ VS.unsafeIndex kern k * VS.unsafeIndex deltas (i - k)
                         | k <- [max 0 (i - n + 1) .. min (taps - 1) i] ]
            tailIn = lcImpTail st
            withTail = VS.imap (\i v -> v + (if i < VS.length tailIn then VS.unsafeIndex tailIn i else 0)) full
            y' = VS.zipWith (+) x (VS.take n withTail)
        in (st { lcImpTail = VS.drop n withTail }, y')
      _ -> (st, x)

    clip x = case chClip ch of
      Nothing -> x
      Just c -> VS.map (max (negate c) . min c) x

    hum x = case chHum ch of
      Nothing -> x
      Just (f, a) -> VS.imap (\i v -> v + a * sin (2 * pi * f * fromIntegral (n0 + i) / fs)) x

    -- Against a running level rather than the block's: the far end is
    -- quiet between its signals, and noise measured against silence is
    -- silence.  The convention is applyChannel's -- sigma is the level
    -- over 10^(snr/20) -- so a number here means what it means there.
    noise st x =
      let here = rms x
          lvl = if here > 1e-6 then 0.9 * lcRms st + 0.1 * here else lcRms st
          st' = st { lcRms = lvl }
      in case chSnrDb ch of
           Just snr | lvl > 0 -> (st', addNoise (seed 4) (lvl / fromDb snr) x)
           _ -> (st', x)
