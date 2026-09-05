{-# LANGUAGE BangPatterns #-}
-- | Asynchronous binary FSK modem (Bell 103, V.21), streaming.
--
-- Receiver pipeline:
--
-- 1. 'fskDiscriminator': an optional band-pass prefilter around the
--    channel's two tones (keeps the other direction's channel and
--    out-of-band noise out), then per sample a complex correlation of
--    the input against the mark and space tones over a one-bit window.
--    The windowed correlations are computed in O(n) with prefix sums; a
--    Hann window is expressed as the combination of three rectangular
--    correlators at f, f - fs/L and f + fs/L.  Output: mark energy,
--    space energy, a normalised decision variable and a carrier flag.
-- 2. 'fskDeframer': a UART-style framer.  It hunts for a mark-to-space
--    zero crossing (start bit, located to sub-sample precision by linear
--    interpolation), then samples data and stop bits at bit centres.
--    Every zero crossing seen inside a character nudges the sampling
--    phase towards the observed bit boundary, which makes it tolerant of
--    clock offset and jitter.
--
-- Everything is sample-rate agnostic: the only timing parameter is
-- samples per bit.  The transmitter is continuous-phase FSK from a phase
-- accumulator.
module Modec.FSK
  ( Framing (..)
  , framing8N1
  , WindowShape (..)
  , DemodParams (..)
  , defaultDemodParams
  , Discriminated (..)
  , fskDiscriminator
  , fskDeframer
  , fskReceiver
  , demodulate
  , flushSilence
  , discriminate
  , frameBits
  , modulateBits
  , txFilter
  , encodeBytes
  ) where

import Data.Bits (setBit, testBit)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)

import Modec.DSP
import Modec.Standards
import Modec.Stream

-- | Asynchronous character framing.  Start bit is always one space;
-- data bits are sent LSB first; no parity support yet.
data Framing = Framing
  { frDataBits :: !Int
  , frStopBits :: !Int
  } deriving (Eq, Show)

framing8N1 :: Framing
framing8N1 = Framing 8 1

data WindowShape = Rect | Hann deriving (Eq, Show)

data DemodParams = DemodParams
  { dpSquelch    :: !Double       -- ^ minimum tone amplitude (full scale = 1) for carrier present
  , dpTimingGain :: !Double       -- ^ in-character timing correction gain, 0 disables
  , dpWindow     :: !WindowShape
  , dpPrefilter  :: !Bool         -- ^ band-pass around the channel's tones before correlating
  , dpSlicer     :: !Bool         -- ^ adaptive threshold between the mark and space levels
  , dpIntegrate  :: !Double       -- ^ half-width of the decision integration window, in bits (0 = one sample)
  } deriving (Show)

defaultDemodParams :: DemodParams
defaultDemodParams = DemodParams
  { dpSquelch = 3e-3, dpTimingGain = 0.5, dpWindow = Rect, dpPrefilter = True
  , dpSlicer = True, dpIntegrate = 0.15 }

-- | Discriminator output for one chunk, one entry per input sample.
data Discriminated = Discriminated
  { dMark    :: !Signal   -- ^ mark tone energy
  , dSpace   :: !Signal   -- ^ space tone energy
  , dDecide  :: !Signal   -- ^ (mark - space) / (mark + space), in [-1, 1]
  , dPresent :: !(VS.Vector Double)  -- ^ 1 where carrier is above squelch, else 0
  }

bitWindow :: Double -> FskSpec -> Int
bitWindow fs spec = max 1 (round (fs / fskBaud spec))

-- | Length of the receive prefilter (and of the transmit filter).
filterTaps :: Double -> Int
filterTaps fs = 2 * round (fs / 25) + 1

-- | Global index of the first carried correlator sample, the prefilter
-- history (taps-1 samples), and the last (L-1) filtered samples.
data DiscState = DiscState !Int !Signal !Signal

-- | Streaming FIR over a chunk with history; output aligned to the chunk
-- (i.e. delayed by the filter's group delay, which is fine here).
firChunk :: VS.Vector Double -> Signal -> Signal -> Signal
firChunk hrev hist chunk = VS.generate n out
  where
    n = VS.length chunk
    t = VS.length hrev
    ext = hist VS.++ chunk
    out i = go 0 0
      where
        go !k !acc
          | k >= t = acc
          | otherwise = go (k + 1) (acc + VS.unsafeIndex hrev k * VS.unsafeIndex ext (i + k))

-- | Rectangular-window complex correlation of @ext@ against
-- @exp(-j w n)@ using global sample indices starting at @n0@.  Returns
-- (re, im) for the @nOut@ windows of length @l@ ending at ext[l-1 ..].
rectCorr :: Double -> Int -> Int -> Int -> Signal -> (Signal, Signal)
rectCorr w n0 l nOut ext = (out pr, out pim)
  where
    re = VS.imap (\i v -> v * cos (w * fromIntegral (n0 + i))) ext
    im = VS.imap (\i v -> negate v * sin (w * fromIntegral (n0 + i))) ext
    pr = VS.scanl' (+) 0 re
    pim = VS.scanl' (+) 0 im
    out p = VS.generate nOut (\i -> VS.unsafeIndex p (i + l) - VS.unsafeIndex p i)

-- | Streaming mark/space correlator.
fskDiscriminator :: Double -> FskSpec -> DemodParams -> Stage Signal Discriminated
fskDiscriminator fs spec params = Stage st0 step
  where
    l = bitWindow fs spec
    lo = min (fskMark spec) (fskSpace spec)
    hi = max (fskMark spec) (fskSpace spec)
    guardHz = 250
    taps = filterTaps fs
    hrev = if dpPrefilter params
             then VS.reverse (firBandpass fs (max 50 (lo - guardHz)) (min (fs / 2 - 50) (hi + guardHz)) taps)
             else VS.singleton 1
    st0 = DiscState 0 (VS.replicate (VS.length hrev - 1) 0) (VS.replicate (l - 1) 0)
    wsum = case dpWindow params of
      Rect -> fromIntegral l
      Hann -> fromIntegral l / 2
    squelchE = (dpSquelch params * wsum / 2) ^ (2 :: Int)
    delta = 2 * pi / fromIntegral l

    -- Windowed correlation energy at angular frequency w.
    energy n0 nOut ext w = case dpWindow params of
      Rect -> let (r, i) = rectCorr w n0 l nOut ext in VS.zipWith sq r i
      Hann ->
        let (r0, i0) = rectCorr w n0 l nOut ext
            (rm, im) = rectCorr (w - delta) n0 l nOut ext
            (rp, ip) = rectCorr (w + delta) n0 l nOut ext
        in VS.generate nOut $ \i ->
             let th = delta * fromIntegral (n0 + i)
                 c = cos th
                 s = sin th
                 -- 0.5*C0 - 0.25*e^{-j th}*Cm - 0.25*e^{+j th}*Cp
                 a = VS.unsafeIndex rm i; b = VS.unsafeIndex im i
                 p = VS.unsafeIndex rp i; q = VS.unsafeIndex ip i
                 bre = a * c + b * s;  bim = b * c - a * s
                 cre = p * c - q * s;  cim = p * s + q * c
                 re = 0.5 * VS.unsafeIndex r0 i - 0.25 * bre - 0.25 * cre
                 imv = 0.5 * VS.unsafeIndex i0 i - 0.25 * bim - 0.25 * cim
             in re * re + imv * imv
    sq a b = a * a + b * b

    step (DiscState n0 hist carry) chunk =
      let filtered = firChunk hrev hist chunk
          n = VS.length chunk
          ext = carry VS.++ filtered
          em = energy n0 n ext (2 * pi * fskMark spec / fs)
          es = energy n0 n ext (2 * pi * fskSpace spec / fs)
          dec = VS.zipWith (\a b -> (a - b) / (a + b + 1e-300)) em es
          pres = VS.zipWith (\a b -> if a + b > squelchE then 1 else 0) em es
          hist' = VS.drop n (hist VS.++ chunk)
          carry' = VS.drop n ext
          st' = DiscState (n0 + n) hist' carry'
      in hist' `seq` carry' `seq` (st', Discriminated em es dec pres)

-- | Hunting for a start bit, or inside a character with the fractional
-- sample index of the next decision, the bit number (0 = start), the
-- bits accumulated so far, whether timing was already corrected for
-- the coming boundary, and the running sum of the decision variable
-- over the central part of the current bit.
data Mode
  = Hunt
  | Char !Double !Int !Int !Bool !Double

-- | Global index of the next sample, previous sliced decision variable,
-- consecutive samples of mark with carrier, adaptive mark level,
-- adaptive space level, time of the last falling crossing that
-- qualified as a start edge, mode.
data DfState = DfState !Int !Double !Int !Double !Double !Double !Mode

-- | Streaming UART-style framer over discriminator output.
--
-- The decision variable is sliced against an adaptive threshold halfway
-- between the running mark and space levels (frequency offset and the
-- non-orthogonal tone spacing make the two levels asymmetric), zero
-- crossings of the sliced variable locate bit boundaries, and each bit
-- is decided by the sign of the variable integrated over the middle
-- half of the bit rather than by a single sample.
fskDeframer :: Double -> FskSpec -> Framing -> DemodParams -> Stage Discriminated [Word8]
fskDeframer fs spec (Framing nData _nStop) params = Stage (DfState 0 0 0 0.5 (-0.5) (-1e9) Hunt) step
  where
    spb = fs / fskBaud spec
    -- a start bit must be preceded by at least half a bit of idle mark
    minIdle = round (0.5 * spb) :: Int
    gain = dpTimingGain params
    alpha = if dpSlicer params then 1 / (2 * spb) else 0   -- level tracker time constant: two bits
    halfWin = dpIntegrate params * spb   -- integrate over [nxt - halfWin, nxt + halfWin]

    step st0 (Discriminated _ _ dec pres) = go st0 0 []
      where
        n = VS.length dec
        go st i acc
          | i >= n = (st, reverse acc)
          | otherwise =
              let d = VS.unsafeIndex dec i
                  p = VS.unsafeIndex pres i > 0
                  (st', out) = sample st d p
              in go st' (i + 1) (maybe acc (: acc) out)

    -- Process one sample; returns the new state and maybe a byte.
    sample (DfState gn prevD markRun hi lo lastFall mode) draw p =
      let thr = 0.5 * (hi + lo)
          d = draw - thr
          -- track the plateau levels of whichever side the sample is on
          (hi', lo') | not p = (hi, lo)
                     | draw > thr = (hi + alpha * (draw - hi), lo)
                     | otherwise = (hi, lo + alpha * (draw - lo))
          crossing = if (prevD >= 0) /= (d >= 0) && prevD /= d
                       then Just (fromIntegral (gn - 1) + prevD / (prevD - d))
                       else Nothing
          markRun' = if p && d >= 0 then markRun + 1 else 0
          tNow = fromIntegral gn :: Double
          -- a falling crossing after enough idle mark is a start-edge candidate
          startEdge = case crossing of
            Just c | p && prevD >= 0 && d < 0 && markRun >= minIdle -> Just c
            _ -> Nothing
          lastFall' = maybe lastFall id startEdge
          next st = (DfState (gn + 1) d markRun' hi' lo' lastFall' st, Nothing)
          -- after a character, resume from a start edge that arrived while
          -- the stop bit was still being decided (integration delay plus
          -- an early next character, e.g. after a jitter-buffer slip)
          afterChar = if tNow - lastFall' <= 0.6 * spb
                        then Char (lastFall' + 0.5 * spb) 0 0 True 0
                        else Hunt
      in case mode of
        Hunt ->
          case startEdge of
            Just c -> next (Char (c + 0.5 * spb) 0 0 True 0)
            Nothing -> next Hunt
        Char nxt bit acc0 corrected sumD ->
          -- only the first crossing near a boundary corrects the timing;
          -- a rectangular window can ring across a transition
          let (nxt', corrected') = case crossing of
                Just c | gain > 0 && not corrected ->
                  let boundary = nxt - 0.5 * spb
                      err = c - boundary
                  in if abs err < 0.35 * spb then (nxt + gain * err, True) else (nxt, corrected)
                _ -> (nxt, corrected)
              sumD' = if tNow >= nxt' - halfWin - 0.5 then sumD + d else sumD
          in if tNow + 0.5 < nxt' + halfWin
               then next (Char nxt' bit acc0 corrected' sumD')
               else
                 let mark = sumD' >= 0
                 in if bit == 0
                      then (if mark || not p then next Hunt else next (Char (nxt' + spb) 1 0 False 0))
                      else if bit <= nData
                        then (if not p then next Hunt   -- carrier lost mid-character
                              else next (Char (nxt' + spb) (bit + 1) (if mark then setBit acc0 (bit - 1) else acc0) False 0))
                        else -- stop bit; a carrier that drops exactly here still yields the byte
                          if mark || not p
                            then (DfState (gn + 1) d markRun' hi' lo' lastFall' afterChar, Just (fromIntegral acc0))
                            else next afterChar

-- | Complete receiver: samples in, bytes out, one list per chunk.
fskReceiver :: Double -> FskSpec -> Framing -> DemodParams -> Stage Signal [Word8]
fskReceiver fs spec fr params = fskDiscriminator fs spec params >>> fskDeframer fs spec fr params

-- | Offline convenience: run the receiver over a whole signal.  The
-- signal is followed by enough silence to flush the prefilter and the
-- correlation window, so a character ending at the last sample is
-- still delivered.
demodulate :: Double -> FskSpec -> Framing -> DemodParams -> Signal -> [Word8]
demodulate fs spec fr params x = concatStage (fskReceiver fs spec fr params) [x, flushSilence fs spec]

-- | Enough zeros to push the last character through the receiver.
flushSilence :: Double -> FskSpec -> Signal
flushSilence fs spec = VS.replicate (filterTaps fs + 2 * bitWindow fs spec + 2 * round (fs / fskBaud spec)) 0

-- | Offline mark and space energies (default window), for inspection.
discriminate :: Double -> FskSpec -> Signal -> (Signal, Signal)
discriminate fs spec x =
  case runStage (fskDiscriminator fs spec defaultDemodParams) [x] of
    [d] -> (dMark d, dSpace d)
    _   -> (VS.empty, VS.empty)

-- | Bytes to a bit stream with start/stop framing.  'True' is mark.
frameBits :: Framing -> [Word8] -> [Bool]
frameBits (Framing nData nStop) = concatMap one
  where
    one b = False : [testBit b k | k <- [0 .. nData - 1]] ++ replicate nStop True

-- | Continuous-phase FSK of a bit stream at the spec's baud rate.
modulateBits :: Double -> FskSpec -> [Bool] -> Signal
modulateBits fs spec bitsL
  | VU.null bits = VS.empty
  | otherwise = VS.unfoldrN total step (0 :: Int, 0 :: Double)
  where
    bits = VU.fromList bitsL
    spb = fs / fskBaud spec
    total = ceiling (fromIntegral (VU.length bits) * spb)
    twoPi = 2 * pi
    step (!k, !ph) =
      let bi = min (VU.length bits - 1) (floor (fromIntegral k / spb))
          f = if bits VU.! bi then fskMark spec else fskSpace spec
          ph' = ph + twoPi * f / fs
          ph'' = if ph' >= twoPi then ph' - twoPi else ph'
      in Just (sin ph, (k + 1, ph''))

-- | Transmit band limiting: real modems filter their output so that
-- keying splatter does not land in the other direction's channel.
txFilter :: Double -> FskSpec -> Signal -> Signal
txFilter fs spec = firCentered (firBandpass fs (max 50 (lo - g)) (min (fs / 2 - 50) (hi + g)) (filterTaps fs))
  where
    lo = min (fskMark spec) (fskSpace spec)
    hi = max (fskMark spec) (fskSpace spec)
    g = 300

-- | Frame and modulate bytes, with @pre@ and @post@ seconds of mark
-- (idle) around the data, at amplitude @amp@, band limited.
encodeBytes :: Double -> FskSpec -> Framing -> Double -> Double -> Double -> [Word8] -> Signal
encodeBytes fs spec fr amp pre post bytes = txFilter fs spec (VS.map (* amp) (modulateBits fs spec bits))
  where
    idle secs = replicate (round (secs * fskBaud spec)) True
    bits = idle pre ++ frameBits fr bytes ++ idle post
