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
  , tddFraming
  , Level (..)
  , Keying
  , frameKeyed
  , modulateKeyed
  , Burst (..)
  , defaultBurst
  , keyedBurst
  , encodeBurst
  , WindowShape (..)
  , DemodParams (..)
  , defaultDemodParams
  , Discriminated (..)
  , fskDiscriminator
  , fskDeframer
  , BurstParams (..)
  , defaultBurstParams
  , fskBurstDeframer
  , fskSyncBits
  , fskReceiver
  , demodulate
  , flushSilence
  , discriminate
  , frameBits
  , modulateBits
  , txFilter
  , txFilterKernel
  , filterTaps
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
--
-- The stop period is measured in bit times and need not be whole: the
-- 5-bit text telephone code specifies a minimum of one and a half.
-- "Minimum" is load bearing -- after the stop bits the line simply
-- idles in mark until the next start bit, for anything from nothing to
-- a second, so a receiver must resynchronise on every start edge rather
-- than assume a fixed character period.  Only 'frameKeyed' can express
-- the fraction; 'frameBits', which frames whole bits for the
-- continuous-carrier modes, rounds up.
data Framing = Framing
  { frDataBits :: !Int
  , frStopBits :: !Double
  } deriving (Eq, Show)

framing8N1 :: Framing
framing8N1 = Framing 8 1

-- | The 5-bit text telephone character (V.18 A.4 / ANSI TIA-825).
--
-- Two stop bits, where the Recommendation asks for a minimum of one and
-- a half.  The minimum is what a receiver may require; it is not what a
-- transmitter should send.  Asterisk sends exactly 1.5 and minimodem's
-- @tdd@ preset requires 2.0, so 1.5 on the line loses characters to
-- minimodem -- 32 of 35, with the clock read 3.9 % fast -- while 2.0 is
-- decoded perfectly by both it and us.  Ultratec's Turbo Code uses two
-- stop bits for the same reason, to give a tone detector enough mark to
-- lock to.  The cost is 11 ms a character, six per cent of a line that
-- is slow anyway.
--
-- The receiver deliberately does not enforce this: it wants a mark stop
-- bit and nothing more, because the stop period is idle mark of
-- unbounded length and a far end sending the 1.5 minimum is correct.
tddFraming :: Framing
tddFraming = Framing 5 2

-- | A line level.  'Off' is no carrier at all, which is where a
-- carrierless mode -- the 5-bit text telephone code -- spends the time
-- between one burst of characters and the next.  No continuous-carrier
-- mode ever visits it.
data Level = Mark | Space | Off deriving (Eq, Show)

-- | Levels and how long to hold each, in seconds.  This is the general
-- form of a transmission: it can say what a uniform bit stream cannot,
-- namely a fractional stop period, a carrier that stops and starts, and
-- a mode whose mark and space bits are different lengths.
type Keying = [(Level, Double)]

data WindowShape = Rect | Hann deriving (Eq, Show)

data DemodParams = DemodParams
  { dpSquelch    :: !Double       -- ^ minimum tone amplitude (full scale = 1) for carrier present
  , dpTimingGain :: !Double       -- ^ in-character timing correction gain, 0 disables
  , dpWindow     :: !WindowShape
  , dpPrefilter  :: !Bool         -- ^ band-pass around the channel's tones before correlating
  , dpSlicer     :: !Bool         -- ^ adaptive threshold between the mark and space levels
  , dpIntegrate  :: !Double       -- ^ half-width of the decision integration window, in bits (0 = one sample)
  , dpStartDepth :: !Double       -- ^ a start bit must average this far below the slicer threshold (0..1)
  } deriving (Show)

defaultDemodParams :: DemodParams
defaultDemodParams = DemodParams
  { dpSquelch = 3e-3, dpTimingGain = 0.5, dpWindow = Rect, dpPrefilter = True
  , dpSlicer = True, dpIntegrate = 0.35, dpStartDepth = 0.10 }
-- Both of the last two were measured rather than guessed.  A decision
-- window of +/-0.15 bit was narrow enough that a space sitting just
-- before a run of marks could be decided from samples whose correlation
-- windows had already slid into the marks; +/-0.35 keeps the window
-- centred on the bit it is deciding.  A start bit was required to sit
-- 0.25 below the threshold, which rejects real start bits that noise has
-- made shallow, and every rejection costs a whole character.  At 0.10 it
-- still turns away the transients it is there for: adjacent channel
-- rejection, clock offset and carrier offset tolerance are unchanged at
-- 86 dB, 3 % and 40 Hz, while the noise floor for error-free reception
-- improves by 1 dB and the character error rate at 14 dB Eb/N0 falls by
-- a factor of eleven.

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
              -- the start bit is checked over the middle 60 % of the bit so that a
              -- transient shorter than half a bit cannot pass for it; data and stop
              -- bits use the configured (narrower) window
              win = if bit == 0 then 0.3 * spb else halfWin
              sumD' = if tNow >= nxt' - win - 0.5 then sumD + d else sumD
          in if tNow + 0.5 < nxt' + win
               then next (Char nxt' bit acc0 corrected' sumD')
               else
                 let mark = sumD' >= 0
                     -- samples integrated: from nxt - win - 0.5 to nxt + win
                     nInt = max 1 (2 * win + 1)
                     -- a real start bit is a clear space; a shallow dip (a transient
                     -- of another channel leaking through) is not
                     shallow = sumD' > negate (dpStartDepth params) * nInt
                 in if bit == 0
                      then (if mark || not p || shallow then next Hunt else next (Char (nxt' + spb) 1 0 False 0))
                      else if bit <= nData
                        then (if not p then next Hunt   -- carrier lost mid-character
                              else next (Char (nxt' + spb) (bit + 1) (if mark then setBit acc0 (bit - 1) else acc0) False 0))
                        else -- stop bit; a carrier that drops exactly here still yields the byte
                          if mark || not p
                            then (DfState (gn + 1) d markRun' hi' lo' lastFall' afterChar, Just (fromIntegral acc0))
                            else next afterChar

-- | Hunting for a burst, or inside a character with the fractional
-- sample index of the next decision, the bit number (0 = start), the
-- bits so far, whether timing was already corrected for the coming
-- boundary, and the running decision sum.
data BMode = BHunt | BChar !Double !Int !Int !Bool !Double

-- | Global sample index, averaged |decision| (the tone detector), tone
-- present on the previous sample, previous sliced decision, consecutive
-- carried mark samples, adaptive mark and space levels, whether this
-- burst has yet shown a space, mode.
data BfState = BfState !Int !Double !Bool !Double !Int !Double !Double !Bool !BMode

-- | Carrier and timing policy for a carrierless mode.
data BurstParams = BurstParams
  { bpTone      :: !Double   -- ^ mean |decision| over the averaging window for carrier
  , bpToneTau   :: !Double   -- ^ that averaging window, in bits
  , bpIntegrate :: !Double   -- ^ half-width of the decision window, in bits
  , bpOnsetSkew :: !Double   -- ^ bits between true carrier onset and the correlator noticing
  } deriving (Show)

defaultBurstParams :: BurstParams
defaultBurstParams = BurstParams
  { bpTone = 0.75, bpToneTau = 1, bpIntegrate = 0.35, bpOnsetSkew = 0.3 }
-- Measured, not chosen, over 30 characters through the telephone
-- channel, scored as edit distance.
--
-- 'bpTone' is the one real trade.  On noise the average of |decide| is
-- one half whatever the noise power, so anything above that separates a
-- tone in principle; in practice a burst of back-to-back characters
-- pulls the average down, because |decide| passes through zero at every
-- bit transition.  At 0.80 a line of continuous text loses the carrier
-- mid-burst and scores 8 to 13 at every SNR from 30 dB down, while
-- 0.75 is perfect over the same range.  Below 0.75 the noise starts to
-- get in: 0.60 costs 20 at 12 dB where 0.75 costs 12.  The continuous
-- burst is what a person typing a line actually produces, so it wins.
--
-- 'bpOnsetSkew' matters only for a burst that opens with the start bit
-- itself, and is the correlator's own lateness in noticing: 0 scores 5
-- at 30 dB, 0.2 to 0.35 score 0, and by 0.8 the start bit is looked for
-- so early that nothing frames at all.  'bpToneTau' is flat between
-- half a bit and two bits and was left at one.

-- | Streaming framer for a mode that keys its carrier off between
-- characters: the 5-bit text telephone code, where the line is silent
-- until someone types and a burst can begin with the start bit itself.
--
-- 'fskDeframer' cannot do this, and not by a small margin.  It hunts
-- for a mark-to-space crossing preceded by half a bit of carried idle
-- mark, which is the right rule for every continuous-carrier mode and
-- exactly the wrong one here: with no lead-in mark to have seen, a
-- third of the characters in a clean synthetic burst never acquire at
-- all.  The two acquisition disciplines are different enough that this
-- is a sibling rather than a flag, which also keeps five measured and
-- tuned modes out of the blast radius.
--
-- Two differences, both forced by the absent carrier:
--
-- 1. A burst is acquired either on a mark-to-space crossing (characters
--    run back to back inside a burst, and a legacy set holds mark for
--    up to a second after the last one) or on the carrier /appearing/
--    already in space, which is a start bit that had no lead-in.  The
--    correlator integrates over a bit, so it notices an onset late;
--    'bpOnsetSkew' is that lateness, and the in-character timing
--    correction cleans up what it leaves behind.
-- 2. Carrier is decided on whether the band holds a /tone/, not on how
--    much energy is in it.  'dpSquelch' alone is what lets noise in the
--    silent gaps frame characters out of nothing: 30 characters sent
--    over a 20 dB channel came back as 55.
--
-- That second one deserves its reasoning, because the obvious answer is
-- wrong three times over.  Comparing energy against a tracked noise
-- floor needs the floor measured while the line is quiet, which means
-- freezing it while the carrier is up, which means the estimate is only
-- as good as its seed and can never recover from a bad one.  Letting it
-- decay under carrier to fix that is positive feedback: one noise spike
-- turns the carrier on, the threshold falls away underneath it, and the
-- detector latches on for the rest of the recording.  Seeding it from a
-- running mean over the first moments instead just moves the problem --
-- a burst inside that window seeds the floor above the signal and the
-- detector never opens at all.  And a fast-down, slow-up tracker
-- converges on the minimum of the noise rather than its mean, which for
-- exponentially distributed correlator energy is far enough below that
-- a margin measured against it is no margin at all.
--
-- The decision variable answers it with no state to get wrong.  It is
-- already normalised, so it says nothing about level: on a tone one of
-- the two correlators has everything and |decide| sits at 1, while on
-- noise the two are independent and identically distributed and
-- |decide| is uniform on [0,1] with a mean of exactly one half,
-- whatever the noise power.  Averaged over a bit and thresholded above
-- that half, it separates a tone from noise without a floor, without a
-- seed, and without any way to latch.
--
-- 'dpSquelch' is still required alongside it: on digital silence the
-- ratio is 0/0 and carries no information at all.
fskBurstDeframer :: Double -> FskSpec -> Framing -> DemodParams -> BurstParams
                 -> Stage Discriminated [Word8]
fskBurstDeframer fs spec (Framing nData _nStop) params bp = Stage st0 step
  where
    spb = fs / fskBaud spec
    minIdle = round (0.5 * spb) :: Int
    gain = dpTimingGain params
    alpha = if dpSlicer params then 1 / (2 * spb) else 0
    halfWin = bpIntegrate bp * spb
    aTone = 1 / max 1 (bpToneTau bp * spb)
    skew = bpOnsetSkew bp * spb
    st0 = BfState 0 0 False 0 0 0.5 (-0.5) False BHunt

    step s0 (Discriminated _ _ dec pres) = go s0 0 []
      where
        n = VS.length dec
        go st i acc
          | i >= n = (st, reverse acc)
          | otherwise =
              let (st', out) = sample st (VS.unsafeIndex dec i) (VS.unsafeIndex pres i > 0)
              in go st' (i + 1) (maybe acc (: acc) out)

    sample (BfState gn tone onPrev prevD markRun hi lo seen mode) draw present =
      let tone' = tone + aTone * (abs draw - tone)
          -- No hangover: the tone detector alone gates acquisition.
          -- Holding the carrier on past the end of a burst is what let
          -- the decaying tail read as a start bit, and since the gap to
          -- the next burst is shorter than a character, that bogus
          -- character found its stop bit in the next lead-in and was
          -- delivered.  One junk character between every pair of real
          -- ones, and it was there on a clean line too.
          on = present && tone' > bpTone bp
          tNow = fromIntegral gn :: Double
          inChar = case mode of { BChar {} -> True; BHunt -> False }
          -- The tone detector gates acquisition; plain energy decides
          -- whether a character already under way still has a line to
          -- run on.  Neither test can do the other's job.  |decide|
          -- passes through zero at every bit transition, so a bit of
          -- alternating data averages below any threshold that
          -- separates a tone from noise, and aborting a character there
          -- cost two thirds of a clean payload.  But a character that
          -- is never aborted survives the silence after its burst and
          -- collects a stop bit from the lead-in of the /next/ one,
          -- which is where the interleaved junk came from.  Energy
          -- answers that: it does not dip at a transition, and in the
          -- gap there is none.
          live = on || inChar
          -- levels are per burst; a character keeps the ones it started with
          fresh = not inChar && not (on && onPrev)
          (hiC, loC, seenC) = if fresh then (0.5, -0.5, False) else (hi, lo, seen)
          -- Until this burst has shown a space the slicer has no space
          -- level to average with, and the stale one it starts with puts
          -- the threshold a quarter of the way up towards mark, where a
          -- dip in a long lead-in reads as a start edge.  Slicing at
          -- zero until then costs nothing at a modulation index near
          -- nine, and the adaptive threshold takes over for the
          -- frequency offset it is there for as soon as it can.
          thr = if seenC then 0.5 * (hiC + loC) else 0
          d = draw - thr
          seen' = seenC || (live && draw < thr)
          (hi', lo') | not live = (hiC, loC)
                     | draw > thr = (hiC + alpha * (draw - hiC), loC)
                     | otherwise = (hiC, loC + alpha * (draw - loC))
          markRun' = if live && d >= 0 then markRun + 1 else 0
          crossing = if (prevD >= 0) /= (d >= 0) && prevD /= d
                       then Just (fromIntegral (gn - 1) + prevD / (prevD - d))
                       else Nothing
          keep m = (BfState (gn + 1) tone' on d markRun' hi' lo' seen' m, Nothing)
          emit m b = (BfState (gn + 1) tone' on d markRun' hi' lo' seen' m, Just b)
      in case mode of
        BHunt
          | not on -> keep BHunt                              -- silence
          -- the carrier just appeared.  In space it appeared as a start
          -- bit with no lead-in; in mark it is a lead-in, and the
          -- crossing that ends it will start the character.
          | not onPrev -> keep (if d < 0 then BChar (tNow - skew + 0.5 * spb) 0 0 True 0 else BHunt)
          | otherwise -> case crossing of
              Just c | prevD >= 0 && d < 0 && markRun >= minIdle ->
                keep (BChar (c + 0.5 * spb) 0 0 True 0)
              _ -> keep BHunt
        BChar _ _ _ _ _ | not present -> keep BHunt      -- the line went away
        BChar nxt bit acc0 corrected sumD ->
          let (nxt', corrected') = case crossing of
                Just c | gain > 0 && not corrected ->
                  let err = c - (nxt - 0.5 * spb)
                  in if abs err < 0.35 * spb then (nxt + gain * err, True) else (nxt, corrected)
                _ -> (nxt, corrected)
              win = if bit == 0 then 0.3 * spb else halfWin
              sumD' = if tNow >= nxt' - win - 0.5 then sumD + d else sumD
          in if tNow + 0.5 < nxt' + win
               then keep (BChar nxt' bit acc0 corrected' sumD')
               else
                 let mark = sumD' >= 0
                     nInt = max 1 (2 * win + 1)
                     shallow = sumD' > negate (dpStartDepth params) * nInt
                 in if bit == 0
                      then keep (if mark || shallow then BHunt else BChar (nxt' + spb) 1 0 False 0)
                      else if bit <= nData
                        then keep (BChar (nxt' + spb) (bit + 1) (if mark then setBit acc0 (bit - 1) else acc0) False 0)
                        else -- The stop bit is the check that a burst of
                             -- noise almost never passes, and it has to
                             -- be a tone and not merely a mark: the ring
                             -- of a burst's own decaying tail frames one
                             -- last character out of nothing otherwise.
                          if mark && on then emit BHunt (fromIntegral acc0) else keep BHunt

-- | Synchronous bit recovery over discriminator output: a bit clock at
-- the nominal rate, nudged towards every zero crossing of the sliced
-- decision variable (HDLC flags guarantee frequent transitions), with
-- each bit decided by the sign of the variable integrated over the
-- middle half of the bit.  Emits nothing while no carrier is present.
-- Global index of the next sample, previous sliced value, mark level,
-- space level, next decision position, integration sum.
fskSyncBits :: Double -> FskSpec -> DemodParams -> Stage Discriminated [Bool]
fskSyncBits fs spec params = Stage (0 :: Int, 0 :: Double, 0.5 :: Double, -0.5 :: Double, spb, 0 :: Double) step
  where
    spb = fs / fskBaud spec
    alpha = 1 / (2 * spb)
    gain = dpTimingGain params
    halfWin = 0.25 * spb
    step st0 (Discriminated _ _ dec pres) = go st0 0 []
      where
        n = VS.length dec
        go st i acc
          | i >= n = (st, reverse acc)
          | otherwise =
              let (st', out) = sample st (VS.unsafeIndex dec i) (VS.unsafeIndex pres i > 0)
              in go st' (i + 1) (maybe acc (: acc) out)
    sample (gn, prevD, hi, lo, nxt, sumD) draw p =
      let thr = 0.5 * (hi + lo)
          d = draw - thr
          (hi', lo') | not p = (hi, lo)
                     | draw > thr = (hi + alpha * (draw - hi), lo)
                     | otherwise = (hi, lo + alpha * (draw - lo))
          tNow = fromIntegral gn :: Double
          -- a zero crossing marks a bit boundary; pull the clock towards it
          nxt1 = if (prevD >= 0) /= (d >= 0) && prevD /= d && p
                   then let c = fromIntegral (gn - 1) + prevD / (prevD - d)
                            boundary = nxt - 0.5 * spb
                            err = c - boundary
                            err' = err - spb * fromIntegral (round (err / spb) :: Int)   -- nearest boundary
                        in nxt + gain * err'
                   else nxt
          sumD' = if tNow >= nxt1 - halfWin - 0.5 then sumD + d else sumD
      in if tNow + 0.5 < nxt1 + halfWin
           then ((gn + 1, d, hi', lo', nxt1, sumD'), Nothing)
           else ((gn + 1, d, hi', lo', nxt1 + spb, 0), if p then Just (sumD' >= 0) else Nothing)

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
    stop = ceiling nStop
    one b = False : [testBit b k | k <- [0 .. nData - 1]] ++ replicate stop True

-- | Bytes to keyed levels at a baud rate.  This is the framer for
-- anything the uniform bit stream cannot say: a fractional stop period,
-- and (later) a mode whose mark and space bits differ in length.
frameKeyed :: Framing -> Double -> [Word8] -> Keying
frameKeyed (Framing nData nStop) baud = concatMap one
  where
    bit = 1 / baud
    one b = (Space, bit)
          : [ (if testBit b k then Mark else Space, bit) | k <- [0 .. nData - 1] ]
         ++ [ (Mark, nStop * bit) ]

-- | How a carrierless mode keys the line around a burst of characters.
data Burst = Burst
  { buLeadIn :: !Double   -- ^ seconds of mark before the first character
  , buHold   :: !Double   -- ^ seconds of mark held after the last one
  } deriving (Eq, Show)

-- | V.18 Annex A recommends opening a burst with 150 ms of mark, which
-- is there to give the far end's tone detector something to lock to
-- before the first start bit arrives.  Legacy sets hold mark for as
-- much as a second after the last character, so a receiver has to
-- tolerate a long tail; 300 ms is a polite amount to send.
defaultBurst :: Burst
defaultBurst = Burst 0.150 0.300

-- | A burst as a carrierless mode keys it: lead-in, characters, hold.
-- The silence on either side is the caller's business -- it is the
-- absence of any keying at all.
keyedBurst :: Framing -> Double -> Burst -> [Word8] -> Keying
keyedBurst fr baud (Burst lead hold) bs
  | null bs = []
  | otherwise = (Mark, lead) : frameKeyed fr baud bs ++ [(Mark, hold)]

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

-- | Continuous-phase FSK of keyed levels, each held for its own time.
--
-- Deliberately not shared with 'modulateBits', which stays as it is.
-- That one places its bit boundaries at @ceiling (i * spb)@ from an
-- exact product; this one has to accumulate durations, and at a rate
-- where a bit is a whole number of samples (300 baud at 48 kHz, say)
-- the rounding of a running sum can land a boundary one sample away
-- from where the product does.  Nothing here is worth risking a tuned
-- mode's timing for, so the two coexist.
--
-- Phase runs on through a level change, as a real keyer's does, and
-- stands still through 'Off' so that a burst resumes where the last one
-- stopped rather than wherever silence would have carried it.
modulateKeyed :: Double -> FskSpec -> Keying -> Signal
modulateKeyed fs spec ks
  | n == 0 || total <= 0 = VS.empty
  | otherwise = VS.unfoldrN total step (0 :: Int, 0 :: Int, 0 :: Double)
  where
    n = length ks
    freqs = VU.fromList [ freqOf lv | (lv, _) <- ks ]
    freqOf Mark = fskMark spec
    freqOf Space = fskSpace spec
    freqOf Off = 0
    ends = VU.fromList [ ceiling (t * fs) | t <- tail (scanl (\a (_, d) -> a + d) 0 ks) ] :: VU.Vector Int
    total = VU.last ends
    twoPi = 2 * pi
    step (!k, !i0, !ph) =
      let i = advance i0
          advance j | j < n - 1 && k >= VU.unsafeIndex ends j = advance (j + 1)
                    | otherwise = j
          f = VU.unsafeIndex freqs i
      in if f <= 0
           then Just (0, (k + 1, i, ph))
           else let ph' = ph + twoPi * f / fs
                    ph'' = if ph' >= twoPi then ph' - twoPi else ph'
                in Just (sin ph, (k + 1, i, ph''))

-- | Transmit band limiting kernel: real modems filter their output so
-- that keying splatter does not land in the other direction's channel.
txFilterKernel :: Double -> FskSpec -> VS.Vector Double
txFilterKernel fs spec = firBandpass fs (max 50 (lo - g)) (min (fs / 2 - 50) (hi + g)) (filterTaps fs)
  where
    lo = min (fskMark spec) (fskSpace spec)
    hi = max (fskMark spec) (fskSpace spec)
    g = 300

-- | Offline transmit band limiting, delay compensated.
txFilter :: Double -> FskSpec -> Signal -> Signal
txFilter fs spec = firCentered (txFilterKernel fs spec)

-- | Frame and modulate bytes, with @pre@ and @post@ seconds of mark
-- (idle) around the data, at amplitude @amp@, band limited.
encodeBytes :: Double -> FskSpec -> Framing -> Double -> Double -> Double -> [Word8] -> Signal
encodeBytes fs spec fr amp pre post bytes = txFilter fs spec (VS.map (* amp) (modulateBits fs spec bits))
  where
    idle secs = replicate (round (secs * fskBaud spec)) True
    bits = idle pre ++ frameBits fr bytes ++ idle post

-- | Offline: one keyed burst with silence around it, band limited.  The
-- carrierless counterpart of 'encodeBytes', whose @pre@ and @post@ are
-- idle mark rather than silence.
encodeBurst :: Double -> FskSpec -> Framing -> Burst -> Double -> Double -> Double -> [Word8] -> Signal
encodeBurst fs spec fr burst amp pre post bytes =
  txFilter fs spec (VS.map (* amp) (modulateKeyed fs spec keyed))
  where
    keyed = (Off, pre) : keyedBurst fr (fskBaud spec) burst bytes ++ [(Off, post)]
