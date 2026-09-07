{-# LANGUAGE BangPatterns #-}
-- | Tone detection: a streaming bank of tone correlators producing one
-- 'ToneFrame' per hop, plus offline helpers that classify a recording
-- (which FSK standard and channel is present, answer tones seen).
module Modec.Detect
  ( ToneFrame (..)
  , ampAt
  , ToneBankConfig (..)
  , defaultToneBank
  , diagnosticToneBank
  , toneBank
  , toneFrames
  , dominant
  , toneAmp
  , detectFsk
  , ToneRun (..)
  , toneRuns
  , toneRunsWith
  ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Modec.DSP
import Modec.Standards
import Modec.Stream

-- | One measurement frame: time of the frame end in seconds, estimated
-- amplitude (full scale = 1) at each configured frequency, and the RMS
-- of the frame.
--
-- A frame carries the frequencies it was measured at, not just the
-- amplitudes, so that reading one is a question about the frame alone.
-- Four banks are in flight here -- the handshake's, the diagnostic one,
-- 'detectFsk'\'s widened one and the call-progress one -- and they
-- deliberately differ in both length and window, so a frame read against
-- the wrong list does not fail: it answers about a frequency that was
-- never measured, or about the wrong one entirely, and the answer is
-- plausible.  That is the failure this field exists to make impossible.
data ToneFrame = ToneFrame
  { tfTime  :: !Double
  , tfFreqs :: !(VU.Vector Double)   -- ^ the bank that measured it
  , tfAmps  :: !(VU.Vector Double)
  , tfRms   :: !Double
  } deriving (Show)

data ToneBankConfig = ToneBankConfig
  { tbFreqs     :: [Double]   -- ^ frequencies to measure
  , tbWindowSec :: Double     -- ^ analysis window length
  , tbHopSec    :: Double     -- ^ time between frames
  } deriving (Show)

-- | All FSK tones of the supported standards, the answer tones, the V.25
-- calling tone and the V.8bis signal tones.  40 ms windows resolve
-- 2025/2100/2225 Hz.
defaultToneBank :: ToneBankConfig
defaultToneBank = ToneBankConfig
  { tbFreqs = [980, 1070, 1180, 1270, 1300, 1650, 1850, 2025, 2100, 2225, 400, 650, 1150, 1375, 1529, 1900, 2002]
  , tbWindowSec = 0.04
  , tbHopSec = 0.02
  }

-- | Global index of the first kept sample, global index of the next
-- frame end, and the samples kept for the window.
data BankState = BankState !Int !Int !Signal

-- | Streaming tone bank.  Emits zero or more frames per chunk.
toneBank :: Double -> ToneBankConfig -> Stage Signal [ToneFrame]
toneBank fs cfg = Stage (BankState 0 win VS.empty) step
  where
    win = max 1 (round (fs * tbWindowSec cfg))
    hop = max 1 (round (fs * tbHopSec cfg))
    freqs = VU.fromList (tbFreqs cfg)
    ws = VU.map (\f -> 2 * pi * f / fs) freqs
    hann = hannWindow win
    hsum = VS.sum hann
    measure buf nEnd =   -- buf holds exactly the window ending at global index nEnd
      let amps = VU.map (\w -> corrAmp w) ws
          corrAmp w =
            let (re, im) = VS.ifoldl' (\(!a, !b) i v ->
                              let x = v * VS.unsafeIndex hann i
                                  th = w * fromIntegral (nEnd - win + i)
                              in (a + x * cos th, b + x * sin th)) (0, 0) buf
            in toneAmplitudeW hsum (re * re + im * im)
      in ToneFrame (fromIntegral nEnd / fs) freqs amps (rms buf)
    step (BankState start nextEnd kept) chunk =
      let ext = kept VS.++ chunk
          total = start + VS.length ext
          ends = takeWhile (<= total) [nextEnd, nextEnd + hop ..]
          frames = [ measure (VS.slice (e - win - start) win ext) e | e <- ends, e - win >= start ]
          nextEnd' = if null ends then nextEnd else last ends + hop
          keepFrom = max start (nextEnd' - win)
          kept' = VS.drop (keepFrom - start) ext
      in kept' `seq` (BankState keepFrom nextEnd' kept', frames)

-- | Offline: all frames of a signal.
toneFrames :: Double -> ToneBankConfig -> Signal -> [ToneFrame]
toneFrames fs cfg x = concatStage (toneBank fs cfg) [x]

-- | Amplitude at a frequency, given the list it was measured against
-- and the amplitudes.  0 if that frequency is not in the list.
ampAt :: VU.Vector Double -> VU.Vector Double -> Double -> Double
ampAt freqs amps f = case VU.elemIndex f freqs of
  Just i | i < VU.length amps -> VU.unsafeIndex amps i
  _ -> 0

-- | Amplitude of a frequency in a frame (0 if not measured).
toneAmp :: ToneFrame -> Double -> Double
toneAmp fr = ampAt (tfFreqs fr) (tfAmps fr)

-- | The dominant tone of a frame, if one is above @squelch@ and at least
-- @ratio@ times every other measured tone.
dominant :: Double -> Double -> ToneFrame -> Maybe Double
dominant squelch ratio fr =
  case sortBy (comparing (Down . snd)) (zip (VU.toList (tfFreqs fr)) (VU.toList (tfAmps fr))) of
    ((f, a) : rest)
      | a > squelch && all (\(_, b) -> a >= ratio * b) rest -> Just f
    _ -> Nothing

-- | Offline classification of a recording: which FSK channel is
-- present.  For every frame the combined energy at each channel's two
-- tones is compared; a channel wins a frame when it has at least twice
-- the energy of every other channel.  The score is the fraction of
-- frames won, sorted best first.  The 40 ms analysis window resolves
-- the 90 Hz between the overlapping V.21 channel 1 and Bell 103
-- originate tones.  Idle mark scores as well as data; this identifies
-- the channel, not activity.
detectFsk :: Double -> Signal -> [(FskSpec, Double)]
detectFsk fs x = sortBy (comparing (Down . snd)) [ (s, score s) | s <- fskStandards ]
  where
    -- Offline this bank can be wider and slower than the handshake's:
    -- it needs the V.23 backward pair, which the handshake deliberately
    -- leaves out (390 Hz is one bin from the V.8bis CRe tone at 400 Hz),
    -- and it needs to separate the V.23 forward mark at 1300 Hz from the
    -- Bell 103 originate mark at 1270 Hz.  Thirty Hz is inside a 40 ms
    -- window's bin, so the window is doubled here; 12.5 Hz bins tell them
    -- apart, at the cost of a resolution in time no report needs.
    --
    -- It also needs the text telephone pair, and that pair is why the
    -- doubled window is not optional: 1800 Hz sits two bins from the
    -- V.21 channel 2 space at 1850 and 1400 Hz two bins from the V.8bis
    -- CRe tone at 1375, and at a 40 ms window neither is separable at
    -- all -- a 45 baud recording measured on the handshake's bank comes
    -- back as V.21 channel 2.
    cfg = defaultToneBank
      { tbFreqs = fskMark v23Backward : fskSpace v23Backward
                : fskMark tdd45 : fskSpace tdd45 : tbFreqs defaultToneBank
      , tbWindowSec = 0.08 }
    frames = toneFrames fs cfg x
    n = max 1 (length frames)
    energy fr s = let m = toneAmp fr (fskMark s); sp = toneAmp fr (fskSpace s) in m * m + sp * sp
    winner fr = case sortBy (comparing (Down . snd)) [ (s, energy fr s) | s <- fskStandards ] of
      ((s, e) : rest) | e > 9e-6 && all (\(_, e') -> e >= 2 * e') rest -> Just (fskName s)
      _ -> Nothing
    wins = map winner frames
    score s = fromIntegral (length (filter (== Just (fskName s)) wins)) / fromIntegral n

-- | A run of frames with the same dominant tone.
data ToneRun = ToneRun
  { trTone  :: !(Maybe Double)   -- ^ Nothing = no dominant tone / silence
  , trStart :: !Double
  , trEnd   :: !Double
  } deriving (Show, Eq)

-- | The bank for reading a recording rather than running a handshake.
--
-- It adds 2250 Hz, the unscrambled binary 1 an answering V.22 modem
-- sends.  The handshake's bank leaves it out on purpose: at a 40 ms
-- window the bins are 25 Hz wide, so 2250 and the 2225 Hz Bell answer
-- tone sit one bin apart and neither would dominate the other, and the
-- handshake would stop recognising the Bell tone.  It does not need the
-- frequency anyway -- it tells the two apart by the quality of the phase
-- steps.  A report has no such fallback, and calling V.22's carrier a
-- Bell answer tone is worse than useless: it reads as evidence that a
-- far end offers Bell modes when it never did.  Doubling the window
-- halves the bin width and separates them.
--
-- The text telephone pair is here for the same reason.  Without it a
-- 45 baud recording reports its 1400 Hz mark as the 1375 Hz V.8bis CRe
-- tone, which reads as a far end offering a capabilities exchange that
-- was never there; the two are 25 Hz apart and only the doubled window
-- tells them apart.
diagnosticToneBank :: ToneBankConfig
diagnosticToneBank = defaultToneBank
  { tbFreqs = 2250 : fskMark tdd45 : fskSpace tdd45 : tbFreqs defaultToneBank
  , tbWindowSec = 0.08
  }

-- | Collapse frames into runs of dominant tones, e.g. to find a 2100 Hz
-- answer tone lasting 2.6-4 s followed by 75 ms of silence.
toneRuns :: Double -> Signal -> [ToneRun]
toneRuns = toneRunsWith defaultToneBank

-- | 'toneRuns' with a bank of your choosing.
toneRunsWith :: ToneBankConfig -> Double -> Signal -> [ToneRun]
toneRunsWith cfg fs x = go (map (\fr -> (dominant 3e-3 1.5 fr, tfTime fr)) frames)
  where
    frames = toneFrames fs cfg x
    go [] = []
    go ((t, time) : rest) =
      let (same, others) = span ((== t) . fst) rest
          end = if null same then time else snd (last same)
      in ToneRun t (time - tbWindowSec cfg) end : go others
