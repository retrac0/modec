{-# LANGUAGE BangPatterns #-}
-- | Tone detection: a streaming bank of tone correlators producing one
-- 'ToneFrame' per hop, plus offline helpers that classify a recording
-- (which FSK standard and channel is present, answer tones seen).
module Modec.Detect
  ( ToneFrame (..)
  , ToneBankConfig (..)
  , defaultToneBank
  , toneBank
  , toneFrames
  , dominant
  , toneAmp
  , detectFsk
  , ToneRun (..)
  , toneRuns
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
data ToneFrame = ToneFrame
  { tfTime :: !Double
  , tfAmps :: !(VU.Vector Double)
  , tfRms  :: !Double
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
      in ToneFrame (fromIntegral nEnd / fs) amps (rms buf)
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

-- | Amplitude of a frequency in a frame (0 if not measured).
toneAmp :: ToneBankConfig -> ToneFrame -> Double -> Double
toneAmp cfg fr f = case lookup f (zip (tbFreqs cfg) (VU.toList (tfAmps fr))) of
  Just a  -> a
  Nothing -> 0

-- | The dominant tone of a frame, if one is above @squelch@ and at least
-- @ratio@ times every other measured tone.
dominant :: ToneBankConfig -> Double -> Double -> ToneFrame -> Maybe Double
dominant cfg squelch ratio fr =
  case sortBy (comparing (Down . snd)) (zip (tbFreqs cfg) (VU.toList (tfAmps fr))) of
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
    cfg = defaultToneBank
    frames = toneFrames fs cfg x
    n = max 1 (length frames)
    energy fr s = let m = toneAmp cfg fr (fskMark s); sp = toneAmp cfg fr (fskSpace s) in m * m + sp * sp
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

-- | Collapse frames into runs of dominant tones, e.g. to find a 2100 Hz
-- answer tone lasting 2.6-4 s followed by 75 ms of silence.
toneRuns :: Double -> Signal -> [ToneRun]
toneRuns fs x = go (map (\fr -> (dominant cfg 3e-3 1.5 fr, tfTime fr)) frames)
  where
    cfg = defaultToneBank
    frames = toneFrames fs cfg x
    go [] = []
    go ((t, time) : rest) =
      let (same, others) = span ((== t) . fst) rest
          end = if null same then time else snd (last same)
      in ToneRun t (time - tbWindowSec cfg) end : go others
