{-# LANGUAGE BangPatterns #-}
-- | Is somebody talking?  A voice detector for reading recorded calls,
-- not for a live modem: it answers whether a stretch of audio is speech
-- -- a person, an answering machine, a recorded announcement -- as
-- opposed to the things a telephone line otherwise carries, which are
-- silence, noise, tones and modem carriers.
--
-- Speech is recognised by two things the others lack together.  Its
-- loudness rises and falls at the rate of syllables, a few times a
-- second, with deep dips between them; and while it does, no one
-- frequency holds the line.  Each alone is not enough.  Noise and a
-- modem carrier are broadband but steady, so the first test throws
-- them out.  Congestion is a pair of tones switched four times a
-- second, which is a syllabic rhythm, so the second throws it out: its
-- bursts are tones the call-progress bank names outright.
--
-- A one-second window can be fooled by a single event: a ring starting,
-- or a modem stepping from its answer tone to its carrier, is one deep
-- change of level with nothing tonal enough in the frames either side
-- to veto it.  Measured over the recorded calls those were the only
-- false alarms, and they are isolated -- a ring is six seconds from the
-- next one.  So a window is only evidence, and speech is a run of such
-- windows lasting a couple of seconds.
module Modec.Speech
  ( SpeechParams (..)
  , defaultSpeechParams
  , SpeechWindow (..)
  , speechBank
  , speechWindows
  , speechWindowsFrames
  , speechRuns
  , speechRunsFrames
  , speechSeconds
  ) where

import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as VU

import Modec.DSP (Signal)
import Modec.Detect
import Modec.Progress

data SpeechParams = SpeechParams
  { spSquelchDb  :: Double
    -- ^ frame level, dB below full scale, under which a frame is quiet
  , spActive     :: Double
    -- ^ share of a window's frames that must be above the squelch
  , spTonal      :: Double
    -- ^ largest share of the active frames that may be tonal
  , spToneShare  :: Double
    -- ^ share of a frame's power one measured tone must hold for the
    -- frame to count as tonal
  , spSpreadDb   :: Double
    -- ^ least standard deviation of the level across a window
  , spSyllabic   :: Double
    -- ^ least share of the level's variation that is at 2-8 Hz
  , spMinWindows :: Int
    -- ^ fewest windows in a run for the run to be speech
  , spMaxGap     :: Double
    -- ^ largest gap, in seconds, between window starts within one run
  } deriving (Eq, Show)

defaultSpeechParams :: SpeechParams
defaultSpeechParams = SpeechParams
  { spSquelchDb = -50
  , spActive = 0.5
  , spTonal = 0.5
  , spToneShare = 0.5
  , spSpreadDb = 4
  , spSyllabic = 0.4
  , spMinWindows = 3
  , spMaxGap = 2.0
  }

-- | One second of audio as the detector measured it.
data SpeechWindow = SpeechWindow
  { swStart    :: !Double
  , swActive   :: !Double   -- ^ share of frames above the squelch
  , swTonal    :: !Double   -- ^ share of those that were tonal
  , swSpreadDb :: !Double   -- ^ standard deviation of the level
  , swSyllabic :: !Double   -- ^ share of the level's variation at 2-8 Hz
  , swSpeech   :: !Bool
  } deriving (Eq, Show)

-- | The call-progress frequencies and every modem tone the handshake
-- bank measures, at the call-progress window.  A frame measured on it
-- can be asked both whether it is a progress signature and whether one
-- modem tone holds the line.
speechBank :: ToneBankConfig
speechBank = progressToneBank
  { tbFreqs = progressFreqs ++ [ f | f <- tbFreqs defaultToneBank, f `notElem` progressFreqs ] }

-- | Frames per window, and frames between window starts.
windowFrames, windowHop :: Int
windowFrames = 50
windowHop = 25

frameSec :: Double
frameSec = tbHopSec speechBank

speechWindows :: Double -> SpeechParams -> Signal -> [SpeechWindow]
speechWindows fs sp x = speechWindowsFrames sp (toneFrames fs speechBank x)

-- | The same from frames already measured on 'speechBank', for a caller
-- that wants them for something else as well.
speechWindowsFrames :: SpeechParams -> [ToneFrame] -> [SpeechWindow]
speechWindowsFrames sp frames = go 0
  where
    levels = VU.fromList [ 20 * logBase 10 (max 1e-9 (tfRms fr)) | fr <- frames ]
    tonal = VU.fromList (map isTonal frames)
    n = VU.length levels
    isTonal fr =
      isJust (sigOfFrame defaultProgressParams fr)
        || let a = VU.maximum (tfAmps fr)
               r = tfRms fr
           in r > 0 && a * a / 2 >= spToneShare sp * r * r
    go i
      | i + windowFrames > n = []
      | otherwise = window i : go (i + windowHop)
    window i =
      let lv = VU.slice i windowFrames levels
          tn = VU.slice i windowFrames tonal
          act = VU.map (> spSquelchDb sp) lv
          nAct = VU.length (VU.filter id act)
          nTon = VU.length (VU.filter id (VU.zipWith (&&) act tn))
          active = fromIntegral nAct / fromIntegral windowFrames
          tonalShare = if nAct == 0 then 0 else fromIntegral nTon / fromIntegral nAct
          clipped = VU.map (max (spSquelchDb sp - 10)) lv
          m = VU.sum clipped / fromIntegral windowFrames
          env = VU.map (subtract m) clipped
          spread = sqrt (VU.sum (VU.map (^ (2 :: Int)) env) / fromIntegral windowFrames)
          -- The level's spectrum in whole cycles per window: with a
          -- one-second window, bin k is k Hz.
          power k =
            let w = 2 * pi * fromIntegral k / fromIntegral windowFrames
                (c, s) = VU.ifoldl' (\(!a, !b) j v -> (a + v * cos (w * fromIntegral j), b + v * sin (w * fromIntegral j))) (0, 0) env
            in c * c + s * s
          total = sum (map power [1 .. windowFrames `div` 2 :: Int])
          syll = if total <= 0 then 0 else sum (map power [2 .. 8 :: Int]) / total
          speech = active >= spActive sp && tonalShare <= spTonal sp
                   && spread >= spSpreadDb sp && syll >= spSyllabic sp
      in SpeechWindow (fromIntegral i * frameSec) active tonalShare spread syll speech

-- | Stretches of speech, as (start, end) in seconds.
speechRuns :: Double -> SpeechParams -> Signal -> [(Double, Double)]
speechRuns fs sp x = speechRunsFrames sp (toneFrames fs speechBank x)

speechRunsFrames :: SpeechParams -> [ToneFrame] -> [(Double, Double)]
speechRunsFrames sp frames =
  [ (swStart (head ws), swStart (last ws) + winSec)
  | ws <- reverse (map reverse (foldl' add [] hits)), length ws >= spMinWindows sp ]
  where
    winSec = fromIntegral windowFrames * frameSec
    hits = filter swSpeech (speechWindowsFrames sp frames)
    add [] w = [[w]]
    add (run@(p : _) : runs) w
      | swStart w - swStart p <= spMaxGap sp = (w : run) : runs
      | otherwise = [w] : run : runs
    add ([] : runs) w = [w] : runs

-- | Total seconds of speech in a set of runs.
speechSeconds :: [(Double, Double)] -> Double
speechSeconds rs = sum [ b - a | (a, b) <- rs ]
