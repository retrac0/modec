-- | Call progress tones: what the network plays back at a caller
-- before, instead of, and after a connection.  Dial tone, ringing,
-- busy, congestion and the special information tone that introduces a
-- recorded announcement.
--
-- A modem that cannot hear these waits out its timeout and says NO
-- CARRIER whatever happened, which is the least informative thing it
-- could say: a number that is busy, a number that no longer exists and
-- a number whose owner is asleep all look the same to it.  They do not
-- sound the same, and the difference is most of what a survey of
-- dial-up numbers is trying to record.
--
-- Two layers.  The first names the tones sounding in each frame
-- ('Sig'); the second reads the rhythm of those frames ('Progress'),
-- because in most of the world rhythm is the only thing that separates
-- the meanings.  North America gives each signal its own pair of
-- frequencies -- 350+440 for dial tone, 440+480 for ringing, 480+620
-- for busy and congestion -- but ITU-T E.180 country practice is one
-- 425 Hz tone for all of them, cut into a different cadence for each:
-- continuous for dial tone, half a second on and half off for busy, a
-- quarter and a quarter for congestion.  So the frequencies alone can
-- never be enough, and the classifier measures the on and off times
-- and matches them against a table.
--
-- The frame window is 60 ms, which is the compromise the whole module
-- turns on.  It has to resolve 440 Hz from 480 Hz, 40 Hz apart, or a
-- lone European tone would read as North American ringing: at 60 ms
-- the bins are 16.7 Hz, so 40 Hz is 2.4 of them, past the main lobe of
-- the Hann window and better than 20 dB down.  It also has to measure
-- a cadence whose shortest element is the 250 ms of congestion, and a
-- window longer than about a quarter of that smears one element into
-- the next.  Sixty milliseconds satisfies both; 100 ms would resolve
-- the frequencies better and report congestion as busy.
--
-- Frequencies closer together than that are deliberately not told
-- apart.  425 and 440 Hz are one bin apart and no window this receiver
-- could afford would separate them, so they are one signature with one
-- meaning: a progress tone whose cadence has yet to be read.
module Modec.Progress
  ( -- * What was heard
    Progress (..)
  , SitSegment (..)
  , ProgressEvent (..)
  , describeProgress
    -- * Tones
  , Sig (..)
  , sigOfFrame
  , progressFreqs
  , progressToneBank
    -- * Detecting
  , ProgressParams (..)
  , defaultProgressParams
  , ProgressRx
  , progressRxInit
  , progressRxBlock
  , progressDetector
  , callProgress
  , callProgressWith
  ) where

import Data.List (dropWhileEnd, foldl', maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)
import qualified Data.Vector.Unboxed as VU

import Modec.DSP (Signal)
import Modec.Detect
import Modec.Stream

-- | Every frequency the classifier measures.
progressFreqs :: [Double]
progressFreqs =
  [ 350, 400, 425, 440, 450, 480, 620   -- dial tone, ringing, busy and congestion
  , 913.8, 950, 985.2                   -- special information tone, first segment
  , 1100                                -- fax calling tone (CNG)
  , 1370.6, 1400, 1428.5                -- special information tone, second segment
  , 1776.7, 1800                        -- special information tone, third segment
  , 2100, 2225                          -- answer tones
  ]

-- | The bank the classifier reads.  See the module header for why the
-- window is 60 ms.
progressToneBank :: ToneBankConfig
progressToneBank = ToneBankConfig
  { tbFreqs = progressFreqs
  , tbWindowSec = 0.06
  , tbHopSec = 0.02
  }

-- | The frequencies of each segment of a special information tone.
-- Both the North American set and the 950/1400/1800 of E.180 are here,
-- because a segment is recognised by its band rather than by which
-- administration's tone it is.
sitBand :: Int -> [Double]
sitBand 1 = [913.8, 950, 985.2]
sitBand 2 = [1370.6, 1400, 1428.5]
sitBand 3 = [1776.7, 1800]
sitBand _ = []

-- | The tones sounding in one frame, named by what combination they
-- are rather than by what they mean; the meaning is the cadence's to
-- decide.
data Sig
  = SigDial          -- ^ 350 + 440 Hz, the North American dial tone pair
  | SigRing          -- ^ 440 + 480 Hz, the North American audible ringing pair
  | SigBusy          -- ^ 480 + 620 Hz, the North American busy and congestion pair
  | SigSingle        -- ^ one tone around 400-450 Hz: the E.180 progress tone, whatever it means
  | SigCng           -- ^ 1100 Hz, a calling fax machine
  | SigAnswer        -- ^ 2100 or 2225 Hz, an answering modem or fax
  | SigSit !Int      -- ^ segment 1, 2 or 3 of a special information tone
  deriving (Eq, Show)

-- | One measured segment of a special information tone.
data SitSegment = SitSegment
  { ssFreq     :: !Double
  , ssDuration :: !Double
  } deriving (Eq, Show)

-- | What the network was saying.
data Progress
  = DialTone
  | Ringback
  | Busy
  | Reorder
    -- ^ congestion, the fast busy: no circuit was available, which is
    -- a different thing from the called line being in use
  | Sit [SitSegment]
    -- ^ the three rising tones that introduce a recorded announcement.
    -- Every one of them means the call did not complete, and no amount
    -- of redialling will change that.
    --
    -- The segments are reported as measured rather than classified.
    -- Telcordia's table gives each combination of segment frequencies
    -- and durations a meaning -- intercept, vacant code, reorder, no
    -- circuit -- and the published copies of that table do not agree
    -- with one another about which combination is which.  Nothing here
    -- has yet heard a real one to settle it against, so the
    -- measurements are handed over and the naming waits until there is
    -- a recording to check a name against.
  | FaxCalling
    -- ^ CNG: a fax machine is calling us
  | AnswerTone
  deriving (Eq, Show)

-- | Something recognised, where it started, and the rhythm it was
-- recognised by.
data ProgressEvent = ProgressEvent
  { peKind    :: !Progress
  , peStart   :: !Double                     -- ^ when the pattern began
  , peCadence :: !(Maybe (Double, Double))   -- ^ mean seconds on and off
  } deriving (Eq, Show)

-- | One line for a log or a report.
describeProgress :: ProgressEvent -> String
describeProgress ev = printf "%7.3f s  %s%s" (peStart ev) (name (peKind ev)) cadence
  where
    cadence = case peCadence ev of
      Just (on, off) -> printf " (%.2f s on, %.2f s off)" on off :: String
      Nothing -> ""
    name k = case k of
      DialTone -> "dial tone"
      Ringback -> "ringing"
      Busy -> "busy"
      Reorder -> "congestion (fast busy): no circuit"
      FaxCalling -> "fax calling tone (CNG)"
      AnswerTone -> "answer tone"
      Sit segs -> "special information tone -- the call did not complete: "
        ++ unwords [ printf "%.0f Hz/%.0f ms" (ssFreq s) (ssDuration s * 1000) | s <- segs ]

data ProgressParams = ProgressParams
  { ppSquelch    :: Double
    -- ^ amplitude the loudest measured tone must reach before a frame
    -- is anything but silence
  , ppRelative   :: Double
    -- ^ how loud, as a fraction of that loudest tone, another
    -- frequency must be to count as sounding as well
  , ppSeparation :: Double
    -- ^ frequencies closer than this to one of a signature's own are
    -- not held against it, because the window cannot separate them
  , ppMinSeg     :: Double
    -- ^ shortest run of frames kept as an element of a cadence;
    -- anything briefer is absorbed into the run before it
  , ppBursts     :: Int
    -- ^ fewest bursts of tone any cadence may be read from; the table
    -- asks for more where more is warranted
  , ppContinuous :: Double
    -- ^ how long an unbroken tone must last to be called continuous
  } deriving (Eq, Show)

defaultProgressParams :: ProgressParams
defaultProgressParams = ProgressParams
  { ppSquelch = 0.004
  , ppRelative = 0.25
  , ppSeparation = 42
  , ppMinSeg = 0.08
  , ppBursts = 2
  , ppContinuous = 1.0
  }

-- | Which signature a frame's tones make, if any.  The candidates are
-- tried in order and the first that fits wins, which is what settles
-- the overlaps: 480 Hz belongs to both the ringing pair and the busy
-- pair, and 620 Hz belongs only to busy, so busy is asked first.
sigOfFrame :: ProgressParams -> ToneFrame -> Maybe Sig
sigOfFrame pp fr
  | peak < ppSquelch pp = Nothing
  | otherwise = case [ s | (s, on, off, tol) <- candidates, fits on off tol ] of
      (s : _) -> Just s
      [] -> Nothing
  where
    amp = toneAmp progressToneBank fr
    peak = VU.maximum (tfAmps fr)
    strong f = amp f >= ppRelative pp * peak
    loudestOf = maximumBy (comparing amp)
    band = [400, 425, 440, 450]
    candidates =
      --  signature      sounding                 silent           allowed
      [ ( SigBusy,   [480, 620],                  [],              [] )
      , ( SigDial,   [350, 440],                  [620],           [] )
      , ( SigRing,   [440, 480],                  [350, 620],      [] )
      , ( SigSingle, [loudestOf band],            [350, 480, 620], band )
      , ( SigCng,    [1100],                      [],              [] )
      , ( SigAnswer, [loudestOf [2100, 2225]],    [],              [2100, 2225] )
      , ( SigSit 1,  [loudestOf (sitBand 1)],     [],              sitBand 1 )
      , ( SigSit 2,  [loudestOf (sitBand 2)],     [],              sitBand 2 )
      , ( SigSit 3,  [loudestOf (sitBand 3)],     [],              sitBand 3 )
      ]
    -- Every tone of the signature has to be sounding, and every tone
    -- that is neither part of it, nor too close to be told apart from
    -- it, nor listed as one it tolerates, has to be silent.  That last
    -- clause is what keeps noise, speech and modem carriers out: they
    -- put energy across the band, so something outside the signature
    -- is always up with the peak.
    --
    -- The tolerated list is for signatures that name a band rather
    -- than a frequency.  A country's ringing tone may be one tone near
    -- 425 Hz or two at 400 and 450, and the segments of a special
    -- information tone come in a North American and an international
    -- flavour 36 Hz apart; in each case what matters is which band
    -- sounded, and the neighbours inside that band are the signature
    -- itself rather than evidence against it.
    fits on off tol = all strong on && not (any strong (off ++ elsewhere on tol))
    elsewhere on tol =
      [ f | f <- progressFreqs, f `notElem` tol, all (\g -> abs (f - g) > ppSeparation pp) on ]

-- | A run of frames that named the same signature.  The summed
-- amplitudes are carried along so that a run can be asked afterwards
-- which frequency of its band it actually was, which is how a special
-- information tone gets reported as measured.
data Seg = Seg
  { segSig   :: !(Maybe Sig)
  , segStart :: !Double
  , segEnd   :: !Double
  , segAmps  :: !(VU.Vector Double)
  }

segDur :: Seg -> Double
segDur s = segEnd s - segStart s

joinSeg :: Seg -> Seg -> Seg
joinSeg a b = a { segEnd = segEnd b, segAmps = addAmps (segAmps a) (segAmps b) }

addAmps :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
addAmps a b
  | VU.null a = b
  | VU.null b = a
  | otherwise = VU.zipWith (+) a b

-- | The loudest frequency of a band over a whole run.
segFreq :: [Double] -> Seg -> Double
segFreq band s
  | VU.null (segAmps s) || null band = 0
  | otherwise = maximumBy (comparing amp) band
  where
    amp f = maybe 0 id (lookup f (zip progressFreqs (VU.toList (segAmps s))))

-- | Frames to runs.  A frame is taken to cover the hop beginning at
-- its window centre, so a run's boundaries fall at the midpoints of
-- the transitions rather than a whole window late.
runsOf :: ProgressParams -> [ToneFrame] -> [Seg]
runsOf pp = foldr add []
  where
    hop = tbHopSec progressToneBank
    add fr acc = case acc of
      (nxt : rest) | segSig nxt == s ->
        nxt { segStart = t, segAmps = addAmps (tfAmps fr) (segAmps nxt) } : rest
      _ -> Seg s t (t + hop) (tfAmps fr) : acc
      where
        s = sigOfFrame pp fr
        t = tfTime fr - tbWindowSec progressToneBank / 2

-- | Absorb any run too short to be an element of a cadence into the
-- run before it, then join runs that have become the same.  One frame
-- disagreeing in the middle of a tone must not read as the tone
-- stopping and starting again.
smooth :: ProgressParams -> [Seg] -> [Seg]
smooth pp = reverse . foldl' add []
  where
    add [] s = [s]
    add (p : ps) s
      | segDur s < ppMinSeg pp = joinSeg p s : ps
      | segSig p == segSig s = joinSeg p s : ps
      | otherwise = s : p : ps

-- | The runs as the classifier should see them.  A final run shorter
-- than the minimum is left out rather than absorbed: it may yet grow
-- into an element of its own on the next chunk of audio, and absorbing
-- it now would decide that permanently and make the answer depend on
-- where the audio was cut.
view :: ProgressParams -> [Seg] -> [Seg]
view pp raw
  | null raw = []
  | segDur (last raw) < ppMinSeg pp = smooth pp (init raw)
  | otherwise = smooth pp raw

-- | The cadence table, in seconds.  Busy and congestion are the same
-- tones at two speeds, so their ranges must not overlap and they meet
-- at 0.32 s; the rest are far enough apart to be generous with.
--
-- Busy and congestion ask for a third burst where ringing and the fax
-- calling tone are content with two, and the reason is speech.  A
-- voice on an answered line puts a wandering tone through the 400 Hz
-- region in bursts of a couple of hundred milliseconds, which is the
-- length and the spacing of congestion; over 221 recorded calls
-- exactly one stretch of speech lined up well enough to produce two
-- such bursts and a gap, and none came close to three.  Ringing and
-- CNG are slow enough that nothing in speech resembles them, and they
-- can afford to be recognised sooner -- which matters, since a third
-- burst of ringing is fourteen seconds away.
data Cadence = Cadence
  { cdKind   :: Progress
  , cdSigs   :: [Sig]
  , cdOn     :: (Double, Double)
  , cdOff    :: (Double, Double)
  , cdBursts :: Int
  }

cadences :: [Cadence]
cadences =
  [ Cadence Reorder    [SigBusy, SigSingle] (0.15, 0.32) (0.15, 0.32) 3
  , Cadence Busy       [SigBusy, SigSingle] (0.32, 0.85) (0.32, 0.85) 3
  , Cadence Ringback   [SigRing, SigSingle] (0.60, 3.20) (1.80, 6.50) 2
  , Cadence FaxCalling [SigCng]             (0.30, 0.70) (2.00, 4.00) 2
  ]

within :: (Double, Double) -> Double -> Bool
within (lo, hi) v = v >= lo && v <= hi

-- | The trailing alternation of one signature and silence: where it
-- began, how long each burst lasted and how long each gap between them
-- lasted.  Silence at either end is not a gap.
alternation :: [Seg] -> Maybe (Sig, Double, [Double], [Double])
alternation segs = case trimmed of
  [] -> Nothing
  (first : _) -> do
    sig <- segSig (last trimmed)
    let run = dropWhile ((== Nothing) . segSig)
                (reverse (takeWhile (\s -> segSig s == Just sig || segSig s == Nothing)
                                    (reverse trimmed)))
        ons = [ segDur s | s <- run, segSig s == Just sig ]
        offs = [ segDur s | s <- run, segSig s == Nothing ]
    if null ons then Nothing else Just (sig, maybe (segStart first) segStart (headMay run), ons, offs)
  where
    trimmed = dropWhileEnd ((== Nothing) . segSig) segs
    headMay xs = case xs of { (x : _) -> Just x; [] -> Nothing }

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- | Match the trailing alternation against the cadence table.
cadenced :: ProgressParams -> [Seg] -> Maybe ProgressEvent
cadenced pp segs = do
  (sig, start, ons, offs) <- alternation segs
  -- The burst that is sounding right now has not finished, so it says
  -- nothing about how long a burst lasts; only the ones behind it do.
  -- Leaving it out of the statistics is also what makes the answer the
  -- same whether the audio arrived in one piece or in 20 ms blocks,
  -- since a recording read in one go would otherwise measure that last
  -- burst complete and a live call would measure it half grown.
  let done = if length ons > 1 then init ons else ons
      -- The first burst goes unchecked as well, because a detector
      -- that started listening in the middle of one measures it short
      -- through no fault of the far end -- and that burst would
      -- otherwise sit at the head of the alternation forever, refusing
      -- every cadence it belongs to.
      checked = if length done > 1 then drop 1 done else done
      onM = mean done
      offM = mean offs
      -- Every burst and every gap has to fit, not merely the average
      -- of them.  A machine-generated cadence is regular, and
      -- insisting on that is what keeps speech out and what stops half
      -- of a double ring -- a short gap and a long one -- from
      -- averaging out to the half second of a busy tone.
      plain = [ cdKind c | c <- cadences, sig `elem` cdSigs c
              , length ons >= cdBursts c
              , all (within (cdOn c)) checked, all (within (cdOff c)) offs ]
      -- A double ring -- two bursts close together, then a long
      -- silence -- has no single gap length, so it is matched on its
      -- shape instead.
      double = sig `elem` [SigRing, SigSingle] && length offs >= 2
        && all (within (0.2, 0.7)) checked
        && any (<= 0.5) offs && any (>= 1.4) offs
  if length ons < ppBursts pp || null offs
    then Nothing
    else if double
      then Just (ProgressEvent Ringback start (Just (onM, maximum offs)))
      else case plain of
        (k : _) -> Just (ProgressEvent k start (Just (onM, offM)))
        [] -> Nothing

-- | Three rising tones in the three special-information bands, back to
-- back, each about a third of a second.
sitOf :: [Seg] -> Maybe ProgressEvent
sitOf segs = case reverse (dropWhileEnd ((== Nothing) . segSig) segs) of
  (c : b : a : _)
    | segSig a == Just (SigSit 1), segSig b == Just (SigSit 2), segSig c == Just (SigSit 3)
    , all (\s -> segDur s >= 0.15 && segDur s <= 0.55) [a, b, c]
    , segStart b - segEnd a < 0.08, segStart c - segEnd b < 0.08
    -> Just (ProgressEvent (Sit [ SitSegment (segFreq (sitBand n) s) (segDur s)
                                | (n, s) <- zip [1 ..] [a, b, c] ])
                           (segStart a) Nothing)
  _ -> Nothing

-- | A tone that has simply stayed on.  Dial tone is the one the
-- network holds indefinitely; an answer tone is held for a few seconds
-- and is worth naming for the same reason.  A lone 425 Hz tone has to
-- wait longer than a North American dial tone pair before it can be
-- called continuous, because the same tone at the same frequency is
-- also that country's busy and ringing tone and only its silences tell
-- them apart.
continuous :: ProgressParams -> [Seg] -> Maybe ProgressEvent
continuous pp segs = case reverse segs of
  (s : _) -> case segSig s of
    Just SigDial   | segDur s >= ppContinuous pp -> Just (ev DialTone s)
    Just SigSingle | segDur s >= 3 * ppContinuous pp -> Just (ev DialTone s)
    Just SigAnswer | segDur s >= ppContinuous pp -> Just (ev AnswerTone s)
    _ -> Nothing
  [] -> Nothing
  where ev k s = ProgressEvent k (segStart s) Nothing

-- | What the most recent runs amount to: a special information tone
-- first, because its segments are a kind of cadence too, then the
-- cadence table, then a tone that never stopped.
classify :: ProgressParams -> [Seg] -> Maybe ProgressEvent
classify pp segs = case sitOf segs of
  Just ev -> Just ev
  Nothing -> case cadenced pp segs of
    Just ev -> Just ev
    Nothing -> continuous pp segs

data ProgressRx = ProgressRx
  { prBank :: Stage Signal [ToneFrame]
  , prRaw  :: [Seg]            -- ^ runs before smoothing, oldest first, bounded
  , prSeen :: !Int             -- ^ runs already offered to the classifier
  , prLast :: Maybe Progress
  }

progressRxInit :: Double -> ProgressParams -> ProgressRx
progressRxInit fs _ = ProgressRx (toneBank fs progressToneBank) [] 0 Nothing

-- | Feed a chunk of audio and get whatever became clear during it.  An
-- event is reported when the classification changes, so a call that
-- rings and then goes to busy reports both.
--
-- The runs are kept unsmoothed and the classifier is offered every
-- prefix of them that is new since the last chunk, so that a recording
-- handed over in one piece and the same recording arriving in 20 ms
-- blocks produce the same events.
progressRxBlock :: ProgressParams -> ProgressRx -> Signal -> (ProgressRx, [ProgressEvent])
progressRxBlock pp st chunk =
  (st { prBank = bank', prRaw = kept, prSeen = seen', prLast = last' }, reverse out)
  where
    (bank', frames) = stepStage (prBank st) chunk
    raw = joinRuns (prRaw st) (runsOf pp frames)
    n = length raw
    (last', out) = foldl' step (prLast st, []) [ view pp (take k raw) | k <- [max 1 (prSeen st) .. n] ]
    step (prev, acc) segs = case classify pp segs of
      Just ev | Just (peKind ev) /= prev -> (Just (peKind ev), ev : acc)
      _ -> (prev, acc)
    -- Only the last dozen runs can matter to any rule here, and a call
    -- that rings for a minute would otherwise keep every one of them.
    excess = max 0 (n - 12)
    kept = drop excess raw
    seen' = max 0 (n - excess)

-- | Append new runs to old, joining across the chunk boundary when the
-- signature did not change there.
joinRuns :: [Seg] -> [Seg] -> [Seg]
joinRuns [] new = new
joinRuns old [] = old
joinRuns old (n : ns)
  | segSig (last old) == segSig n = init old ++ (joinSeg (last old) n : ns)
  | otherwise = old ++ (n : ns)

-- | The detector as a stream stage, for the live modem.
progressDetector :: Double -> ProgressParams -> Stage Signal [ProgressEvent]
progressDetector fs pp = Stage (progressRxInit fs pp) (progressRxBlock pp)

-- | Everything a recording says happened, in order.
callProgress :: Double -> Signal -> [ProgressEvent]
callProgress fs = callProgressWith fs defaultProgressParams

callProgressWith :: Double -> ProgressParams -> Signal -> [ProgressEvent]
callProgressWith fs pp x = snd (progressRxBlock pp (progressRxInit fs pp) x)
