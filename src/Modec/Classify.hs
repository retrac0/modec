-- | What answered: one verdict for a recorded call, from everything the
-- tone detectors can hear in it.
--
-- The modem's own outcome for a call says what the modem managed, which
-- is a different thing.  A call it logged as "no answer" may have been
-- answered by a modem it never recognised, and a call that ended in
-- congestion may have played a recorded announcement first.  This reads
-- the audio afresh, gathers the evidence with times, and applies a fixed
-- order of precedence to it:
--
-- 1. a modem: an answer tone, ANSam, or a steady carrier
-- 2. a fax: CNG, or a T.30 frame on V.21 channel 2
-- 3. a special information tone
-- 4. busy or congestion, unless speech was sounding when it began
-- 5. speech
-- 6. ringing that nobody answered
-- 7. no audio at all, or silence
--
-- The order matters where the evidence overlaps.  A modem that answers
-- plays its answer tone after ringing, so ringing is not a verdict while
-- anything later is.  An intercept plays speech before its tones, so
-- the tones win over speech that came earlier -- but a congestion
-- cadence read in the middle of speech is speech, which is the one
-- false positive the call-progress detector is known to have.
--
-- A modem answer tone and a fax answer tone (CED) are the same 2100 Hz,
-- so a fax is told apart only by what follows: T.30's HDLC frames on
-- V.21 channel 2.  Modems use that channel too, for V.8 and for the
-- V.22 ladder's 300 bit/s rung, but asynchronously, and nothing a data
-- modem sends there passes an HDLC frame check with T.30's address and
-- control octets.  A data/fax modem whose data modes all failed falls
-- back to answering as a fax, and on the recorded calls that is where
-- every fax came from.  Its CSI carries the station's own number, which
-- is worth reporting: it says which line the modem thinks it is on.
module Modec.Classify
  ( CallClass (..)
  , className
  , Evidence (..)
  , ClassifyParams (..)
  , defaultClassifyParams
  , classifyCall
  , classifyCallWith
  , describeClass
  , describeEvidence
  , echoMask
  , t30Frames
  , T30Frame (..)
  ) where

import Data.Bits ((.&.))
import Data.List (find, foldl', intercalate)
import Data.Word (Word8)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Text.Printf (printf)

import Modec.DSP (Signal, chunksOf)
import Modec.Detect
import Modec.FSK
import Modec.Hdlc (hdlcRxBits, hdlcRxInit)
import Modec.Progress hiding (Busy)
import qualified Modec.Progress as P
import Modec.Speech
import Modec.Standards (v21Channel2)
import Modec.Stream
import Modec.V8 (ansamBlock, ansamInit)

data CallClass
  = Modem String            -- ^ what gave it away
  | Fax String
  | SpecialInfo [SitSegment]
  | Busy
  | Congestion
  | Voice
  | NoAnswer                -- ^ ringing, and nothing after it
  | NoAudio                 -- ^ not one sample that was not zero: no media arrived
  | Silence                 -- ^ audio, but nothing above the squelch
  | Unclassified
  deriving (Eq, Show)

-- | The class without its detail, for tables.
className :: CallClass -> String
className c = case c of
  Modem _ -> "modem"
  Fax _ -> "fax"
  SpecialInfo _ -> "sit"
  Busy -> "busy"
  Congestion -> "congestion"
  Voice -> "voice"
  NoAnswer -> "no answer"
  NoAudio -> "no audio"
  Silence -> "silence"
  Unclassified -> "unclassified"

data Evidence = Evidence
  { evDuration  :: !Double
  , evProgress  :: [ProgressEvent]
  , evAnsam     :: Maybe Double          -- ^ first window ANSam was confirmed in
  , evCarrier   :: [(Double, Double)]    -- ^ steady untonal-to-the-progress-bank signal
  , evSpeech    :: [(Double, Double)]
  , evT30       :: [T30Frame]
  , evFullScale :: !Double               -- ^ share of frames within 3 dB of full scale: not line audio
  , evActive    :: !Double               -- ^ share of frames above the squelch
  , evPeak      :: !Double               -- ^ largest sample magnitude
  , evMasked    :: !Double               -- ^ share of frames set aside as our own echo
  } deriving (Show)

data ClassifyParams = ClassifyParams
  { cpSpeech       :: SpeechParams
  , cpSquelchDb    :: Double
  , cpCarrierDb    :: Double   -- ^ least level of a steady carrier
  , cpCarrierSpread :: Double  -- ^ most a carrier's level may vary, dB standard deviation
  , cpCarrierSec   :: Double   -- ^ shortest carrier that counts
  , cpMinSpeechSec :: Double
  , cpFaxSearchSec :: Double   -- ^ how long after an answer tone to look for T.30
  , cpMaxLineDb    :: Double   -- ^ loudest a frame of real line audio can be
  , cpEchoDb       :: Double   -- ^ how far below our own transmission the line must be to be its echo
  , cpTxActiveDb   :: Double   -- ^ level at which we count as transmitting
  } deriving (Eq, Show)

defaultClassifyParams :: ClassifyParams
defaultClassifyParams = ClassifyParams
  { cpSpeech = defaultSpeechParams
  , cpSquelchDb = -50
  , cpCarrierDb = -40
  , cpCarrierSpread = 1.5
  , cpCarrierSec = 3
  , cpMinSpeechSec = 2
  , cpMaxLineDb = -6
  , cpFaxSearchSec = 15
  , cpEchoDb = 3
  , cpTxActiveDb = -40
  }

classifyCall :: Double -> Maybe Signal -> Signal -> (CallClass, Evidence)
classifyCall = classifyCallWith defaultClassifyParams

dbOf :: Double -> Double
dbOf r = 20 * logBase 10 (max 1e-9 r)

frameSec :: Double
frameSec = tbHopSec speechBank

-- | Which frames of the received audio are no more than our own
-- transmission coming back: we were sending, and what came in was
-- quieter than what went out.  A far end talking over us is louder than
-- its echo of us and is kept.  The mask is on the 'speechBank' frame
-- grid, and is empty without a transmit recording.
echoMask :: ClassifyParams -> Double -> Maybe Signal -> [ToneFrame] -> VU.Vector Bool
echoMask cp fs mtx rxFrames = case mtx of
  Nothing -> VU.replicate (length rxFrames) False
  Just tx ->
    let txFrames = VU.fromList (map tfRms (toneFrames fs speechBank tx))
        at i = if i < VU.length txFrames then VU.unsafeIndex txFrames i else 0
    in VU.fromList
         [ dbOf t > cpTxActiveDb cp && dbOf (tfRms fr) < dbOf t - cpEchoDb cp
         | (i, fr) <- zip [0 ..] rxFrames, let t = at i ]

classifyCallWith :: ClassifyParams -> Double -> Maybe Signal -> Signal -> (CallClass, Evidence)
classifyCallWith cp fs mtx rx = (verdict, ev)
  where
    frames = toneFrames fs speechBank rx
    mask = echoMask cp fs mtx frames
    nFrames = VU.length mask
    maskedAt t = let i = floor (t / frameSec) :: Int
                 in i >= 0 && i < nFrames && VU.unsafeIndex mask i
    maskedShare (a, b) =
      let i0 = max 0 (floor (a / frameSec)); i1 = min nFrames (ceiling (b / frameSec))
          n = max 1 (i1 - i0)
      in fromIntegral (VU.length (VU.filter id (VU.slice i0 (max 0 (i1 - i0)) mask))) / fromIntegral n :: Double

    progress = filter (not . maskedAt . peStart) (callProgress fs rx)
    speech = filter ((< 0.5) . maskedShare) (speechRunsFrames (cpSpeech cp) frames)
    -- ANSam is an answer tone with a 15 Hz ripple, so it is only looked
    -- for where the progress detector already heard the tone.
    ansam = listToMaybe
      [ t0 + t | e <- progress, peKind e == AnswerTone
               , let t0 = max 0 (peStart e - 0.5)
               , Just t <- [ansamTime fs (VS.slice (sampleAt t0) (sampleAt (t0 + 6) - sampleAt t0) rx)] ]
    carrier = filter ((< 0.5) . maskedShare) (carrierRuns cp frames)
    answerAt = [ peStart e | e <- progress, peKind e == AnswerTone ]
    t30 = case answerAt ++ maybe [] pure ansam of
      [] -> []
      ts -> let t0 = minimum ts
            in t30Frames fs (VS.slice (sampleAt t0) (sampleAt (t0 + cpFaxSearchSec cp) - sampleAt t0) rx) t0
    sampleAt t = max 0 (min (VS.length rx) (round (t * fs)))
    levels = [ tfRms fr | fr <- frames ]
    active = if null levels then 0
             else fromIntegral (length (filter ((> cpSquelchDb cp) . dbOf) levels)) / fromIntegral (length levels)
    peak = if VS.null rx then 0 else VS.maximum (VS.map abs rx)

    ev = Evidence
      { evDuration = fromIntegral (VS.length rx) / fs
      , evProgress = progress
      , evAnsam = ansam
      , evCarrier = carrier
      , evSpeech = speech
      , evT30 = t30
      , evFullScale = if null levels then 0 else fromIntegral (length (filter ((> -3) . dbOf) levels)) / fromIntegral (length levels)
      , evActive = active
      , evPeak = peak
      , evMasked = if nFrames == 0 then 0 else fromIntegral (VU.length (VU.filter id mask)) / fromIntegral nFrames
      }

    kinds = map peKind progress
    -- A speech run is measured in one-second windows, so it ends up to a
    -- second after the speech does; a cadence that begins as a recorded
    -- announcement ends is not inside it.  One read out of speech itself
    -- has speech going on well past its first burst.
    speechAt t = any (\(a, b) -> t >= a && b > t + 1.5) speech
    firstOf p = find (p . peKind) progress
    verdict
      | (f : _) <- t30 = Fax (printf "T.30 %s at %.1f s%s" (t30Name f) (t3Time f)
                                (maybe "" (\n -> ", station \"" ++ n ++ "\"") (stationId t30)))
      | Just t <- ansam = Modem (printf "ANSam at %.1f s" t)
      | (t : _) <- answerAt = Modem (printf "answer tone at %.1f s" t)
      | ((a, b) : _) <- filter (\(a, b) -> b - a >= cpCarrierSec cp) carrier =
          Modem (printf "steady carrier %.1f-%.1f s" a b)
      | Just e <- firstOf (== FaxCalling) = Fax (printf "calling tone at %.1f s" (peStart e))
      | Just e <- firstOf isSit, Sit segs <- peKind e = SpecialInfo segs
      | Just e <- find (\e -> peKind e `elem` [P.Busy, Reorder] && not (speechAt (peStart e))) progress =
          if peKind e == Reorder then Congestion else Busy
      | speechSeconds speech >= cpMinSpeechSec cp = Voice
      | Ringback `elem` kinds = NoAnswer
      | peak == 0 = NoAudio
      | active < 0.02 = Silence
      | otherwise = Unclassified
    isSit k = case k of { Sit _ -> True; _ -> False }

-- | When ANSam was first confirmed, reading the recording in the
-- detector's own 0.4 s windows.
ansamTime :: Double -> Signal -> Maybe Double
ansamTime fs x = go (ansamInit fs) 0 (chunksOf block x)
  where
    block = round (0.4 * fs)
    go _ _ [] = Nothing
    go st i (c : cs) = case ansamBlock st c of
      (_, True) -> Just (fromIntegral (i * block) / fs)
      (st', False) -> go st' (i + 1 :: Int) cs

-- | Stretches where something steady and loud held the line for whole
-- seconds without being a call-progress tone: a modem's carrier, or its
-- answer ladder's unscrambled ones.  Dial tone is steady too, and the
-- progress bank names it, so frames it names are not carrier.
carrierRuns :: ClassifyParams -> [ToneFrame] -> [(Double, Double)]
carrierRuns cp frames = merge [ (t, t + 1) | (t, ok) <- secs, ok ]
  where
    perSec = round (1 / frameSec) :: Int
    secs = zip [0 ..] (map steady (takeFull (chunkList perSec frames)))
    takeFull = takeWhile ((== perSec) . length)
    steady frs =
      let lv = map (dbOf . tfRms) frs
          m = sum lv / fromIntegral (length lv)
          sd = sqrt (sum [ (l - m) ^ (2 :: Int) | l <- lv ] / fromIntegral (length lv))
          sigs = length (filter (isJust . sigOfFrame defaultProgressParams) frs)
      in m > cpCarrierDb cp && m < cpMaxLineDb cp && sd < cpCarrierSpread cp && sigs * 10 < length frs
    merge = reverse . foldl' add []
    add ((a, b) : rs) (c, d) | c <= b = (a, max b d) : rs
    add rs r = r : rs

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = let (a, b) = splitAt n xs in a : chunkList n b

-- | A T.30 frame heard on V.21 channel 2: the second it completed in,
-- and its octets after the address and control fields, facsimile
-- control field first.
data T30Frame = T30Frame
  { t3Time :: !Double
  , t3Fcf  :: !Word8
  , t3Info :: [Word8]
  } deriving (Eq, Show)

-- | Every frame on V.21 channel 2 that passes the HDLC frame check and
-- carries T.30's address (0xFF) and control field (0x03, or 0x13 on the
-- last frame of a command).  Times count from @t0@, a second at a time.
t30Frames :: Double -> Signal -> Double -> [T30Frame]
t30Frames fs x t0 = go hdlcRxInit (zip [0 :: Int ..] secBits)
  where
    stage = fskDiscriminator fs v21Channel2 defaultDemodParams >>> fskSyncBits fs v21Channel2 defaultDemodParams
    secBits = runStage stage (chunksOf (round fs) x)
    go _ [] = []
    go h ((i, bs) : rest) =
      let (h', frames) = hdlcRxBits h bs
      in [ T30Frame (t0 + fromIntegral i) fcf info
         | (0xFF : ctl : fcf : info) <- frames, ctl `elem` [0x03, 0x13] ] ++ go h' rest

-- | The name of a frame's facsimile control field, the direction bit
-- aside.
t30Name :: T30Frame -> String
t30Name f = case t3Fcf f .&. 0xFE of
  0x80 -> "DIS"
  0x40 -> "CSI"
  0x20 -> "NSF"
  0x82 -> "DCS"
  0x42 -> "TSI"
  0x84 -> "CFR"
  0x44 -> "FTT"
  0xFA -> "DCN"
  v -> printf "FCF %02x" v

-- | The station identifier a CSI or TSI carries.  T.30 sends its twenty
-- characters last one first, so they are turned round.
stationId :: [T30Frame] -> Maybe String
stationId fs = listToMaybe
  [ trimmed | f <- fs, t3Fcf f .&. 0xFE `elem` [0x40, 0x42]
  , let trimmed = trim (map (toEnum . fromIntegral) (reverse (t3Info f)))
  , not (null trimmed) ]
  where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

describeClass :: CallClass -> String
describeClass c = case c of
  Modem why -> "modem (" ++ why ++ ")"
  Fax why -> "fax (" ++ why ++ ")"
  SpecialInfo segs -> "special information tone: "
    ++ unwords [ printf "%.0f Hz/%.0f ms" (ssFreq s) (ssDuration s * 1000) | s <- segs ]
  _ -> className c

-- | The evidence, one line: what was heard and when.
describeEvidence :: Evidence -> String
describeEvidence ev = intercalate "; " (filter (not . null) parts)
  where
    parts =
      [ printf "%.1f s" (evDuration ev)
      , if evPeak ev == 0 then "all zero" else printf "%.0f%% active" (100 * evActive ev)
      , intercalate ", " [ printf "%s at %.1f" (progressName (peKind e)) (peStart e) | e <- evProgress ev ]
      , maybe "" (printf "ANSam at %.1f") (evAnsam ev)
      , spans "carrier" (evCarrier ev)
      , spans "speech" (evSpeech ev)
      , case evT30 ev of { [] -> ""; (f : _) -> printf "T.30 %s at %.0f (%d frames)" (t30Name f) (t3Time f) (length (evT30 ev)) }
      , if evFullScale ev > 0.01 then printf "%.0f%% at full scale" (100 * evFullScale ev) else ""
      , if evMasked ev > 0.01 then printf "%.0f%% echo" (100 * evMasked ev) else ""
      ]
    spans _ [] = ""
    spans what rs = what ++ " " ++ intercalate "," [ printf "%.1f-%.1f" a b | (a, b) <- rs ]

progressName :: Progress -> String
progressName k = case k of
  DialTone -> "dial tone"
  Ringback -> "ringing"
  P.Busy -> "busy"
  Reorder -> "congestion"
  Sit _ -> "SIT"
  FaxCalling -> "CNG"
  AnswerTone -> "answer tone"
