{-# LANGUAGE BangPatterns #-}
-- | DTMF (ITU-T Q.23 for the tones, Q.24 for what a receiver must
-- accept and reject): the sixteen keypad characters as one low-group
-- and one high-group tone, generated and detected.
--
-- Detection is the classic eight-Goertzel block receiver.  The signal
-- is cut into fixed blocks, each block is measured at the eight tone
-- frequencies, and a block passes when the strongest low tone and the
-- strongest high tone look like a keypad pair and nothing else in the
-- block does.  Four tests decide that, and each one throws out a
-- different impostor:
--
--   * a level floor, so silence and line noise are not read as digits;
--   * twist, the level difference between the two groups, which a real
--     telephone set introduces on purpose (the high group is sent
--     louder because the line attenuates it more) and a long line then
--     undoes;
--   * a relative-peak test against the other three tones in each group,
--     which rejects any signal carrying more than one tone per group;
--   * and a fraction-of-total-power test, which is what rejects speech.
--     Talk-off -- a voice imitating a digit -- is the failure mode a
--     DTMF receiver is actually judged on, and no amount of per-tone
--     thresholding catches it, because a vowel really does have energy
--     at 770 and 1336 Hz.  What a vowel does not have is /only/ that
--     energy: its formants come with harmonics and breath spread over
--     the band, so the two candidate tones hold a small share of the
--     block's power, where a genuine pair holds nearly all of it.
--
-- A digit is then a run of consecutive blocks that look like the same
-- pair, of which at least 'dtBlocks' passed every test.
--
-- How long the tone lasted is measured rather than counted, and it has
-- to be.  Q.24 wants a receiver to accept a tone of 40 ms and reject
-- one of 23 ms, and those are 1.3 blocks apart: whatever number of
-- blocks is demanded, one of the two requirements fails at some
-- alignments of the tone against the block grid, because a block the
-- tone only partly fills sometimes passes and sometimes does not.
-- Counting cannot separate them.
--
-- Measuring can, because the Goertzel is linear in coverage: a block
-- the tone fills for a fraction @f@ of its length reads @f@ times the
-- amplitude of a block the tone fills completely.  Summing the run's
-- levels and dividing by the largest of them therefore gives the tone
-- length in blocks, edges included, and the same ratio on the first
-- block alone says how far into it the tone began, which is where
-- 'ddStart' comes from.
--
-- The estimate runs short, by nothing at all to about four tenths of a
-- block, and never long.  A block the tone barely reaches into is a
-- brief burst inside a long window, and a brief burst is broad: its
-- energy lands on the neighbouring rows and columns as much as on its
-- own, so the block names some other pair and drops out of the run,
-- taking its sliver of coverage with it.  Measured over every
-- alignment of the tone against the block grid at 8 kHz, 100 ms reads
-- 93.4 to 98.2 ms and 40 ms reads 35.3 to 40.2 ms, with the start
-- within 3 ms.  That is far inside what this has to decide: 40 ms
-- never measures below 35 and 23 ms never above 26, so 'dtMinSec' at
-- 32 ms separates them at every alignment.
--
-- One non-passing block is forgiven inside a run ('dtHangover'), so a
-- dropout does not split a digit in two, while the minimum interdigit
-- pause of 40 ms still ends it.
module Modec.Dtmf
  ( -- * Generating
    dtmfPair
  , dtmfDialSignal
  , DialParams (..)
  , defaultDialParams
  , dtmfDialSignalWith
    -- * The keypad
  , dtmfLowTones
  , dtmfHighTones
  , dtmfKey
    -- * Detecting
  , DtmfParams (..)
  , defaultDtmfParams
  , DtmfDigit (..)
  , DtmfRx
  , dtmfRxInit
  , dtmfRxBlock
  , dtmfRxFlush
  , dtmfDetector
  , dtmfDecode
  , dtmfDigits
  ) where

import Data.List (foldl')
import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal, db, fromDb, goertzel, toneAmplitude)
import Modec.Stream (Stage (..))

-- | Low and high tone of a keypad character, if it is one.
dtmfPair :: Char -> Maybe (Double, Double)
dtmfPair c = case c of
  '1' -> Just (697, 1209); '2' -> Just (697, 1336); '3' -> Just (697, 1477); 'A' -> Just (697, 1633)
  '4' -> Just (770, 1209); '5' -> Just (770, 1336); '6' -> Just (770, 1477); 'B' -> Just (770, 1633)
  '7' -> Just (852, 1209); '8' -> Just (852, 1336); '9' -> Just (852, 1477); 'C' -> Just (852, 1633)
  '*' -> Just (941, 1209); '0' -> Just (941, 1336); '#' -> Just (941, 1477); 'D' -> Just (941, 1633)
  _ -> Nothing

-- | Audio for a dial string: 80 ms tone and 80 ms silence per digit at
-- amplitude @amp@ per tone, one second per ',', other characters ignored.
dtmfDialSignal :: Double -> Double -> String -> Signal
dtmfDialSignal = dtmfDialSignalWith defaultDialParams

-- | The timings a Hayes modem keeps in S-registers: how long to wait off
-- hook before dialling (S6), how long each digit sounds (S11, which is
-- also the gap after it), and how long a comma pauses (S8).
data DialParams = DialParams
  { dpBlindWait :: !Double   -- ^ seconds of silence before the first digit
  , dpToneMs    :: !Int      -- ^ milliseconds of tone per digit, and of silence after it
  , dpPauseSec  :: !Double   -- ^ seconds per ','
  } deriving (Eq, Show)

-- | What 'dtmfDialSignal' has always produced: no wait, 80 ms, 1 s.
defaultDialParams :: DialParams
defaultDialParams = DialParams 0 80 1

dtmfDialSignalWith :: DialParams -> Double -> Double -> String -> Signal
dtmfDialSignalWith dp fs amp str =
  VS.concat (VS.replicate (round (fs * max 0 (dpBlindWait dp))) 0 : concatMap one str)
  where
    onN = round (fs * fromIntegral (max 1 (dpToneMs dp)) / 1000) :: Int
    one c = case dtmfPair c of
      Just (lo, hi) ->
        [ VS.generate onN (\i -> let t = fromIntegral i / fs in amp * (sin (2 * pi * lo * t) + sin (2 * pi * hi * t)))
        , VS.replicate onN 0 ]
      Nothing
        | c == ',' -> [VS.replicate (round (fs * max 0 (dpPauseSec dp))) 0]
        | otherwise -> []

-- | The low group, in row order.
dtmfLowTones :: [Double]
dtmfLowTones = [697, 770, 852, 941]

-- | The high group, in column order.  1633 Hz is the fourth column,
-- A to D, which no telephone has had since the military ones.
dtmfHighTones :: [Double]
dtmfHighTones = [1209, 1336, 1477, 1633]

-- | The keypad character at a row and column of the tone tables.
dtmfKey :: Int -> Int -> Char
dtmfKey r c = (["123A", "456B", "789C", "*0#D"] !! r) !! c

-- | What the receiver will accept as a digit.
data DtmfParams = DtmfParams
  { dtBlockSec   :: Double
    -- ^ analysis block length.  12.75 ms at 8 kHz is 102 samples, the
    -- length this kind of receiver has used since it was a DSP chip:
    -- the resulting 78 Hz bin is wider than the 73 Hz between adjacent
    -- rows, but a neighbour still reads 22 dB down, which is more
    -- separation than 'dtRelativeDb' asks for.
  , dtSquelch    :: Double
    -- ^ minimum amplitude of each of the two tones, full scale = 1
  , dtTwistDb    :: Double
    -- ^ how far the low group may sit above the high group.  The far
    -- end sends the high group louder and the line takes it back, so
    -- this direction is the ordinary one and gets the wider allowance.
  , dtRevTwistDb :: Double
    -- ^ how far the high group may sit above the low group
  , dtRelativeDb :: Double
    -- ^ how far every other tone of a group must sit below the
    -- strongest one of that group
  , dtFraction   :: Double
    -- ^ share of the block's power the two tones must hold between
    -- them; this is the speech rejector
  , dtBlocks     :: Int
    -- ^ blocks of the run that must pass every test, not merely look
    -- like the pair
  , dtMinSec     :: Double
    -- ^ shortest measured tone accepted as a digit
  , dtHangover   :: Int
    -- ^ non-passing blocks forgiven inside a digit before it ends
  } deriving (Eq, Show)

-- | Q.24's tolerances, with the level floor at about -40 dBFS and the
-- shortest accepted tone at 32 ms, between the 23 ms that must be
-- rejected and the 40 ms that must be accepted.
defaultDtmfParams :: DtmfParams
defaultDtmfParams = DtmfParams
  { dtBlockSec = 0.01275
  , dtSquelch = 0.01
  , dtTwistDb = 8
  , dtRevTwistDb = 4
  , dtRelativeDb = 8
  , dtFraction = 0.35
  , dtBlocks = 2
  , dtMinSec = 0.032
  , dtHangover = 1
  }

-- | One received digit.  The times are interpolated inside the block
-- grid, so they are good to a fraction of a block rather than to the
-- 12.75 ms the blocks are cut at.
data DtmfDigit = DtmfDigit
  { ddChar     :: !Char
  , ddStart    :: !Double   -- ^ when the tone began, seconds from the start of the stream
  , ddDuration :: !Double   -- ^ how long it lasted
  , ddLevel    :: !Double   -- ^ amplitude of the stronger tone in the fullest block
  } deriving (Eq, Show)

-- | What one block looked like: the strongest pair in it, at what
-- level, and whether it passed the tests that a mere pair of loud
-- tones does not -- the ones that tell a keypad pair from speech or
-- from a signal carrying more than two tones.
data Look = Look
  { lkRow   :: !Int
  , lkCol   :: !Int
  , lkLevel :: !Double
  , lkFirm  :: !Bool
  }

-- | A digit being built up out of consecutive blocks.
data Partial = Partial
  { paRow   :: !Int
  , paCol   :: !Int
  , paStart :: !Int     -- ^ first sample of the first block of the run
  , paFirst :: !Double  -- ^ level of that first block
  , paSum   :: !Double  -- ^ levels summed over the run
  , paPeak  :: !Double  -- ^ largest level in the run
  , paFirm  :: !Int     -- ^ blocks of the run that passed every test
  , paGap   :: !Int     -- ^ blocks since the last one that did
  }

data DtmfRx = DtmfRx
  { drRate  :: !Double
  , drStart :: !Int      -- ^ global index of the first buffered sample
  , drBuf   :: !Signal
  , drCur   :: !(Maybe Partial)
  }

dtmfRxInit :: Double -> DtmfRx
dtmfRxInit fs = DtmfRx fs 0 VS.empty Nothing

-- | Feed a chunk of audio; whole blocks are consumed and the remainder
-- is carried, so the result does not depend on how the audio is cut up.
-- Digits are reported when their tone ends.
dtmfRxBlock :: DtmfParams -> DtmfRx -> Signal -> (DtmfRx, [DtmfDigit])
dtmfRxBlock p st chunk = go (drStart st) (drCur st) [] ext
  where
    fs = drRate st
    n = max 1 (round (fs * dtBlockSec p))
    ext = drBuf st VS.++ chunk
    go !off cur acc rest
      | VS.length rest < n =
          (st { drStart = off, drBuf = rest, drCur = cur }, reverse acc)
      | otherwise =
          let (blk, rest') = VS.splitAt n rest
              (cur', out) = feed p fs off (measure p fs n blk) cur
          in go (off + n) cur' (maybe acc (: acc) out) rest'

-- | Close a digit that was still sounding when the stream ended.
dtmfRxFlush :: DtmfParams -> DtmfRx -> [DtmfDigit]
dtmfRxFlush p st = maybe [] (maybe [] (: []) . finish p (drRate st)) (drCur st)

-- | Measure one block: 'Nothing' when nothing in it could be a keypad
-- pair at all.
measure :: DtmfParams -> Double -> Int -> Signal -> Maybe Look
measure p fs n blk
  | ra <= dtSquelch p || ca <= dtSquelch p = Nothing
  | db (ra / ca) > dtTwistDb p = Nothing
  | db (ca / ra) > dtRevTwistDb p = Nothing
  | otherwise = Just (Look ri ci (max ra ca) firm)
  where
    amp f = toneAmplitude n (goertzel fs f blk)
    lows = map amp dtmfLowTones
    highs = map amp dtmfHighTones
    (ri, ra) = strongest lows
    (ci, ca) = strongest highs
    strongest = foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, -1) . zip [0 ..]
    firm = clear ri ra lows && clear ci ca highs && tonePower >= dtFraction p * total
    -- Compared by position, not by value: two tones of exactly equal
    -- amplitude are two tones, and a tie must fail the test rather than
    -- mistake the runner-up for the winner it is tied with.
    clear i a group = and [ v <= a * fromDb (negate (dtRelativeDb p)) | (j, v) <- zip [0 :: Int ..] group, j /= i ]
    -- Mean square of the block against the mean square the two tones
    -- alone would produce.  Both are powers, so the ratio is what a
    -- share of the block's power means here.
    tonePower = (ra * ra + ca * ca) / 2
    total = VS.sum (VS.map (\v -> v * v) blk) / fromIntegral n

-- | Fold one block's measurement into the digit under construction.
feed :: DtmfParams -> Double -> Int -> Maybe Look -> Maybe Partial -> (Maybe Partial, Maybe DtmfDigit)
feed p fs off m cur = case (m, cur) of
  (Nothing, Nothing) -> (Nothing, Nothing)
  (Nothing, Just pa)
    | paGap pa < dtHangover p -> (Just pa { paGap = paGap pa + 1 }, Nothing)
    | otherwise -> (Nothing, finish p fs pa)
  (Just lk, Just pa)
    | paRow pa == lkRow lk && paCol pa == lkCol lk ->
        let pa' = extend lk pa
        in if paGap pa' > dtHangover p
             then (Nothing, finish p fs pa')
             else (Just pa', Nothing)
    | otherwise -> (Just (start lk), finish p fs pa)
  (Just lk, Nothing) -> (Just (start lk), Nothing)
  where
    start lk = Partial (lkRow lk) (lkCol lk) off (lkLevel lk) (lkLevel lk) (lkLevel lk)
                       (if lkFirm lk then 1 else 0) (if lkFirm lk then 0 else 1)
    extend lk pa = pa
      { paSum = paSum pa + lkLevel lk
      , paPeak = max (paPeak pa) (lkLevel lk)
      , paFirm = paFirm pa + (if lkFirm lk then 1 else 0)
      , paGap = if lkFirm lk then 0 else paGap pa + 1
      }

-- | A digit, if the run held enough good blocks and the tone it
-- measures out to was long enough.
finish :: DtmfParams -> Double -> Partial -> Maybe DtmfDigit
finish p fs pa
  | paFirm pa < dtBlocks p = Nothing
  | paPeak pa <= 0 = Nothing
  | len < dtMinSec p = Nothing
  | otherwise = Just DtmfDigit
      { ddChar = dtmfKey (paRow pa) (paCol pa)
      , ddStart = fromIntegral (paStart pa) / fs + (1 - min 1 (paFirst pa / paPeak pa)) * dtBlockSec p
      , ddDuration = len
      , ddLevel = paPeak pa
      }
  where len = paSum pa / paPeak pa * dtBlockSec p

-- | The receiver as a stream stage, for the live modem.
dtmfDetector :: Double -> DtmfParams -> Stage Signal [DtmfDigit]
dtmfDetector fs p = Stage (dtmfRxInit fs) (dtmfRxBlock p)

-- | Every digit in a recording, with a tone still sounding at the end
-- of it counted.
dtmfDecode :: Double -> DtmfParams -> Signal -> [DtmfDigit]
dtmfDecode fs p x = let (st, ds) = dtmfRxBlock p (dtmfRxInit fs) x in ds ++ dtmfRxFlush p st

-- | 'dtmfDecode' as the dial string it spells.
dtmfDigits :: Double -> Signal -> String
dtmfDigits fs = map ddChar . dtmfDecode fs defaultDtmfParams
