{-# LANGUAGE BangPatterns #-}
-- | A window correlated against every offset of a longer signal at once.
--
-- The echo search asks one question of two signals: where in what we
-- sent does what arrived fit best.  Asked a lag at a time it is a dot
-- product per lag -- seven thousand lags of a sixteen-thousand-sample
-- window for a reflection that may be 900 ms away, which is a hundred
-- million multiplies and was spread over a hundred and fifty blocks of
-- a call because no one block could afford it.  Asked through a Fourier
-- transform it is three transforms, and every lag's answer at once.
--
-- Nothing here knows about echoes.
module Modec.Xcorr
  ( xcorrValid
  , prefixSums
    -- * The same, a transform at a time
  , Spectrum
  , transformSize
  , transformReal
  , crossSpectrum
  , correlationFrom
    -- * And a few stages of a transform at a time
  , Staged
  , stageReal
  , stageCross
  , stageRun
  , stageDone
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- | @xcorrValid x y@, for @x@ no longer than @y@: element @o@ is the sum
-- over @i@ of @x[i] * y[o + i]@, for every offset at which @x@ lies
-- wholly inside @y@ -- @length y - length x + 1@ of them.
xcorrValid :: VS.Vector Double -> VS.Vector Double -> VS.Vector Double
xcorrValid x y
  | w == 0 || w > m = VS.empty
  | otherwise = correlationFrom (m - w + 1) (crossSpectrum (transformReal n x) (transformReal n y))
  where
    w = VS.length x
    m = VS.length y
    n = transformSize m

-- | A transform: real parts and imaginary parts.
--
-- The three transforms of a correlation are offered one at a time
-- because a real-time loop is judged by its worst block.  On a busy
-- machine one transform of 32768 points was seven milliseconds, and
-- three of them in the block that asked was a block over its twenty.
type Spectrum = (VU.Vector Double, VU.Vector Double)

-- | The power of two a correlation against a signal this long is done
-- in.  No offset it keeps wraps: the last one reads the longer signal up
-- to its end, and the transform is at least that long.
transformSize :: Int -> Int
transformSize m = until (>= m) (* 2) 2

-- | The transform of a real signal, padded with zeros to @n@ points.
transformReal :: Int -> VS.Vector Double -> Spectrum
transformReal n x = forward (VU.generate n (\i -> if i < VS.length x then VS.unsafeIndex x i else 0), VU.replicate n 0)

-- | X times the conjugate of Y, transformed again.
--
-- The correlation of x against y is the inverse transform of conj X * Y,
-- and it is real, so it is also the real part of the forward transform
-- of X * conj Y, over n: one more forward transform and no inverse to
-- write.
crossSpectrum :: Spectrum -> Spectrum -> Spectrum
crossSpectrum (xr, xi) (yr, yi) = forward
  ( VU.generate n (\k -> VU.unsafeIndex xr k * VU.unsafeIndex yr k + VU.unsafeIndex xi k * VU.unsafeIndex yi k)
  , VU.generate n (\k -> VU.unsafeIndex xi k * VU.unsafeIndex yr k - VU.unsafeIndex xr k * VU.unsafeIndex yi k) )
  where n = VU.length xr

-- | The first @count@ offsets of the correlation, from 'crossSpectrum'.
correlationFrom :: Int -> Spectrum -> VS.Vector Double
correlationFrom count (re, _) = VS.generate count (\o -> VU.unsafeIndex re o / fromIntegral (VU.length re))

forward :: Spectrum -> Spectrum
forward (re0, im0) = runST $ do
  re <- VU.thaw re0
  im <- VU.thaw im0
  fft (twiddles n) re im
  (,) <$> VU.unsafeFreeze re <*> VU.unsafeFreeze im
  where n = VU.length re0

-- | A transform under way: the points, already in bit-reversed order,
-- and the length of the groups its next stage joins -- 2 before the
-- first, twice the size after the last.
--
-- A transform at a time was not little enough.  One of 32768 points was
-- measured at two milliseconds, and at seven on a busy machine; taken
-- the way a call takes it -- a burst of work, then asleep until the next
-- block -- on an idle machine whose processor had slowed down to match,
-- it was 6 to 24.  The loop it runs in has 20 ms a block and reports
-- any block over 12, and on a call to a line with no reflection to find
-- the search ran once a second for as long as the call lasted: a
-- hundred and fifty-nine such reports in a hundred seconds.  A stage of
-- a transform is the same work whichever stage it is, so the work can
-- be cut wherever a block's share of it ends.
data Staged = Staged !Int !(VU.Vector Double) !(VU.Vector Double)

-- | Begin the transform of a real signal padded to @n@ points.
stageReal :: Int -> VS.Vector Double -> Staged
stageReal n x = permuted (VU.generate n (\i -> if i < VS.length x then VS.unsafeIndex x i else 0), VU.replicate n 0)

-- | Begin the transform 'crossSpectrum' takes.
stageCross :: Spectrum -> Spectrum -> Staged
stageCross (xr, xi) (yr, yi) = permuted
  ( VU.generate n (\k -> VU.unsafeIndex xr k * VU.unsafeIndex yr k + VU.unsafeIndex xi k * VU.unsafeIndex yi k)
  , VU.generate n (\k -> VU.unsafeIndex xi k * VU.unsafeIndex yr k - VU.unsafeIndex xr k * VU.unsafeIndex yi k) )
  where n = VU.length xr

permuted :: Spectrum -> Staged
permuted (re0, im0) = runST $ do
  re <- VU.thaw re0
  im <- VU.thaw im0
  fftPermute re im
  Staged 2 <$> VU.unsafeFreeze re <*> VU.unsafeFreeze im

-- | Take as many more stages as come to about @work@ butterflies, and at
-- least one.  The points are copied first: what was handed in is still
-- what it was, which a replay that keeps its states relies on, and the
-- copy is a hundredth of a stage.
stageRun :: Int -> Staged -> Staged
stageRun work st@(Staged len re0 im0)
  | len > n = st
  | otherwise = runST $ do
      re <- VU.thaw re0
      im <- VU.thaw im0
      len' <- fftStages (twiddles n) re im len (max 1 (work `quot` max 1 (n `quot` 2)))
      Staged len' <$> VU.unsafeFreeze re <*> VU.unsafeFreeze im
  where n = VU.length re0

-- | The transform, once every stage has been taken.
stageDone :: Staged -> Maybe Spectrum
stageDone (Staged len re im)
  | len > VU.length re = Just (re, im)
  | otherwise = Nothing

-- | The cosine and sine of the first half turn, in @n@ steps.  One table
-- serves every size up to its own -- a smaller transform reads it at a
-- stride -- and the sizes the echo search asks for are all under it, so
-- the thirty thousand cosines are worked out once in the life of the
-- program and not once a scan.
twiddles :: Int -> (Int, VU.Vector Double, VU.Vector Double)
twiddles n
  | n <= tableSize = (tableSize `quot` n, cosTable, sinTable)
  | otherwise = (1, quarter n cos, quarter n sin)

tableSize :: Int
tableSize = 65536

cosTable, sinTable :: VU.Vector Double
cosTable = quarter tableSize cos
sinTable = quarter tableSize sin
{-# NOINLINE cosTable #-}
{-# NOINLINE sinTable #-}

quarter :: Int -> (Double -> Double) -> VU.Vector Double
quarter n f = VU.generate (n `quot` 2) (\k -> f (2 * pi * fromIntegral k / fromIntegral n))

-- | The forward transform, in place: the textbook radix-2 one, on a
-- power of two.
fft :: (Int, VU.Vector Double, VU.Vector Double) -> MVU.MVector s Double -> MVU.MVector s Double -> ST s ()
fft tw re im = fftPermute re im >> fftStages tw re im 2 maxBound >> return ()

-- | The usual permutation: each element to the index with its bits
-- reversed.
fftPermute :: MVU.MVector s Double -> MVU.MVector s Double -> ST s ()
fftPermute re im = reverseBits 0 0
  where
    !n = MVU.length re
    reverseBits !i !j
      | i >= n - 1 = return ()
      | otherwise = do
          when (i < j) $ MVU.unsafeSwap re i j >> MVU.unsafeSwap im i j
          reverseBits (i + 1) (carry (n `quot` 2) j)
    carry !k !j
      | k <= j = carry (k `quot` 2) (j - k)
      | otherwise = j + k

-- | Up to @count@ stages of butterflies, from the one that joins groups
-- into groups of @len0@.  Returns the group length the next stage would
-- make, which is past the transform's size once there is none.
fftStages :: (Int, VU.Vector Double, VU.Vector Double) -> MVU.MVector s Double -> MVU.MVector s Double
          -> Int -> Int -> ST s Int
fftStages (stride, cosT, sinT) re im len0 count = stage len0 count
  where
    !n = MVU.length re
    stage !len !left
      | len > n || left <= 0 = return len
      | otherwise = groups (len `quot` 2) (stride * (n `quot` len)) len 0 >> stage (len * 2) (left - 1)
    groups !h !step !len !i
      | i >= n = return ()
      | otherwise = pairs h step i 0 >> groups h step len (i + len)
    pairs !h !step !i !j
      | j >= h = return ()
      | otherwise = do
          let !wr = VU.unsafeIndex cosT (j * step)
              !wi = negate (VU.unsafeIndex sinT (j * step))
              !a = i + j
              !b = a + h
          br <- MVU.unsafeRead re b
          bi <- MVU.unsafeRead im b
          ar <- MVU.unsafeRead re a
          ai <- MVU.unsafeRead im a
          let !tr = br * wr - bi * wi
              !ti = br * wi + bi * wr
          MVU.unsafeWrite re b (ar - tr)
          MVU.unsafeWrite im b (ai - ti)
          MVU.unsafeWrite re a (ar + tr)
          MVU.unsafeWrite im a (ai + ti)
          pairs h step i (j + 1)

-- | Running sums: element @k@ is the sum of the first @k@ elements, so
-- there is one more of them than of the signal and the sum of any
-- stretch is a difference of two.
prefixSums :: VS.Vector Double -> VS.Vector Double
prefixSums = VS.scanl' (+) 0
