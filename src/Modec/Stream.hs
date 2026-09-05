{-# LANGUAGE ExistentialQuantification #-}
-- | A minimal chunk-oriented stream transformer: an explicit-state Mealy
-- machine.  Receivers are built from these so the same code runs on a
-- whole WAV file, on 20 ms RTP frames, or on PipeWire buffers.
module Modec.Stream
  ( Stage (..)
  , runStage
  , (>>>)
  , mapStage
  , concatStage
  ) where

data Stage i o = forall s. Stage !s !(s -> i -> (s, o))

-- | Feed a list of chunks through a stage.
runStage :: Stage i o -> [i] -> [o]
runStage (Stage s0 f) = go s0
  where
    go _ [] = []
    go s (x : xs) = let (s', y) = f s x in s' `seq` (y : go s' xs)

infixr 1 >>>

-- | Sequential composition.
(>>>) :: Stage a b -> Stage b c -> Stage a c
Stage s0 f >>> Stage t0 g = Stage (s0, t0) $ \(s, t) a ->
  let (s', b) = f s a
      (t', c) = g t b
  in s' `seq` t' `seq` ((s', t'), c)

-- | A stateless stage.
mapStage :: (a -> b) -> Stage a b
mapStage f = Stage () (\_ a -> ((), f a))

-- | Run a stage over chunks and concatenate list outputs.
concatStage :: Stage i [o] -> [i] -> [o]
concatStage st = concat . runStage st
