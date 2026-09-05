-- | DTMF dialling tones (ITU-T Q.23): digits 0-9, * and #, A-D, with
-- the pause character ',' as one second of silence.
module Modec.Dtmf
  ( dtmfPair
  , dtmfDialSignal
  ) where

import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal)

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
dtmfDialSignal fs amp = VS.concat . concatMap one
  where
    onN = round (fs * 0.08) :: Int
    one c = case dtmfPair c of
      Just (lo, hi) ->
        [ VS.generate onN (\i -> let t = fromIntegral i / fs in amp * (sin (2 * pi * lo * t) + sin (2 * pi * hi * t)))
        , VS.replicate onN 0 ]
      Nothing
        | c == ',' -> [VS.replicate (round fs) 0]
        | otherwise -> []
