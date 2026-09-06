-- | Pull MNP frames out of a recording of a call.
--
-- Reads what the far end sent (the high channel, since we are the calling
-- modem) at whichever rate yields more frames, and decodes both framings:
-- start-stop octets for class 2 and the establishment exchange, and
-- bit-oriented HDLC for class 3 and 4 once the link has switched.  Prints
-- the frames and reassembles whatever the information fields carried.
--
-- Build and run:
--
-- > cabal build lib:modec
-- > cabal exec -- ghc -O2 -package modec -package bytestring scripts/mnp-decode.hs -o /tmp/mnp-decode
-- > /tmp/mnp-decode recordings/*.wav
--
-- Recordings come from @modec modem --record-rx FILE.wav@.  Nothing here
-- places a call; it is for reading back one that was already made.
module Main (main) where
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as VS
import Data.List ()
import Data.Word (Word8)
import System.Environment (getArgs)
import Text.Printf (printf)
import Modec.Async
import Modec.FSK (framing8N1)
import Modec.Hdlc
import Modec.MnpFrame
import Modec.V22
import Modec.Wav

chunk :: Int -> VS.Vector Double -> [VS.Vector Double]
chunk n v | VS.null v = []
          | otherwise = let (a, b) = VS.splitAt n v in a : chunk n b

-- descrambled bits of one V.22 channel at one rate
bitsOf :: Double -> V22Channel -> Rate -> VS.Vector Double -> [Bool]
bitsOf fs ch rate sig = go (v22RxSetRate rate (v22RxInit fs)) (chunk 160 sig) []
  where
    go _ [] acc = acc
    go st (b : bs) acc = let (st', o) = v22RxBlock fs ch b st in go st' bs (acc ++ roBits o)

-- MNP frames under start-stop framing (mode 2)
mode2Frames :: [Bool] -> ([MnpFrame], Int)
mode2Frames bits =
  let (_, octets) = asyncRxBits (asyncRxInit framing8N1) bits
      (_, out) = mode2RxOctets mode2RxInit octets
  in ([ f | Right b <- out, Right f <- [decodeFrame b] ], length [ () | Left _ <- out ])

-- MNP frames under bit-oriented framing (mode 3)
mode3Frames :: [Bool] -> [MnpFrame]
mode3Frames bits =
  let (_, bodies) = hdlcRxBits hdlcRxInit bits
  in [ f | b <- bodies, Right f <- [decodeFrame b] ]

payloadOf :: [MnpFrame] -> [Word8]
payloadOf fs = concat [ inf | FrLT _ inf <- fs ]

main :: IO ()
main = do
  paths <- getArgs
  mapM_ one paths
  where
    one path = do
      w <- either fail return . decodeWav =<< B.readFile path
      let fs = fromIntegral (wavRate w)
          sig = wavSamples w
          try rate = let bits = bitsOf fs HighChannel rate sig
                         (m2, bad) = mode2Frames bits
                         m3 = mode3Frames bits
                     in (rate, m2, bad, m3)
          cands = map try [R1200, R2400]
          score (_, m2, _, m3) = length m2 + length m3
          best = last (sortOn score cands)
          (rate, m2, bad, m3) = best
      printf "%s\n" (short path)
      if null m2 && null m3
        then printf "    no MNP frames in either framing\n"
        else do
          printf "    rate %s: %d frames octet-framed (%d failed the check), %d bit-framed\n"
                 (show rate) (length m2) bad (length m3)
          mapM_ (\f -> printf "      %s\n" (brief f)) (take 6 (m2 ++ m3))
          let txt = payloadOf (m2 ++ m3)
          if null txt then return () else
            printf "    payload carried in LT frames (%d octets): %s\n"
                   (length txt) (show (map (\c -> if c >= 32 && c < 127 then toEnum (fromIntegral c) else '.') txt))
    sortOn f = foldr ins []
      where ins x [] = [x]
            ins x (y : ys) | f x <= f y = x : y : ys
                           | otherwise = y : ins x ys
    short p = let n = reverse (takeWhile (/= '/') (reverse p)) in n
    brief :: MnpFrame -> String
    brief f = case f of
      FrLR lr -> printf "LR   framing=%d k=%d N401=%d dpo=%d const2=%s"
                   (fromIntegral (lrFraming lr) :: Int) (fromIntegral (lrK lr) :: Int)
                   (lrN401 lr) (fromIntegral (lrDpo lr) :: Int) (show (lrConst2 lr))
      FrLA nr nk -> printf "LA   N(R)=%d N(k)=%d" (fromIntegral nr :: Int) (fromIntegral nk :: Int)
      FrLT ns inf -> printf "LT   N(S)=%d %d octets" (fromIntegral ns :: Int) (length inf)
      FrLD r _ -> printf "LD   reason %d" (fromIntegral r :: Int)
      FrLN a b -> printf "LN   %d %d" (fromIntegral a :: Int) (fromIntegral b :: Int)
      FrLNA a -> printf "LNA  %d" (fromIntegral a :: Int)
      FrOther t _ -> printf "type %d" (fromIntegral t :: Int)
