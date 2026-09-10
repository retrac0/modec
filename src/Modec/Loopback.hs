-- | Two whole modems calling each other through the channel simulator.
--
-- The point of this over "Modec.Replay" is that both ends are live, so
-- a receiver's difficulties feed back into what it transmits and the
-- call either establishes or does not.  The point of it over the
-- harness the test suite has always used is that the line between them
-- is 'Modec.Channel' rather than a hand-rolled addition of noise, which
-- means what a mode survives can be asked in the same units everything
-- else here is measured in.
--
-- The channel is applied to each 20 ms block as it crosses, which is
-- exact for the impairments that have no memory -- noise, level,
-- clipping, the nonlinearities, the codec, impulses -- and an
-- approximation for the ones that do.  A 401-tap band-pass is longer
-- than the block it would be filtering, so it does not belong here;
-- band-limiting a loopback is a job for the receivers' own front ends.
-- 'loopSnr' exists to make the well-behaved case easy to reach.
module Modec.Loopback
  ( LoopConfig (..)
  , defaultLoop
  , loopSnr
  , LoopResult (..)
  , loopback
  , loopOk
  , loopPassRate
  ) where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

import Modec.Channel
import Modec.DSP (Signal, fromDb)
import Modec.Handshake (Role (..), Standard)
import Modec.Modem

data LoopConfig = LoopConfig
  { lcOrig    :: ModemConfig
  , lcAnswer  :: ModemConfig
  , lcLine    :: Channel   -- ^ applied to each direction, per block
  , lcEcho    :: [(Double, Double)]
    -- ^ Near-end echo: each modem's own transmit, reflected back into its
    -- own receiver as @(delay seconds, linear gain)@ taps.  This is the
    -- hybrid, not the line -- 'lcLine' carries the far end's signal and
    -- reflecting that would be multipath, which is a different thing and
    -- is not what "Modec.Echo" cancels.  It is applied before 'lcGainDb',
    -- because a reflection of our own transmit has not crossed the line
    -- and has not been attenuated by it; that is exactly why the echo
    -- arrives louder than the signal it sits on top of.  Empty by
    -- default: the modes below V.32 split the band and do not care.
  , lcGainDb  :: Double    -- ^ loss on the way, before the noise
  , lcBlockSec :: Double
  , lcMaxT    :: Double    -- ^ give up after this much simulated call
  }

-- | Both ends configured the same way, on an otherwise ideal line.
defaultLoop :: Double -> [Standard] -> LoopConfig
defaultLoop fs modes = LoopConfig
  { lcOrig = defaultModemConfig fs Originate modes
  , lcAnswer = defaultModemConfig fs Answer modes
  , lcLine = idealChannel
  , lcEcho = []
  , lcGainDb = -20
  , lcBlockSec = 0.02
  , lcMaxT = 45
  }

-- | The same at a stated signal-to-noise ratio, which is the sweep this
-- is mostly used for.
loopSnr :: Double -> [Standard] -> Double -> LoopConfig
loopSnr fs modes snr =
  let c = defaultLoop fs modes in c { lcLine = idealChannel { chSnrDb = Just snr } }

data LoopResult = LoopResult
  { lrRxOrig   :: [Word8]        -- ^ what the calling modem heard
  , lrRxAnswer :: [Word8]
  , lrEvOrig   :: [ModemEvent]
  , lrEvAnswer :: [ModemEvent]
  , lrConnected :: Bool          -- ^ both ends reported a connection
  , lrErleOrig :: Maybe Double   -- ^ echo return loss enhancement, dB, at the end
  , lrErleAnswer :: Maybe Double
  , lrDelayOrig :: Maybe Int     -- ^ where each canceller decided the echo was
  , lrDelayAnswer :: Maybe Int
  , lrEvmOrig :: Maybe Double   -- ^ slicer error at the end, negative once armed
  , lrEvmAnswer :: Maybe Double
  }

-- | Place the call, send @textO@ one way and @textA@ the other, and stop
-- a couple of seconds after both have arrived or at 'lcMaxT'.
loopback :: LoopConfig -> [Word8] -> [Word8] -> LoopResult
loopback lc textO textA =
    go 0 (modemInit cfgO) (modemInit cfgA) quiet quiet VS.empty VS.empty False False Nothing [] [] [] []
  where
    cfgO = lcOrig lc
    cfgA = lcAnswer lc
    fs = mcRate cfgO
    blk = max 1 (round (fs * lcBlockSec lc)) :: Int
    quiet = VS.replicate blk 0
    gain = fromDb (lcGainDb lc)
    -- Only as much of our own transmit as the longest tap can still
    -- reach back into; kept bounded so appending per block stays cheap.
    taps = [ (d * fs, g) | (d, g) <- lcEcho lc ]
    histN = blk + ceiling (maximum (0 : map fst taps)) + 8
    push h x = let h' = h VS.++ x in VS.drop (max 0 (VS.length h' - histN)) h'
    -- the reflection of the block we are about to hear ourselves say
    reflect h
      | null taps = quiet
      | otherwise = let e = echoPath taps h in VS.drop (max 0 (VS.length e - blk)) e
    -- the echo rides on top of whatever arrived from the far end; the
    -- lengths agree in practice, and mix does not assume it
    mix a b = VS.generate (VS.length a) $ \i ->
      a VS.! i + (if i < VS.length b then b VS.! i else 0)
    -- Each direction gets its own seed so the two are not the same
    -- noise, and each block its own so a longer call is not the same
    -- noise repeated.
    line :: Int -> Int -> Signal -> Signal
    line k i x =
      let ch = lcLine lc
      in applyChannel fs ch { chSeed = chSeed ch * 1000003 + k * 7919 + i } (VS.map (* gain) x)
    go t so sa fromA fromO hO hA sentO sentA fullAt rxO rxA evO evA
      | t >= lcMaxT lc = out
      | Just t0 <- fullAt, t - t0 >= 2 = out
      | otherwise =
          let i = round (t / lcBlockSec lc)
              queueO = if modemConnected so && not sentO then textO else []
              queueA = if modemConnected sa && not sentA then textA else []
              hO' = push hO fromO
              hA' = push hA fromA
              (so', audioO, bytesO, eO) = modemStep cfgO so (mix (line 1 i fromA) (reflect hO')) queueO
              (sa', audioA, bytesA, eA) = modemStep cfgA sa (mix (line 2 i fromO) (reflect hA')) queueA
              rxO' = reverse bytesO ++ rxO
              rxA' = reverse bytesA ++ rxA
              sentO' = sentO || not (null queueO)
              sentA' = sentA || not (null queueA)
              full = sentO' && sentA'
                     && length rxO' >= length textA && length rxA' >= length textO
              fullAt' = case fullAt of
                Just _ -> fullAt
                Nothing -> if full then Just t else Nothing
          in go (t + fromIntegral blk / fs) so' sa' audioA audioO hO' hA' sentO' sentA' fullAt'
                rxO' rxA' (reverse eO ++ evO) (reverse eA ++ evA)
      where
        out = LoopResult (reverse rxO) (reverse rxA) (reverse evO) (reverse evA)
                (any isUp evO && any isUp evA)
                (modemEchoErle so) (modemEchoErle sa)
                (modemEchoDelay so) (modemEchoDelay sa)
                (modemV32Evm so) (modemV32Evm sa)
        isUp e = case e of { EvConnected {} -> True; _ -> False }

-- | Did the call come up and carry both texts without an error?
loopOk :: LoopConfig -> [Word8] -> [Word8] -> Bool
loopOk lc textO textA =
  let r = loopback lc textO textA
  in lrConnected r && lrRxOrig r == textA && lrRxAnswer r == textO

-- | How many of @n@ noise realisations a call survives, at one signal
-- to noise ratio.
--
-- A single call is not a measurement.  Near its threshold a mode fails
-- on the noise it happened to get, not on the ratio: a run that dies at
-- 30 dB and lives at 24 dB is ordinary, and a search that walks down
-- from a clean line and stops at the first failure will report whatever
-- the first unlucky realisation was.  Counting how many of several
-- seeds survive gives a curve with a knee in it instead of a number
-- with a coin flip in it, and a change to a receiver moves the curve.
loopPassRate :: Int -> (Int -> LoopConfig) -> [Word8] -> [Word8] -> Int
loopPassRate n mk textO textA =
  length [ () | s <- [1 .. n], loopOk (mk s) textO textA ]
