-- | Impairment sweeps: modulate a payload, push it through a list of
-- channel conditions, and report errors.
--
-- The default command sweeps the FSK and V.22 modems.  The @v32-@
-- commands sweep the trained V.32 receiver and its two loops; they
-- print tables and assert nothing, which is why they live here and not
-- in the test suite.  A survey is a thing to read, not a thing to pass.
module Main (main) where

import Control.Monad (forM_)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Options.Applicative
import Text.Printf (printf)

import Modec.Channel
import Modec.DSP
import Modec.FSK
import Modec.Metrics
import Modec.QAM
import Modec.Standards
import Modec.Async
import Modec.V22
import Modec.V32
import Modec.V32Pump

data Opts = Opts
  { oRate    :: Double
  , oBytes   :: Int
  , oChannel :: String
  , oGain    :: Double
  , oWindow  :: String
  }

optsP :: Parser Opts
optsP = Opts
  <$> option auto (long "rate" <> value 8000 <> showDefault)
  <*> option auto (long "bytes" <> value 400 <> showDefault <> help "payload length")
  <*> strOption (long "channel" <> value "answer" <> showDefault <> help "answer | originate | v21 | v22low | v22high | v22bislow | v22bishigh")
  <*> option auto (long "timing-gain" <> value 0.5 <> showDefault)
  <*> strOption (long "window" <> value "rect" <> showDefault <> help "hann | rect")

-- | The FSK sweep keeps the bare options it always had, so
-- @modec-bench --channel answer@ still means what it used to; the V.32
-- surveys hang off subcommands beside it.
data Cmd = Fsk Opts | V32Timing | V32Carrier | V32Trace | V32Survey

cmdP :: Parser Cmd
cmdP = hsubparser
  (  command "v32-timing"  (info (pure V32Timing)  (progDesc "V.32: timing loop gains against the hard channels"))
  <> command "v32-carrier" (info (pure V32Carrier) (progDesc "V.32: carrier loop gains against the hard channels"))
  <> command "v32-trace"   (info (pure V32Trace)   (progDesc "V.32: the timing loop block by block through a clock offset"))
  <> command "v32-survey"  (info (pure V32Survey)  (progDesc "V.32: every impairment axis, per rate"))
  ) <|> (Fsk <$> optsP)

payloadBytes :: Int -> Int -> [Word8]
payloadBytes salt n = [fromIntegral ((i * 7919 + salt * 104729 + 13) `mod` 256) | i <- [1 .. n]]

main :: IO ()
main = do
  c <- execParser (info (cmdP <**> helper) (fullDesc <> progDesc "modec impairment sweep"))
  case c of
    Fsk o      -> fskSweep o
    V32Timing  -> v32TimingSweep
    V32Carrier -> v32CarrierSweep
    V32Trace   -> v32LoopTrace
    V32Survey  -> v32Survey

fskSweep :: Opts -> IO ()
fskSweep o = do
  let fs = oRate o
      isV22 = oChannel o `elem` ["v22low", "v22high", "v22bislow", "v22bishigh"]
      is2400 = oChannel o `elem` ["v22bislow", "v22bishigh"]
      v22ch = if oChannel o `elem` ["v22low", "v22bislow"] then LowChannel else HighChannel
      v22other = if v22ch == LowChannel then HighChannel else LowChannel
      (spec, other) = case oChannel o of
        "originate" -> (bell103Originate, bell103Answer)
        "v21"       -> (v21Channel2, v21Channel1)
        _           -> (bell103Answer, bell103Originate)
      params = defaultDemodParams { dpTimingGain = oGain o, dpWindow = if oWindow o == "hann" then Hann else Rect }
      payload = payloadBytes 1 (oBytes o)
      framed bs = replicate 120 True ++ frameBits framing8N1 bs ++ replicate 120 True
      -- V.22bis: 0.6 s of scrambled ones at 1200 bit/s (coherent loop training), then 2400 bit/s data
      modulateV22 ch bs
        | is2400 =
            let fr = framing8N1
                (stPre, pre) = v22TxBlock fs ch fr 0.5 False R1200 TxScrambledOnes [] 4800 v22TxInit
                blocks st rest
                  | null rest && null (txBitsOf st) = [snd (v22TxBlock fs ch fr 0.5 False R2400 TxScrambledOnes [] 400 st)]
                  | otherwise = let (st', sig) = v22TxBlock fs ch fr 0.5 False R2400 TxScrambledData [] 160 (withBits st (take 2000 rest)) in sig : blocks st' (drop 2000 rest)
            in pre VS.++ VS.concat (blocks stPre bs)
        | otherwise = v22Modulate fs ch 0.5 bs
      clean = if isV22 then modulateV22 v22ch (framed payload)
                       else encodeBytes fs spec framing8N1 0.5 0.2 0.2 payload
      adjacent = if isV22 then modulateV22 v22other (framed (payloadBytes 2 (oBytes o)))
                          else encodeBytes fs other framing8N1 0.5 0.05 0.2 (payloadBytes 2 (oBytes o))
      decodeV22 sig
        | is2400 =
            let chunks v | VS.null v = [] | otherwise = VS.take 160 v : chunks (VS.drop 160 v)
                run _ _ [] = []
                run i st (c : cs) = let st1 = if i == (30 :: Int) then v22RxSetRate R2400 st else st
                                        (st', r) = v22RxBlock fs v22ch c st1 in r : run (i + 1) st' cs
            in concatMap roBits (drop 32 (run 0 (v22RxInit fs) (chunks sig)))
        | otherwise = drop 60 (v22Demodulate fs v22ch sig)
      decode sig = if isV22
                     then let (_, bytes) = asyncRxBits (asyncRxInit framing8N1) (decodeV22 sig) in bytes
                     else demodulate fs spec framing8N1 params sig
      name = if isV22 then (if is2400 then "V.22bis 2400 " else "V.22 1200 ") ++ show v22ch else fskName spec
      bp = Just (300, 3400)
      base = idealChannel { chBandpass = bp }
      conditions :: [(String, Signal -> Signal)]
      conditions =
        [ ("clean", id)
        , ("bandpass 300-3400", applyChannel fs base)
        ] ++
        [ (printf "bandpass, SNR %2.0f dB" s, applyChannel fs base { chSnrDb = Just s }) | s <- [30, 20, 15, 12, 10, 8, 6] ] ++
        [ (printf "rate offset %+.1f %%" (r * 100), applyChannel fs base { chRateOffset = r }) | r <- [0.01, -0.01, 0.03, -0.03, 0.05] ] ++
        [ (printf "freq offset %+.0f Hz" f, applyChannel fs base { chFreqOffsetHz = f }) | f <- [7, -7, 30] ] ++
        [ ("sine jitter 3 smp @ 2 Hz", applyChannel fs base { chJitter = SineJitter 3 2 })
        , ("walk jitter 0.05/smp max 10", applyChannel fs base { chJitter = WalkJitter 0.05 10 })
        , ("slips 4 smp every 0.3 s", applyChannel fs base { chJitter = Slips 0.3 4 })
        , ("dropouts 20 ms p=0.01", applyChannel fs base { chDropout = Just (0.02, 0.01) })
        , ("dropouts 20 ms p=0.05", applyChannel fs base { chDropout = Just (0.02, 0.05) })
        , ("echo 5 ms -12 dB", applyChannel fs base { chEcho = Just (0.005, fromDb (-12)) })
        , ("delay distortion 1 ms at band edges", applyChannel fs base { chDelayDist = 1 })
        , ("delay distortion 3 ms at band edges", applyChannel fs base { chDelayDist = 3 })
        -- the adjacent channel is mixed in after the line, so SNR figures refer to the wanted signal
        , ("adjacent channel +10 dB", mixAt 10 adjacent . applyChannel fs base)
        , ("adjacent channel +20 dB", mixAt 20 adjacent . applyChannel fs base)
        , ("adjacent channel +30 dB", mixAt 30 adjacent . applyChannel fs base)
        , ("adjacent +10 dB, clip 0.3", applyChannel fs idealChannel { chClip = Just 0.3 } . mixAt 10 adjacent . applyChannel fs base)
        , ("level -40 dBFS", applyChannel fs base { chGain = fromDb (-40) / 0.5 })
        , ("level -50 dBFS", applyChannel fs base { chGain = fromDb (-50) / 0.5 })
        , ("hum 60 Hz amp 0.3", applyChannel fs base { chHum = Just (60, 0.3) })
        , ("dc offset 0.3", applyChannel fs base { chDcOffset = 0.3 })
        , ("realistic acoustic: adj +20, SNR 25, rate 0.5 %, 3 Hz, jitter"
          , mixAt 20 adjacent . applyChannel fs base { chSnrDb = Just 25, chRateOffset = 0.005, chFreqOffsetHz = 3, chJitter = SineJitter 2 1 })
        , ("VoIP: SNR 35, slips, dropouts p=0.005, rate 0.1 %"
          , applyChannel fs base { chSnrDb = Just 35, chJitter = Slips 0.5 8, chDropout = Just (0.02, 0.005), chRateOffset = 0.001 })
        ]
  printf "%s at %.0f Hz, %d bytes, timing gain %.2f, window %s\n" name fs (oBytes o) (oGain o) (oWindow o)
  forM_ conditions $ \(cname, f) -> do
    let sig = f clean
        got = decode sig
        d = editDistance payload got
    printf "  %-62s %5d errors  (%5.1f %%)  rms %.3f\n" cname d (100 * fromIntegral d / fromIntegral (oBytes o) :: Double) (rms sig)
    VS.length sig `seq` return ()

-- Fuzz the trained V.32 receiver along each impairment axis and report
-- where it breaks, per rate.  A survey, not an assertion.
v32Fuzz :: V32Rate -> [(String, Channel)] -> [(String, Int)]
v32Fuzz = v32FuzzWith id

v32FuzzWith :: (QamRxCfg -> QamRxCfg) -> V32Rate -> [(String, Channel)] -> [(String, Int)]
v32FuzzWith tune r conds =
  [ (nm, errs) | (nm, ch) <- conds
  , let (clean, preSyms) = v32ModulateTrained 8000 Calling r 0.5 1400 payload
        sig = applyChannel 8000 ch clean
        got = v32DemodulateTrainedWith tune 8000 Answering r preSyms sig
        errs = minimum [ length (filter id (zipWith (/=) (drop 200 payload) (drop (200 + o) got)))
                       | o <- [0 .. 300] ] ]
  where payload = prbs (11, 9) 4000

v32TimingSweep :: IO ()
v32TimingSweep = do
  let tel = telephoneChannel 25
      hard = [ ("jit 3@5", tel { chJitter = SineJitter 3 5 })
             , ("jit 5@2", tel { chJitter = SineJitter 5 2 })
             , ("walk .02/5", tel { chJitter = WalkJitter 0.02 5 })
             , ("walk .05/10", tel { chJitter = WalkJitter 0.05 10 })
             , ("clock +1%", tel { chRateOffset = 0.01 })
             , ("clock +2%", tel { chRateOffset = 0.02 })
             , ("slips .5/2", tel { chJitter = Slips 0.5 2 })
             , ("SNR 14", telephoneChannel 14) ]
  forM_ [V32R4800, V32R9600] $ \r -> do
    putStrLn ("=== " ++ show r)
    forM_ [ (kp, ki) | kp <- [0.12, 0.25, 0.4], ki <- [0.0015, 0.005, 0.015] ] $ \(kp, ki) -> do
      let res = v32FuzzWith (\c -> c { qrKp = kp, qrKi = ki }) r hard
      putStrLn ("  kp=" ++ show kp ++ " ki=" ++ show ki ++ "  "
                ++ unwords [ take 11 (nm ++ ":" ++ (if e == 0 then "ok" else show e) ++ repeat ' ') | (nm, e) <- res ])

v32CarrierSweep :: IO ()
v32CarrierSweep = do
  let tel = telephoneChannel 25
      hard = [ ("walk .02/5", tel { chJitter = WalkJitter 0.02 5 })
             , ("walk .05/10", tel { chJitter = WalkJitter 0.05 10 })
             , ("jit 3@5", tel { chJitter = SineJitter 3 5 })
             , ("jit 2@2", tel { chJitter = SineJitter 2 2 })
             , ("clock +1%", tel { chRateOffset = 0.01 })
             , ("carrier +20", tel { chFreqOffsetHz = 20 })
             , ("slips .5/2", tel { chJitter = Slips 0.5 2 })
             , ("SNR 14", telephoneChannel 14) ]
      cell (nm, e) = take 14 (nm ++ ":" ++ (if e == 0 then "ok" else show e) ++ repeat ' ')
  forM_ [V32R4800, V32R9600T] $ \r -> do
    putStrLn ("=== " ++ show r)
    forM_ [ (kp, ki) | kp <- [0.03, 0.08, 0.15, 0.25], ki <- [0.0015, 0.005, 0.015] ] $ \(kp, ki) ->
      putStrLn ("  thKp=" ++ show kp ++ " thKi=" ++ show ki ++ "  "
                ++ concatMap cell (v32FuzzWith (\c -> c { qrThKp = kp, qrThKi = ki }) r hard))

-- Watch the timing loop's sps estimate and the decision error through a
-- clock offset, block by block.
v32LoopTrace :: IO ()
v32LoopTrace =
  forM_ [ ("clock +1%", (telephoneChannel 25) { chRateOffset = 0.01 })
        , ("clock -1%", (telephoneChannel 25) { chRateOffset = -0.01 })
        , ("walk .02/5", (telephoneChannel 25) { chJitter = WalkJitter 0.02 5 }) ] $ \(nm, ch) -> do
    let r = V32R4800
        payload = prbs (11, 9) 4000
        (clean, _) = v32ModulateTrained 8000 Calling r 0.5 1400 payload
        sig = applyChannel 8000 ch clean
        p = v32Params 8000
        cfg = v32RxCfg V32R4800
        walk st k acc s
          | VS.null s = reverse acc
          | otherwise =
              let (c, rest) = VS.splitAt 800 s
                  (st', _) = qamRxBlock p cfg c st
                  row = (k, qamRxSps st', qamRxEvm st', qamRxFreq st' * 2400 / (2 * pi))
              in walk st' (k + 1) (row : acc) rest
    putStrLn ("=== " ++ nm ++ " (nominal sps 3.3333)")
    forM_ (walk (qamRxInit p cfg) (0 :: Int) [] sig) $ \(k, sps, evm, hz) ->
      putStrLn ("  block " ++ show k ++ "  sps " ++ show (fromIntegral (round (sps * 10000)) / 10000 :: Double)
                ++ "  evm " ++ show (fromIntegral (round (evm * 1000)) / 1000 :: Double)
                ++ "  carrier " ++ show (fromIntegral (round hz) :: Double) ++ " Hz")

v32Survey :: IO ()
v32Survey =
  forM_ [V32R4800, V32R9600, V32R9600T] $ \r -> do
    let tel s = telephoneChannel s
        axes =
          [ ("SNR " ++ show s, tel s) | s <- [20, 16, 14, 12, 10, 8 :: Double] ] ++
          [ ("sine jitter a=" ++ show a ++ " f=" ++ show f, (tel 25) { chJitter = SineJitter a f })
          | (a, f) <- [(1, 2), (2, 2), (3, 2), (5, 2), (3, 5), (3, 10), (8, 1)] ] ++
          [ ("walk jitter step=" ++ show st ++ " max=" ++ show mx, (tel 25) { chJitter = WalkJitter st mx })
          | (st, mx) <- [(0.02, 5), (0.05, 10), (0.1, 20)] ] ++
          [ ("slips every " ++ show e ++ "s of " ++ show k, (tel 25) { chJitter = Slips e k })
          | (e, k) <- [(0.5, 2), (0.3, 4), (0.2, 8)] ] ++
          [ ("clock " ++ show c, (tel 25) { chRateOffset = c }) | c <- [0.005, 0.01, 0.02, -0.01] ] ++
          [ ("carrier " ++ show hz, (tel 25) { chFreqOffsetHz = hz }) | hz <- [10, 15, 20, -15] ] ++
          [ ("delay dist " ++ show ms, (tel 25) { chDelayDist = ms }) | ms <- [1, 2, 3] ]
    putStrLn ("=== " ++ show r)
    forM_ (v32Fuzz r axes) $ \(nm, e) ->
      putStrLn ("  " ++ take 30 (nm ++ repeat ' ') ++ (if e == 0 then "ok" else show e ++ " errors"))
