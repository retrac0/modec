-- | Impairment sweeps: modulate a payload, push it through a list of
-- channel conditions, and report errors.
--
-- The default command sweeps the FSK and V.22 modems.  The @v32-@
-- commands sweep the trained V.32 receiver and its two loops; they
-- print tables and assert nothing, which is why they live here and not
-- in the test suite.  A survey is a thing to read, not a thing to pass.
module Main (main) where

import Control.Monad (forM_)
import Data.List (isInfixOf, nub)
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
import Modec.Loopback
import Modec.Modem (ModemConfig (..), ModemEvent (..))
import Modec.Echo (EchoConfig (..))
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
data Cmd = Fsk Opts | V32Timing | V32Carrier | V32Trace | V32Survey | Loopback (Maybe String)
         | V32Echo (Maybe String)

cmdP :: Parser Cmd
cmdP = hsubparser
  (  command "v32-timing"  (info (pure V32Timing)  (progDesc "V.32: timing loop gains against the hard channels"))
  <> command "v32-carrier" (info (pure V32Carrier) (progDesc "V.32: carrier loop gains against the hard channels"))
  <> command "v32-trace"   (info (pure V32Trace)   (progDesc "V.32: the timing loop block by block through a clock offset"))
  <> command "v32-survey"  (info (pure V32Survey)  (progDesc "V.32: every impairment axis, per rate"))
  <> command "loopback"    (info (Loopback <$> optional (strArgument (metavar "MODE")))
       (progDesc "Two whole modems calling each other through the channel simulator: the lowest SNR each mode still carries text at"))
  <> command "v32-echo"    (info (V32Echo <$> optional (strArgument (metavar "TABLE")))
       (progDesc "V.32/V.32bis per rate against near-end echo and noise, over a real call: echo | noise | both"))
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
    Loopback m -> loopbackSweep m
    V32Echo w  -> v32EchoSweep w

-- | The echo canceller, measured the only way that means anything: over
-- a call that has to establish itself with the echo already there.
--
-- The offline sweeps hand a trained receiver a clean signal.  V.32's
-- difficulty is that both ends occupy the same band at the same time, so
-- each one hears its own transmit reflected off its hybrid on top of the
-- signal it is trying to read -- and, because the reflection has not
-- crossed the line, it arrives louder than the far end does.  At
-- 'lcGainDb' of -20 dB an echo at -12 dB sits 8 dB above the wanted
-- signal.  That is the ordinary case, not the hard one.
--
-- The taps are at fractional sample delays on purpose.  A single tap at
-- a whole number of samples is cancelled exactly by a linear FIR, and a
-- canceller measured against one reports a number it will not repeat on
-- a telephone line.
v32EchoSweep :: Maybe String -> IO ()
v32EchoSweep want = do
  let tables = [ ("echo", echoTable), ("noise", noiseTable), ("both", bothTable)
               , ("detail", detailTable), ("probe", probeTable)
               , ("gain", gainTable), ("tune", tuneTable), ("level", levelTable)
               , ("top", topTable), ("long", longTable), ("gate", gateTable)
               , ("rot", rotTable), ("margin", marginTable) ]
  forM_ [ (n, t) | (n, t) <- tables, maybe True (== n) want ] $ \(_, t) -> t
  where
    trials = 3
    textO = map (fromIntegral . fromEnum) "Hello from the caller, 0123456789 !\r\n"
    textA = map (fromIntegral . fromEnum) "Answerer here; all bytes: \255\0\128 end\r\n"
    -- Every rate the two Recommendations define, with the modes that can
    -- actually reach it: 7200 and up are V.32bis only.
    rates =
      [ ("4800",   [V32],    V32R4800)
      , ("9600",   [V32],    V32R9600)
      , ("9600t",  [V32],    V32R9600T)
      , ("7200",   [V32bis], V32R7200)
      , ("12000",  [V32bis], V32R12000)
      , ("14400",  [V32bis], V32R14400)
      ]
    -- (name, taps as (delay seconds, linear gain)).  ERL is quoted
    -- against the transmit, so -12 dB means the hybrid returns a quarter
    -- of what we put into it.
    echoes =
      [ ("none",  [])
      , ("-20dB", [(0.0031, 0.100)])
      , ("-12dB", [(0.0043, 0.250), (0.0091, 0.090)])
      , ("-6dB",  [(0.0037, 0.500), (0.0113, 0.220), (0.0207, 0.080)])
      -- a near hybrid plus a network echo 45 ms out: two reflections
      -- further apart than one 256-tap filter can span at once
      , ("far",   [(0.0041, 0.300), (0.0451, 0.260)])
      ]
    mk ms rate taps snr seed =
      let base = defaultLoop 8000 ms
          pin c = c { mcV32Rates = Just (chosenRate rate) }
      in base { lcOrig = pin (lcOrig base)
              , lcAnswer = pin (lcAnswer base)
              , lcEcho = taps
              , lcLine = (lcLine base) { chSnrDb = snr, chSeed = seed }
              , lcMaxT = 30 }
    -- ERLE is what the canceller believes it achieved; printed beside the
    -- pass count because a rate can fail for reasons that are not echo,
    -- and the two numbers together say which it was.  The worse of the
    -- two ends is the one that decides whether the call works.
    -- Delivery is scored on the text arriving whole, not on the receive
    -- buffer equalling it exactly.  14400 is known to bracket a payload
    -- with a few bytes of residue on an ideal line (Suite.Link, "V.32bis
    -- reaches 12000 and 14400") while the decision error is still
    -- settling, and scoring that as a failure here would report the
    -- residue as an echo problem, which it is not.
    cell ms rate taps snr = do
      let rs = [ loopback (mk ms rate taps snr s) textO textA | s <- [1 .. trials] ]
          up r = lrConnected r
          got r = up r && textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
          erles = [ min a b | r <- rs, Just a <- [lrErleOrig r], Just b <- [lrErleAnswer r] ]
          erle | null erles = "   -" :: String
               | otherwise = printf "%4.0f" (sum erles / fromIntegral (length erles))
      printf "  %d-%d %s" (length (filter up rs)) (length (filter got rs)) erle
    colHeads cols = do
      printf "%-8s" ("rate" :: String)
      forM_ cols $ \c -> printf "  %-9s" c
      putStrLn ""
    echoTable = do
      putStrLn "V.32/V.32bis over a call, per rate, against near-end echo (SNR 26 dB)"
      putStrLn ("cell: connected-delivered out of " ++ show trials
                ++ " tries, then mean ERLE in dB (worse end)")
      putStrLn ""
      colHeads (map fst echoes)
      forM_ rates $ \(rn, ms, rate) -> do
        printf "%-8s" rn
        forM_ echoes $ \(_, taps) -> cell ms rate taps (Just 26)
        putStrLn ""
      putStrLn ""
    noiseTable = do
      putStrLn "The same, per rate against noise, with no echo at all"
      putStrLn ""
      colHeads (map (\d -> show (round d :: Int) ++ "dB") snrs)
      forM_ rates $ \(rn, ms, rate) -> do
        printf "%-8s" rn
        forM_ snrs $ \snr -> cell ms rate [] (Just snr)
        putStrLn ""
      putStrLn ""
    bothTable = do
      putStrLn "And both together: an ordinary hybrid at -12 dB, through noise"
      putStrLn ""
      colHeads (map (\d -> show (round d :: Int) ++ "dB") snrs)
      forM_ rates $ \(rn, ms, rate) -> do
        printf "%-8s" rn
        forM_ snrs $ \snr -> cell ms rate (snd (echoes !! 2)) (Just snr)
        putStrLn ""
      putStrLn ""
    snrs = [30, 26, 22, 18] :: [Double]
    -- Loudness and tap count, separated.  In the table above, every
    -- profile with more than one tap failed and every profile with one
    -- or none passed -- but the multi-tap profiles were also the loud
    -- ones, so the two explanations are not yet told apart.  These four
    -- cross them over: same loudness, different tap counts.
    probeTable = do
      putStrLn "Is it the echo's strength or the number of reflections?"
      putStrLn ""
      printf "%-14s %-6s %-5s %-5s %s\n" ("profile" :: String) ("taps" :: String)
        ("up" :: String) ("got" :: String) ("what it is" :: String)
      forM_ probes $ \(nm, taps, note) -> do
        let r = loopback (mk [V32] V32R9600T taps (Just 26) 1) textO textA
            yn b = if b then "yes" else "no" :: String
        printf "%-14s %-6d %-5s %-5s %s\n" nm (length taps)
          (yn (lrConnected r))
          (yn (textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r)) note
      putStrLn ""
    -- The first thing either end gave up on, which names the phase of
    -- Figure 4 the echo broke rather than just saying the call failed.
    why r = case [ w | EvFailed w <- lrEvOrig r ] ++ [ w | EvFailed w <- lrEvAnswer r ] of
      (w : _) -> w
      [] -> if lrConnected r then "-" else "no event"
    gains = [0.10, 0.14, 0.18, 0.22, 0.26] :: [Double]
    gainTable = do
      putStrLn "Where the cliff is: one reflection at 3.9 ms, rising gain (SNR 26 dB)"
      putStrLn "the far end arrives at 0.05; gain is against our own transmit"
      putStrLn ""
      forM_ [("9600t", [V32], V32R9600T), ("14400", [V32bis], V32R14400)] $ \(rn, ms, rate) ->
        forM_ gains $ \g -> do
          let r = loopback (mk ms rate [(0.0039, g)] (Just 26) 1) textO textA
          printf "%-8s gain %.2f  up %-4s got %-4s  %s\n" rn g
            (if lrConnected r then "yes" else "no" :: String)
            (if textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
               then "yes" else "no" :: String)
            (why r)
      putStrLn ""
    -- Whether the cliff is the canceller running out of convergence in
    -- the window it is given, or something else.  Only the bench's own
    -- copy of the config is touched; the library's defaults stand.
    tuneTable = do
      putStrLn "Can the canceller be tuned past it?  One reflection at gain 0.22"
      putStrLn ""
      printf "%-6s %-6s %-5s %-5s %s\n" ("mu" :: String) ("taps" :: String)
        ("up" :: String) ("got" :: String) ("why" :: String)
      forM_ [ (mu, tp) | mu <- [0.1, 0.3, 0.6, 1.0 :: Double], tp <- [256, 512 :: Int] ] $ \(mu, tp) -> do
        let base = mk [V32] V32R9600T [(0.0039, 0.22)] (Just 26) 1
            tw c = c { mcEcho = (mcEcho c) { ecMu = mu, ecTaps = tp } }
            r = loopback base { lcOrig = tw (lcOrig base), lcAnswer = tw (lcAnswer base) } textO textA
        printf "%-6.1f %-6d %-5s %-5s %s\n" mu tp
          (if lrConnected r then "yes" else "no" :: String)
          (if textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
             then "yes" else "no" :: String)
          (why r)
      putStrLn ""
    -- If the AC trackers are being held down by a shared wideband
    -- denominator, the cliff is a ratio and nothing else: make the far
    -- end louder by the same factor the echo was raised by and detection
    -- should come back at the same echo gain.  If instead the echo were
    -- genuinely masking a 600 Hz sideband -- which a pure 1800 Hz tone
    -- cannot do -- the far end's level would not buy it back.
    levelTable = do
      putStrLn "Echo fixed at gain 0.22; the far end's arriving level raised"
      putStrLn ""
      printf "%-9s %-9s %-5s %s\n" ("lineloss" :: String) ("echo/far" :: String)
        ("up" :: String) ("why" :: String)
      forM_ [-20, -17, -14, -11, -8 :: Double] $ \gdb -> do
        let base = mk [V32] V32R9600T [(0.0039, 0.22)] (Just 26) 1
            r = loopback base { lcGainDb = gdb } textO textA
        printf "%-9.0f %-9.1f %-5s %s\n" gdb
          (20 * logBase 10 (0.22 / (10 ** (gdb / 20))))
          (if lrConnected r then "yes" else "no" :: String) (why r)
      putStrLn ""
    -- 14400 connects at every SNR here and delivers at none of them,
    -- which noise does not explain.  The suite's own 14400 test passes,
    -- and it runs over 'modemDuplex' -- an ideal line at full level.
    -- This crosses the two things that differ: how far down the signal
    -- arrives, and whether there is any noise on it at all.
    topTable = do
      putStrLn "12000 and 14400: arriving level against noise, no echo"
      putStrLn "cell: connected-delivered out of 1, and the bytes the caller got"
      putStrLn ""
      forM_ [("12000", V32R12000), ("14400", V32R14400)] $ \(rn, rate) -> do
        forM_ [0, -6, -12, -20 :: Double] $ \gdb -> do
          forM_ [Nothing, Just 34, Just 30, Just 26] $ \snr -> do
            let base = mk [V32bis] rate [] snr 1
                r = loopback base { lcGainDb = gdb } textO textA
            printf "%-6s loss %3.0f dB  snr %-6s up %-4s got %-4s  rx %d/%d\n"
              rn gdb (maybe "none" (\d -> show (round d :: Int)) snr :: String)
              (if lrConnected r then "yes" else "no" :: String)
              (if textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
                 then "yes" else "no" :: String)
              (length (lrRxOrig r)) (length textA)
        putStrLn ""
    -- The suite's own 14400 test gives the call 45 seconds; the tables
    -- above gave it 30.  If the payload is simply arriving late that is
    -- the whole difference, and it is worth knowing before calling the
    -- rate broken.
    longTable = do
      putStrLn "12000 and 14400 with the 45 s budget the suite's own test uses, no echo"
      putStrLn ""
      forM_ [("12000", V32R12000), ("14400", V32R14400)] $ \(rn, rate) ->
        forM_ [(0, "no loss"), (-20, "-20 dB" :: String)] $ \(gdb, ln) ->
          forM_ [1 .. 3 :: Int] $ \sd -> do
            let base = mk [V32bis] rate [] (Just 30) sd
                r = loopback base { lcGainDb = gdb, lcMaxT = 45 } textO textA
            printf "%-6s %-8s seed %d  up %-4s got %-4s  rx %d/%d\n" rn ln sd
              (if lrConnected r then "yes" else "no" :: String)
              (if textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
                 then "yes" else "no" :: String)
              (length (lrRxOrig r)) (length textA)
      putStrLn ""
    -- The byte gate, not the modulation.  'trust' compares the slicer's
    -- error against a fraction of the constellation's own minimum
    -- distance, and 'armed' needs 'trust' before the framer will start
    -- at all -- which is why a 14400 call that connects can hand over
    -- nothing whatever.  A trellis rate is not decoded by that geometry:
    -- the Viterbi decoder resolves points the slicer cannot, so measuring
    -- it against half the distance between neighbours asks the receiver
    -- to be clean in a way the decoder does not need it to be.  If
    -- loosening the ceiling delivers the payload *exactly*, the gate is
    -- the whole of the fault and the receiver underneath is sound.
    gateTable = do
      putStrLn "Byte gate: max-evm against what actually arrives (SNR 30, no echo)"
      putStrLn "exact = the payload matched byte for byte, not merely contained"
      putStrLn ""
      printf "%-8s %-9s %-6s %-7s %-7s %s\n" ("rate" :: String) ("max-evm" :: String)
        ("up" :: String) ("infix" :: String) ("exact" :: String) ("rx/want, evm" :: String)
      forM_ [ (rn, rate, ev) | (rn, rate) <- [("9600t", V32R9600T), ("12000", V32R12000), ("14400", V32R14400)]
            , ev <- [1, 2, 4, 8, 1e9 :: Double] ] $ \(rn, rate, ev) -> do
        let base = mk [V32bis] rate [] (Just 30) 1
            gt c = c { mcMaxEvmV32 = ev }
            r = loopback base { lcOrig = gt (lcOrig base), lcAnswer = gt (lcAnswer base)
                              , lcMaxT = 45 } textO textA
            yn b = if b then "yes" else "no" :: String
        printf "%-8s %-9.0f %-6s %-7s %-7s %d/%d %s\n" rn ev
          (yn (lrConnected r))
          (yn (textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r))
          (yn (lrRxOrig r == textA && lrRxAnswer r == textO))
          (length (lrRxOrig r)) (length textA)
          (maybe "-" (printf "%.3f" . abs) (lrEvmOrig r) :: String)
      putStrLn ""
    -- V.32 5.1.3 differentially encodes Q1 Q2 so the code survives the
    -- receiver settling on any of the four carrier phases.  That only
    -- delivers the *data* if the constellation is labelled so a 90 degree
    -- rotation leaves the uncoded bits Q3..Qn alone: the differential
    -- decoder undoes the rotation in the coded pair, and nothing undoes
    -- it anywhere else.  This asks the point tables that question
    -- directly -- arithmetic, no channel and no receiver.
    rotTable = do
      putStrLn "Rotate every point 90, 180, 270 degrees: do the uncoded bits survive?"
      putStrLn "'off grid' means the rotated point is not in the constellation at all"
      putStrLn ""
      printf "%-8s %-7s %-8s %-10s %-10s %s\n" ("rate" :: String) ("points" :: String)
        ("uncoded" :: String) ("90 bad" :: String) ("180 bad" :: String) ("270 bad" :: String)
      forM_ [ ("7200", V32R7200), ("9600t", V32R9600T)
            , ("12000", V32R12000), ("14400", V32R14400) ] $ \(rn, rate) -> do
        let n = 2 ^ (1 + rateBitsPerSymbol rate) :: Int
            u = 2 ^ rateUncoded rate :: Int
            rot90 (x, y) = (negate y, x)
            turn k = foldr (.) id (replicate k rot90)
            d2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)
            countBad k =
              let offs = [ () | i <- [0 .. n - 1]
                         , let q = turn k (constellation rate i)
                         , d2 q (constellation rate (slicePoint rate q)) > 1e-9 ]
                  wrong = [ () | i <- [0 .. n - 1]
                          , let j = slicePoint rate (turn k (constellation rate i))
                          , i `mod` u /= j `mod` u ]
              in if not (null offs) then "off grid" else show (length wrong) ++ "/" ++ show n
        printf "%-8s %-7d %-8d %-10s %-10s %s\n" rn n (rateUncoded rate)
          (countBad 1) (countBad 2) (countBad 3)
      putStrLn ""
      -- The uncoded bits are only half of it.  The Viterbi decoder works
      -- on the subset named by Y0 Y1 Y2, and for a rotation the
      -- differential decoder can undo, every point of one subset must
      -- rotate into one and the same other subset.  If a subset scatters
      -- across several, a receiver that locked 90 degrees out decodes
      -- into the wrong trellis branch and the coded bits are lost even
      -- though the constellation looks perfectly invariant.
      putStrLn "And do whole subsets rotate together?  (scattered = they do not)"
      putStrLn ""
      printf "%-8s %-12s %-12s %s\n" ("rate" :: String) ("90" :: String)
        ("180" :: String) ("270" :: String)
      forM_ [ ("7200", V32R7200), ("9600t", V32R9600T)
            , ("12000", V32R12000), ("14400", V32R14400) ] $ \(rn, rate) -> do
        let n = 2 ^ (1 + rateBitsPerSymbol rate) :: Int
            u = 2 ^ rateUncoded rate :: Int
            rot90 (x, y) = (negate y, x)
            turn k = foldr (.) id (replicate k rot90)
            sub i = i `div` u
            check k =
              let imgs sb = nub [ sub (slicePoint rate (turn k (constellation rate i)))
                                | i <- [0 .. n - 1], sub i == sb ]
                  bad = [ sb | sb <- [0 .. 7 :: Int], length (imgs sb) /= 1 ]
              in if null bad then "together" else "scattered " ++ show (length bad) ++ "/8"
        printf "%-8s %-12s %-12s %s\n" rn (check 1) (check 2) (check 3)
      putStrLn ""
    -- The trained pump reads 14400 cleanly at 26 dB and above.  A live
    -- call was being given 30 -- four decibels of margin, out of which
    -- the start-up handoff still has to pay for residual carrier and
    -- timing error.  If the payload arrives once the line is quiet
    -- enough, 14400 is marginal rather than broken, and the question
    -- becomes how much the handoff costs.
    marginTable = do
      putStrLn "14400 and 12000 over a real call, above the pump's own threshold"
      putStrLn ""
      forM_ [("12000", V32R12000), ("14400", V32R14400)] $ \(rn, rate) ->
        forM_ [Just 46, Just 40, Just 34, Just 30] $ \snr ->
          forM_ [1 .. 3 :: Int] $ \sd -> do
            let base = mk [V32bis] rate [] snr sd
                r = loopback base { lcMaxT = 45 } textO textA
            printf "%-6s snr %-4s seed %d  up %-4s got %-4s exact %-4s rx %d/%d\n"
              rn (maybe "none" (\d -> show (round d :: Int)) snr :: String) sd
              (if lrConnected r then "yes" else "no" :: String)
              (if textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r
                 then "yes" else "no" :: String)
              (if lrRxOrig r == textA && lrRxAnswer r == textO then "yes" else "no" :: String)
              (length (lrRxOrig r)) (length textA)
      putStrLn ""
    probes =
      [ ("quiet-1tap", [(0.0031, 0.100)], "as -20dB: quiet, one reflection" :: String)
      , ("quiet-2tap", [(0.0031, 0.072), (0.0091, 0.070)], "as quiet, split over two")
      , ("loud-1tap",  [(0.0043, 0.266)], "as loud as -12dB, one reflection")
      , ("loud-2tap",  [(0.0043, 0.250), (0.0091, 0.090)], "the -12dB profile itself")
      ]
    -- One call per line, with what the canceller made of it.  The
    -- expected delay is the one block of loop latency this harness adds
    -- (160 samples at 8 kHz) plus the first tap, so a search that is
    -- working lands near 185 for the profiles below.
    detailTable = do
      putStrLn "One call each: what the echo canceller found and what it achieved"
      putStrLn ""
      printf "%-8s %-7s %-4s %-4s %8s %8s %7s %7s\n"
        ("rate" :: String) ("echo" :: String) ("up" :: String) ("got" :: String)
        ("erleO" :: String) ("erleA" :: String) ("dlyO" :: String) ("dlyA" :: String)
      forM_ [ (rn, ms, rate, en, taps)
            | (rn, ms, rate) <- rates, rn `elem` ["9600t", "12000", "14400"]
            , (en, taps) <- echoes, en `elem` ["none", "-20dB", "-12dB"] ] $
        \(rn, ms, rate, en, taps) -> do
          let r = loopback (mk ms rate taps (Just 26) 1) textO textA
              yn b = if b then "yes" else "no" :: String
              mdb (Just x) = printf "%8.1f" (x :: Double)
              mdb Nothing  = "       -" :: String
              mi (Just x) = printf "%7d" (x :: Int)
              mi Nothing  = "      -" :: String
          printf "%-8s %-7s %-4s %-4s %s %s %s %s\n" rn en
            (yn (lrConnected r))
            (yn (textA `isInfixOf` lrRxOrig r && textO `isInfixOf` lrRxAnswer r))
            (mdb (lrErleOrig r)) (mdb (lrErleAnswer r))
            (mi (lrDelayOrig r)) (mi (lrDelayAnswer r))
      putStrLn ""

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
  -- All six, not the three that were here.  The V.32bis rates were
  -- never swept offline at all, which is exactly where a trained
  -- receiver's behaviour at 12000 and 14400 would have shown up.
  forM_ [V32R4800, V32R9600, V32R9600T, V32R7200, V32R12000, V32R14400] $ \r -> do
    let tel s = telephoneChannel s
        axes =
          -- Up to 40, not from 20.  The axis was sized for rates that
          -- work below 20 dB; 128-cross needs more than that before it
          -- can be expected to read anything, so a sweep starting at 20
          -- cannot tell a broken 14400 from a correct one.
          [ ("SNR " ++ show s, tel s) | s <- [40, 34, 30, 26, 22, 20, 16, 14, 12, 10, 8 :: Double] ] ++
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


-- | What each mode survives, end to end, over a call that has to
-- establish itself before it can carry anything.
--
-- This is a harder question than the offline pump sweeps ask.  Those
-- hand a trained receiver a modulated signal; this one has to get the
-- handshake through the same noise, agree a mode, train, and then keep
-- the link up long enough to deliver the text -- and it fails at
-- whichever of those is weakest, which is the number that matters to
-- somebody placing a call.
loopbackSweep :: Maybe String -> IO ()
loopbackSweep want = do
  forM_ (maybe modes pick want) $ \(name, ms, maxT, snrs) -> do
    printf "%-10s" name
    forM_ snrs $ \snr -> do
      let mk s = (loopSnr 8000 ms snr) { lcMaxT = maxT
                                       , lcLine = (lcLine (loopSnr 8000 ms snr)) { chSeed = s } }
          got = loopPassRate trials mk textO textA
      printf "%5.0f:%s" snr (show got ++ "/" ++ show trials)
    putStrLn ""
  putStrLn ""
  putStrLn "SNR in dB, and how many noise realisations came up and carried both texts intact"
  where
    trials = 3
    pick n = [ m | m@(nm, _, _, _) <- modes, nm == n ]
    -- each mode swept around its own knee, because sweeping all of them
    -- over all of it is mostly time spent watching V.32 fail at 2 dB
    modes =
      [ ("bell103", [Bell103], 20, [8, 6, 4, 2, 0])
      , ("v21", [V21], 20, [8, 6, 4, 2, 0])
      , ("v23", [V23], 24, [14, 12, 10, 8, 6])
      , ("bell212a", [Bell212A, Bell103], 20, [12, 10, 8, 6, 4])
      , ("v22", [V22], 20, [12, 10, 8, 6, 4])
      , ("v22bis", [V22bis, V22], 20, [18, 16, 14, 12, 10])
      , ("v32", [V32], 26, [24, 22, 20, 18, 16])
      , ("v32bis", [V32bis], 26, [24, 22, 20, 18, 16])
      ]
    textO = map (fromIntegral . fromEnum) "Hello from the caller, 0123456789 !\r\n"
    textA = map (fromIntegral . fromEnum) "Answerer here; all bytes: \255\0\128 end\r\n"
