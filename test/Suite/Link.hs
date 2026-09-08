-- | Bringing a call up: automode, V.8, the whole modem end to end, and
-- the telnet stream the bytes leave by.
module Suite.Link (handshakeTests, modemTests, telnetTests, v8Tests) where

import qualified Data.ByteString as B
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Handshake
import Modec.Modem
import Modec.Telnet
import Modec.V22
import Modec.V32
import Modec.V32Start
import Modec.V8
import Modec.Standards
import Harness

handshakeTests :: TestTree
handshakeTests = testGroup "handshake simulation"
  [ testCase "the calling modem holds unscrambled binary 1 until it is answered" $
      -- 6.3.1.2: the answering modem sends scrambled binary 1 only once
      -- it has detected the calling modem's unscrambled binary 1.  A
      -- caller that sends its own for a fixed 406 ms and then moves on
      -- talks to an answerer that keys off scrambled ones -- modec used
      -- to be one, so the two faults cancelled -- and is ignored by one
      -- that follows the Recommendation.  A recorded call to a real BBS
      -- sat on unscrambled ones for three seconds and then gave up and
      -- offered V.21 instead.
      forM_ [([V22], "1200"), ([V22bis, V22], "2400")] $ \(modes, what) -> do
        let tr = callerAgainstU11 modes 8
            isU11 c = case c of { TxV22 _ _ TxU11 -> True; _ -> False }
            isScr c = case c of { TxV22 _ _ TxScrambledOnes -> True; _ -> False }
            firstOf p = case [ t | (t, c) <- tr, p c ] of { (t : _) -> Just t; [] -> Nothing }
        case (firstOf isU11, firstOf isScr) of
          (Nothing, _) -> assertFailure (what ++ ": never sent unscrambled binary 1: " ++ show tr)
          (Just u, Just sc) -> assertBool
            (what ++ ": held unscrambled ones only " ++ show (sc - u) ++ " s")
            (sc - u >= 1.0)
          (Just _, Nothing) -> return ()
  -- the tone-only simulation cannot carry V.22, so these cases enable the
  -- FSK modes only; the answerer probes V.21 first, so V.21 wins
  , call "FSK modes both ends -> V.21" fsk fsk V21
  , call "originate V.21 / answer auto" [V21] allStandards V21
  , call "originate Bell 103 / answer auto" [Bell103] allStandards Bell103
  , call "originate auto / answer Bell 103" allStandards [Bell103] Bell103
  , call "originate auto / answer V.21" allStandards [V21] V21
  , call "Bell 103 / Bell 103" [Bell103] [Bell103] Bell103
  , call "V.23 duplex both ends" [V23] [V23] V23
  , testCase "V.23 gives each end the other's channel" $ do
      -- the asymmetry is the point: the caller sends 75 bit/s and
      -- receives 1200, and a link that had it the other way round would
      -- still connect but never decode anything
      assertEqual "originate" (FskLink v23Backward v23Forward) (linkFor Originate V23)
      assertEqual "answer" (FskLink v23Forward v23Backward) (linkFor Answer V23)
  , testCase "answerer respects V.25 timing" $ do
      let (_, a) = simulateCall (defaultHsConfig Originate) (defaultHsConfig Answer) 30 20
          tr = sdTrace a
          ansStart = case [ t | (t, TxTone 2100) <- tr ] of { (t : _) -> t; [] -> -1 }
          ansLen = runLength (== TxTone 2100) tr
          gapLen = runLength (== TxSilence) (dropWhile ((/= TxTone 2100) . snd) tr)
      assertBool ("billing delay " ++ show ansStart) (ansStart >= 1.8 && ansStart <= 2.5)
      assertBool ("ANS duration " ++ show ansLen) (maybe False (\d -> d >= 2.6 && d <= 4.0) ansLen)
      assertBool ("gap " ++ show gapLen) (maybe False (\d -> d >= 0.055 && d <= 0.095) gapLen)
  , testCase "no answer -> caller fails after timeout" $ do
      let cfg = (defaultHsConfig Originate) { hcTimeout = 5 }
          (o, _) = simulateCall cfg (defaultHsConfig Answer) { hcBilling = 100 } 30 8
      assertEqual "status" (HsFailed "timeout") (sdStatus o)
  ]
  where
    fsk = [V21, Bell103]
    call name so sa expect = testCase name $ do
      let (o, a) = simulateCall (withModes so (defaultHsConfig Originate)) (withModes sa (defaultHsConfig Answer)) 30 20
      assertEqual "originate" (HsConnected expect (linkFor Originate expect)) (sdStatus o)
      assertEqual "answer" (HsConnected expect (linkFor Answer expect)) (sdStatus a)

modemTests :: TestTree
modemTests = testGroup "full modem duplex" $
  [ testCase "a V.32bis call: Figure 4, then 9600 bit/s trellis coded, text both ways" $ do
      -- Two whole modems this time, not just the start-up machine: the
      -- V.32 exchange, the rate signals settling on the best rate both
      -- ends offer, the handover to the data pump, and the echo canceller
      -- in the path.
      let cfg r = defaultModemConfig 8000 r [V32]
          (rxO, rxA, evO, evA) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32 (V32Link Originate V32R9600T) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA)
        (case evA of (EvConnected V32 (V32Link Answer V32R9600T) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  ] ++
  -- How loud a reflection a 9600 bit/s call carries, on the same
  -- dispersive path Modec.Echo's own tests use.  The first tap is the
  -- hybrid's own return and the other two its dispersion; the gains run
  -- from a quiet ATA to a badly matched one.
  --
  -- Two things at once.  That the text arrives at all is the call
  -- surviving the echo.  That the return loss is a real number is the
  -- canceller doing something: it was wired in, and switched off, for as
  -- long as it had existed -- the adapt flag hardcoded False, the
  -- reference never fed once the call reached data, the bulk delay the
  -- start-up measures read from the config rather than the state -- and
  -- every one of those failed silently, with the return loss sitting at
  -- exactly 0 dB.
  --
  -- The reported loss rises with the echo rather than staying flat,
  -- which is what it should do: it is measured against everything that
  -- arrived, so a quiet echo leaves little to take out and reads as a
  -- small number even when it is taking all of it out.
  --
  -- One call per gain rather than four in a loop: each is ten-odd
  -- seconds of simulation, and tasty can only schedule what it can see.
  [ testCase ("text survives a hybrid at " ++ show (round (20 * logBase 10 g) :: Int) ++ " dB") $ do
      let cfg r = defaultModemConfig 8000 r [V32]
          path = [(200, g), (203.5, g / 2), (209.2, g / 5)]
          (rxO, rxA, evO, evA, erleO, erleA) =
            modemDuplexEcho path (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32 _ : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA)
        (case evA of (EvConnected V32 _ : _) -> True; _ -> False)
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
      assertBool ("return loss " ++ show (erleO, erleA))
        (erleO > wantErle && erleA > wantErle)
  | (g, wantErle) <- [(0.05, 5), (0.10, 9), (0.20, 14), (0.30, 17 :: Double)] ] ++
  [ testCase "V.8 picks V.32 out of a list, and the start-up follows without a second answer tone" $ do
      -- The path a live V.32 call actually takes.  A modem configured for
      -- V.32 alone goes straight into Figure 4; one configured for V.32
      -- among others runs V.8 first, offers V.32 in its menu (Table 4
      -- item 3), and only on the far end selecting it hands the line to
      -- the V.32 start-up -- which must begin at the answering modem's
      -- AC, because ANSam has already been the answer tone.
      let cfg r = withV8 (defaultModemConfig 8000 r [V32, V22bis, V22])
          withV8 c = c { mcHandshake = (mcHandshake c) { hcV8 = True } }
          (rxO, rxA, evO, evA) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      -- the far end's menu is reported first, and must name V.32
      assertBool ("originate events " ++ show evO)
        (any (\e -> case e of EvV8Menu m -> MV32 `elem` v8Mods m; _ -> False) evO)
      assertBool ("originate events " ++ show evO)
        (EvConnected V32 (V32Link Originate V32R9600T) `elem` evO)
      assertBool ("answer events " ++ show evA)
        (EvConnected V32 (V32Link Answer V32R9600T) `elem` evA)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "a V.32bis call held down to 7200 bit/s, text both ways" $ do
      -- 7200 is V.32bis's addition below 9600, for a line that will not
      -- carry 9600: the rate signal names it in a bit V.32 had reserved,
      -- so a V.32 modem on the other end would simply not see it.
      let cfg role = (defaultModemConfig 8000 role [V32bis]) { mcV32Rates = Just (chosen V32R7200) }
          (rxO, rxA, evO, _) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V32bis (V32Link Originate V32R7200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      -- Both directions exactly, head of the session included.  This
      -- used to arrive behind a dozen bytes of rubbish, which was not
      -- the descrambler coming into step but the receiver acquiring the
      -- data constellation on the carrier loop's tracking gain.
      assertEqual "text from originate to answer" textO rxA
  , testCase "a V.32bis call at 12000 and at 14400" $ do
      -- The two rates V.32bis adds above 9600.  Both carry a call now:
      -- what had stopped 14400 was never the modulation but the gate in
      -- front of the terminal, which measured the slicer error against
      -- the distance between neighbouring points and so held a trellis
      -- rate to the geometry of a constellation it is not decoded by.
      let call r = let cfg role = (defaultModemConfig 8000 role [V32bis]) { mcV32Rates = Just (chosen r) }
                   in modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      -- 12000 carries a whole call both ways.
      let (rxO, rxA, evO, _) = call V32R12000
      assertBool ("12000 originate events " ++ show evO)
        (case evO of (EvConnected V32bis (V32Link Originate V32R12000) : _) -> True; _ -> False)
      assertEqual "12000: text from answer to originate" textA rxO
      assertEqual "12000: text from originate to answer" textO rxA
      -- 14400 carries both directions too, with a few bytes of rubbish
      -- around them: it is the top of what this receiver can read, and
      -- the residue is what the gate lets through while the decision
      -- error is still settling.  Both texts whole is the assertion;
      -- tightening it to exact equality is the next piece of work, not
      -- a reason to claim the rate does not work.
      let (rxO', rxA', evO', _) = call V32R14400
      assertBool ("14400 originate events " ++ show evO')
        (case evO' of (EvConnected V32bis (V32Link Originate V32R14400) : _) -> True; _ -> False)
      assertBool ("14400: text from answer to originate: " ++ show rxO')
        (textA `isInfixOf` rxO')
      assertBool ("14400: text from originate to answer: " ++ show rxA')
        (textO `isInfixOf` rxA')
  , testCase "a V.32 call wrecked mid-session retrains and carries on" $ do
      -- 5.5.  Half a second of noise loud enough that no receiver could
      -- read through it, on one direction only, and then the line is
      -- fine again.  Before this there was no way out of data mode
      -- except the end of the call: a link that went bad stayed bad and
      -- went on handing up whatever it could make of the noise.
      let cfg role = defaultModemConfig 8000 role [V32]
      forM_ [(False, "the answering modem goes deaf"), (True, "the calling modem does")] $ \(wreckCaller, what) -> do
       let (rxO, rxA, evO, evA) =
             modemDuplexDisturb (cfg Originate) (cfg Answer) 30 textO textA wreckCaller 14 14.5
       assertBool (what ++ ": events " ++ show (evO ++ evA))
         (any (\e -> case e of EvRetrain _ -> True; _ -> False) (evO ++ evA))
       -- and the session is still there afterwards: the second text is
       -- sent only once the line has been clean again for three seconds,
       -- so it can only arrive through a link that put itself back.
       -- Both ways round, because only the deaf end asks: the other has
       -- to recognise the request from the tone alone, and the two ends
       -- watch for different tones.
       assertBool (what ++ ": answer heard " ++ show rxA) (textO `isInfixOf` rxA)
       assertBool (what ++ ": originate heard " ++ show rxO) (textA `isInfixOf` rxO)
  , testCase "a V.32bis call that keeps breaking falls to a rate it can hold" $ do
      -- The other half of 5.5: a retrain that goes back in at the rate
      -- which had just stopped working negotiates its way straight back
      -- to it.  A modem asking for a retrain therefore narrows its own
      -- offer on the way in, and a line that keeps breaking walks down
      -- the rate list instead of hammering the top of it.
      let cfg role = (defaultModemConfig 8000 role [V32bis])
                       { mcV32Rates = Just v32bisRates }
          (_, _, evO, evA) =
            modemDuplexDisturb (cfg Originate) (cfg Answer) 30 textO textA False 14 15
          started = [ r | EvConnected _ (V32Link _ r) <- evO ++ evA ]
          ended = [ r | EvRate r <- evO ++ evA ]
      assertBool ("no rate change in " ++ show (evO ++ evA)) (not (null ended))
      case (started, ended) of
        (s : _, e : _) -> assertBool
          ("started at " ++ show s ++ " and moved to " ++ show e ++ ", which is not lower")
          (rateBitRate e < rateBitRate s)
        _ -> assertFailure ("no connection and rate change: " ++ show (evO ++ evA))
  , testCase "V.32 both ends with no V.8 at all, down the classic ladder" $ do
      -- Annex A.2.2 is an integral part of the Recommendation and this
      -- is the ladder it defines: the answering modem sends ANS while
      -- listening for the calling modem's carrier state A, then USB1 for
      -- Ta = 1500 +/- 50 ms, and only then the alternating pair of
      -- 5.4.2.  V.32 is six years older than V.8 and needs none of it.
      --
      -- Before this the answering side had no V.32 rung at all -- it
      -- could only be reached through a CM/JM exchange -- so two modecs
      -- that both offered V.32 settled on V.22bis at a sixth of the
      -- speed.
      -- Held at 9600 trellis so the assertion is about the ladder and
      -- not about rate selection: unpinned this reaches 14400, which is
      -- the top of what the receiver carries and retrains its way down
      -- again, and the text queued at the moment of connection goes with
      -- it.  Which rates get negotiated is what the 12000 case above is
      -- for.
      let cfg role = (defaultModemConfig 8000 role [V32bis, V32, V22bis])
                       { mcV32Rates = Just (chosen V32R9600T) }
          (rxO, rxA, evO, evA) = modemDuplex (cfg Originate) (cfg Answer) 30 textO textA 45
      -- V32 rather than V32bis because pinning to one rate clears B4,
      -- which is how Note 1 has a modem say it is not speaking V.32bis
      assertBool ("originate events " ++ show evO)
        (any (\e -> case e of EvConnected s (V32Link _ V32R9600T) -> isV32 s; _ -> False) evO)
      assertBool ("answer events " ++ show evA)
        (any (\e -> case e of EvConnected s (V32Link _ V32R9600T) -> isV32 s; _ -> False) evA)
      assertBool ("answer heard " ++ show rxA) (textO `isInfixOf` rxA)
      assertBool ("originate heard " ++ show rxO) (textA `isInfixOf` rxO)
  , testCase "an answerer offering V.32 still falls back to a caller without it" $ do
      -- The other half, and the reason the offer is bounded.  A modem
      -- that offers the alternating pair is guessing: for as long as it
      -- holds it, it is transmitting 600 and 3000 Hz that no V.22, V.21
      -- or Bell caller understands.  It gives up after hcV32Offer and
      -- comes back to the rung after the V.22 probe with the offer spent,
      -- so the rest of the ladder is still reachable.
      let answerer = defaultModemConfig 8000 Answer [V32bis, V32, V22bis, V22, V21, Bell103]
      forM_ [ ([V22bis, V22], "V22bis", 45)
            , ([V21], "V21", 45) ] $ \(callerModes, want, budget) -> do
        let (rxO, rxA, evO, _) =
              modemDuplex (defaultModemConfig 8000 Originate callerModes) answerer
                          30 textO textA budget
            got = [ show s | EvConnected s _ <- evO ]
        assertBool (want ++ ": originate events " ++ show evO) (got == [want])
        assertBool (want ++ ": answer heard " ++ show rxA) (textO `isInfixOf` rxA)
        assertBool (want ++ ": originate heard " ++ show rxO) (textA `isInfixOf` rxO)
  , testCase "a V.32 call notices when the far end stops" $ do
      -- It could not.  The carrier watchdog was handed "the decision
      -- error is under 1e3", which is an EWMA of a squared error on a
      -- unit-power constellation: it settles near 0.005 and its own
      -- give-up threshold is 0.4, so the test was true for ever and
      -- msLost never grew.  The far end could hang up and the modem
      -- would sit there transmitting into a dead line until the process
      -- was killed, reporting nothing.  V.22 has always had a real
      -- energy test; V.32 has one now.
      let cfg role = defaultModemConfig 8000 role [V32]
          (_, _, evO, _) = modemDuplexCut (cfg Originate) (cfg Answer) 30 textO textA 30 12
      assertBool ("originate events " ++ show evO)
        (any (\e -> case e of EvConnected V32 _ -> True; _ -> False) evO)
      assertBool ("expected a drop after the line went quiet, got " ++ show evO)
        (EvDropped `elem` evO)
  , testCase "automode call -> V.22bis at 2400 bit/s" $ do
      let (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate allStandards) (defaultModemConfig 8000 Answer allStandards) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link LowChannel HighChannel R2400) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected V22bis (V22Link HighChannel LowChannel R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22-only caller (no S1) -> 1200 bit/s" $ do
      let noS1 = defaultModemConfig 8000 Originate [V22]   -- V.22 only: never offers S1
          (rxO, rxA, evO, _) = modemDuplex noS1 (defaultModemConfig 8000 Answer allStandards) 30 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22 (V22Link _ _ R1200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "originate fixed V.21, answer automode -> V.21" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V21]) (defaultModemConfig 8000 Answer allStandards) 30 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V21 _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.23 duplex fixed both sides -> 1200 down, 75 up" $ do
      -- the caller's line is 37 characters at 75 bit/s, five seconds of
      -- transmission on its own, so this call runs longer than the
      -- symmetric ones do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V23]) (defaultModemConfig 8000 Answer [V23]) 30 textO textA 20
      assertBool ("originate events " ++ show evO)
        (case evO of (EvConnected V23 (FskLink tx rx) : _) -> fskBaud tx == 75 && fskBaud rx == 1200; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 fixed both sides, 20 dB -> 2400 bit/s" $ do
      let (rxO, rxA, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [V22bis, V22]) (defaultModemConfig 8000 Answer [V22bis, V22]) 20 textO textA 16
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected V22bis (V22Link _ _ R2400) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "V.22 no handshake, 15 dB" $ do
      let cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 15 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  , testCase "a carrier that stops delivers no junk behind it" $ do
      -- A far end that hangs up mid-call used to cost about a hundred
      -- characters of noise: the receiver kept framing its own decisions
      -- while the carrier decayed, and the DTE had no way to tell those
      -- from the banner that came before them.  The receiver knows,
      -- though -- its decision error goes from about 0.01 to past 100 --
      -- so it now stops handing bytes over.
      let cfg r = (defaultModemConfig 8000 r [V22]) { mcNoHandshake = True }
          textA = map (fromIntegral . fromEnum) "BANNER\r\n"
          (rxO, _, _, _) = modemDuplexCut (cfg Originate) (cfg Answer) 30 [] textA 12 6
      assertEqual "the banner, and nothing after it" textA rxO
  , testCase "Bell 212A both ends -> 1200 bit/s DPSK, text both ways" $ do
      let bell = [Bell212A, Bell103]
          (rxO, rxA, evO, evA) = modemDuplex (defaultModemConfig 8000 Originate bell) (defaultModemConfig 8000 Answer bell) 25 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected Bell212A (V22Link LowChannel HighChannel R1200) : _) -> True; _ -> False)
      assertBool ("answer events " ++ show evA) (case evA of (EvConnected Bell212A (V22Link HighChannel LowChannel R1200) : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
      assertEqual "text from originate to answer" textO rxA
  , testCase "Bell 103 caller, Bell 212A capable answerer -> Bell 103" $ do
      let (rxO, _, evO, _) = modemDuplex (defaultModemConfig 8000 Originate [Bell103]) (defaultModemConfig 8000 Answer [Bell212A, Bell103]) 25 textO textA 14
      assertBool ("originate events " ++ show evO) (case evO of (EvConnected Bell103 _ : _) -> True; _ -> False)
      assertEqual "text from answer to originate" textA rxO
  , testCase "a Bell-only modem never sends the ITU answer tone" $ do
      let (_, a) = simulateCall (defaultHsConfig Originate) { hcModes = [Bell103] } (defaultHsConfig Answer) { hcModes = [Bell212A, Bell103] } 30 12
      assertBool ("answer trace " ++ show (sdTrace a)) (not (any (\(_, c) -> c == TxTone 2100) (sdTrace a)))
  , testCase "Bell 103 fixed, no handshake, 20 dB" $ do
      let cfg r = (defaultModemConfig 8000 r [Bell103]) { mcNoHandshake = True }
          (rxO, rxA, _, _) = modemDuplex (cfg Originate) (cfg Answer) 20 textO textA 6
      assertEqual "answer to originate" textA rxO
      assertEqual "originate to answer" textO rxA
  ]
  where
    textO = map (fromIntegral . fromEnum) "Hello from the caller, 0123456789 !\r\n"
    textA = map (fromIntegral . fromEnum) "Answerer here; all bytes: \255\0\128 end\r\n"

telnetTests :: TestTree
telnetTests = testGroup "telnet codec"
  [ testCase "negotiation and IAC escapes" $ do
      let (st, _) = telnetHello telnetInit
          input = B.pack ([104, 105, iac, doOpt, optBinary, iac, iac, iac, doOpt, 24, iac, will, optSga, iac, 250, 24, 1, iac, 240, 33])
          (_, payload, reply) = telnetDecode st input
      assertEqual "payload" (B.pack [104, 105, 255, 33]) payload
      -- BINARY and SGA were already offered in the hello, so only the refusal of TERMINAL-TYPE (24) comes back
      assertEqual "reply" (B.pack [iac, wont, 24]) reply
  , testCase "fresh state answers DO/WILL for supported options" $ do
      let (_, _, reply) = telnetDecode telnetInit (B.pack [iac, doOpt, optBinary, iac, will, optBinary])
      assertEqual "reply" (B.pack [iac, will, optBinary, iac, doOpt, optBinary]) reply
  , testCase "encode doubles 0xFF" $
      assertEqual "encoded" (B.pack [1, 255, 255, 2]) (telnetEncode (B.pack [1, 255, 2]))
  ]

v8Tests :: TestTree
v8Tests = testGroup "V.8 menus and ANSam"
  [ testCase "a CM carries the preamble, call function and modulation octets" $ do
      let bits = sequenceBits SeqCM ourMenu
      assertEqual "ten ONEs then the CM sync" (map (== '1') "11111111110000001111") (take 20 bits)
      -- four octets: call function and three modulation octets, each
      -- with a start and a stop bit
      assertEqual "length" (20 + 4 * 10) (length bits)
      assertEqual "every octet framed" (replicate 4 (False, True))
        [ (o !! 0, o !! 9) | i <- [0 .. 3], let o = take 10 (drop (20 + 10 * i) bits) ]
  , testCase "CM round-trips through the receiver" $ do
      -- a sequence is emitted when its framing stops holding, which on
      -- the line is the ten ONEs opening whatever comes next
      let (_, evs) = v8RxBits v8RxInit
            (sequenceBits SeqCM ourMenu ++ sequenceBits SeqCM ourMenu ++ replicate 10 True)
      case evs of
        [V8Sequence SeqCM a, V8Sequence SeqCM b] -> do
          assertEqual "call function" (Just CfData) (v8Call a)
          assertEqual "modulations" [MV22, MV21] (v8Mods a)
          assertEqual "three modulation octets" 3 (v8ModOctets a)
          assertEqual "both alike" a b
        _ -> assertFailure ("expected two CM sequences, got " ++ show (length evs))
  , testCase "a menu with every option set survives the round trip" $ do
      let full = emptyMenu { v8Call = Just CfFaxFromCaller
                           , v8Mods = [MV34Duplex, MV32, MV22, MV27ter, MV23Duplex, MV21]
                           , v8Lapm = True
                           , v8Pcm = Just (True, False, False)
                           , v8Access = Just (False, False, True) }
          (_, evs) = v8RxBits v8RxInit (sequenceBits SeqCM full ++ replicate 10 True)
      case evs of
        [V8Sequence _ m] -> do
          assertEqual "call function" (Just CfFaxFromCaller) (v8Call m)
          assertEqual "modulations" [MV34Duplex, MV32, MV22, MV27ter, MV23Duplex, MV21] (v8Mods m)
          assertEqual "LAPM" True (v8Lapm m)
          assertEqual "PCM" (Just (True, False, False)) (v8Pcm m)
          assertEqual "access" (Just (False, False, True)) (v8Access m)
        _ -> assertFailure ("expected one sequence, got " ++ show evs)
  , testCase "no HDLC flag can appear in a signal" $ do
      -- the fixed bits in 5.1 and 5.2 exist so that a T.30 receiver on
      -- the same V.21 channel never mistakes JM for a frame
      let menus = [ emptyMenu { v8Call = Just cf, v8Mods = ms }
                  | cf <- [minBound .. maxBound]
                  , ms <- [[], [MV21], [MV22, MV21], [minBound .. maxBound]] ]
          flag = map (== '1') "01111110"
          hasFlag bs = any (\i -> take 8 (drop i bs) == flag) [0 .. length bs - 8]
      assertEqual "flags" [] [ describeMenu m | m <- menus, hasFlag (sequenceBits SeqCM m) ]
  , testCase "CJ is three zero octets and is recognised" $ do
      assertEqual "30 bits" 30 (length cjBits)
      let (_, evs) = v8RxBits v8RxInit (sequenceBits SeqCM ourMenu ++ cjBits)
      assertBool "CJ seen" (V8CJ `elem` evs)
  , testCase "the lowest item number wins" $ do
      -- 7.4: of the modes in common, the one with the lowest item number
      let far = emptyMenu { v8Mods = [MV34Duplex, MV32, MV22, MV21] }
      assertEqual "V.22 beats V.21" (Just MV22) (commonModulation ourMenu far)
      assertEqual "only V.21 in common" (Just MV21)
        (commonModulation ourMenu far { v8Mods = [MV34Duplex, MV21] })
      assertEqual "nothing in common" Nothing
        (commonModulation ourMenu far { v8Mods = [MV34Duplex, MV32] })
      assertEqual "item order" [1, 3, 4, 12] (map modItem [MV34Duplex, MV32, MV22, MV21])
  , testCase "a JM saying nothing in common still matches the CM octet count" $ do
      -- 8.2.3: same number of modulation octets, all zeros
      let jm = emptyMenu { v8Call = Just CfData, v8ModOctets = 3 }
          (_, evs) = v8RxBits v8RxInit (sequenceBits SeqJM jm ++ replicate 10 True)
      case evs of
        [V8Sequence _ m] -> do
          assertEqual "no modulations" [] (v8Mods m)
          assertEqual "three octets all the same" 3 (v8ModOctets m)
        _ -> assertFailure "expected one sequence"
  , testCase "ANSam is told apart from a plain answer tone" $ do
      let fs = 8000 :: Double
          tone f = VS.generate (round (fs * 3)) (\i -> 0.2 * sin (2 * pi * f * fromIntegral i / fs))
          saw x = snd (ansamBlock (ansamInit fs) x)
      assertBool "ANSam" (saw (ansamSignal fs 0.2 3 False))
      assertBool "ANSam with phase reversals" (saw (ansamSignal fs 0.2 3 True))
      assertBool "plain ANS is not ANSam" (not (saw (tone 2100)))
      assertBool "the Bell answer tone is not ANSam" (not (saw (tone 2225)))
      assertBool "silence is not ANSam" (not (saw (VS.replicate (round (fs * 3)) 0)))
  , testCase "a modem carrier does not read as ANSam" $ do
      -- any fluctuating envelope has some 15 Hz in it, so the detector
      -- has to insist that 2100 Hz is actually what is on the line
      let fs = 8000 :: Double
          noisy = VS.generate (round (fs * 3)) (\i ->
            let t = fromIntegral i / fs
            in 0.2 * sin (2 * pi * 1200 * t) * (1 + 0.3 * sin (2 * pi * 15 * t)))
      assertBool "1200 Hz carrier" (not (snd (ansamBlock (ansamInit fs) noisy)))
  ]
  where
    -- what modec can offer: V.8 has no codepoint for the Bell modes
    ourMenu = emptyMenu { v8Call = Just CfData, v8Mods = [MV22, MV21] }
