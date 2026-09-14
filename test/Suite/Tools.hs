-- | The parts around the modem: Hayes commands, baresip, PipeWire.
module Suite.Tools (hayesTests, baresipTests, pipewireTests, hermeticTests) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Link
import Modec.Standards
import Modec.Handshake
import Modec.Hayes
import Modec.Dtmf
import Modec.Baresip
import Modec.Json
import Modec.Pipewire
import qualified Modec.V32 as V32

h0 :: HayesState
h0 = hayesInit defaultProfile

hayesTests :: TestTree
hayesTests = testGroup "Hayes AT interpreter"
  [ testCase "AT, ATE0, ATI, S0" $ do
      let (s1, b1, _, a1) = hayesInput 0 h0 (BC.pack "AT\r")
      assertEqual "OK" "AT\r\r\nOK\r\n" (BC.unpack b1)
      assertEqual "no actions" [] a1
      let (s2, b2, _, _) = hayesInput 0.1 s1 (BC.pack "ATE0\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b2)
      let (s3, b3, _, _) = hayesInput 0.2 s2 (BC.pack "ATS0=2\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b3)
      assertBool "auto-answer set" (hayesAutoAnswer s3)
      let (_, b4, _, _) = hayesInput 0.3 s3 (BC.pack "ATS0?\r")
      assertBool "002" ("002" `isInfixOf` BC.unpack b4)
      let (_, b5, _, _) = hayesInput 0.4 s3 (BC.pack "ATY9\r")
      assertBool "Y accepted" ("OK" `isInfixOf` BC.unpack b5)
      let (_, b6, _, _) = hayesInput 0.4 s3 (BC.pack "AT#UD\r")
      assertBool "ERROR" ("ERROR" `isInfixOf` BC.unpack b6)
      let (_, b7, _, _) = hayesInput 0.5 s3 (BC.pack "ATV0\r")
      assertEqual "numeric OK" "0\r" (BC.unpack b7)
  , testCase "ATDT dials, CONNECT goes online, data passes, +++ escapes, ATH hangs up" $ do
      let (s1, _, _, a1) = hayesInput 0 h0 (BC.pack "atdt 555-1234\r")
      assertEqual "dial, as typed" [ActDial "t555-1234"] a1
      let (s2, b2, _) = hayesEvent s1 (EvConnect 2400)
      assertBool "CONNECT 2400" ("CONNECT 2400" `isInfixOf` BC.unpack b2)
      assertBool "online" (hayesOnline s2)
      let (s3, _, fwd3, _) = hayesInput 1 s2 (BC.pack "hello")
      assertEqual "data forwarded" "hello" (BC.unpack fwd3)
      -- escape needs a second of silence before and after
      let (s4, _, fwd4, _) = hayesInput 2.5 s3 (BC.pack "+++")
      assertEqual "pluses withheld" "" (BC.unpack fwd4)
      let (s5, b5) = hayesTick 3.6 s4
      assertBool "OK after escape" ("OK" `isInfixOf` BC.unpack b5)
      assertBool "command mode" (not (hayesOnline s5))
      let (s6, b6, _, a6) = hayesInput 4 s5 (BC.pack "ATO\r")
      assertEqual "online again" [ActOnline] a6
      assertBool "ATO says CONNECT again" ("CONNECT 2400" `isInfixOf` BC.unpack b6)
      assertBool "online" (hayesOnline s6)
      let (s7, _, fwd7, _) = hayesInput 5 s6 (BC.pack "+x")
      assertEqual "lone plus is data" "+x" (BC.unpack fwd7)
      let (s8, _, _, _) = hayesInput 7 s7 (BC.pack "+++")
          (s9, _) = hayesTick 8.1 s8
          (_, b10, _, a10) = hayesInput 8.2 s9 (BC.pack "ATH\r")
      assertEqual "hangup" [ActHangup] a10
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b10)
      let (_, _, _, a11) = hayesInput 9 s9 (BC.pack "ATH1\r")
      assertEqual "H1 is not a hang-up" [] a11
  , testCase "S0 counts rings, and the count lapses" $ do
      let (s1, _, _, _) = hayesInput 0 h0 (BC.pack "ATS0=3\r")
          (s2, o2, a2) = hayesEvent s1 EvRing
      assertBool "RING" ("RING" `isInfixOf` BC.unpack o2)
      assertEqual "first ring" [] a2
      let (s3, _) = hayesTick 2 s2
          (s4, _, a4) = hayesEvent s3 EvRing
      assertEqual "second ring" [] a4
      let (_, b5, _, _) = hayesInput 2.5 s4 (BC.pack "ATS1?\r")
      assertBool "S1 is 2" ("002" `isInfixOf` BC.unpack b5)
      let (s6, _) = hayesTick 4 s4
          (_, _, a6) = hayesEvent s6 EvRing
      assertEqual "third ring answers" [ActAnswer] a6
      -- eight quiet seconds and the count starts again
      let (s7, _) = hayesTick 11 s4
          (_, _, a7) = hayesEvent s7 EvRing
      assertEqual "lapsed" [] a7
      -- and a terminal that is not there is not answered for
      let (_, _, a8) = hayesEvent (hayesDtr False s6) EvRing
      assertEqual "DTR off" [] a8
      let (d0, _, _, _) = hayesInput 4 s6 (BC.pack "AT&D0\r")
          (_, _, a9) = hayesEvent (hayesDtr False d0) EvRing
      assertEqual "&D0 answers anyway" [ActAnswer] a9
  , testCase "dial strings keep their case; SIP addresses are told apart" $ do
      let (_, _, _, a1) = hayesInput 0 h0 (BC.pack "ATDsip:Joe@Host.example\r")
      assertEqual "as typed" [ActDial "sip:Joe@Host.example"] a1
      assertEqual "T before a SIP number" (DialSip "sip:1001@192.168.30.105:22097") (dialTarget "T1001@192.168.30.105:22097")
      assertEqual "a user part starting with P" (DialSip "sip:pbx@example.org") (dialTarget "pbx@example.org")
      assertEqual "scheme, any case" (DialSip "sip:tom@h") (dialTarget "TSIP:tom@h")
      assertEqual "a number" (DialNumber "555-1234") (dialTarget "T555-1234")
      assertEqual "dialUri strips the T" "sip:1001@h" (dialUri "d" "T1001@h")
      assertEqual "dialUri numbers" "sip:5551234@d" (dialUri "d" "t555-1234")
  , testCase "A/, &Z, DS=n and DL" $ do
      let (s1, _, _, a1) = hayesInput 0 h0 (BC.pack "ATDT123\r")
      assertEqual "dial" [ActDial "T123"] a1
      let (s2, _, _, a2) = hayesInput 1 s1 (BC.pack "A/")
      assertEqual "A/ repeats it" [ActDial "T123"] a2
      let (s3, b3, _, _) = hayesInput 2 s2 (BC.pack "AT&Z1=sip:bbs@example.org\r")
      assertBool "stored" ("OK" `isInfixOf` BC.unpack b3)
      let (s4, _, _, a4) = hayesInput 3 s3 (BC.pack "ATDS=1\r")
      assertEqual "DS=1" [ActDial "sip:bbs@example.org"] a4
      let (_, _, _, a5) = hayesInput 4 s4 (BC.pack "ATDL\r")
      assertEqual "DL" [ActDial "sip:bbs@example.org"] a5
      let (_, b6, _, a6) = hayesInput 4 s4 (BC.pack "ATDS=2\r")
      assertEqual "empty slot" [] a6
      assertBool "ERROR" ("ERROR" `isInfixOf` BC.unpack b6)
  , testCase "+MS, B and N choose the modes" $ do
      let (s1, b1, _, _) = hayesInput 0 h0 (BC.pack "AT+MS=V22B,1,1200,2400\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b1)
      assertEqual "V.22bis and down, 1200 and up" [V22bis, V22, Bell212A] (hayesModes (hayesProfile s1))
      let (_, b2, _, _) = hayesInput 0 s1 (BC.pack "AT+MS?\r")
      assertBool "reads back" ("+MS: V22B,1,1200,2400,1200,2400" `isInfixOf` BC.unpack b2)
      let (s3, _, _, _) = hayesInput 0 s1 (BC.pack "ATB1\r")
      assertEqual "Bell first" [V22bis, Bell212A, V22] (hayesModes (hayesProfile s3))
      let (s4, _, _, _) = hayesInput 0 s1 (BC.pack "ATN0\r")
      assertEqual "no automode" [V22bis] (hayesModes (hayesProfile s4))
      let (s5, _, _, _) = hayesInput 0 h0 (BC.pack "at+ms=v32b,0,4800,9600\r")
      assertEqual "V.32bis only" [V32bis] (hayesModes (hayesProfile s5))
      assertEqual "range" (V32Range 4800 9600) (hpV32 (hayesProfile s5))
      let (s6, _, _, _) = hayesInput 0 h0 (BC.pack "AT+MS=B103\r")
      assertEqual "Bell 103" [Bell103] (hayesModes (hayesProfile s6))
      forM_ ["AT+MS=V34\r", "AT+MS=V32,1,300,200\r", "AT+MS=V32,0,300,2400\r", "AT+XYZ\r"] $ \c -> do
        let (_, b, _, _) = hayesInput 0 h0 (BC.pack c)
        assertBool ("ERROR for " ++ c) ("ERROR" `isInfixOf` BC.unpack b)
      let (_, b7, _, _) = hayesInput 0 h0 (BC.pack "AT+MS=?;+GCAP\r")
      assertBool "range query and a second command" ("(B103" `isInfixOf` BC.unpack b7 && "+GCAP: +MS" `isInfixOf` BC.unpack b7)
  , testCase "S12 sets the escape guard, S2 the character" $ do
      let (s1, _, _, _) = hayesInput 0 h0 (BC.pack "ATS12=100\r")
          (s2, _, _) = hayesEvent s1 (EvConnect 1200)
          (s3, _, _, _) = hayesInput 1 s2 (BC.pack "x")
          (_, _, fwd4, _) = hayesInput 2.5 s3 (BC.pack "+++")
      assertEqual "too soon: data" "+++" (BC.unpack fwd4)
      let (s5, _, fwd5, _) = hayesInput 5 s3 (BC.pack "+++")
      assertEqual "withheld" "" (BC.unpack fwd5)
      assertEqual "still online at 1.1 s" "" (BC.unpack (snd (hayesTick 6.1 s5)))
      assertBool "OK at 2 s" ("OK" `isInfixOf` BC.unpack (snd (hayesTick 7.1 s5)))
      let (t1, _, _, _) = hayesInput 0 h0 (BC.pack "ATS2=33\r")
          (t2, _, _) = hayesEvent t1 (EvConnect 1200)
          (t3, _, fwd, _) = hayesInput 3 t2 (BC.pack "!!!")
      assertEqual "! withheld" "" (BC.unpack fwd)
      assertBool "escaped on !!!" ("OK" `isInfixOf` BC.unpack (snd (hayesTick 4.1 t3)))
  , testCase "&W, ATZ, &F and &V" $ do
      let (s1, _, _, _) = hayesInput 0 h0 (BC.pack "ATS0=3S7=30&D3&W\r")
          (s2, _, _, _) = hayesInput 0 s1 (BC.pack "ATS0=0\r")
          (_, v, _, _) = hayesInput 0 s1 (BC.pack "AT&V\r")
      assertBool "&V shows S0" ("S00:003" `isInfixOf` BC.unpack v)
      assertBool "&V shows S7" ("S07:030" `isInfixOf` BC.unpack v)
      assertBool "&V shows &D3" ("&D3" `isInfixOf` BC.unpack v)
      let (s3, _, _, a3) = hayesInput 0 s2 (BC.pack "ATZ\r")
      assertEqual "Z hangs up" [ActHangup] a3
      assertEqual "Z restores the stored S0" 3 (hayesReg (hayesProfile s3) 0)
      let (s4, _, _, _) = hayesInput 0 s3 (BC.pack "AT&F\r")
      assertEqual "&F restores the factory S0" 0 (hayesReg (hayesProfile s4) 0)
      let (_, b5, _, _) = hayesInput 0 h0 (BC.pack "ATS300=1\r")
      assertBool "no S300" ("ERROR" `isInfixOf` BC.unpack b5)
      let custom = hayesInit defaultProfile { hpModes = [V21], hpX = 1 }
          (s6, _, _, _) = hayesInput 0 custom (BC.pack "ATX4+MS=V22\r")
          (s7, _, _, _) = hayesInput 0 s6 (BC.pack "AT&F\r")
      assertEqual "factory is the profile it started with" ([V21], 1) (hpModes (hayesProfile s7), hpX (hayesProfile s7))
  , testCase "X sets the result codes; \\N sets error control" $ do
      let (x0, _, _, _) = hayesInput 0 h0 (BC.pack "ATX0\r")
          (_, c0, _) = hayesEvent x0 (EvConnect 2400)
      assertEqual "bare CONNECT" "\r\nCONNECT\r\n" (BC.unpack c0)
      let (x2, _, _, _) = hayesInput 0 h0 (BC.pack "ATX2\r")
          (_, b2, _) = hayesEvent x2 EvBusy
      assertBool "X2: NO CARRIER" ("NO CARRIER" `isInfixOf` BC.unpack b2)
      assertBool "X2 does not detect busy" (not (hayesDetectsBusy x2))
      let (_, b4, _) = hayesEvent h0 EvBusy
      assertBool "X4: BUSY" ("BUSY" `isInfixOf` BC.unpack b4)
      let (n3, _, _, _) = hayesInput 0 h0 (BC.pack "AT\\N3\r")
      assertEqual "MNP on" (Just 4) (hpMnp (hayesProfile n3))
      let (n0, _, _, _) = hayesInput 0 n3 (BC.pack "AT\\N0\r")
      assertEqual "MNP off" Nothing (hpMnp (hayesProfile n0))
      let (_, bl, _, _) = hayesInput 0 h0 (BC.pack "AT\\N4\r")
      assertBool "no LAPM" ("ERROR" `isInfixOf` BC.unpack bl)
  , testCase "DTR drop follows &D" $ do
      let (on, _, _) = hayesEvent h0 (EvConnect 2400)
          (d2, _, a2) = hayesDtrDrop on
      assertEqual "&D2 hangs up" [ActHangup] a2
      assertBool "command mode" (not (hayesOnline d2))
      let (s0, _, _, _) = hayesInput 0 h0 (BC.pack "AT&D0\r")
          (on0, _, _) = hayesEvent s0 (EvConnect 2400)
          (d0, _, a0) = hayesDtrDrop on0
      assertEqual "&D0 ignores it" [] a0
      assertBool "still online" (hayesOnline d0)
      let (s1, _, _, _) = hayesInput 0 h0 (BC.pack "AT&D1\r")
          (on1, _, _) = hayesEvent s1 (EvConnect 2400)
          (d1, _, a1) = hayesDtrDrop on1
      assertEqual "&D1 does not hang up" [] a1
      assertBool "but leaves data mode" (not (hayesOnline d1))
  , testCase "phone book" $ do
      let book = parsePhonebook "# BBSes\n\n5551234  sip:bbs@example.org\n 1001 1001@192.168.30.105:22097 trailing\n"
      assertEqual "entries" [("5551234", "sip:bbs@example.org"), ("1001", "1001@192.168.30.105:22097")] book
      assertEqual "digits match" (Just "sip:bbs@example.org") (phonebookLookup book "T555-1234")
      assertEqual "unknown" Nothing (phonebookLookup book "5550000")
      assertEqual "a SIP address is not looked up" Nothing (phonebookLookup book "1001@elsewhere")
  , testCase "V.32 rate ranges" $ do
      let r = V32.ratesBetween 7200 12000 V32.v32bisRates
      assertEqual "in" (True, True, True) (V32.rsCan7200 r, V32.rsCan9600 r, V32.rsCan12000 r)
      assertEqual "out" (False, False) (V32.rsCan4800 r, V32.rsCan14400 r)
      assertBool "still V.32bis" (V32.rateSeqV32bis r)
      assertEqual "a pin" (V32.chosenRate V32R14400) (V32.ratesBetween 14400 14400 (V32.chosenRate V32R14400))
  , testCase "DTMF pairs and dial signal length" $ do
      assertEqual "5" (Just (770, 1336)) (dtmfPair '5')
      assertEqual "#" (Just (941, 1477)) (dtmfPair '#')
      assertEqual "length" (round (8000 * (3 * 0.16 + 1)) :: Int) (VS.length (dtmfDialSignal 8000 0.3 "1,23"))
  ]

baresipTests :: TestTree
baresipTests = testGroup "baresip control protocol and SIP line"
  [ testCase "netstrings" $ do
      assertEqual "encode" "5:hello," (BC.unpack (netstringEncode (BC.pack "hello")))
      let (msgs, rest) = netstringDecode (BC.pack "2:ab,3:cde,7:incompl")
      assertEqual "decoded" ["ab", "cde"] (map BC.unpack msgs)
      assertEqual "rest" "7:incompl" (BC.unpack rest)
  , testCase "JSON parse and encode" $ do
      let ev = BC.pack "{\"event\":true,\"class\":\"call\",\"type\":\"CALL_CLOSED\",\"param\":\"Connection reset by peer\",\"peeruri\":\"sip:bob@biloxi.com\",\"n\":-1.5}"
      assertEqual "event" (Just (BsEvent "call" "CALL_CLOSED" "Connection reset by peer" [("peeruri", "sip:bob@biloxi.com")])) (decodeBsMessage ev)
      assertEqual "response" (Just (BsResponse True "" "t1")) (decodeBsMessage (BC.pack "{\"response\":true,\"ok\":true,\"data\":\"\",\"token\":\"t1\"}"))
      assertEqual "command" "53:{\"command\":\"dial\",\"params\":\"sip:1@x.org\",\"token\":\"a\"}," (BC.unpack (commandJson "dial" "sip:1@x.org" "a"))
      assertEqual "roundtrip" (Just (JObj [("a", JStr "x\"y"), ("b", JArr [JNum 1, JBool False, JNull])])) (jsonParse (jsonEncode (JObj [("a", JStr "x\"y"), ("b", JArr [JNum 1, JBool False, JNull])])))
  , testCase "SIP line: dial, established, closed" $ do
      let l0 = sipLineInit "sip.example.org"
          (l1, a1) = sipLineHayes l0 (ActDial "T555-1234")
      assertEqual "dial" [SipCommand "dial" "sip:5551234@sip.example.org"] a1
      let (l2, a2) = sipLineEvent 1 l1 (BsEvent "call" "CALL_ESTABLISHED" "" [])
      assertEqual "start as caller" [SipStartModem Originate] a2
      assertEqual "in call" (Just Originate) (sipLineInCall l2)
      let (l3, a3) = sipLineEvent 2 l2 (BsEvent "call" "CALL_CLOSED" "Connection reset by peer" [])
      assertEqual "closed" [SipStopModem, SipToDte EvNoCarrier] a3
      assertEqual "idle" Nothing (sipLineInCall l3)
  , testCase "SIP line: incoming, ring repeats, answer, hang up" $ do
      let l0 = sipLineInit "sip.example.org"
          (l1, a1) = sipLineEvent 0 l0 (BsEvent "call" "CALL_INCOMING" "" [("peeruri", "sip:bbs@example.org")])
      assertEqual "ring" [SipToDte EvRing] a1
      assertEqual "no ring yet" [] (snd (sipLineTick 1 l1))
      assertEqual "ring again" [SipToDte EvRing] (snd (sipLineTick 2.1 l1))
      let (l2, a2) = sipLineHayes l1 ActAnswer
      assertEqual "accept" [SipCommand "accept" ""] a2
      let (l3, a3) = sipLineEvent 3 l2 (BsEvent "call" "CALL_ESTABLISHED" "" [])
      assertEqual "start as answerer" [SipStartModem Answer] a3
      let (_, a4) = sipLineHayes l3 ActHangup
      assertEqual "hangup" [SipStopModem, SipCommand "hangup" ""] a4
      assertEqual "caller" "sip:bbs@example.org" (sipLinePeer l1)
  , testCase "SIP line: S0 answers through the interpreter" $ do
      -- baresip rings; the interpreter counts the ring against S0 and
      -- answers the way ATA does, and the line controller accepts
      let l0 = sipLineInit "sip.example.org"
          (l1, a1) = sipLineEvent 0 l0 (BsEvent "call" "CALL_INCOMING" "" [("peeruri", "sip:1234@example.org")])
      assertEqual "ring only" [SipToDte EvRing] a1
      assertEqual "caller" "sip:1234@example.org" (sipLinePeer l1)
      let (h1, _, _, _) = hayesInput 0 h0 (BC.pack "ATS0=1\r")
          (_, _, acts) = hayesEvent h1 EvRing
      assertEqual "answer" [ActAnswer] acts
      let (l2, a2) = foldl (\(l, xs) a -> let (l', ys) = sipLineHayes l a in (l', xs ++ ys)) (l1, []) acts
      assertEqual "accepted" [SipCommand "accept" ""] a2
      let (l3, a3) = sipLineEvent 1 l2 (BsEvent "call" "CALL_ESTABLISHED" "" [])
      assertEqual "answered" [SipStartModem Answer] a3
      assertEqual "in call" (Just Answer) (sipLineInCall l3)
      -- and with S0 clear it only rings
      let (_, _, none) = hayesEvent h0 EvRing
      assertEqual "no answer" [] none
  , testCase "SIP line: full URI and no answer" $ do
      let (l1, a1) = sipLineHayes (sipLineInit "d") (ActDial "sip:bbs@example.org")
      assertEqual "uri passes" [SipCommand "dial" "sip:bbs@example.org"] a1
      let (_, a2) = sipLineEvent 5 l1 (BsEvent "call" "CALL_CLOSED" "Busy" [])
      assertEqual "no answer" [SipToDte EvNoAnswer] a2
  ]

pipewireTests :: TestTree
pipewireTests = testGroup "PipeWire device discovery"
  [ testCase "parse pw-dump Node output" $
      assertEqual "nodes" expected (parseNodes dump)
  , testCase "match by node id" $
      assertEqual "id 56" (Unique (nodes !! 1)) (matchNode "56" nodes)
  , testCase "match by exact node name and description" $ do
      assertEqual "name" (Unique (head nodes)) (matchNode "alsa_output.pci-0000_00_1f.3.analog-stereo" nodes)
      assertEqual "description" (Unique (nodes !! 2)) (matchNode "USB Audio" nodes)
  , testCase "match by case-insensitive substring" $
      assertEqual "usb" (Unique (nodes !! 2)) (matchNode "usb" nodes)
  , testCase "an ambiguous substring is refused, not guessed" $
      assertEqual "analog" (Ambiguous [head nodes, nodes !! 1]) (matchNode "ANALOG" nodes)
  , testCase "no match" $
      assertEqual "nothing" NoMatch (matchNode "hdmi" nodes)
  , testCase "unknown ids and malformed input do not throw" $ do
      assertEqual "id" NoMatch (matchNode "999" nodes)
      assertEqual "garbage" [] (parseNodes (BC.pack "not json"))
      assertEqual "empty" [] (parseNodes (BC.pack "[]"))
  , testCase "read the volume a session manager applied to a stream" $ do
      -- 0.421824 is 0.75 cubed: a mixer slider left at three quarters,
      -- which WirePlumber restores onto every stream sharing the
      -- application name.  On the transmit stream that is 7.5 dB the far
      -- end never gets back, so it has to be visible.
      assertEqual "gains"
        [ ("modec-tx", 0.421824, False), ("modec-rx", 1.0, False), ("muted-one", 1.0, True) ]
        (parseGains volDump)
      assertEqual "no volume control, no report" [] (parseGains (BC.pack "[]"))
  ]
  where
    volDump = BC.pack (concat
      [ "[ {\"id\": 70, \"info\": { \"props\": { \"node.name\": \"modec-tx\" },"
      , " \"params\": { \"Props\": [ { \"volume\": 1.0, \"mute\": false,"
      , " \"channelVolumes\": [0.421824] } ] } } },"
      , " {\"id\": 71, \"info\": { \"props\": { \"node.name\": \"modec-rx\" },"
      , " \"params\": { \"Props\": [ { \"mute\": false, \"channelVolumes\": [1.0, 1.0] } ] } } },"
      , " {\"id\": 72, \"info\": { \"props\": { \"node.name\": \"muted-one\" },"
      , " \"params\": { \"Props\": [ { \"mute\": true, \"channelVolumes\": [1.0] } ] } } },"
      , " {\"id\": 73, \"info\": { \"props\": { \"node.name\": \"no-props\" } } } ]" ])
    dump = BC.pack (concat
      [ "[ {\"id\": 52, \"type\": \"PipeWire:Interface:Node\", \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_output.pci-0000_00_1f.3.analog-stereo\","
      , " \"node.description\": \"Built-in Audio Analog Stereo\", \"media.class\": \"Audio/Sink\" } } },"
      , " {\"id\": 56, \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_input.pci-0000_00_1f.3.analog-stereo\","
      , " \"node.description\": \"Built-in Audio Analog Stereo\", \"media.class\": \"Audio/Source\" } } },"
      , " {\"id\": 61, \"info\": { \"props\": {"
      , " \"node.name\": \"alsa_input.usb-Focusrite\", \"node.description\": \"USB Audio\","
      , " \"media.class\": \"Audio/Source\" } } },"
      , " {\"id\": 29, \"info\": { \"props\": { \"node.name\": \"Dummy-Driver\" } } } ]" ])
    expected =
      [ PwNode 52 "alsa_output.pci-0000_00_1f.3.analog-stereo" "Built-in Audio Analog Stereo" PwSink
      , PwNode 56 "alsa_input.pci-0000_00_1f.3.analog-stereo" "Built-in Audio Analog Stereo" PwSource
      , PwNode 61 "alsa_input.usb-Focusrite" "USB Audio" PwSource
      , PwNode 29 "Dummy-Driver" "" (PwOther "") ]
    nodes = take 3 expected

-- | The suite places no call and opens no device, and this is what keeps
-- it that way.
--
-- Being hermetic by habit is not the same as being hermetic: a habit is
-- one careless import away from a test that dials a number, and a test
-- that dials a number is slow, flaky, and occasionally expensive.  The
-- library and the test suite therefore depend on nothing that can start
-- a process, open a socket or signal anything.  A test cannot reach a
-- sound card because it cannot link the code that would.
--
-- The cabal file is the one place that guarantee can be undone, so the
-- alarm goes there.  If a stanza below genuinely needs one of these,
-- the thing to move is the code that needs it -- into the executable,
-- as app/PipewireIO.hs was -- rather than this list.
hermeticTests :: TestTree
hermeticTests = testGroup "the suite cannot place a call"
  [ testCase "neither the library nor the tests depend on process, network or unix" $ do
      cabal <- readFile "modec.cabal"
      forM_ ["library", "test-suite modec-test"] $ \stanza ->
        forM_ ["process", "network", "unix"] $ \pkg ->
          assertBool (stanza ++ " depends on " ++ pkg)
            (not (pkg `elem` dependencies stanza cabal))
  ]

-- | The build-depends of one stanza, by name.  Stanzas start in the
-- first column and their fields are indented, which is enough structure
-- to read a dependency list without a cabal parser.
dependencies :: String -> String -> [String]
dependencies stanza cabal =
  [ takeWhile (\c -> not (isSpace c) && c /= ',') (dropWhile isSpace d)
  | d <- concatMap (split ',') (field "build-depends:" (stanzaOf stanza cabal)) ]
  where
    stanzaOf name src =
      case dropWhile (not . (name `isPrefixOf`)) (lines src) of
        [] -> error ("no stanza " ++ show name ++ " in modec.cabal")
        (h : rest) -> h : takeWhile indented rest
    indented l = null l || " " `isPrefixOf` l
    -- a field runs to the next line at the field's own indentation
    field key ls = case dropWhile (not . (key `isInfixOf`)) ls of
      [] -> []
      (h : rest) ->
        drop 1 (dropWhile (/= ':') h) : takeWhile (\l -> "                 " `isPrefixOf` l) rest
    split c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : b) -> a : split c b
