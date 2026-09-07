-- | The parts around the modem: Hayes commands, baresip, PipeWire.
module Suite.Tools (hayesTests, baresipTests, pipewireTests, hermeticTests) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isSpace)
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit
import Modec.Handshake
import Modec.Hayes
import Modec.Dtmf
import Modec.Baresip
import Modec.Json
import Modec.Pipewire

hayesTests :: TestTree
hayesTests = testGroup "Hayes AT interpreter"
  [ testCase "AT, ATE0, ATI, S0" $ do
      let (s1, b1, _, a1) = hayesInput 0 hayesInit (BC.pack "AT\r")
      assertEqual "OK" "AT\r\r\nOK\r\n" (BC.unpack b1)
      assertEqual "no actions" [] a1
      let (s2, b2, _, _) = hayesInput 0.1 s1 (BC.pack "ATE0\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b2)
      let (s3, b3, _, _) = hayesInput 0.2 s2 (BC.pack "ATS0=2\r")
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b3)
      assertBool "auto-answer set" (hayesAutoAnswer s3)
      let (_, b4, _, _) = hayesInput 0.3 s3 (BC.pack "ATS0?\r")
      assertBool "002" ("002" `isInfixOf` BC.unpack b4)
      let (_, b5, _, _) = hayesInput 0.4 s3 (BC.pack "ATY\r")
      assertBool "ERROR" ("ERROR" `isInfixOf` BC.unpack b5)
  , testCase "ATDT dials, CONNECT goes online, data passes, +++ escapes, ATH hangs up" $ do
      let (s1, _, _, a1) = hayesInput 0 hayesInit (BC.pack "atdt 555-1234\r")
      assertEqual "dial" [ActDial "T555-1234"] a1
      let (s2, b2) = hayesEvent s1 (EvConnect 2400)
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
      let (s6, _, _, a6) = hayesInput 4 s5 (BC.pack "ATO\r")
      assertEqual "online again" [ActOnline] a6
      assertBool "online" (hayesOnline s6)
      let (s7, _, fwd7, _) = hayesInput 5 s6 (BC.pack "+x")
      assertEqual "lone plus is data" "+x" (BC.unpack fwd7)
      let (s8, _, _, _) = hayesInput 7 s7 (BC.pack "+++")
          (s9, _) = hayesTick 8.1 s8
          (_, b10, _, a10) = hayesInput 8.2 s9 (BC.pack "ATH\r")
      assertEqual "hangup" [ActHangup] a10
      assertBool "OK" ("OK" `isInfixOf` BC.unpack b10)
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
