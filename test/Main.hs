-- | The tree.  Everything in it is pure: the suite reads fixture files
-- and simulates telephone audio, and that is the whole of its dealings
-- with the outside world.  It places no call and opens no device, and
-- the test-suite stanza in modec.cabal is what makes that structural
-- rather than a habit -- see the note there.
module Main (main) where

import Test.Tasty

import Corpus
import Suite.Channel
import Suite.Classify
import Suite.LiveChannel
import Suite.Dsp
import Suite.Fsk
import Suite.Link
import Suite.Mnp
import Suite.Sample
import Suite.Session
import Suite.Tones
import Suite.Tools
import Suite.V22
import Suite.V32
import Suite.Voice

main :: IO ()
main = do
  fx <- fixtureTests
  live <- liveTests
  specs <- specTests
  calls <- callFixtureTests
  defaultMain $ testGroup "modec"
    [ testGroup "primitives"
        [wavTests, sampleTests, dspTests, scramblerTests, stageTests, toneFrameTests]
    -- What the simulator every other measurement here leans on actually
    -- does, as against what it says it does.
    , channelSimTests
    , liveChannelTests
    , specs
    , testGroup "frequency shift"
        [fx, chunkTests, propertyTests, errorRateTests, channelTests, detectTests, resampleTests]
    , testGroup "bringing a call up"
        [handshakeTests, modemTests, v8Tests, telnetTests, framerTests]
    -- Recorded calls, replayed through the whole modem: the only tests
    -- here whose far end was a real modem on a real line.
    , live
    , testGroup "phase and quadrature"
        [v22Tests, v32Tests, v32PumpTests, v32FloorTests, v32SignalTests, v32ListenTests, v32StartTests, echoTests, rateTests]
    , testGroup "error correction"
        [hdlcTests, mnpFrameTests, mnpTests, mnpModemTests, mnpFieldTests]
    , testGroup "around the modem"
        [hayesTests, baresipTests, pipewireTests, sessionTests, voiceTests, hermeticTests]
    , testGroup "tones as meaning"
        [ttyTests, dtmfTests, progressTests]
    -- What answered a call, read from the audio rather than taken from
    -- what the modem managed; the recorded calls are the ones the rules
    -- were settled on.
    , testGroup "what answered"
        [speechTests, classifyTests, calls]
    ]
