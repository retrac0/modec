-- | The tree.  Everything in it is pure: the suite reads fixture files
-- and simulates telephone audio, and that is the whole of its dealings
-- with the outside world.  It places no call and opens no device, and
-- the test-suite stanza in modec.cabal is what makes that structural
-- rather than a habit -- see the note there.
module Main (main) where

import Test.Tasty

import Corpus
import Suite.Dsp
import Suite.Fsk
import Suite.Link
import Suite.Mnp
import Suite.Tones
import Suite.Tools
import Suite.V22
import Suite.V32

main :: IO ()
main = do
  fx <- fixtureTests
  defaultMain $ testGroup "modec"
    [ testGroup "primitives"
        [wavTests, dspTests, scramblerTests, stageTests, toneFrameTests]
    , testGroup "frequency shift"
        [fx, chunkTests, propertyTests, errorRateTests, channelTests, detectTests]
    , testGroup "bringing a call up"
        [handshakeTests, modemTests, v8Tests, telnetTests]
    , testGroup "phase and quadrature"
        [v22Tests, v32Tests, v32PumpTests, v32SignalTests, v32StartTests, echoTests]
    , testGroup "error correction"
        [hdlcTests, mnpFrameTests, mnpTests, mnpModemTests, mnpFieldTests]
    , testGroup "around the modem"
        [hayesTests, baresipTests, pipewireTests]
    , testGroup "tones as meaning"
        [ttyTests, dtmfTests, progressTests]
    ]
