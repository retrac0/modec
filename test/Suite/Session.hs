-- | The block loop itself, driven with a scripted audio interface and no
-- device at all.
--
-- This is coverage the executable never had.  Both of the bugs its own
-- comments record -- a flag that quietly did nothing in one of two
-- copies of the loop, and a 'ModemEvent' constructor handled in one
-- renderer and not the other, which killed a live call outright -- were
-- in code no test could reach, because the loop lived in an executable
-- the suite cannot link.  It lives in "Modec.Session" now.
--
-- The far end, where there is one, runs inside the audio interface: our
-- side's written block is handed to another 'modemStep' and its answer
-- comes back as our next read.  So a whole call goes through the real
-- loop, in one thread, with nothing scheduled and nothing timed.
module Suite.Session (sessionTests) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Vector.Storable as VS
import Test.Tasty
import Test.Tasty.HUnit

import Modec.DSP (Signal, addNoise, fromDb)
import Modec.Link (V32Rate (..))
import Modec.Modem
import Modec.Progress (defaultProgressParams, progressRxInit)
import Modec.Session
import Modec.Standards

fs :: Double
fs = 8000

blockN :: Int
blockN = 160

cfgFor :: Role -> ModemConfig
cfgFor r = defaultModemConfig fs r [Bell103]

startCall :: Role -> Line
startCall r = LineCall (modemInit (cfgFor r)) (cfgFor r)
  (if r == Originate then Just (progressRxInit fs defaultProgressParams) else Nothing)

silence :: Int -> [Signal]
silence n = replicate n (VS.replicate blockN 0)

-- | A session reading a fixed list of blocks and piling up what it
-- writes.  Running out of audio is a short read, which is what a dead
-- capture stream is.
scripted :: [Signal] -> IO (Session, IORef [Signal], IORef B.ByteString, IORef [String])
scripted blocks = do
  inp <- newIORef blocks
  out <- newIORef []
  toDte <- newIORef B.empty
  lg <- newIORef []
  line <- newIORef (startCall Originate)
  banner <- newIORef B.empty
  blk <- newIORef 0
  let se = Session
        { seFs = fs, seBlockN = blockN
        , seRead = atomicModifyIORef' inp (\bs -> case bs of
            []       -> ([], VS.empty)
            (b : r)  -> (r, b))
        , seWrite = \b -> modifyIORef' out (++ [b])
        , seRecv = return B.empty
        , seSend = \b -> modifyIORef' toDte (<> b)
        , seSay = \m -> modifyIORef' lg (++ [m])
        , seObserve = \_ _ _ -> return ()
        , seLost = return False
        , seStartCall = startCall
        , seParams = defaultProgressParams
        , seLine = line, seBanner = banner, seBlock = blk
        }
  return (se, out, toDte, lg)

-- | Stops on a final event, as the executable's plain path does.
untilFinal :: IO Controller
untilFinal = do
  done <- newIORef False
  return quietController
    { coEvent = \ev -> case ev of
        EvDropped  -> writeIORef done True
        EvFailed _ -> writeIORef done True
        _          -> return ()
    , coDone = readIORef done
    }

sessionTests :: TestTree
sessionTests = testGroup "the block loop"
  [ testCase "one block out for every block in, until the audio ends" $ do
      (se, out, _, _) <- scripted (silence 50)
      co <- untilFinal
      runLoop se co
      bs <- readIORef out
      assertEqual "blocks written" 50 (length bs)
      forM_ bs $ \b -> assertEqual "block size" blockN (VS.length b)

  -- The greeting is the flag that did nothing in one of the two copies
  -- of the loop.  There is one expression now, governed by tuOnline.
  , testCase "a banner waits for the DTE to come online" $ do
      (se, _, _, _) <- scripted (silence 20)
      writeIORef (seBanner se) (BC.pack "hello")
      done <- newIORef False
      runLoop se quietController
        { coTurn = \_ -> return (Turn B.empty False), coDone = readIORef done }
      held <- readIORef (seBanner se)
      assertEqual "held while the DTE is offline" (BC.pack "hello") held

      (se2, _, _, _) <- scripted (silence 20)
      writeIORef (seBanner se2) (BC.pack "hello")
      co2 <- untilFinal
      runLoop se2 co2
      held2 <- readIORef (seBanner se2)
      assertEqual "taken as soon as it is online" B.empty held2

  , testCase "an idle line writes silence and never runs the modem" $ do
      (se, out, _, _) <- scripted (silence 10)
      writeIORef (seLine se) LineIdle
      seen <- newIORef (0 :: Int)
      done <- newIORef False
      runLoop se quietController
        { coIdle = \_ -> modifyIORef' seen (+ 1)
        , coEvent = \_ -> assertFailure "the modem ran on an idle line"
        , coDone = readIORef done
        }
      n <- readIORef seen
      assertEqual "every block offered to the idle hook" 10 n
      bs <- readIORef out
      forM_ bs $ \b -> assertEqual "silence" 0 (VS.sum (VS.map abs b))

  , testCase "dialling plays its signal out and then the call starts" $ do
      (se, out, _, _) <- scripted (silence 10)
      writeIORef (seLine se) (LineDialing (VS.replicate (3 * blockN) 0.4))
      done <- newIORef False
      runLoop se quietController { coDone = readIORef done }
      line <- readIORef (seLine se)
      case line of
        LineCall {} -> return ()
        _ -> assertFailure "the dial signal never became a call"
      bs <- readIORef out
      let loud = length [ () | b <- take 3 bs, VS.sum (VS.map abs b) > 1 ]
      assertEqual "the dial tones went out" 3 loud

  -- A plain call survives an audio restart with its modem state; a Hayes
  -- session drops the call.  The loop does not choose -- it tells the
  -- controller and the controller decides -- which is what keeps the two
  -- behaviours from having to be two loops again.
  , testCase "a short read is the controller's to interpret" $ do
      (se, _, _, _) <- scripted (silence 3)
      lost <- newIORef (0 :: Int)
      co <- untilFinal
      runLoop se co { coCarrier = modifyIORef' lost (+ 1) }
      n <- readIORef lost
      assertEqual "told once, and the line left alone" 1 n
      line <- readIORef (seLine se)
      case line of
        LineCall {} -> return ()
        _ -> assertFailure "the loop dropped the call itself"

  , testCase "the block counter advances once per block, whatever is tracing" $ do
      (se, _, _, _) <- scripted (silence 30)
      seen <- newIORef ([] :: [Int])
      co <- untilFinal
      runLoop se { seObserve = \k _ _ -> modifyIORef' seen (++ [k]) } co
      ks <- readIORef seen
      assertEqual "one observation per block" [0 .. 29] ks

  , testCase "a whole Bell 103 call, through the loop, both directions" $ callThroughLoop
  ]

-- | A call placed through 'runLoop' against an answering modem that
-- lives inside the audio interface.
--
-- Our side's written block is fed to the far end's 'modemStep' and its
-- answer comes back as our next read, attenuated and noisy.  One thread,
-- no clock: the far end runs exactly one block per block we write.
callThroughLoop :: Assertion
callThroughLoop = do
  let farCfg = defaultModemConfig fs Answer [Bell103]
  far <- newIORef (modemInit farCfg)
  farSays <- newIORef (BC.pack "answered\r\n")
  pending <- newIORef (VS.replicate blockN 0)
  fromLine <- newIORef B.empty
  blocks <- newIORef (0 :: Int)
  line <- newIORef (startCall Originate)
  banner <- newIORef B.empty
  blk <- newIORef 0
  toSend <- newIORef (BC.pack "called\r\n")
  lg <- newIORef ([] :: [String])
  heard <- newIORef B.empty

  let impair k x = addNoise (k * 7919) (0.05 * 0.707 / fromDb 20) (VS.map (* 0.1) x)
      -- One turn of the far end: it hears what we last wrote and we hear
      -- what it says back.
      readBlock = do
        k <- atomicModifyIORef' blocks (\n -> (n + 1, n))
        if k > 2000 then return VS.empty else do
          ours <- readIORef pending
          st <- readIORef far
          out <- atomicModifyIORef' farSays (\b -> (B.empty, b))
          let (st', audio, rxBytes, _) = modemStep farCfg st (impair 1 ours) (B.unpack out)
          writeIORef far st'
          modifyIORef' fromLine (<> B.pack rxBytes)
          return (impair 2 audio)
      se = Session
        { seFs = fs, seBlockN = blockN
        , seRead = readBlock
        , seWrite = writeIORef pending
        , seRecv = atomicModifyIORef' toSend (\b -> (B.empty, b))
        , seSend = \b -> modifyIORef' heard (<> b)
        , seSay = \m -> modifyIORef' lg (++ [m])
        , seObserve = \_ _ _ -> return ()
        , seLost = return False
        , seStartCall = startCall
        , seParams = defaultProgressParams
        , seLine = line, seBanner = banner, seBlock = blk
        }

  connected <- newIORef False
  done <- newIORef False
  let co = quietController
        { coTurn = \_ -> do
            p <- atomicModifyIORef' toSend (\b -> (B.empty, b))
            return (Turn p True)
        , coEvent = \ev -> case ev of
            EvConnected _ _ -> writeIORef connected True
            EvDropped -> writeIORef done True
            EvFailed _ -> writeIORef done True
            _ -> return ()
        -- Stop when both texts have arrived whole, not on the first
        -- byte of either: a call that delivers "a" and stops has not
        -- carried anything.
        , coDone = do
            us <- readIORef heard
            them <- readIORef fromLine
            d <- readIORef done
            return (d || (BC.pack "answered" `B.isInfixOf` us
                          && BC.pack "called" `B.isInfixOf` them))
        }
  runLoop se co

  c <- readIORef connected
  assertBool "the call connected" c
  us <- readIORef heard
  them <- readIORef fromLine
  assertBool ("we heard the far end: " ++ show us)
             (BC.pack "answered" `B.isInfixOf` us)
  assertBool ("the far end heard us: " ++ show them)
             (BC.pack "called" `B.isInfixOf` them)
