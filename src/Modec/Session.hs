-- | The block loop: read a block of audio, run the modem over it, write
-- a block back, and hand the DTE whatever came out.
--
-- There were two of these, in one function in the executable.  One ran a
-- single call in a fixed role and exited; the other ran an AT command
-- interpreter, a SIP line controller and a dialler on top of the same
-- eight steps.  They drifted, as two copies of anything do: a flag that
-- did nothing in one of them was this codebase's oldest bug, and a
-- 'ModemEvent' constructor handled in one renderer and not the other
-- killed a live call outright, with no test able to see it -- because
-- nothing in the suite could reach the executable at all.
--
-- So there is one loop, and what differs between a plain call and a
-- Hayes session is a 'Controller': what the DTE has to say each block,
-- what a modem event means to it, and when it is finished.  A plain call
-- is a session whose line starts in 'LineCall' and never leaves it,
-- which is why the fork could go: there is nothing left to fork on.
--
-- This is in the library, and it does IO, which the rest of the library
-- does not.  What makes that safe is not purity but dependencies: the
-- audio device and the DTE arrive as records of 'IO' actions, so
-- everything needing @process@, @network@ or @unix@ stays in the
-- executable and the guarantee that modec.cabal encodes -- and
-- @Suite.Tools@ enforces -- is untouched.  What it buys is that a test
-- can drive a whole call through this loop with a scripted audio
-- interface and no device at all.
module Modec.Session
  ( -- * What the line is doing
    Line (..)
    -- * The loop's surroundings
  , Session (..)
    -- * What makes a session a plain call or a Hayes one
  , Controller (..)
  , Turn (..)
  , quietController
  , runLoop
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as B
import Data.IORef
import GHC.Clock (getMonotonicTime)
import qualified Data.Vector.Storable as VS

import Modec.DSP (Signal)
import Modec.Modem
import Modec.Progress
import Modec.Standards (Role (..))

-- | What the line is doing.  In a Hayes session it moves between all
-- three; in a plain call it is 'LineCall' from the first block.
data Line
  = LineIdle
    -- ^ on hook.  Sustained energy here is a calling signal.
  | LineDialing Signal
    -- ^ off hook with dial tones still to play.
  | LineCall ModemState ModemConfig (Maybe ProgressRx)
    -- ^ a call up, with the progress watcher a call we placed keeps on
    -- the line until the modems are talking.

-- | Everything the loop needs that does not change from block to block.
--
-- 'seRead' is the clock: the loop runs at whatever rate blocks arrive,
-- and a short read means the capture stream stopped.
data Session = Session
  { seFs        :: !Double
  , seBlockN    :: !Int
  , seRead      :: IO Signal
    -- ^ one block of samples, recordings and all.  Short means the
    -- stream stopped.  What the bytes on the device meant was the
    -- audio backend's business; here there are only samples.
  , seWrite     :: Signal -> IO ()
  , seRecv      :: IO B.ByteString           -- ^ bytes the DTE has for the line
  , seSend      :: B.ByteString -> IO ()     -- ^ bytes the line has for the DTE
  , seSay       :: String -> IO ()           -- ^ the call log
  , seObserve   :: Int -> ModemState -> ModemState -> IO ()
    -- ^ tracing, given the block index.  Reads the clock; must not move
    -- it -- a trace may not be able to change what it is tracing.
  , seLost      :: IO Bool
    -- ^ the capture stream stopped.  'True' to carry on with a restored
    -- stream, 'False' to end the session.
  , seStartCall :: Role -> Line              -- ^ a fresh call in this role
  , seParams    :: ProgressParams
  , seLine      :: IORef Line
  , seBanner    :: IORef B.ByteString
    -- ^ a greeting held until the link can carry it and the DTE is
    -- online to send it.
  , seBlock     :: IORef Int
  , seSlow      :: Double -> Double -> IO ()
    -- ^ told of a block that took longer than it should: seconds it took, seconds into the session
  }

-- | What the DTE has to say this block.
data Turn = Turn
  { tuDte    :: B.ByteString
    -- ^ bytes for the line.
  , tuOnline :: !Bool
    -- ^ whether the DTE side is through to the line.  A Hayes session in
    -- command mode is not, and a greeting drained then would go into a
    -- modem still holding its transmit queue for the settle window.
  }

-- | The half of a session that a plain call and a Hayes one disagree
-- about.  Everything here runs in the executable, where the sockets are.
data Controller = Controller
  { coTurn     :: Double -> IO Turn
    -- ^ before the modem runs, given the time since the session started.
  , coEvent    :: ModemEvent -> IO ()
    -- ^ what a modem event means to this DTE.
  , coProgress :: ProgressEvent -> IO ()
    -- ^ what the network played back at a call we placed.  What counts
    -- as a refusal, and what a refusal does, is the controller's: a
    -- plain call gives up, a Hayes session hangs up and tells the DTE.
  , coCarrier  :: IO ()
    -- ^ the capture stream stopped.  Whether that ends a call in
    -- progress is the controller's to decide -- a plain call survives an
    -- audio restart with its modem state, a Hayes session does not.
  , coIdle     :: Signal -> IO ()
    -- ^ a block arriving while the line is idle, for ring detection.
  , coDone     :: IO Bool
  }

-- | A controller that does nothing and never finishes: a base to
-- override, so a new field cannot be forgotten by a caller.
quietController :: Controller
quietController = Controller
  { coTurn = \_ -> return (Turn B.empty True)
  , coEvent = \_ -> return ()
  , coProgress = \_ -> return ()
  , coCarrier = return ()
  , coIdle = \_ -> return ()
  , coDone = return False
  }

-- | Run until the controller says it is finished, or the audio ends and
-- cannot be put back.
runLoop :: Session -> Controller -> IO ()
runLoop se co = loop
  where
    blockN = seBlockN se
    silence = VS.replicate blockN 0

    loop = do
      raw <- seRead se
      if VS.length raw < blockN
        then do
          coCarrier co
          ok <- seLost se
          when ok loop
        else do
          k <- readIORef (seBlock se)
          turn <- coTurn co (fromIntegral (k * blockN) / seFs se)
          line <- readIORef (seLine se)
          case line of
            LineIdle -> do
              coIdle co raw
              seWrite se silence
            LineDialing sig -> do
              -- The dial signal is played out a block at a time; the
              -- call starts on the block the last of it goes out.
              let (now, rest) = VS.splitAt blockN sig
              seWrite se (now VS.++ VS.replicate (blockN - VS.length now) 0)
              writeIORef (seLine se)
                (if VS.null rest then seStartCall se Originate else LineDialing rest)
            LineCall st c watch -> do
              -- A real-time loop is judged by its worst block.  Anything
              -- over twelve of the twenty milliseconds is reported,
              -- because the far end reads a late block as a gap in the
              -- carrier and the recording shows nothing.
              t0 <- getMonotonicTime
              call k turn st c watch raw
              t1 <- getMonotonicTime
              when (t1 - t0 > 0.012) $ seSlow se (t1 - t0) (fromIntegral (k * blockN) / seFs se)
          modifyIORef' (seBlock se) (+ 1)
          done <- coDone co
          unless done loop

    call k turn st c watch rx = do
      greet <- if tuOnline turn
                 then atomicModifyIORef' (seBanner se) (\b -> (B.empty, b))
                 else return B.empty
      let dte = if tuOnline turn then B.unpack (greet <> tuDte turn) else []
          (st', audio, rxBytes, events) = modemStep c st rx dte
      seObserve se k st st'
      seWrite se audio
      when (tuOnline turn && not (null rxBytes)) $ seSend se (B.pack rxBytes)
      watch' <- listen st' watch rx
      -- The watcher may have hung the call up between here and there.
      line' <- readIORef (seLine se)
      case line' of
        LineIdle -> return ()
        _ -> writeIORef (seLine se) (LineCall st' c watch')
      mapM_ (coEvent co) events

    -- What the network is playing back, until the modems are talking;
    -- after that the line carries a carrier and nothing else.
    listen st' (Just w) rx | not (modemConnected st') = do
      let (w', pevs) = progressRxBlock (seParams se) w rx
      forM_ pevs $ \e -> seSay se (describeProgress e) >> coProgress co e
      return (Just w')
    listen _ watch _ = return watch
