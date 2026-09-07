-- | Running @pw-dump@ and @pw-link@, and nothing else.
--
-- This half of the PipeWire interface lives in the executable rather
-- than in the library, and deliberately: it is the only code in the
-- project that starts a child process to look at the machine, and the
-- library it is split out of is what the test suite links against.  A
-- test cannot reach a sound card from here because it cannot reach here.
-- Parsing what these commands say is "Modec.Pipewire", which is pure.
module PipewireIO
  ( pwNodes
  , pwAudioNodes
  , hasCaptureDevice
  , resolveNode
  , waitForNodes
  , pwLinks
  , pwUnlink
  , pruneCompetingInputs
  , nodeGains
  , attenuatedNodes
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BC
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)

import Modec.Pipewire

-- | All PipeWire nodes, or an empty list if @pw-dump@ is unavailable.
pwNodes :: IO [PwNode]
pwNodes = do
  r <- try (readProcess "pw-dump" ["Node"] "") :: IO (Either IOException String)
  return $ case r of
    Left _ -> []
    Right out -> parseNodes (BC.pack out)

-- | Only the audio sinks and sources.
pwAudioNodes :: IO [PwNode]
pwAudioNodes = filter (\n -> pnClass n `elem` [PwSink, PwSource]) <$> pwNodes

-- | True when PipeWire offers a capture device.  Monitors of outputs are
-- ports of their sink rather than nodes of their own, so any
-- @Audio/Source@ node is a real capture.  If @pw-dump@ cannot be run at
-- all, assume there is one and let @pw-cat@ report the trouble.
hasCaptureDevice :: IO Bool
hasCaptureDevice = do
  ns <- pwNodes
  return (null ns || any ((== PwSource) . pnClass) ns)

-- | Resolve a device specification, returning either a node or a message
-- naming the candidates.
resolveNode :: String -> PwClass -> IO (Either String PwNode)
resolveNode spec want = do
  ns <- pwAudioNodes
  let pool = filter ((== want) . pnClass) ns
  return $ case matchNode spec pool of
    Unique n -> Right n
    Ambiguous cands -> Left ("device " ++ show spec ++ " is ambiguous:\n" ++ describeNodes cands)
    NoMatch -> Left ("no " ++ kind ++ " matches " ++ show spec ++
                     (if null pool then " (none present)" else ":\n" ++ describeNodes pool))
  where
    kind = case want of
      PwSink -> "output"
      PwSource -> "input"
      PwOther s -> s

-- | Wait for nodes with the given names to appear, polling @pw-dump@.
-- Returns the names still missing when the timeout expires.
waitForNodes :: [String] -> Double -> IO [String]
waitForNodes names timeout = go (max 1 (round (timeout / 0.1) :: Int))
  where
    go 0 = missing
    go k = do
      left <- missing
      if null left then return [] else threadDelay 100000 >> go (k - 1)
    missing = do
      ns <- pwNodes
      return [ nm | nm <- names, not (any ((== nm) . pnName) ns) ]

-- | Every link in the graph, from @pw-link -I -l@.
pwLinks :: IO [PwLink]
pwLinks = do
  r <- try (readProcess "pw-link" ["-I", "-l"] "") :: IO (Either IOException String)
  return $ case r of
    Left _ -> []
    Right out -> parseLinks out

-- | Destroy a link by id.
pwUnlink :: Int -> IO Bool
pwUnlink lid = do
  r <- try (readProcessWithExitCode "pw-link" ["-d", show lid] "")
         :: IO (Either IOException (ExitCode, String, String))
  return $ case r of
    Right (ExitSuccess, _, _) -> True
    _ -> False

-- | PipeWire's session manager often links the default capture device
-- into a softphone's input as well as the node the softphone asked for,
-- so the far end hears the microphone mixed with the modem.  Remove any
-- link that feeds a node our line source feeds, unless it comes from the
-- line source itself.  Returns what was removed.
pruneCompetingInputs :: String -> IO [PwLink]
pruneCompetingInputs lineNode = do
  ls <- pwLinks
  let fedByUs = [ plDst l | l <- ls, plSrc l == lineNode ]
      stray = [ l | l <- ls, plDst l `elem` fedByUs, plSrc l /= lineNode ]
  mapM_ (pwUnlink . plId) stray
  return stray

-- | 'parseGains' over a live @pw-dump@.
nodeGains :: IO [(String, Double, Bool)]
nodeGains = do
  r <- try (readProcess "pw-dump" [] "") :: IO (Either IOException String)
  return $ case r of
    Left _ -> []
    Right out -> parseGains (BC.pack out)

-- | 'attenuated' over a live @pw-dump@.
attenuatedNodes :: [String] -> IO [(String, Double, Bool)]
attenuatedNodes names = attenuated names <$> nodeGains
