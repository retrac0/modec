-- | PipeWire device discovery for the live modem.
--
-- Everything here goes through the @pw-dump@ command rather than
-- libpipewire, matching the way audio itself is moved (child @pw-cat@
-- processes).  The parsing and matching are pure so they can be tested
-- against a captured dump.
--
-- Device specifications given on the command line are resolved with
-- 'matchNode': a decimal string is a node id, otherwise an exact node
-- name or description wins, and failing that a case-insensitive
-- substring of either.  An ambiguous substring is an error rather than a
-- silent pick, because choosing the wrong sound card is worse than
-- refusing to start.
module Modec.Pipewire
  ( PwClass (..)
  , PwNode (..)
  , NodeMatch (..)
  , parseNodes
  , matchNode
  , pwNodes
  , pwAudioNodes
  , hasCaptureDevice
  , resolveNode
  , waitForNodes
  , describeNodes
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, toLower)
import Data.List (isInfixOf)
import System.Process (readProcess)

import Modec.Json

data PwClass = PwSink | PwSource | PwOther String deriving (Eq, Show)

data PwNode = PwNode
  { pnId    :: !Int
  , pnName  :: String
  , pnDesc  :: String
  , pnClass :: PwClass
  } deriving (Eq, Show)

-- | Parse the output of @pw-dump Node@ (a JSON array of objects with
-- @id@ and @info.props@).  Objects without a media class are kept as
-- 'PwOther' so that listings can show them.
parseNodes :: B.ByteString -> [PwNode]
parseNodes bs = case jsonParse bs of
  Just (JArr objs) -> [ n | Just n <- map node objs ]
  _ -> []
  where
    node o = do
      i <- jsonInt "id" o
      props <- jsonLookup "info" o >>= jsonLookup "props"
      let name = jsonString "node.name" props
          desc = jsonString "node.description" props
          cls = case jsonString "media.class" props of
            "Audio/Sink" -> PwSink
            "Audio/Source" -> PwSource
            "Audio/Source/Virtual" -> PwSource
            other -> PwOther other
      if null name then Nothing else Just (PwNode i name desc cls)

data NodeMatch = NoMatch | Unique PwNode | Ambiguous [PwNode] deriving (Eq, Show)

-- | Resolve a device specification against a node list.
matchNode :: String -> [PwNode] -> NodeMatch
matchNode spec nodes
  | all isDigit spec && not (null spec) = uniqueOf [ n | n <- nodes, pnId n == read spec ]
  | otherwise = case [ n | n <- nodes, pnName n == spec || pnDesc n == spec ] of
      exact@(_ : _) -> uniqueOf exact
      [] -> uniqueOf [ n | n <- nodes, lc spec `isInfixOf` lc (pnName n) || lc spec `isInfixOf` lc (pnDesc n) ]
  where
    lc = map toLower
    uniqueOf [] = NoMatch
    uniqueOf [n] = Unique n
    uniqueOf ns = Ambiguous ns

-- | All PipeWire nodes, or an empty list if @pw-dump@ is unavailable.
pwNodes :: IO [PwNode]
pwNodes = do
  r <- try (readProcess "pw-dump" ["Node"] "") :: IO (Either SomeException String)
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

-- | A human-readable listing, one node per line.
describeNodes :: [PwNode] -> String
describeNodes ns = unlines
  [ "  " ++ pad 5 (show (pnId n)) ++ pad 9 (kind (pnClass n)) ++ pnName n ++
    (if null (pnDesc n) || pnDesc n == pnName n then "" else "  (" ++ pnDesc n ++ ")")
  | n <- ns ]
  where
    pad w s = s ++ replicate (max 1 (w - length s)) ' '
    kind c = case c of
      PwSink -> "output"
      PwSource -> "input"
      PwOther s -> s
