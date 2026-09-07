-- | What @pw-dump@ and @pw-link@ say, parsed.
--
-- PipeWire is reached through those commands rather than through
-- libpipewire, matching the way audio itself is moved (child @pw-cat@
-- processes).  Running them is "PipewireIO", in the executable; this
-- module is the half that turns their output into values, and it is
-- pure, so it can be tested against a captured dump -- and so that the
-- library cannot start a process at all.
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
  , describeNodes
    -- * Links
  , PwLink (..)
  , parseLinks
    -- * Volumes
  , parseGains
  , attenuated
  ) where

import qualified Data.ByteString as B
import Data.Char (isDigit, toLower)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)

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

-- | A link between two ports, named by the nodes at each end.
data PwLink = PwLink
  { plId  :: !Int
  , plSrc :: String
  , plDst :: String
  } deriving (Eq, Show)

-- | Parse @pw-link -I -l@.  Its output lists each port on an unindented
-- line and each of that port's links indented with an arrow giving the
-- direction.
parseLinks :: String -> [PwLink]
parseLinks out = go "" (lines out)
  where
    go _ [] = []
    go cur (l : ls) = case words l of
      (lid : arrow : _ : rest)
        | arrow == "|->" , Just i <- readMaybeInt lid -> PwLink i cur (nodeOf (unwords rest)) : go cur ls
        | arrow == "|<-" , Just i <- readMaybeInt lid -> PwLink i (nodeOf (unwords rest)) cur : go cur ls
      (pid : rest) | Just _ <- readMaybeInt pid, not (null rest) -> go (nodeOf (unwords rest)) ls
      _ -> go cur ls
    nodeOf s = takeWhile (/= ':') s
    readMaybeInt s = case reads s of { [(i, "")] -> Just (i :: Int); _ -> Nothing }

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

-- | The volume WirePlumber has actually applied to a node, as
-- @(name, loudest channel, muted)@.
--
-- This is worth checking because WirePlumber restores per-application
-- volumes from @stream-properties@, keyed by @application.name@ among
-- other things.  Every @pw-cat@ stream on a machine shares that name, so
-- one stray slider drag in a mixer silently attenuates the modem's
-- transmit for good, on a control no modem operator would think to look
-- at.  A modem's send level is part of the protocol, not a listening
-- preference, so the streams are created with restore disabled and this
-- confirms it took.
parseGains :: B.ByteString -> [(String, Double, Bool)]
parseGains bs = case jsonParse bs of
  Just (JArr objs) -> mapMaybe gains objs
  _ -> []
  where
    gains o = do
      info <- jsonLookup "info" o
      name <- case jsonString "node.name" <$> jsonLookup "props" info of
        Just n | not (null n) -> Just n
        _ -> Nothing
      JArr props <- jsonLookup "params" info >>= jsonLookup "Props"
      let vols = [ v | p <- props
                     , Just (JArr cs) <- [jsonLookup "channelVolumes" p]
                     , JNum v <- cs ]
          muted = or [ True | p <- props, Just (JBool True) <- [jsonLookup "mute" p] ]
      if null vols then Nothing else Just (name, maximum vols, muted)

-- | Of the named nodes, those a mixer would be quietening: muted, or more
-- than a quarter of a decibel down.  Nodes that are absent or carry no
-- volume control are not reported.
attenuated :: [String] -> [(String, Double, Bool)] -> [(String, Double, Bool)]
attenuated names gs = [ g | g@(n, v, m) <- gs, n `elem` names, m || v < 0.97 ]
