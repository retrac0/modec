-- | baresip control protocol (the @ctrl_tcp@ module) and a line controller
-- that maps Hayes actions and baresip call events to modem actions.
--
-- Wire format: netstrings (@<length>:<payload>,@) carrying flat JSON
-- objects.  Commands are @{"command":"dial","params":"sip:...","token":
-- "..."}@; responses @{"response":true,"ok":true,"data":"...","token":
-- "..."}@; events @{"event":true,"class":"call","type":"CALL_ESTABLISHED",
-- "param":"...","direction":"incoming","peeruri":"...","id":"..."}@.
-- Only strings, booleans and numbers occur, so a small parser suffices.
module Modec.Baresip
  ( netstringEncode
  , netstringDecode
  , BsMessage (..)
  , decodeBsMessage
  , commandJson
  , SipLine
  , sipLineInit
  , SipAction (..)
  , sipLineHayes
  , sipLineEvent
  , sipLineTick
  , sipLineInCall
  , sipLineSetAuto
  , sipLinePeer
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.List (isPrefixOf)

import Modec.Standards (Role (..))
import Modec.Json
import Modec.Hayes (HayesAction (..), HayesEvent (..))

-- | Netstring framing.
netstringEncode :: B.ByteString -> B.ByteString
netstringEncode p = BC.pack (show (B.length p)) <> BC.pack ":" <> p <> BC.pack ","

-- | Decode as many complete netstrings as the buffer holds; returns the
-- payloads and the unconsumed remainder.
netstringDecode :: B.ByteString -> ([B.ByteString], B.ByteString)
netstringDecode = go []
  where
    go acc buf =
      let (lenS, rest) = BC.span isDigit buf
      in if B.null lenS || B.null rest || BC.head rest /= ':'
           then (reverse acc, buf)
           else
             let n = read (BC.unpack lenS) :: Int
                 body = B.drop 1 rest
             in if B.length body >= n + 1 && BC.index body n == ','
                  then go (B.take n body : acc) (B.drop (n + 1) body)
                  else (reverse acc, buf)

-- | Messages from baresip.
data BsMessage
  = BsResponse Bool String String            -- ^ ok, data, token
  | BsEvent String String String [(String, String)]   -- ^ class, type, param, other string fields
  | BsUnknown Json
  deriving (Eq, Show)

decodeBsMessage :: B.ByteString -> Maybe BsMessage
decodeBsMessage bs = do
  j <- jsonParse bs
  case j of
    JObj kvs ->
      let str k = case lookup k kvs of { Just (JStr s) -> s; _ -> "" }
          isTrue k = lookup k kvs == Just (JBool True) || lookup k kvs == Just (JStr "true")
      in Just $ if isTrue "response"
                  then BsResponse (isTrue "ok") (str "data") (str "token")
                  else if isTrue "event"
                    then BsEvent (str "class") (str "type") (str "param") [ (k, s) | (k, JStr s) <- kvs, k `notElem` ["class", "type", "param"] ]
                    else BsUnknown j
    _ -> Just (BsUnknown j)

-- | A command frame.
commandJson :: String -> String -> String -> B.ByteString
commandJson cmd params token =
  netstringEncode (jsonEncode (JObj ([("command", JStr cmd)] ++ [ ("params", JStr params) | not (null params) ] ++ [("token", JStr token)])))

-- | Line controller state.
data SipLine = SipLine
  { slDomain   :: String
  , slDialing  :: Bool            -- ^ we placed the call
  , slIncoming :: Bool            -- ^ an unanswered incoming call is ringing
  , slInCall   :: Maybe Role      -- ^ established call and the modem role we take
  , slLastRing :: Double
  , slAuto     :: !Bool          -- ^ answer without waiting to be told (Hayes S0)
  , slPeer     :: String         -- ^ who is calling, from the INVITE
  }

sipLineInit :: String -> SipLine
sipLineInit domain = SipLine domain False False Nothing (-10) False ""

-- | What the modem should do.
data SipAction
  = SipCommand String String       -- ^ command and params for baresip
  | SipStartModem Role             -- ^ media is up: run the modem in this role
  | SipStopModem
  | SipToDte HayesEvent            -- ^ tell the DTE
  deriving (Eq, Show)

-- | Number to a SIP URI: full URIs pass through, digits get the domain;
-- Hayes dial modifiers (T, P, W, commas) are dropped.
dialUri :: String -> String -> String
dialUri domain s
  | "sip:" `isPrefixOf` s || "sips:" `isPrefixOf` s = s
  | '@' `elem` s = "sip:" ++ s
  | otherwise = "sip:" ++ filter (\c -> isDigit c || c `elem` "*#+") s ++ "@" ++ domain

-- | Hayes actions in SIP mode.
sipLineHayes :: SipLine -> HayesAction -> (SipLine, [SipAction])
sipLineHayes st a = case a of
  ActDial s -> (st { slDialing = True }, [SipCommand "dial" (dialUri (slDomain st) s)])
  ActAnswer
    | slIncoming st -> (st { slIncoming = False }, [SipCommand "accept" ""])
    | otherwise -> (st, [SipToDte EvNoCarrier])
  ActHangup -> (st { slDialing = False, slIncoming = False, slInCall = Nothing }
               , [SipStopModem, SipCommand "hangup" ""])
  ActOnline -> (st, [])

-- | baresip call events.
sipLineEvent :: Double -> SipLine -> BsMessage -> (SipLine, [SipAction])
sipLineEvent t st msg = case msg of
  -- S0 is answered here rather than in the modem's idle loop, which only
  -- sees ringing as sustained line energy and so is switched off in SIP
  -- mode.  Ring the DTE either way: a watching terminal should see the
  -- call arrive, not just the CONNECT that follows it.
  BsEvent "call" "CALL_INCOMING" _ fields ->
    ( st { slIncoming = True, slLastRing = t
         , slPeer = maybe (slPeer st) id (lookup "peeruri" fields) }
    , SipToDte EvRing : [ SipCommand "accept" "" | slAuto st ] )
  BsEvent "call" "CALL_ESTABLISHED" _ _ ->
    let role = if slDialing st then Originate else Answer
    in (st { slInCall = Just role, slIncoming = False }, [SipStartModem role])
  BsEvent "call" "CALL_CLOSED" _ _ ->
    let wasCall = slInCall st /= Nothing
        dte = if wasCall then [SipToDte EvNoCarrier] else if slDialing st then [SipToDte EvNoAnswer] else []
    in (st { slDialing = False, slIncoming = False, slInCall = Nothing }, [SipStopModem | wasCall] ++ dte)
  _ -> (st, [])

-- | Repeat RING every two seconds while an incoming call waits.
sipLineTick :: Double -> SipLine -> (SipLine, [SipAction])
sipLineTick t st
  | slIncoming st && t - slLastRing st >= 2 = (st { slLastRing = t }, [SipToDte EvRing])
  | otherwise = (st, [])

sipLineInCall :: SipLine -> Maybe Role
sipLineInCall = slInCall

-- | Track Hayes S0.  The register lives in the Hayes state, which this
-- module does not see, so the modem loop pushes it in each block.
sipLineSetAuto :: Bool -> SipLine -> SipLine
sipLineSetAuto a st = st { slAuto = a }

-- | Who called, as baresip reported it; empty for a call we placed.
sipLinePeer :: SipLine -> String
sipLinePeer = slPeer
