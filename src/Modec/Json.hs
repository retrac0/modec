-- | A small JSON parser and encoder.  Enough for the flat objects
-- baresip sends over its control port and for @pw-dump@ output; no
-- dependency on aeson, which is not in the build plan.
module Modec.Json
  ( Json (..)
  , jsonParse
  , jsonEncode
  , jsonLookup
  , jsonString
  , jsonInt
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)

-- | A flat JSON value.
data Json = JStr String | JBool Bool | JNum Double | JNull | JObj [(String, Json)] | JArr [Json]
  deriving (Eq, Show)

-- | Parse a JSON document (objects, arrays, strings with escapes, numbers,
-- booleans, null).  Returns 'Nothing' on malformed input.
jsonParse :: B.ByteString -> Maybe Json
jsonParse bs = case value (skipWs (BC.unpack bs)) of
  Just (v, rest) | all (`elem` " \t\r\n") rest -> Just v
  _ -> Nothing
  where
    skipWs = dropWhile (`elem` " \t\r\n")
    value s = case s of
      ('{' : r) -> object (skipWs r) []
      ('[' : r) -> array (skipWs r) []
      ('"' : r) -> fmap (\(str, r') -> (JStr str, r')) (string r "")
      ('t' : 'r' : 'u' : 'e' : r) -> Just (JBool True, r)
      ('f' : 'a' : 'l' : 's' : 'e' : r) -> Just (JBool False, r)
      ('n' : 'u' : 'l' : 'l' : r) -> Just (JNull, r)
      _ -> number s
    object s acc = case s of
      ('}' : r) -> Just (JObj (reverse acc), r)
      ('"' : r) -> do
        (k, r1) <- string r ""
        case skipWs r1 of
          (':' : r2) -> do
            (v, r3) <- value (skipWs r2)
            case skipWs r3 of
              (',' : r4) -> object (skipWs r4) ((k, v) : acc)
              ('}' : r4) -> Just (JObj (reverse ((k, v) : acc)), r4)
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
    array s acc = case s of
      (']' : r) -> Just (JArr (reverse acc), r)
      _ -> do
        (v, r1) <- value s
        case skipWs r1 of
          (',' : r2) -> array (skipWs r2) (v : acc)
          (']' : r2) -> Just (JArr (reverse (v : acc)), r2)
          _ -> Nothing
    string s acc = case s of
      ('"' : r) -> Just (reverse acc, r)
      ('\\' : c : r) -> case c of
        'n' -> string r ('\n' : acc)
        'r' -> string r ('\r' : acc)
        't' -> string r ('\t' : acc)
        'u' -> let (h, r') = splitAt 4 r in string r' (toEnum (read ("0x" ++ h)) : acc)
        _ -> string r (c : acc)
      (c : r) -> string r (c : acc)
      [] -> Nothing
    number s =
      let (numS, r) = span (`elem` "-+.eE0123456789") s
      in if null numS then Nothing else case reads (fixup numS) of
           [(d, "")] -> Just (JNum d, r)
           _ -> Nothing
    fixup n = let n1 = if "-." `isPrefixOf` n then "-0" ++ drop 1 n else if "." `isPrefixOf` n then '0' : n else n
              in if last n1 == '.' then n1 ++ "0" else n1

jsonEncode :: Json -> B.ByteString
jsonEncode v = BC.pack (enc v)
  where
    enc j = case j of
      JStr s -> '"' : concatMap esc s ++ "\""
      JBool b -> if b then "true" else "false"
      JNum d -> if d == fromIntegral (round d :: Int) then show (round d :: Int) else show d
      JNull -> "null"
      JObj kvs -> "{" ++ commas [ enc (JStr k) ++ ":" ++ enc x | (k, x) <- kvs ] ++ "}"
      JArr xs -> "[" ++ commas (map enc xs) ++ "]"
    commas = foldr (\a b -> if null b then a else a ++ "," ++ b) ""
    esc c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ -> [c]

-- | Look up a key in an object.
jsonLookup :: String -> Json -> Maybe Json
jsonLookup k (JObj kvs) = lookup k kvs
jsonLookup _ _ = Nothing

-- | A string field of an object, or the empty string.
jsonString :: String -> Json -> String
jsonString k j = case jsonLookup k j of
  Just (JStr s) -> s
  _ -> ""

-- | An integral field of an object.
jsonInt :: String -> Json -> Maybe Int
jsonInt k j = case jsonLookup k j of
  Just (JNum d) -> Just (round d)
  Just (JStr s) -> case reads s of { [(n, "")] -> Just n; _ -> Nothing }
  _ -> Nothing
