-- | Minimal telnet (RFC 854) codec for an 8-bit clean byte pipe: strips
-- and answers option negotiation, unescapes IAC IAC, and escapes 0xFF
-- on the way out.  BINARY (RFC 856) and SUPPRESS-GO-AHEAD (RFC 858) are
-- accepted in both directions; everything else is refused.
module Modec.Telnet
  ( TelnetState
  , telnetInit
  , telnetHello
  , telnetDecode
  , telnetEncode
  , iac, will, wont, doOpt, dont, optBinary, optSga
  ) where

import qualified Data.ByteString as B
import Data.Word (Word8)

iac, will, wont, doOpt, dont, sb, se, optBinary, optSga :: Word8
iac = 255
dont = 254
doOpt = 253
wont = 252
will = 251
sb = 250
se = 240
optBinary = 0
optSga = 3

data Mode = Normal | SawIac | SawCmd !Word8 | InSub | InSubIac

data TelnetState = TelnetState
  { tsMode   :: Mode
  , tsLocal  :: [Word8]   -- ^ options we have offered or agreed to provide
  , tsRemote :: [Word8]   -- ^ options the peer provides
  }

telnetInit :: TelnetState
telnetInit = TelnetState Normal [] []

supported :: Word8 -> Bool
supported o = o == optBinary || o == optSga

-- | Opening negotiation: offer and request BINARY and SGA.  Returns the
-- bytes to send and the state that remembers the offers.
telnetHello :: TelnetState -> (TelnetState, B.ByteString)
telnetHello st =
  ( st { tsLocal = [optBinary, optSga], tsRemote = [optBinary, optSga] }
  , B.pack (concat [ [iac, will, optBinary], [iac, doOpt, optBinary], [iac, will, optSga], [iac, doOpt, optSga] ]) )

-- | Decode inbound bytes: returns the new state, the payload bytes and
-- any negotiation replies to send back.
telnetDecode :: TelnetState -> B.ByteString -> (TelnetState, B.ByteString, B.ByteString)
telnetDecode st0 input = go st0 (B.unpack input) [] []
  where
    go st [] out reply = (st, B.pack (reverse out), B.pack (reverse reply))
    go st (b : bs) out reply = case tsMode st of
      Normal
        | b == iac -> go st { tsMode = SawIac } bs out reply
        | otherwise -> go st bs (b : out) reply
      SawIac
        | b == iac -> go st { tsMode = Normal } bs (b : out) reply
        | b == sb -> go st { tsMode = InSub } bs out reply
        | b `elem` [will, wont, doOpt, dont] -> go st { tsMode = SawCmd b } bs out reply
        | otherwise -> go st { tsMode = Normal } bs out reply   -- NOP, AYT, GA, ...: swallow
      SawCmd cmd ->
        let (st', r) = negotiate st cmd b
        in go st' { tsMode = Normal } bs out (reverse r ++ reply)
      InSub
        | b == iac -> go st { tsMode = InSubIac } bs out reply
        | otherwise -> go st bs out reply
      InSubIac
        | b == se -> go st { tsMode = Normal } bs out reply
        | otherwise -> go st { tsMode = InSub } bs out reply

    negotiate st cmd opt
      | cmd == doOpt =
          if supported opt
            then if opt `elem` tsLocal st then (st, []) else (st { tsLocal = opt : tsLocal st }, [iac, will, opt])
            else (st, [iac, wont, opt])
      | cmd == will =
          if supported opt
            then if opt `elem` tsRemote st then (st, []) else (st { tsRemote = opt : tsRemote st }, [iac, doOpt, opt])
            else (st, [iac, dont, opt])
      | cmd == dont =
          if opt `elem` tsLocal st then (st { tsLocal = filter (/= opt) (tsLocal st) }, [iac, wont, opt]) else (st, [])
      | otherwise =  -- wont
          if opt `elem` tsRemote st then (st { tsRemote = filter (/= opt) (tsRemote st) }, [iac, dont, opt]) else (st, [])

-- | Escape payload for the wire.
telnetEncode :: B.ByteString -> B.ByteString
telnetEncode = B.concatMap (\b -> if b == iac then B.pack [iac, iac] else B.singleton b)
