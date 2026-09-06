-- | A small Hayes AT command interpreter: enough for terminal programs
-- and BBS software to dial, answer, escape and hang up.  Pure: bytes from
-- the DTE go in, response bytes and modem actions come out.
--
-- Supported: AT, ATA, ATD<string> (any dial modifiers; digits are
-- dialled as DTMF), ATH, ATO, ATZ, AT&F, ATE0/1, ATV0/1, ATQ0/1, ATI,
-- ATS0=n / ATS0? (auto-answer; the modem answers on a detected calling
-- signal when non-zero), ATX<n> and AT&C/&D/&K/&W accepted and ignored,
-- and the "+++" escape with a one second guard time on either side.
-- Result codes: OK, CONNECT <rate>, RING, NO CARRIER, ERROR, NO ANSWER,
-- BUSY (numeric equivalents with ATV0).
module Modec.Hayes
  ( HayesState
  , hayesInit
  , HayesAction (..)
  , HayesEvent (..)
  , hayesInput
  , hayesEvent
  , hayesTick
  , hayesOnline
  , hayesAutoAnswer
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, toUpper)

data Mode = Command | Online deriving (Eq, Show)

data HayesState = HayesState
  { hsMode      :: !Mode
  , hsLine      :: !B.ByteString   -- ^ command line being typed
  , hsEcho      :: !Bool
  , hsVerbose   :: !Bool
  , hsQuiet     :: !Bool
  , hsS0        :: !Int
  , hsConnected :: !Bool
  , hsPlusCount :: !Int            -- ^ "+" characters seen so far in an escape attempt
  , hsPlusAt    :: !Double         -- ^ time of the last "+"
  , hsLastData  :: !Double         -- ^ time of the last non-escape data byte
  , hsT         :: !Double
  }

hayesInit :: HayesState
hayesInit = HayesState Command B.empty True True False 0 False 0 (-10) (-10) 0

-- | What the modem should do.
data HayesAction
  = ActDial String      -- ^ dial (digits are sent as DTMF) then originate
  | ActAnswer
  | ActHangup
  | ActOnline           -- ^ return to data mode
  deriving (Eq, Show)

-- | What the modem reports.
data HayesEvent
  = EvConnect Int       -- ^ connected at this bit rate
  | EvNoCarrier
  | EvNoAnswer
  | EvRing              -- ^ a calling signal was detected while idle
  | EvProtocol String   -- ^ an error-correcting protocol came up, named here
  deriving (Eq, Show)

hayesOnline :: HayesState -> Bool
hayesOnline st = hsMode st == Online

hayesAutoAnswer :: HayesState -> Bool
hayesAutoAnswer st = hsS0 st > 0

crlf :: B.ByteString
crlf = BC.pack "\r\n"

result :: HayesState -> String -> Int -> B.ByteString
result st verbose numeric
  | hsQuiet st = B.empty
  | hsVerbose st = crlf <> BC.pack verbose <> crlf
  | otherwise = BC.pack (show numeric) <> BC.pack "\r"

ok, err :: HayesState -> B.ByteString
ok st = result st "OK" 0
err st = result st "ERROR" 4

-- | Bytes from the DTE at time @t@.  Returns the new state, bytes to send
-- back to the DTE, bytes to pass to the modem (online mode), and actions.
hayesInput :: Double -> HayesState -> B.ByteString -> (HayesState, B.ByteString, B.ByteString, [HayesAction])
hayesInput t st0 input = go (st0 { hsT = t }) (B.unpack input) B.empty B.empty []
  where
    go st [] back fwd acts = (st, back, fwd, acts)
    go st (b : bs) back fwd acts = case hsMode st of
      Online ->
        -- escape sequence: guard, +++, guard
        let c = toEnum (fromIntegral b) :: Char
            guardOk = t - hsLastData st >= 1
        in if c == '+' && (hsPlusCount st > 0 || guardOk) && hsPlusCount st < 3
             then let n = hsPlusCount st + 1
                      st' = st { hsPlusCount = n, hsPlusAt = t }
                  in if n == 3
                       then go st' bs back fwd acts   -- wait for the trailing guard in 'hayesTick'
                       else go st' bs back fwd acts
             else
               -- not an escape: flush any withheld pluses as data
               let held = B.replicate (hsPlusCount st) (fromIntegral (fromEnum '+'))
               in go st { hsPlusCount = 0, hsLastData = t } bs back (fwd <> held <> B.singleton b) acts
      Command ->
        let c = toUpper (toEnum (fromIntegral b) :: Char)
            echoed = if hsEcho st then B.singleton b else B.empty
        in case c of
             '\r' ->
               let (st', out, acts') = execute st { hsLine = B.empty } (BC.unpack (hsLine st))
               in go st' bs (back <> echoed <> out) fwd (acts ++ acts')
             '\n' -> go st bs back fwd acts
             '\b' -> go st { hsLine = B.take (B.length (hsLine st) - 1) (hsLine st) } bs (back <> echoed) fwd acts
             '\DEL' -> go st { hsLine = B.take (B.length (hsLine st) - 1) (hsLine st) } bs (back <> echoed) fwd acts
             _ -> go st { hsLine = hsLine st <> B.singleton (fromIntegral (fromEnum c)) } bs (back <> echoed) fwd acts

-- | Execute one command line.
execute :: HayesState -> String -> (HayesState, B.ByteString, [HayesAction])
execute st line0 =
  let line = filter (/= ' ') line0
  in case line of
       "" -> (st, B.empty, [])
       ('A' : 'T' : cmds) -> commands st cmds [] B.empty
       ('A' : '/' : _) -> (st, err st, [])
       _ -> (st, err st, [])

commands :: HayesState -> String -> [HayesAction] -> B.ByteString -> (HayesState, B.ByteString, [HayesAction])
commands st cmds acts out = case cmds of
  [] -> (st, out <> ok st, acts)
  ('D' : rest) ->
    -- dial: the whole remainder is the dial string; the modem reports CONNECT or NO CARRIER
    let dialStr = filter (/= ';') rest
    in (st, out, acts ++ [ActDial dialStr])
  ('A' : rest) -> commands st rest (acts ++ [ActAnswer]) out
  ('H' : rest) -> let (_, rest') = digits rest
                  in commands st { hsMode = Command, hsConnected = False } rest' (acts ++ [ActHangup]) out
  ('O' : rest) -> let (_, rest') = digits rest
                  in if hsConnected st then (st { hsMode = Online }, out, acts ++ [ActOnline]) else (st, out <> err st, acts)
  ('Z' : rest) -> let (_, rest') = digits rest in commands (reset st) rest' (acts ++ [ActHangup]) out
  ('&' : 'F' : rest) -> let (_, rest') = digits rest in commands (reset st) rest' acts out
  ('&' : _ : rest) -> let (_, rest') = digits rest in commands st rest' acts out
  ('E' : rest) -> let (n, rest') = digits rest in commands st { hsEcho = n /= 0 } rest' acts out
  ('V' : rest) -> let (n, rest') = digits rest in commands st { hsVerbose = n /= 0 } rest' acts out
  ('Q' : rest) -> let (n, rest') = digits rest in commands st { hsQuiet = n /= 0 } rest' acts out
  ('X' : rest) -> let (_, rest') = digits rest in commands st rest' acts out
  ('L' : rest) -> let (_, rest') = digits rest in commands st rest' acts out
  ('M' : rest) -> let (_, rest') = digits rest in commands st rest' acts out
  ('I' : rest) -> let (_, rest') = digits rest
                  in commands st rest' acts (out <> crlf <> BC.pack "modec software modem: Bell 103, V.21, V.22, V.22bis, V.8bis" <> crlf)
  ('S' : rest) ->
    let (reg, rest1) = digits rest
    in case rest1 of
         ('=' : rest2) -> let (v, rest3) = digits rest2
                          in if reg == 0 then commands st { hsS0 = v } rest3 acts out else commands st rest3 acts out
         ('?' : rest2) -> commands st rest2 acts (out <> crlf <> BC.pack (pad3 (if reg == 0 then hsS0 st else 0)) <> crlf)
         _ -> (st, out <> err st, acts)
  _ -> (st, out <> err st, acts)
  where
    digits s = let (ds, rest) = span isDigit s in (if null ds then 0 else read ds, rest)
    pad3 n = let s = show n in replicate (3 - length s) '0' ++ s
    reset s = s { hsEcho = True, hsVerbose = True, hsQuiet = False, hsS0 = 0, hsMode = Command, hsConnected = False }

-- | Modem events (connect, loss of carrier, incoming call) to DTE bytes.
hayesEvent :: HayesState -> HayesEvent -> (HayesState, B.ByteString)
hayesEvent st ev = case ev of
  EvConnect rate -> (st { hsMode = Online, hsConnected = True, hsLastData = hsT st, hsPlusCount = 0 }, result st ("CONNECT " ++ show rate) (connectCode rate))
  EvNoCarrier -> (st { hsMode = Command, hsConnected = False }, result st "NO CARRIER" 3)
  EvNoAnswer -> (st { hsMode = Command, hsConnected = False }, result st "NO ANSWER" 8)
  EvRing -> (st, result st "RING" 2)
  -- Reported the way the modems that spoke this protocol reported it, on
  -- its own line after CONNECT.  There is no numeric result code for it,
  -- so a terminal in numeric mode hears nothing, which is what those
  -- modems did too.
  EvProtocol name
    | hsQuiet st || not (hsVerbose st) -> (st, B.empty)
    | otherwise -> (st, crlf <> BC.pack ("PROTOCOL: " ++ name) <> crlf)
  where
    connectCode r = case r of { 300 -> 1; 1200 -> 5; 2400 -> 10; _ -> 1 }

-- | Time passes: completes a "+++" escape once the trailing guard time
-- has elapsed.  Returns bytes for the DTE.
hayesTick :: Double -> HayesState -> (HayesState, B.ByteString)
hayesTick t st
  | hsMode st == Online && hsPlusCount st == 3 && t - hsPlusAt st >= 1 =
      (st { hsMode = Command, hsPlusCount = 0, hsT = t }, ok st)
  | hsMode st == Online && hsPlusCount st > 0 && hsPlusCount st < 3 && t - hsPlusAt st >= 1 =
      -- fewer than three pluses: they were data after all (delivered late)
      (st { hsPlusCount = 0, hsT = t }, B.empty)
  | otherwise = (st { hsT = t }, B.empty)
