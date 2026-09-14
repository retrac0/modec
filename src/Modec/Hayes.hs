-- | A Hayes AT command interpreter, in the Rockwell dialect most terminal
-- programs and BBS packages were written against.  Pure: bytes from the
-- DTE go in, response bytes and modem actions come out.
--
-- Everything the command set can change lives in a 'HayesProfile'.  The
-- profile the interpreter starts with is its factory profile -- in the
-- executable, whatever the command line said -- so @AT&F@ restores the
-- command line's settings rather than some other modem's.  @AT&W@ keeps
-- a copy that @ATZ@ goes back to; it lives as long as the process.
--
-- Commands:
--
-- * Calls: @D@ (dial string: digits, @T@ @P@ @W@ @,@ @!@ modifiers, a
--   trailing @;@ ignored; @DL@ redials, @DS=n@ dials stored number n;
--   anything with an \@ or a @sip:@ scheme is a SIP address), @A@, @H0@
--   (@H1@ accepted), @O@ (back on line, repeating the CONNECT), @A/@
--   repeats the last command line.
-- * Result codes: @E@ @V@ @Q@, @X0@-@X4@ (X0 plain CONNECT; below X3 a
--   busy line reports NO CARRIER and is not detected; NO DIALTONE is never
--   reported, because the modem dials blind), @W@ accepted.
-- * Modulation: @+MS=carrier,automode,min,max[,min rx,max rx]@ with the
--   carriers B103 B212 V21 V22 V22B V23C V32 V32B, @+MS?@ and @+MS=?@;
--   @B0@/@B1@ prefers the ITU or the Bell modulation of a pair; @N0@
--   connects only at the first carrier, @N1@ steps down.
-- * Error control: @\\N0@/@\\N1@ none, @\\N2@ @\\N3@ @\\N5@ MNP (class 4
--   and below; with no answer the call runs unprotected), @\\N4@ (LAPM)
--   is an error.  @%C@ is stored; there is no compression.
-- * Profile: @Z@, @&F@, @&W@, @&Y@ (accepted), @&V@, @&Z n=@, @&C@, @&D@
--   (@&D0@ ignore DTR, @&D1@ command mode, @&D2@ hang up, @&D3@ hang up
--   and reset), @&K@ (stored: nothing here can pause a modem), and the
--   remaining @&@, @\\@ and @%@ commands accepted and ignored.
-- * S-registers: @Sn=v@ and @Sn?@ for 0 to 255; every one reads back what
--   was written.  The ones that do something: S0 rings to answer, S1 ring
--   count, S2 escape character, S3 carriage return, S4 line feed, S5
--   backspace, S6 seconds before blind dialling, S7 seconds to wait for
--   carrier, S8 seconds per comma, S10 tenths of a second of lost carrier
--   before hanging up, S11 milliseconds per DTMF digit, S12 escape guard
--   time in fiftieths of a second.
-- * Identification: @I0@-@I9@, @+GMI@ @+GMM@ @+GMR@ @+GCAP@, @+FCLASS@
--   (data only).
--
-- Result codes: OK, CONNECT [rate], RING, NO CARRIER, ERROR, NO ANSWER,
-- BUSY (numeric equivalents with ATV0).  BUSY is reported for anything
-- the network plays back to refuse a call -- a busy tone, congestion,
-- or the special information tone before a recorded announcement --
-- which is the distinction a Hayes modem draws and all the result
-- codes have room for; the log says which of the three it was.
module Modec.Hayes
  ( -- * The profile
    HayesProfile (..)
  , V32Offer (..)
  , defaultProfile
  , defaultRegisters
  , hayesReg
  , hayesModes
  , hayesLadder
    -- * The interpreter
  , HayesState
  , hayesInit
  , hayesProfile
  , HayesAction (..)
  , HayesEvent (..)
  , hayesInput
  , hayesEvent
  , hayesTick
  , hayesDtr
  , hayesDtrDrop
  , hayesOnline
  , hayesAutoAnswer
  , hayesDetectsBusy
  , hayesLastRate
    -- * Dial strings
  , DialTarget (..)
  , dialTarget
  , parsePhonebook
  , phonebookLookup
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, isSpace, toLower, toUpper)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Vector.Unboxed as VU

import Modec.Link (V32Rate (..))
import Modec.Standards (Standard (..))

-- | Which V.32 rates a call offers.
data V32Offer
  = V32ByModes          -- ^ what the mode list implies (V.32 or V.32bis rates)
  | V32Pin V32Rate      -- ^ exactly this rate (@--v32-rate@)
  | V32Range Int Int    -- ^ the rates between these bit rates, inclusive (@+MS@)
  deriving (Eq, Show)

-- | Every setting the command set can change.
data HayesProfile = HayesProfile
  { hpEcho        :: !Bool
  , hpVerbose     :: !Bool
  , hpQuiet       :: !Bool
  , hpX           :: !Int            -- ^ result code set, 0 to 4
  , hpDcd         :: !Int            -- ^ &C (stored; a pty has no DCD line)
  , hpDtr         :: !Int            -- ^ &D
  , hpFlow        :: !Int            -- ^ &K (stored)
  , hpCompression :: !Int            -- ^ %C (stored)
  , hpBell        :: !Bool           -- ^ B1: Bell 212A and Bell 103 before V.22 and V.21
  , hpAutomode    :: !Bool           -- ^ N1: step down through the modes
  , hpModes       :: [Standard]      -- ^ best first, the carrier first
  , hpRange       :: Maybe (Int, Int) -- ^ the rates +MS last asked for
  , hpV32         :: V32Offer
  , hpMnp         :: Maybe Int       -- ^ highest MNP class, if error control is on
  , hpRegs        :: VU.Vector Int   -- ^ S0 to S255
  , hpProduct     :: String          -- ^ ATI0, +GMM
  , hpVersion     :: String          -- ^ ATI3, +GMR
  } deriving (Eq, Show)

-- | S-register defaults.  Where a Rockwell modem's default differs from
-- how modec has always behaved, modec's is kept: S7 45 is the handshake
-- timeout it has always had, S10 5 its half-second carrier-loss window,
-- S11 80 its DTMF digit.
defaultRegisters :: VU.Vector Int
defaultRegisters = VU.replicate 256 0 VU.// defaults
  where
    defaults = [ (2, 43), (3, 13), (4, 10), (5, 8), (6, 2), (7, 45), (8, 2)
               , (9, 6), (10, 5), (11, 80), (12, 50), (25, 5), (38, 20) ]

defaultProfile :: HayesProfile
defaultProfile = HayesProfile
  { hpEcho = True, hpVerbose = True, hpQuiet = False, hpX = 4
  , hpDcd = 1, hpDtr = 2, hpFlow = 3, hpCompression = 0
  , hpBell = False, hpAutomode = True
  , hpModes = [V22bis, V22, Bell212A, V21, Bell103], hpRange = Nothing
  , hpV32 = V32ByModes, hpMnp = Nothing
  , hpRegs = defaultRegisters, hpProduct = "modec", hpVersion = "" }

hayesReg :: HayesProfile -> Int -> Int
hayesReg p n = maybe 0 id (hpRegs p VU.!? n)

setReg :: Int -> Int -> HayesProfile -> HayesProfile
setReg n v p = p { hpRegs = hpRegs p VU.// [(n, v)] }

-- | Every modulation, best first, the way @+MS@ automode steps down.
-- V.23 is left out: it is reached only by naming it.
hayesLadder :: [Standard]
hayesLadder = [V32bis, V32, V22bis, V22, Bell212A, V21, Bell103]

-- | The modes a call is placed with: ATB's preference applied, and only
-- the first with ATN0.
hayesModes :: HayesProfile -> [Standard]
hayesModes p =
  let ms = if hpBell p then prefer Bell212A V22 (prefer Bell103 V21 (hpModes p)) else hpModes p
  in if hpAutomode p then ms else take 1 ms
  where
    -- @a@ just before @b@, where the pair first appears, when both are there
    prefer a b ms
      | a `elem` ms && b `elem` ms =
          let i = length (takeWhile (`notElem` [a, b]) ms)
              rest = filter (`notElem` [a, b]) ms
          in take i rest ++ [a, b] ++ drop i rest
      | otherwise = ms

data Mode = Command | Online deriving (Eq, Show)

data HayesState = HayesState
  { hsMode      :: !Mode
  , hsLine      :: !B.ByteString   -- ^ command line being typed, as typed
  , hsActive    :: !HayesProfile
  , hsStored    :: !HayesProfile   -- ^ &W
  , hsFactory   :: !HayesProfile   -- ^ &F
  , hsNumbers   :: [String]        -- ^ &Z0 to &Z3
  , hsLastLine  :: String          -- ^ for A/
  , hsLastDial  :: Maybe String    -- ^ for DL
  , hsLastRate  :: Maybe Int
  , hsRingAt    :: !Double
  , hsDtr       :: !Bool
  , hsConnected :: !Bool
  , hsPlusCount :: !Int            -- ^ escape characters seen so far in an escape attempt
  , hsPlusAt    :: !Double         -- ^ time of the last one
  , hsLastData  :: !Double         -- ^ time of the last non-escape data byte
  , hsT         :: !Double
  }

-- | An interpreter whose factory profile is this one.
hayesInit :: HayesProfile -> HayesState
hayesInit p = HayesState
  { hsMode = Command, hsLine = B.empty, hsActive = p, hsStored = p, hsFactory = p
  , hsNumbers = replicate 4 "", hsLastLine = "", hsLastDial = Nothing, hsLastRate = Nothing
  , hsRingAt = -100, hsDtr = True, hsConnected = False
  , hsPlusCount = 0, hsPlusAt = -10, hsLastData = -10, hsT = 0 }

-- | The settings in force.
hayesProfile :: HayesState -> HayesProfile
hayesProfile = hsActive

-- | What the modem should do.
data HayesAction
  = ActDial String      -- ^ dial this string as typed, then originate
  | ActAnswer
  | ActHangup
  | ActOnline           -- ^ return to data mode
  deriving (Eq, Show)

-- | What the modem reports.
data HayesEvent
  = EvConnect Int       -- ^ connected at this bit rate
  | EvNoCarrier
  | EvNoAnswer
  | EvBusy              -- ^ the far end returned busy, congestion, or a special information tone
  | EvRing              -- ^ a calling signal was detected while idle
  | EvProtocol String   -- ^ an error-correcting protocol came up, named here
  deriving (Eq, Show)

hayesOnline :: HayesState -> Bool
hayesOnline st = hsMode st == Online

hayesAutoAnswer :: HayesState -> Bool
hayesAutoAnswer st = reg st 0 > 0

-- | Whether a call refused by the network is noticed and hung up: X3 and X4.
hayesDetectsBusy :: HayesState -> Bool
hayesDetectsBusy st = hpX (hsActive st) >= 3

-- | The rate of the last connection, for ATO.
hayesLastRate :: HayesState -> Maybe Int
hayesLastRate = hsLastRate

-- | The DTE's DTR, as far as the transport can tell.  While it is low the
-- modem does not answer, unless &D0 says to ignore DTR.
hayesDtr :: Bool -> HayesState -> HayesState
hayesDtr b st = st { hsDtr = b }

-- | DTR went from on to off: what &D says to do about it.
hayesDtrDrop :: HayesState -> (HayesState, B.ByteString, [HayesAction])
hayesDtrDrop st = case hpDtr (hsActive st) of
  0 -> (st, B.empty, [])
  1 | hsMode st == Online -> (st { hsMode = Command, hsPlusCount = 0 }, ok st, [])
    | otherwise -> (st, B.empty, [])
  2 -> (hungUp st, B.empty, [ActHangup])
  _ -> (hungUp (restore (hsStored st) st), B.empty, [ActHangup])
  where hungUp s = s { hsMode = Command, hsConnected = False, hsPlusCount = 0 }

reg :: HayesState -> Int -> Int
reg st = hayesReg (hsActive st)

chr8 :: Int -> B.ByteString
chr8 = B.singleton . fromIntegral

-- | End of a response line, per S3 and S4.
eol :: HayesState -> B.ByteString
eol st = chr8 (reg st 3) <> chr8 (reg st 4)

-- | A line of information text.
info :: HayesState -> String -> B.ByteString
info st s = eol st <> BC.pack s <> eol st

result :: HayesState -> String -> Int -> B.ByteString
result st verbose numeric
  | hpQuiet p = B.empty
  | hpVerbose p = eol st <> BC.pack verbose <> eol st
  | otherwise = BC.pack (show numeric) <> chr8 (reg st 3)
  where p = hsActive st

ok, err :: HayesState -> B.ByteString
ok st = result st "OK" 0
err st = result st "ERROR" 4

connectResult :: HayesState -> Int -> B.ByteString
connectResult st rate
  | hpX (hsActive st) == 0 = result st "CONNECT" 1
  | otherwise = result st ("CONNECT " ++ show rate) (connectCode rate)
  where
    connectCode r = case r of
      { 300 -> 1; 1200 -> 5; 2400 -> 10; 4800 -> 11; 9600 -> 12
      ; 7200 -> 13; 12000 -> 14; 14400 -> 15; _ -> 1 }

-- | Bytes from the DTE at time @t@.  Returns the new state, bytes to send
-- back to the DTE, bytes to pass to the modem (online mode), and actions.
hayesInput :: Double -> HayesState -> B.ByteString -> (HayesState, B.ByteString, B.ByteString, [HayesAction])
hayesInput t st0 input = go (st0 { hsT = t }) (B.unpack input) B.empty B.empty []
  where
    go st [] back fwd acts = (st, back, fwd, acts)
    go st (b : bs) back fwd acts = case hsMode st of
      Online ->
        -- escape sequence: guard, three escape characters, guard
        let escChar = reg st 2
            guardOk = t - hsLastData st >= guardTime st
        in if escChar <= 127 && fromIntegral b == escChar
                && (hsPlusCount st > 0 || guardOk) && hsPlusCount st < 3
             then go st { hsPlusCount = hsPlusCount st + 1, hsPlusAt = t } bs back fwd acts
             else
               -- not an escape: flush any withheld escape characters as data
               let held = B.replicate (hsPlusCount st) (fromIntegral escChar)
               in go st { hsPlusCount = 0, hsLastData = t } bs back (fwd <> held <> B.singleton b) acts
      Command
        | fromIntegral b == reg st 3 ->
            let line = BC.unpack (hsLine st)
                (st', out, acts') = execute st { hsLine = B.empty } line
            in go st' bs (back <> echoed <> out) fwd (acts ++ acts')
        | b == 10 -> go st bs back fwd acts
        | fromIntegral b == reg st 5 || b == 127 ->
            go st { hsLine = B.take (B.length (hsLine st) - 1) (hsLine st) } bs (back <> echoed) fwd acts
        | b == 47 && map toUpper (BC.unpack (hsLine st)) == "A" ->
            -- A/ runs at once, without waiting for a carriage return
            let (st', out, acts') = execute st { hsLine = B.empty } (hsLastLine st)
                st'' = st' { hsLastLine = hsLastLine st }
            in go st'' bs (back <> echoed <> out) fwd (acts ++ acts')
        | B.length (hsLine st) >= 255 -> go st bs back fwd acts
        | otherwise -> go st { hsLine = hsLine st `B.snoc` b } bs (back <> echoed) fwd acts
      where echoed = if hpEcho (hsActive st) then B.singleton b else B.empty

guardTime :: HayesState -> Double
guardTime st = fromIntegral (reg st 12) / 50

-- | Execute one command line, as typed.
execute :: HayesState -> String -> (HayesState, B.ByteString, [HayesAction])
execute st line0 =
  let line = dropWhile isSpace line0
  in case line of
       "" -> (st, B.empty, [])
       (a : t : cmds) | toUpper a == 'A' && toUpper t == 'T' ->
         commands st { hsLastLine = line } cmds [] B.empty
       _ -> (st, err st, [])

restore :: HayesProfile -> HayesState -> HayesState
restore p st = st { hsActive = p { hpRegs = hpRegs p VU.// [(1, 0)] } }

commands :: HayesState -> String -> [HayesAction] -> B.ByteString -> (HayesState, B.ByteString, [HayesAction])
commands st cmds acts out = case cmds of
  [] -> (st, out <> ok st, acts)
  (' ' : rest) -> commands st rest acts out
  (c0 : rest0) -> case toUpper c0 of
    'D' -> dial rest0
    'A' -> (st, out, acts ++ [ActAnswer])        -- the rest of the line is ignored, as on the modems
    'B' -> num rest0 $ \n -> if n > 1 then Nothing else Just (prof (\p -> p { hpBell = n == 1 }))
    'E' -> num rest0 $ \n -> Just (prof (\p -> p { hpEcho = n /= 0 }))
    'V' -> num rest0 $ \n -> Just (prof (\p -> p { hpVerbose = n /= 0 }))
    'Q' -> num rest0 $ \n -> Just (prof (\p -> p { hpQuiet = n /= 0 }))
    'X' -> num rest0 $ \n -> if n > 4 then Nothing else Just (prof (\p -> p { hpX = n }))
    'N' -> num rest0 $ \n -> if n > 1 then Nothing else Just (prof (\p -> p { hpAutomode = n == 1 }))
    'H' -> num rest0 $ \n -> case n of
             0 -> Just (st { hsMode = Command, hsConnected = False }, B.empty, [ActHangup])
             1 -> Just (st, B.empty, [])
             _ -> Nothing
    'O' | hsConnected st ->
            let rate = maybe 300 id (hsLastRate st)
            in (st { hsMode = Online, hsLastData = hsT st, hsPlusCount = 0 }, out <> connectResult st rate, acts ++ [ActOnline])
        | otherwise -> (st, out <> err st, acts)
    'Z' -> num rest0 $ \_ -> Just (restore (hsStored st) st { hsMode = Command, hsConnected = False }, B.empty, [ActHangup])
    'I' -> num rest0 $ \n -> Just (st, identify n, [])
    'L' -> num rest0 accept
    'M' -> num rest0 accept
    'P' -> num rest0 accept
    'T' -> num rest0 accept
    'W' -> num rest0 accept
    'Y' -> num rest0 accept
    'S' -> register rest0
    '&' -> ampersand rest0
    '\\' -> case rest0 of
      (c : r) | toUpper c == 'N' -> num r $ \n -> case n of
                  _ | n <= 1 -> Just (prof (\p -> p { hpMnp = Nothing }))
                    | n == 4 || n > 5 -> Nothing
                    | otherwise -> Just (prof (\p -> p { hpMnp = Just (maybe 4 id (hpMnp p)) }))
              | isLetter c -> num r accept
      _ -> bad
    '%' -> case rest0 of
      (c : r) | toUpper c == 'C' -> num r $ \n -> if n > 3 then Nothing else Just (prof (\p -> p { hpCompression = n }))
              | isLetter c -> num r accept
      _ -> bad
    '+' -> extended rest0
    _ -> bad
  where
    p0 = hsActive st
    bad = (st, out <> err st, acts)
    isLetter c = toUpper c >= 'A' && toUpper c <= 'Z'
    accept _ = Just (st, B.empty, [])
    prof f = (st { hsActive = f p0 }, B.empty, [])
    -- a command with an optional number: run it, then carry on along the line
    num :: String -> (Int -> Maybe (HayesState, B.ByteString, [HayesAction])) -> (HayesState, B.ByteString, [HayesAction])
    num s f = let (n, rest) = digits s
              in case f n of
                   Nothing -> bad
                   Just (st', o, as) -> commands st' rest (acts ++ as) (out <> o)

    -- ATD: the rest of the line is the dial string
    dial s0 =
      let s = filter (/= ';') (dropWhile (== ' ') s0)
          upper = map toUpper (filter (/= ' ') s)
          go' n = (st { hsLastDial = Just n }, out, acts ++ [ActDial n])
      in case upper of
           "L" -> maybe bad go' (hsLastDial st)
           ('S' : '=' : ds) | all isDigit ds, not (null ds) ->
             let i = read (take 3 ds) :: Int
             in if i < length (hsNumbers st) && not (null (hsNumbers st !! i))
                  then go' (hsNumbers st !! i) else bad
           _ -> go' (filter (/= ' ') s)

    register s =
      let (r, rest1) = digits s
      in if r > 255 then bad else case rest1 of
           ('=' : rest2) -> let (v, rest3) = digits rest2
                            in if v > 255 then bad
                               else commands st { hsActive = setReg r v p0 } rest3 acts out
           ('?' : rest2) -> commands st rest2 acts (out <> info st (pad3 (hayesReg p0 r)))
           _ -> bad

    ampersand s = case s of
      (c : r) -> case toUpper c of
        'F' -> num r $ \_ -> Just (restore (hsFactory st) st, B.empty, [])
        'W' -> num r $ \_ -> Just (st { hsStored = p0 }, B.empty, [])
        'V' -> num r $ \_ -> Just (st, view, [])
        'C' -> num r $ \n -> if n > 1 then Nothing else Just (prof (\p -> p { hpDcd = n }))
        'D' -> num r $ \n -> if n > 3 then Nothing else Just (prof (\p -> p { hpDtr = n }))
        'K' -> num r $ \n -> if n > 6 then Nothing else Just (prof (\p -> p { hpFlow = n }))
        'Z' -> let (i, r1) = digits r
               in case r1 of
                    ('=' : number) | i < 4 ->
                      let nums = hsNumbers st
                          number' = filter (/= ' ') number
                      in (st { hsNumbers = take i nums ++ [number'] ++ drop (i + 1) nums }, out <> ok st, acts)
                    ('?' : _) | i < 4 -> (st, out <> info st (hsNumbers st !! i) <> ok st, acts)
                    _ -> bad
        _ | isLetter c -> num r accept
        _ -> bad
      [] -> bad

    -- +NAME, +NAME?, +NAME=?, +NAME=params; the rest of the line, or up to a ';'
    extended s =
      let (body, rest) = break (== ';') s
          (name, arg) = span (\ch -> isLetter ch || isDigit ch) body
          continue (st', o) = commands st' (drop 1 rest) acts (out <> o)
          fail' = bad
      in case (map toUpper name, arg) of
           ("MS", "?") -> continue (st, info st ("+MS: " ++ msRead p0))
           ("MS", "=?") -> continue (st, info st "+MS: (B103,B212,V21,V22,V22B,V23C,V32,V32B),(0,1),(300-14400),(300-14400),(300-14400),(300-14400)")
           ("MS", '=' : params) -> maybe fail' (\p -> continue (st { hsActive = p }, B.empty)) (msSet p0 params)
           ("FCLASS", "?") -> continue (st, info st "0")
           ("FCLASS", "=?") -> continue (st, info st "0")
           ("FCLASS", "=0") -> continue (st, B.empty)
           ("GMI", "") -> continue (st, info st "modec")
           ("GMM", "") -> continue (st, info st (hpProduct p0 ++ " software modem"))
           ("GMR", "") -> continue (st, info st (hpVersion p0))
           ("GCAP", "") -> continue (st, info st "+GCAP: +MS")
           _ -> fail'

    identify n = case n of
      0 -> info st (hpProduct p0)
      3 -> info st (unwords (filter (not . null) [hpProduct p0, hpVersion p0]))
      4 -> info st "modec software modem: Bell 103, V.21, V.23, Bell 212A, V.22, V.22bis, V.32, V.32bis; MNP 2-4"
      _ -> B.empty

    view = eol st <> BC.pack (intercalate (BC.unpack (eol st))
      ([ "ACTIVE PROFILE:" ] ++ profileLines p0 ++ [ "", "STORED PROFILE 0:" ] ++ profileLines (hsStored st)
       ++ [ "", "TELEPHONE NUMBERS:" ]
       ++ [ show i ++ "=" ++ (hsNumbers st !! i) | i <- [0 .. 3] ])) <> eol st

    profileLines p =
      [ unwords [ "B" ++ b (hpBell p), "E" ++ b (hpEcho p), "N" ++ b (hpAutomode p), "Q" ++ b (hpQuiet p)
                , "V" ++ b (hpVerbose p), "X" ++ show (hpX p), "&C" ++ show (hpDcd p), "&D" ++ show (hpDtr p)
                , "&K" ++ show (hpFlow p), "\\N" ++ maybe "0" (const "3") (hpMnp p), "%C" ++ show (hpCompression p) ]
      , "+MS=" ++ msRead p ]
      ++ chunks 8 [ "S" ++ pad2 r ++ ":" ++ pad3 (hayesReg p r) | r <- shownRegs ]
    b x = if x then "1" else "0"
    shownRegs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 38]
    chunks k xs = if null xs then [] else unwords (take k xs) : chunks k (drop k xs)

-- | Digits read through Integer and clamped, rather than straight to Int:
-- the digits come from whatever the terminal typed, and ATS0=<twenty
-- digits> should be a silly register value and not an overflow.
digits :: String -> (Int, String)
digits s = let (ds, rest) = span isDigit s
               v = if null ds then 0 else read ds :: Integer
           in (fromIntegral (max 0 (min 999 v)), rest)

pad2, pad3 :: Int -> String
pad2 n = let s = show n in replicate (2 - length s) '0' ++ s
pad3 n = let s = show n in replicate (3 - length s) '0' ++ s

-- | Carrier names as the Conexant and Rockwell parts spell them.
carriers :: [(String, Standard)]
carriers = [ ("B103", Bell103), ("B212", Bell212A), ("V21", V21), ("V22", V22)
           , ("V22B", V22bis), ("V23C", V23), ("V32", V32), ("V32B", V32bis) ]

-- | The rates a modulation can run at.
span' :: Standard -> (Int, Int)
span' s = case s of
  Bell103 -> (300, 300); V21 -> (300, 300); V23 -> (1200, 1200)
  Bell212A -> (1200, 1200); V22 -> (1200, 1200); V22bis -> (1200, 2400)
  V32 -> (4800, 9600); V32bis -> (4800, 14400)

msRead :: HayesProfile -> String
msRead p =
  let ms = hpModes p
      name = case ms of
        (m : _) -> maybe "V22B" id (lookup m [ (std, n) | (n, std) <- carriers ])
        [] -> "V22B"
      (lo, hi) = maybe (minimum (map (fst . span') ms'), maximum (map (snd . span') ms')) id (hpRange p)
      ms' = if null ms then [V22bis] else ms
      auto = if hpAutomode p then "1" else "0"
  in intercalate "," [name, auto, show lo, show hi, show lo, show hi]

-- | @+MS=carrier,automode,min,max[,min rx,max rx]@; empty fields keep
-- their defaults.  The receive rates are accepted and follow the transmit
-- ones, since every modulation here is symmetric.
msSet :: HayesProfile -> String -> Maybe HayesProfile
msSet p params = do
  let fields = splitOn ',' (filter (/= ' ') params)
      field i = if i < length fields then fields !! i else ""
  std <- lookup (map toUpper (field 0)) carriers
  auto <- case field 1 of { "" -> Just True; "1" -> Just True; "0" -> Just False; _ -> Nothing }
  lo <- rate (field 2) 300
  hi <- rate (field 3) 14400
  _ <- rate (field 4) 300
  _ <- rate (field 5) 14400
  if lo > hi || length fields > 6 then Nothing else Just ()
  let family = if auto then (if std == V23 then [V23] else dropWhile (/= std) hayesLadder) else [std]
      inRange s = let (a, b) = span' s in a <= hi && b >= lo
      modes = filter inRange family
      explicit = field 2 /= "" || field 3 /= ""
  if null modes then Nothing else Just ()
  return p { hpModes = modes, hpAutomode = auto
           , hpRange = if explicit then Just (lo, hi) else Nothing
           , hpV32 = if lo <= 4800 && hi >= 14400 then V32ByModes else V32Range lo hi }
  where
    rate s d | null s = Just d
             | all isDigit s = let v = read (take 6 s) :: Int in if v >= 300 && v <= 14400 then Just v else Nothing
             | otherwise = Nothing

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- | Modem events (connect, loss of carrier, incoming call) to DTE bytes,
-- and what the interpreter does about them itself: answering on the S0th
-- ring.
hayesEvent :: HayesState -> HayesEvent -> (HayesState, B.ByteString, [HayesAction])
hayesEvent st ev = case ev of
  EvConnect rate ->
    ( st { hsMode = Online, hsConnected = True, hsLastData = hsT st, hsPlusCount = 0, hsLastRate = Just rate }
    , connectResult st rate, [] )
  EvNoCarrier -> down (result st "NO CARRIER" 3)
  EvNoAnswer -> down (result st "NO ANSWER" 8)
  EvBusy
    | hayesDetectsBusy st -> down (result st "BUSY" 7)
    | otherwise -> down (result st "NO CARRIER" 3)
  EvRing ->
    let rings = min 255 (reg st 1 + 1)
        s0 = reg st 0
        dtrOk = hsDtr st || hpDtr (hsActive st) == 0
        answer = s0 > 0 && rings >= s0 && dtrOk && not (hsConnected st)
        st' = st { hsActive = setReg 1 (if answer then 0 else rings) (hsActive st), hsRingAt = hsT st }
    in (st', result st "RING" 2, [ ActAnswer | answer ])
  -- Reported the way the modems that spoke this protocol reported it, on
  -- its own line after CONNECT.  There is no numeric result code for it,
  -- so a terminal in numeric mode hears nothing, which is what those
  -- modems did too.
  EvProtocol name
    | hpQuiet (hsActive st) || not (hpVerbose (hsActive st)) -> (st, B.empty, [])
    | otherwise -> (st, info st ("PROTOCOL: " ++ name), [])
  where
    down out = (st { hsMode = Command, hsConnected = False }, out, [])

-- | Time passes: completes an escape once the trailing guard time has
-- elapsed, and forgets a ring count eight seconds after the last ring.
-- Returns bytes for the DTE.
hayesTick :: Double -> HayesState -> (HayesState, B.ByteString)
hayesTick t st0
  | hsMode st == Online && hsPlusCount st == 3 && t - hsPlusAt st >= guardTime st =
      (st { hsMode = Command, hsPlusCount = 0 }, ok st)
  | hsMode st == Online && hsPlusCount st > 0 && hsPlusCount st < 3 && t - hsPlusAt st >= guardTime st =
      -- fewer than three: they were data after all (delivered late)
      (st { hsPlusCount = 0 }, B.empty)
  | otherwise = (st, B.empty)
  where
    st1 = st0 { hsT = t }
    st | reg st1 1 > 0 && t - hsRingAt st1 > 8 = st1 { hsActive = setReg 1 0 (hsActive st1) }
       | otherwise = st1

-- | Where a dial string goes.
data DialTarget
  = DialNumber String   -- ^ a telephone number, with any dial modifiers
  | DialSip String      -- ^ a SIP URI
  deriving (Eq, Show)

-- | A dial string as typed after ATD.  Anything with an \@, or a @sip:@ or
-- @sips:@ scheme, is a SIP address; everything else is a number.  A
-- leading T or P (tone or pulse) is dropped when a digit or a scheme
-- follows it, so @ATDT1001\@host@ reaches @sip:1001\@host@ while
-- @ATDpbx\@host@ keeps its user part; write @ATDsip:tom\@host@ for a
-- user part that begins with T or P followed by a digit.
dialTarget :: String -> DialTarget
dialTarget s0 =
  let s1 = dropWhile isSpace s0
      s = case s1 of
        (c : rest@(d : _)) | toUpper c `elem` "TP"
                           , isDigit d || d `elem` "+*#,W!" || schemed rest -> rest
        _ -> s1
      lower = map toLower s
  in if "sip:" `isPrefixOf` lower then DialSip ("sip:" ++ drop 4 s)
     else if "sips:" `isPrefixOf` lower then DialSip ("sips:" ++ drop 5 s)
     else if '@' `elem` s then DialSip ("sip:" ++ s)
     else DialNumber s
  where schemed r = let l = map toLower r in "sip:" `isPrefixOf` l || "sips:" `isPrefixOf` l

-- | A phone book: one entry a line, the number a terminal dials and then
-- what to dial instead, a SIP address or another number.  Blank lines and
-- lines starting with @#@ are ignored.
parsePhonebook :: String -> [(String, String)]
parsePhonebook text =
  [ (key, target)
  | l <- lines text
  , let t = dropWhile isSpace l
  , not (null t), take 1 t /= "#"
  , (key : target : _) <- [words t] ]

-- | The entry for a dialled number, matching on the dialling digits alone,
-- so @ATDT555-1234@ finds @5551234@.
phonebookLookup :: [(String, String)] -> String -> Maybe String
phonebookLookup book dialled = case dialTarget dialled of
  DialSip _ -> Nothing
  DialNumber n ->
    let key = keyOf n
    in if null key then Nothing else lookup key [ (keyOf k, v) | (k, v) <- book ]
  where keyOf = filter (\c -> isDigit c || c `elem` "*#+")
