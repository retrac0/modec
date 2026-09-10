{-# LANGUAGE OverloadedStrings #-}
-- | A voice-mode modem, faked on a pseudo-terminal, with a modem on
-- the far end of its line.
--
-- The serial backend cannot be tested by the suite -- it opens a
-- device, and the suite is built so that it cannot -- and the real
-- part is not on the desk.  So this is the other side of "Serial": it
-- answers the AT dialogue the way a CX93010 does, and once asked to
-- stream it carries "Modec.Voice"'s framing in the chosen format, with
-- a whole modem behind it running 'modemStep' on what arrives.  Every
-- byte the backend sends or receives goes through the same termios,
-- the same shielding and the same format as it will with the dongle;
-- only the DAA is missing.
--
-- The slave's path goes to stdout, for a script to hand to
-- @--audio-serial@.
module FakeDongle (FakeOpts (..), runFakeDongle) where

import Control.Exception (IOException, try)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Vector.Storable as VS
import System.IO
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Terminal

import Modec.DSP (Signal)
import Modec.Modem
import Modec.Sample
import Modec.Standards (Role (..), Standard)
import Modec.Voice
import Modec.Wav

data FakeOpts = FakeOpts
  { fkFormat  :: SampleFormat
  , fkRole    :: Role              -- ^ the far modem's; a calling one rings the line
  , fkModes   :: [Standard]
  , fkSay     :: String            -- ^ what the far end sends once connected
  , fkHeard   :: Maybe FilePath    -- ^ where what it received goes (else stderr)
  , fkRecord  :: Maybe FilePath    -- ^ what arrived over the port, as 16-bit PCM
  , fkPlay    :: Maybe FilePath    -- ^ send this recording instead of a modem's audio
  , fkSeconds :: Double            -- ^ end the stream after this much audio
  }

say :: String -> IO ()
say s = hPutStrLn stderr ("fake-dongle: " ++ s)

runFakeDongle :: FakeOpts -> IO ()
runFakeDongle o = do
  (master, slave) <- openPseudoTerminal
  name <- getSlaveTerminalName master
  -- raw from the start, so the line discipline neither echoes the
  -- backend's commands back at it nor translates its line ends
  a <- getTerminalAttributes slave
  setTerminalAttributes slave (rawMode a) Immediately
  putStrLn name
  hFlush stdout
  h <- fdToHandle master
  hSetBinaryMode h True
  hSetBuffering h NoBuffering
  say (name ++ ": " ++ formatName (fkFormat o) ++ ", far end " ++ show (fkRole o))
  slaveRef <- newIORef (Just slave)
  let reply r = say ("> " ++ r) >> B.hPut h (BC.pack ("\r\n" ++ r ++ "\r\n"))
      commands = do
        ml <- readUntil h "\r"
        case ml of
          Nothing -> say "the port closed"
          Just l -> do
            -- The backend has the slave open now, so ours can go: its
            -- closing is then an end of file here rather than a hang.
            readIORef slaveRef >>= mapM_ (\s -> closeFd s >> writeIORef slaveRef Nothing)
            let cmd = BC.unpack (BC.strip l)
            if null cmd then commands else do
              say ("< " ++ cmd)
              case cmd of
                _ | "AT+VTR" `isPrefixOf` cmd -> reply "VCON" >> stream o h >> commands
                  | "AT+VPR" `isPrefixOf` cmd -> do
                      reply "OK"
                      when (fkRole o == Originate) (reply "RING")
                      commands
                  | cmd == "ATH" -> reply "OK" >> say "on hook"
                  | "AT" `isPrefixOf` cmd -> reply "OK" >> commands
                  | otherwise -> reply "ERROR" >> commands
  commands

rawMode :: TerminalAttributes -> TerminalAttributes
rawMode a = (`withTime` 0) . (`withMinInput` 1)
  $ foldl withoutMode a
      [ EnableEcho, EchoErase, EchoKill, EchoLF, ProcessInput, ProcessOutput
      , ExtendedFunctions, KeyboardInterrupts, MapCRtoLF, MapLFtoCR, IgnoreCR
      , StartStopInput, StartStopOutput, StripHighBit ]

-- | Bytes up to a terminator, a byte at a time; 'Nothing' at end of file.
readUntil :: Handle -> B.ByteString -> IO (Maybe B.ByteString)
readUntil h end = go mempty
  where
    go acc = do
      r <- try (B.hGetSome h 1) :: IO (Either IOException B.ByteString)
      case r of
        Right b | not (B.null b) -> if b == end then return (Just acc) else go (acc <> b)
        _ -> return (if B.null acc then Nothing else Just acc)

-- | The duplex state: a block in, the far modem's block out, until the
-- time is up or the backend leaves.
stream :: FakeOpts -> Handle -> IO ()
stream o h = do
  let fs = 8000 :: Double
      blockN = 160
      fmt = fkFormat o
      need = blockN * bytesPerSample fmt
      cfg = defaultModemConfig fs (fkRole o) (fkModes o)
      dte0 = B.unpack (BC.pack (fkSay o))
  play <- mapM (fmap wavSamples . readWav) (fkPlay o)
  heard <- newIORef B.empty
  recorded <- newIORef ([] :: [Signal])
  let fill dst acc
        | B.length acc >= need = return (Right (dst, B.take need acc, B.drop need acc))
        | otherwise = do
            r <- try (B.hGetSome h 4096) :: IO (Either IOException B.ByteString)
            case r of
              Right bs | not (B.null bs) ->
                let (dst', d) = dleDecode dst bs
                in if 0x5E `elem` dlEvents d || dlRest d /= Nothing
                     then return (Left "the backend left the stream")
                     else fill dst' (acc <> dlPayload d)
              _ -> return (Left "the port closed")
      go st dst leftover k
        | fromIntegral (k * blockN) / fs >= fkSeconds o = finish "time is up"
        | otherwise = do
            r <- fill dst leftover
            case r of
              Left why -> finish why
              Right (dst', payload, rest) -> do
                let rx = decodeSamples fmt payload
                    (st', audio, rxBytes, _) = modemStep cfg st rx (if k == 0 then dte0 else [])
                    out = case play of
                      Just p -> let b = VS.take blockN (VS.drop (k * blockN) p)
                                in b VS.++ VS.replicate (blockN - VS.length b) 0
                      Nothing -> audio
                modifyIORef' recorded (rx :)
                unless (null rxBytes) $ modifyIORef' heard (<> B.pack rxBytes)
                w <- try (B.hPut h (dleEncode (encodeSamples fmt out))) :: IO (Either IOException ())
                case w of
                  Left _ -> finish "the port closed"
                  Right () -> go st' dst' rest (k + 1)
      finish why = do
        say ("stream over: " ++ why)
        void (try (B.hPut h dleEtx) :: IO (Either IOException ()))
        hd <- readIORef heard
        case fkHeard o of
          Just f -> B.writeFile f hd
          Nothing -> say ("heard " ++ show hd)
        forM_ (fkRecord o) $ \f -> do
          blocks <- readIORef recorded
          writeWav16Mono f (round fs) (VS.concat (reverse blocks))
  go (modemInit cfg) dleInit B.empty (0 :: Int)
