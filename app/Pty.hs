-- | A pseudo-terminal a terminal program can open as if it were a modem's
-- serial port.
--
-- The slave side is what the program opens: @/dev/pts/N@, or a symlink
-- with a stable name.  This process keeps only the master.  Holding the
-- slave open too would be simpler, and would hide the one thing a pty can
-- say about the program on the other end: whether it has the port open.
-- With no slave open a read on the master fails with EIO at once, and
-- poll reports a hang-up; with a slave open and nothing typed, poll waits.
-- That difference is DTR.  Opening the port raises it and closing it
-- drops it, which is what a real modem's &D setting acts on.
--
-- What a pty cannot carry: DCD and RI (a terminal program asking the port
-- for carrier gets whatever the pty driver makes up, so tell it to ignore
-- carrier), the baud rate the program sets (bytes go as fast as they
-- come), and hardware flow control.
module Pty
  ( PtyPort (..)
  , withPty
  , rawMode
  ) where

import Control.Concurrent
import Control.Exception (IOException, finally, try)
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.Posix.Files (FileStatus, createSymbolicLink, getSymbolicLinkStatus, isSymbolicLink, removeLink)
import System.Posix.IO (closeFd)
import qualified System.Posix.IO.ByteString as PB
import System.Posix.Terminal
import System.Posix.Types (ByteCount, Fd)
import System.Timeout (timeout)

data PtyPort = PtyPort
  { ppName    :: String                    -- ^ the slave device, /dev/pts/N
  , ppRecv    :: IO B.ByteString           -- ^ whatever has arrived, without waiting
  , ppSend    :: B.ByteString -> IO ()     -- ^ queued; dropped while nobody has the port open
  , ppDtr     :: IO Bool                   -- ^ a program has the port open
  , ppDropped :: IO Bool                   -- ^ it closed since the last time this was asked
  }

-- | Termios for a byte pipe: no echo, no line editing, no translation of
-- line ends, no signals, no flow control characters.
rawMode :: TerminalAttributes -> TerminalAttributes
rawMode a = (`withTime` 0) . (`withMinInput` 1)
  $ foldl withoutMode a
      [ EnableEcho, EchoErase, EchoKill, EchoLF, ProcessInput, ProcessOutput
      , ExtendedFunctions, KeyboardInterrupts, MapCRtoLF, MapLFtoCR, IgnoreCR
      , StartStopInput, StartStopOutput, StripHighBit ]

-- | Open a pty, optionally link a name to its slave, and run the body with
-- the port.  The link is removed on the way out.
withPty :: (String -> IO ()) -> Maybe FilePath -> (PtyPort -> IO a) -> IO a
withPty say link body = do
  (master, slave) <- openPseudoTerminal
  name <- getSlaveTerminalName master
  a <- getTerminalAttributes slave
  setTerminalAttributes slave (rawMode a) Immediately
  -- ours goes at once, so that a program closing the port is visible
  closeFd slave
  say ("serial port on " ++ name ++ maybe "" (" as " ++) link)
  putStrLn name
  hFlush stdout
  inBuf <- newIORef ([] :: [B.ByteString])
  dtrRef <- newIORef False
  droppedRef <- newIORef False
  lostRef <- newIORef (0 :: Int)
  out <- newChan
  let setDtr up = do
        was <- atomicModifyIORef' dtrRef (\w -> (up, w))
        when (was && not up) $ do
          writeIORef droppedRef True
          say "the terminal closed the serial port (DTR off)"
          -- Whoever opens it next gets a raw port again, whatever the
          -- last program left behind.  Nobody has it open, so this
          -- cannot race a program setting its own modes.
          void (try (getTerminalAttributes master >>= \t -> setTerminalAttributes master (rawMode t) Immediately) :: IO (Either IOException ()))
        when (up && not was) $ do
          n <- atomicModifyIORef' lostRef (\k -> (0, k))
          say ("a terminal opened the serial port (DTR on)"
               ++ if n > 0 then "; " ++ show n ++ " bytes went nowhere while it was closed" else "")
      reader = forever $ do
        ready <- timeout 100000 (threadWaitRead master)
        case ready of
          Nothing -> setDtr True                  -- open, and quiet
          Just () -> do
            r <- try (PB.fdRead master 4096) :: IO (Either IOException B.ByteString)
            case r of
              Right bs | not (B.null bs) -> do
                setDtr True
                atomicModifyIORef' inBuf (\xs -> (bs : xs, ()))
              _ -> setDtr False >> threadDelay 100000
      writer = forever $ do
        bs <- readChan out
        writeAll master bs
  _ <- forkIO reader
  _ <- forkIO writer
  let port = PtyPort
        { ppName = name
        , ppRecv = B.concat . reverse <$> atomicModifyIORef' inBuf (\xs -> ([], xs))
        , ppSend = \bs -> do
            up <- readIORef dtrRef
            if up then writeChan out bs
                  else modifyIORef' lostRef (+ B.length bs)
        , ppDtr = readIORef dtrRef
        , ppDropped = atomicModifyIORef' droppedRef (\d -> (False, d))
        }
  case link of
    Nothing -> body port
    Just path -> do
      existing <- try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus)
      case existing of
        Right st
          | isSymbolicLink st -> removeLink path
          | otherwise -> do
              say (path ++ " exists and is not a symlink; not replacing it")
              exitFailure
        Left _ -> return ()
      createSymbolicLink name path
      body port `finally` void (try (removeLink path) :: IO (Either IOException ()))

-- | The whole of it, or as much as goes before the program closes the port.
writeAll :: Fd -> B.ByteString -> IO ()
writeAll fd bs = unless (B.null bs) $ do
  r <- try (PB.fdWrite fd bs) :: IO (Either IOException ByteCount)
  case r of
    Right n -> writeAll fd (B.drop (fromIntegral n) bs)
    Left _ -> return ()
