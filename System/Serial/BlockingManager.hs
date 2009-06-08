-- | This is a largely drop-in replacement for "System.Serial.Manager" which sends and receives one command at a time from the port, and so is garuanteed to send the write information back to the right function.  It may be necessary when working with devices that have ambiguous returns, such as a single acknowledgement character for all successful commands.  See the analagous functions in "System.Serial.Manager" for the full documentation.  The notes here only point out the differences.
module System.Serial.BlockingManager (serialManager, wrapCommand, BlockingSerialManager, BlockingSerialCommand) where

import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

type BlockingSerialCommand = (String, MVar (Maybe String))
type BlockingSerialManager = MVar BlockingSerialCommand

-- | The blocking 'serialManager' function takes one additional argument, the timeout (since it cannot continue executing commands in parallel while one command freezes).
serialManager :: Handle 
              -> Int -- ^ timeout (in ms)
              -> IO BlockingSerialManager
serialManager h timeout = do mv <- newEmptyMVar
                             forkIO $ process h mv timeout
                             return mv

process :: Handle
        -> MVar (String, MVar (Maybe [Char]))
        -> Int
        -> IO ()
process h mv timeout = do (cmd,resVar) <- takeMVar mv
                          hPutStr h cmd
                          r <- hWaitForInput h timeout
                          if not r then putMVar resVar Nothing 
                           else do
                              let loop = do st <- hReady h
                                            if st then do l <- hGetLine h
                                                          q <- loop
                                                          return (l ++ q)
                                             else return "" 
                              res <- loop
                              putMVar resVar (Just res)
                              process h mv timeout

-- | Wrapping commands is identical to the non-blocking version except that there is no predicate to recognize return values.
wrapCommand :: String -- ^ The end of line character for this port
            -> String -- ^ The command to send
            -> BlockingSerialManager -- ^ The serial port to access
            -> IO (Maybe String) -- ^ 'Nothing' if there was a timeout, other 'Just' and the response string
wrapCommand eol cmd mgr = do
  mv <- newEmptyMVar
  putMVar mgr (cmd ++ eol, mv)
  takeMVar mv