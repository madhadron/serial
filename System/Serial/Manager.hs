-- | Many serial devices allow multiple commands to run at once, and return their results as they finish.  To make use of this, multiple commands needs to read and write to the serial port at once, and the return values must somehow be sorted and returned back to the callers.

module System.Serial.Manager (serialManager, wrapCommand) where

import System.IO (hGetLine, Handle, hPutStr)
import Text.ParserCombinators.Parsec (Parser, runParser, GenParser, parse)
import Control.Concurrent.Chan (Chan, newChan, writeChan, getChanContents)
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, isEmptyMVar, takeMVar, tryTakeMVar)
import Control.Monad (foldM_)

type SerialCommand = (String, Parser Bool, MVar String)
type SerialManager = Chan (Either SerialCommand String)

-- | 'serialManager' takes produces a structure around a 'Handle' to handle multiple callers to the serial port.  The return value is the channel to which all commands will flow.  Users should use the 'wrapCommand' function to access it instead of trying to access its details directly.
serialManager :: Handle -> IO SerialManager
serialManager h = do channel <- newChan
                     -- I use lists to hold the waiting commands, because I
                     -- don't anticipate there being that many at once.
                     ls <- getChanContents channel
                     forkIO (foldM_ (traverseCommands h) [] ls)
                     portWatcher h channel
                     return channel

traverseCommands :: Handle -> [SerialCommand] -> Either SerialCommand String -> IO [SerialCommand]
traverseCommands h [] _ = return []
traverseCommands h ws (Left (cmd,pr,res)) = do
  hPutStr h cmd
  return (ws ++ [(cmd,pr,res)])
traverseCommands h ((cmd,pr,res):ws) (Right str)
    | matchedBy str pr = do putMVar res str
                            return ws
    | otherwise = do ws' <- traverseCommands h ws (Right str)
                     return ((cmd,pr,res):ws')

matchedBy :: String -> Parser Bool -> Bool
matchedBy str pr = case (parse pr "" str) of
                     Left _ -> False
                     Right x -> x
                                                                    

portWatcher :: Handle -> SerialManager -> IO ThreadId
portWatcher h m = forkIO portWatcher'
    where portWatcher' = do l <- hGetLine h
                            writeChan m (Right l)
                            portWatcher'

-- | All the commands to operate a 'SerialManager' should be
-- specializations of 'wrapCommand', created by applying it to the
-- first two arguments, then using that thereafter as the command to
-- the serial port.
wrapCommand :: SerialManager -- ^ The serial port to access
            -> String        -- ^ The end of line character for this port
            -> String        -- ^ The command to send
            -> Parser Bool   -- ^ The parser to recognize the returning value
            -> IO String     -- ^ The return value
wrapCommand mgr eol cmd pr = do
  mv <- newEmptyMVar
  tryTakeMVar mv >> return ()
  writeChan mgr (Left (cmd ++ eol, pr, mv))
  takeMVar mv
