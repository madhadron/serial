-- | Many serial devices allow multiple commands to run at once, and
-- | return their results as they finish.  To make use of this,
-- | multiple commands needs to read and write to the serial port at
-- | once, and the return values must somehow be sorted and returned
-- | back to the callers.

module System.Serial.Manager (serialManager, wrapCommand, SerialManager, SerialCommand) where

import System.IO
import Text.ParserCombinators.Parsec
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

type SerialCommand = (String, Parser Bool, MVar String)
type SerialManager = MVar (Either SerialCommand String)

-- | 'serialManager' takes produces a structure around a 'Handle' to
-- | handle multiple callers to the serial port.  The return value is
-- | the channel to which all commands will flow.  Users should use
-- | the 'wrapCommand' function to access it instead of trying to
-- | access its details directly.
serialManager :: Handle -> IO SerialManager
serialManager h = do mv <- newEmptyMVar
                     -- I use lists to hold the waiting commands, because I
                     -- don't anticipate there being that many at once.
                     portWatcher h mv
                     threadDelay 1000
                     forkIO (foldM_ (process h mv) [] (repeat ()))
                     return mv

-- Fetch from mvar, operate on it, recurse with updated ws list
process :: Handle -> MVar (Either SerialCommand String) -> [SerialCommand] -> () -> IO [SerialCommand]
process h mv ws _ = do
  v <- takeMVar mv
  process' v
      where process' (Left (cmd,pr,res)) = do hPutStr h cmd
                                              return $ ws ++ [(cmd,pr,res)]
            process' (Right str) = case (isolateWhere (\(_,pr,_) -> str `matchedBy` pr) ws) of
                                     (Nothing,ws') -> return ws'
                                     (Just (cmd,pr,res), ws') -> do
                                       putMVar res str
                                       return ws'

isolateWhere p [] = (Nothing,[])
isolateWhere p (l:ls) | p l = (Just l,ls)
                          | otherwise = (l', l:ls')
                          where (l',ls') = isolateWhere p ls

matchedBy :: String -> Parser Bool -> Bool
matchedBy str pr = case (parse pr "" str) of
                     Left _ -> False
                     Right x -> x
                                                                    

portWatcher :: Handle -> SerialManager -> IO ThreadId
portWatcher h m = forkIO portWatcher'
    where portWatcher' = do l <- hGetLine h
                            putMVar m (Right l)
                            portWatcher'

-- | All the commands to operate a 'SerialManager' should be
-- specializations of 'wrapCommand', created by applying it to the
-- first three arguments, then using that thereafter as the command to
-- the serial port.
-- 
-- For example, the Olympus IX-81 requires a login command from the
-- user (@2LOG IN@) followed by @\r\n@ as an end of line.  The
-- response will be @2LOG +@ followed by @\r@.  So a login command
-- would look like
-- 
-- > p = do string "2LOG +\r"
-- >        return True
-- 
-- > login mgr = wrapCommand "\r\n" "2LOG IN" p
-- 
-- 'wrapCommand' uses parsers that return 'Bool' so users can choose
-- whether or not to match any given command based upon its contents,
-- rather than just blindly saying whether it matches or not.  This
-- may change to simple predicates of @String -> Bool@ in future.

wrapCommand :: String        -- ^ The end of line character for this port
            -> String        -- ^ The command to send
            -> Parser Bool   -- ^ The parser to recognize the returning value
            -> SerialManager -- ^ The serial port to access
            -> IO String     -- ^ The return value
wrapCommand eol cmd pr mgr = do
  mv <- newEmptyMVar
  tryTakeMVar mv >> return ()
  putMVar mgr (Left (cmd ++ eol, pr, mv))
  takeMVar mv

