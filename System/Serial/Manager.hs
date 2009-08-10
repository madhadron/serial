-- | Many serial devices allow multiple commands to run at once, and
-- | return their results as they finish.  To make use of this,
-- | multiple commands needs to read and write to the serial port at
-- | once, and the return values must somehow be sorted and returned
-- | back to the callers.

module System.Serial.Manager (serialManager, closeSerialManager, wrapCommand, wrapCommandWithCallback, SerialManager, SerialCommand, wrapDeafCommand) where

import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.List (isPrefixOf)

data SerialCommand = SerialCommand { cmd :: String, predicate :: (String -> Bool), returnPtr :: MVar String }
                   | DeafCommand { cmd :: String }

isDeaf :: SerialCommand -> Bool
isDeaf (DeafCommand _) = True
isDeaf _ = False

toMemory :: SerialCommand -> [SerialMemory]
toMemory (DeafCommand _) = []
toMemory (SerialCommand c pr res) = [(c,pr,res)]

type SerialMemory = (String, String->Bool, MVar String)
data SerialManager = SerialManager { managedHandle :: Handle,
                                     storage :: MVar (Either SerialCommand String),
                                     inputTerminator, outputTerminator :: String,
                                     portMonitorThread :: ThreadId
                                   }

-- | 'serialManager' takes produces a structure around a 'Handle' to
-- | handle multiple callers to the serial port.  The return value is
-- | the channel to which all commands will flow.  Users should use
-- | the 'wrapCommand' function to access it instead of trying to
-- | access its details directly.
serialManager :: Handle -- ^ the handle to wrap
              -> String -- ^ the termination string for commands received from the port
              -> String -- ^ the termination string for command send to the port
              -> IO SerialManager
serialManager h inT outT = do mv <- newEmptyMVar
                              -- I use lists to hold the waiting commands, because I
                              -- don't anticipate there being that many at once.
                              thr <- portWatcher h inT mv
                              let st = SerialManager h mv inT outT thr
                              threadDelay 1000
                              forkIO (foldM_ (process st) [] (repeat ()))
                              return st

-- | Having multiple serial managers running on the same port is a disaster waiting
-- to happen.  When you're done with a 'SerialManager', run 'closeSerialManager' on
-- it to shut it down.
closeSerialManager :: SerialManager -> IO ()
closeSerialManager m = killThread $ portMonitorThread m

-- Fetch from mvar, operate on it, recurse with updated ws list
process :: SerialManager -> [SerialMemory] -> () -> IO [SerialMemory]
process st ws _ = do
  v <- takeMVar (storage st)
  process' v
      where process' (Left c) = do hPutStr (managedHandle st) ((cmd c) ++ outputTerminator st)
                                   -- putStrLn $ "Sending command: " ++ cmd
                                   return $ ws ++ toMemory c
            process' (Right str) = case (isolateWhere (\(_,pr,_) -> pr str) ws) of
                                     (Nothing,ws') -> do
                                       -- putStrLn ("Unmatched return: " ++ str)
                                       return ws'
                                     (Just (_,_,res), ws') -> do
                                       -- putStrLn $ "Matched return: " ++ str
                                       putMVar res str
                                       return ws'

isolateWhere :: (a -> Bool) -> [a] -> (Maybe a, [a])
isolateWhere _ [] = (Nothing,[])
isolateWhere p (l:ls) | p l = (Just l,ls)
                      | otherwise = (l', l:ls')
                          where (l',ls') = isolateWhere p ls

portWatcher :: Handle -> String -> MVar (Either SerialCommand String) -> IO ThreadId
portWatcher h inT stor = forkIO portWatcher'
    where portWatcher' = do s <- takeUntil h inT
                            -- putStrLn $ "Read " ++ s
                            putMVar stor (Right s)
                            portWatcher'

takeUntil :: Handle -> String -> IO String
takeUntil h term = takeUntil' ""
    where takeUntil' s = if rterm `isPrefixOf` s then return (reverse s) else hGetChar h >>= \c -> takeUntil' (c:s)
          rterm = reverse term


-- portWatcher :: SerialManager -> IO ThreadId
-- portWatcher m = forkIO portWatcher'
--     where portWatcher' = do l <- hGetLine (managedHandle m)
--                             putMVar (storage m) (Right l)
--                             portWatcher'


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
-- > p = ("2LOG" `isPrefixOf`)
-- 
-- > login mgr = wrapCommand "\r\n" "2LOG IN" p
-- 
-- 'wrapCommand' uses functions of type 'String -> Bool' users can choose
-- whether or not to match any given command based upon its contents,
-- rather than just blindly saying whether it matches or not.

wrapCommand :: String        -- ^ The command to send
            -> (String -> Bool)   -- ^ The predicate to recognize the returning value
            -> SerialManager -- ^ The serial port to access
            -> IO String     -- ^ The response from the port
wrapCommand cmd pr mgr = do
  mv <- newEmptyMVar
  tryTakeMVar mv >> return ()
  putMVar (storage mgr) (Left $ SerialCommand (cmd ++ outputTerminator mgr) pr mv)
  takeMVar mv

-- | Some commands don't expect any response from the hardware on the far end.
-- For these cases, use 'wrapDeafCommand'.

wrapDeafCommand :: String    -- ^ The command to send
                -> SerialManager -- ^ The serial port to access
                -> IO ()
wrapDeafCommand cmd mgr = putMVar (storage mgr) (Left $ DeafCommand (cmd ++ outputTerminator mgr))
  

-- | Sometimes we don't want the current thread to block, but we still 
-- want some action when the a command returns from the serial port.  To
-- that end, 'wrapCommandWithCallback' lets us pass a function of type
-- 'String -> IO ()' to be executed when a response is recognized
-- by the predicate.
wrapCommandWithCallback :: String -- ^ The command to send
                        -> (String -> Bool) -- ^ The predicate to recognize the returning value
                        -> (String -> IO ()) -- ^ The callback to run when the command returns
                        -> SerialManager -- ^ The serial port to access
                        -> IO ThreadId -- ^ The thread id in which the command is being run
wrapCommandWithCallback cmd pr callback mgr = do
  forkIO $ wrapCommand cmd pr mgr >>= callback

