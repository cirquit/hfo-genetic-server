module HFO.Server 
  (
    module HFO.Server.Conf
  , runServer
  , runServer_
  , serverWatcher
  ) where

import System.Process
import System.IO          (Handle(..))
import Control.Concurrent (threadDelay)



import HFO.ToFlags        (toFlags_)
import HFO.Server.Conf

-- | run Half-Field Offense binary with the specified flags defined in HFO.Server.Conf
--   
runServer :: ServerConf -> IO ProcessHandle
runServer conf = getInfo <$> createProcess cproc { cwd = cwd }
    where

        cproc :: CreateProcess
        cproc = proc "./bin/HFO" args

        args :: [String]
        args = toFlags_ conf

        getInfo :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> ProcessHandle
        getInfo = (\(_,_,_,ph) -> ph)

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO"

-- | runServer without Handle/ProcessHandle
--
runServer_ :: ServerConf -> IO ()
runServer_ conf = runServer conf >> return ()

-- | a single thread to watch over the hfo-server
--
--   if a simulation bugs out, wait for a fixed time (currently 15 min) and kill the process.
--   'startSingleSimulation' takes care of the results
--
serverWatcher :: ProcessHandle -> IO ()
serverWatcher server = do
    let seconds = 1000 * 1000
        minutes = 60 * seconds
        time    = 15 * minutes
    threadDelay time
    putStrLn "Haskell-Server-Watcher: Time's out, killing server."
    terminateProcess server
