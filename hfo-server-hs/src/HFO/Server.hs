module HFO.Server 
  (
    module HFO.Server.Conf
  , runServer
  , runServer_ 
  ) where

import System.Process
import System.IO        (Handle(..))

import HFO.ToFlags      (toFlags_)
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
