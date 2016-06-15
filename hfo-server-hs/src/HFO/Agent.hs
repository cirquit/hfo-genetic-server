module HFO.Agent
  (
    module HFO.Agent.Data
  , module HFO.Agent.Conf
  , runAgent
    )where

import System.IO                (Handle(..))
import System.Process
import Control.Concurrent       (threadDelay)

import HFO.ToFlags              (toFlags_)
import HFO.Agent.Data
import HFO.Agent.Conf

-- | run agent python script with the corresponding configuration
--
runAgent :: AgentConf -> IO (Handle, ProcessHandle)
runAgent conf = sleep 1 >> getInfo <$> createProcess cproc { cwd = cwd, std_err = CreatePipe }
    where

        cproc :: CreateProcess
        cproc = proc "./genetic-agent.py" args

        args :: [String]
        args = toFlags_ conf

        getInfo :: (a, b, Maybe Handle, ProcessHandle) -> (Handle, ProcessHandle)
        getInfo = (\(_,_,Just err,ph) -> (err, ph))

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py"

-- | delay execution for i seconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^6)