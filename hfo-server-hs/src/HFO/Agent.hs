module HFO.Agent where

import System.IO                (Handle(..))
import System.Process

data AgentConf = AgentConf
    { agentPort :: Int }

defaultAgent :: AgentConf
defaultAgent = AgentConf
    { agentPort = 6000
    }

runAgent :: AgentConf -> IO (Handle, ProcessHandle)
runAgent conf = getInfo <$> createProcess cproc { cwd = cwd, std_err = CreatePipe }
    where

        cproc :: CreateProcess
        cproc = proc "./example/custom-agents/genetic-agent.py" args

        args :: [String]
        args = []

        getInfo :: (a, b, Maybe Handle, ProcessHandle) -> (Handle, ProcessHandle)
        getInfo = (\(_,_,Just err,ph) -> (err, ph))

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO"
