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
runAgent conf = getInfo <$> createProcess cproc { cwd = cwd }
    where

        cproc :: CreateProcess
        cproc = proc "./example/custom-agents/genetic-agent.py" args

        args :: [String]
        args = []

        getInfo :: (a, Maybe Handle, c, ProcessHandle) -> (Handle, ProcessHandle)
        getInfo = (\(_,Just out,_,ph) -> (out, ph))

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO"
