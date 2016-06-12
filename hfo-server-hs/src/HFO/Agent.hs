{-# LANGUAGE RecordWildCards #-}

module HFO.Agent where

import System.IO                (Handle(..))
import System.Process
import Control.Concurrent       (threadDelay)
import Debug.Trace              (trace)


import HFO.ToFlags              (ToFlags(..))

data AgentConf = AgentConf
    { teamName   :: String -- this is currently 'base_left' (offense) and 'base_right' (defense) only
    , isGoalie   :: Bool
    , episodes   :: Int    -- how many episodes are played
    }

instance ToFlags AgentConf where

    toFlags AgentConf{..} = concat $ [teamName', isGoalie', episodes']
        where

            teamName' = ["--team", teamName]

            isGoalie'
                | isGoalie  = ["--goalie"]
                | otherwise = [""]

            episodes'
                | episodes > 0 = ["--episodes", show episodes]
                | otherwise    = error $ "Agent.AgentConf.episodes: This value should never be below 1, it's: " ++ show episodes

-- | Default settings
--
defaultAgent :: AgentConf
defaultAgent = AgentConf
    { teamName = "base_left"
    , isGoalie = False
    , episodes = 1
    }

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