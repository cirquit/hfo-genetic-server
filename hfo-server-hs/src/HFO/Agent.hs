{-# LANGUAGE RecordWildCards #-}

module HFO.Agent
  (
    module HFO.Agent.Data
  , module HFO.Agent.Conf
  , runAgent
  , runAgent_
  , runOffenseTeam
  , runDefenseTeam
    ) where

import System.IO                (Handle(..))
import System.Process
import Control.Concurrent       (threadDelay)

import HFO.ToFlags              (toFlags_)
import HFO.Agent.Data
import HFO.Agent.Conf


-- | shortcut to start a whole offense team
--
--  *) this should be ran before the defense team, because HFO wants offense first
-- 
runOffenseTeam :: AgentConf -> OffenseTeam -> IO ()
runOffenseTeam conf OffenseTeam{..} = do
    runAgent_ $ conf { actions  = Left op1 , teamName = "base_left" }
    runAgent_ $ conf { actions  = Left op2 , teamName = "base_left" }
    runAgent_ $ conf { actions  = Left op3 , teamName = "base_left" }
    runAgent_ $ conf { actions  = Left op4 , teamName = "base_left" }

-- | shortcut to start a whole defense team
-- 
--  *) this should be ran after the offense team, because HFO wants offense first
--  *) the goalie should be the first one to be connected, because HFO wants it like that
--
--   returns the ProcessHandle of the Goalie so we know use it to notice the end of the simulation (in Main.startSimulation)
--
runDefenseTeam :: AgentConf -> DefenseTeam -> IO ProcessHandle
runDefenseTeam conf DefenseTeam{..} = do
   (_, ph) <- runAgent $ conf { actions  = Right goalie , teamName = "base_right", isGoalie = True }
   runAgent_           $ conf { actions  = Right dp2    , teamName = "base_right"                  }
   runAgent_           $ conf { actions  = Right dp3    , teamName = "base_right"                  }
   runAgent_           $ conf { actions  = Right dp4    , teamName = "base_right"                  }
   return ph


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

-- | runAgent without the return value
--
runAgent_ :: AgentConf -> IO ()
runAgent_ conf = runAgent conf >> return ()

-- | delay execution for i seconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^6)