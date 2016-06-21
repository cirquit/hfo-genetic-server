{-# LANGUAGE RecordWildCards #-}

module HFO.Agent
  (
    module HFO.Agent.Data
  , module HFO.Agent.Conf
  , runAgent
  , runAgent_
  , runOffenseTeam
  , runDefenseTeam
  , waitForProcesses
    ) where

import System.IO                (Handle(..))
import System.Process
import Control.Concurrent       (threadDelay)
import Data.Maybe               (isJust)

import HFO.ToFlags              (toFlags_)
import HFO.Agent.Data
import HFO.Agent.Conf


-- | shortcut to start a whole offense team
--
--  *) this should be ran before the defense team, because HFO wants offense first
-- 
--   returns all ProcessHandles so we know use them to notice the end of the simulation (in Main.startSimulation)
--
runOffenseTeam :: AgentConf -> OffenseTeam -> IO [ProcessHandle]
runOffenseTeam conf OffenseTeam{..} = do
    (_, ph1) <- runAgent $ conf { actions  = Left op1 , teamName = "base_left" }
    (_, ph2) <- runAgent $ conf { actions  = Left op2 , teamName = "base_left" }
    (_, ph3) <- runAgent $ conf { actions  = Left op3 , teamName = "base_left" }
    (_, ph4) <- runAgent $ conf { actions  = Left op4 , teamName = "base_left" }
    return [ph1, ph2, ph3, ph4]

-- | shortcut to start a whole defense team
-- 
--  *) this should be ran after the offense team, because HFO wants offense first
--  *) the goalie should be the first one to be connected, because HFO wants it like that
--
--   returns all ProcessHandles so we know use them to notice the end of the simulation (in Main.startSimulation)
--
runDefenseTeam :: AgentConf -> DefenseTeam -> IO [ProcessHandle]
runDefenseTeam conf DefenseTeam{..} = do
   (_, ph1) <- runAgent $ conf { actions  = Right goalie , teamName = "base_right", isGoalie = True }
   (_, ph2) <- runAgent $ conf { actions  = Right dp2    , teamName = "base_right"                  }
   (_, ph3) <- runAgent $ conf { actions  = Right dp3    , teamName = "base_right"                  }
   (_, ph4) <- runAgent $ conf { actions  = Right dp4    , teamName = "base_right"                  }
   return [ph1, ph2, ph3, ph4]


-- | Checks every 100ms if any of the Processes have exited - if yes, then we terminate the simulation
--
--   This needs to be done, because in the not standartPace simulation not every player is kicked
--   after the games are finished (it's undefined which one terminates the first) 
--
waitForProcesses :: [ProcessHandle] -> IO ()
waitForProcesses phs = do
    mexits <- mapM getProcessExitCode phs
    if any isJust mexits
        then return ()
        else sleep 100 >> waitForProcesses phs


-- | run agent python script with the corresponding configuration
--
runAgent :: AgentConf -> IO (Handle, ProcessHandle)
runAgent conf = sleep 1000 >> getInfo <$> createProcess cproc { cwd = cwd, std_err = CreatePipe }
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

-- | delay execution for i milliseconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^3)