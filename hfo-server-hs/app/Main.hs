module Main where

import System.IO
import System.Exit              (exitSuccess)
import System.Process
import Control.Monad
import Control.Concurrent


import HFO.Server               (ServerConf(..), defaultServer, runServer_)
import HFO.Agent                (AgentConf(..), defaultAgent, runAgent)
import HFO.Parser               (getResults, cleanLog)
-- import System.TimeIt

main :: IO ()
main = do
    cleanLog
    replicateM_ 2 startSimulation

-- | Main entry point for simulation
--   
startSimulation :: IO ()
startSimulation = do

    let serverConf = defaultServer { offenseAgents = 3
                                   , defenseAgents = 4      -- minimum is 1 for the goalie
                                   , untouchedTime = 1000
                                   , trials        = 1
--                                   , showMonitor   = False
--                                   , recordLogs    = True
                                   , standartPace  = True
                                   , giveBallToPlayer = 1 }

    let agentConf  = defaultAgent { episodes = trials serverConf
                                  }

--  Start the server
    runServer_ serverConf

--  Start the offensive agents
    replicateM_ (offenseAgents serverConf) $ runAgent agentConf { teamName = "base_left" }

--  Start the goalie (somehow hardcoded in HFO binary)
    (_, phagent) <- runAgent $ defaultAgent { teamName = "base_right", isGoalie = True }

--  Start the defensive agents (minus the goalie)
    replicateM_ (defenseAgents serverConf - 1) $ runAgent agentConf { teamName = "base_right" }

    aExit <- waitForProcess phagent
-- putStrLn $ " || hfo-genetic-server: Player exited with " ++ show aExit

    dirtyExit
    getResults >>= mapM_ print


-- | stops the execution of HFO & friends
--  
--  System.Process.terminateProcess can not be used because HFO itself spawns processes that somehow
--  are not grouped together. There are no ProcessHandles for those and we have to resort to an ugly solution (for now)
--
dirtyExit :: IO ()
dirtyExit = do
    _ <- rawSystem "killall" ["-9", "rcssserver"]
    _ <- rawSystem "killall" ["-9", "soccerwindow2"]
    _ <- rawSystem "killall" ["-9", "python"]
    _ <- rawSystem "killall" ["-9", "sample_player"]
    return ()