module Main where

import System.IO
import System.Exit              (exitSuccess)
import System.Process
import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Concurrent

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, runAgent, defaultOffense, defaultDefense)
import HFO.Parser               (getResults, cleanLog, HFOStates(..))


serverConf :: ServerConf
serverConf = defaultServer { offenseAgents = 3
                           , defenseAgents = 4      -- minimum is 1 for the goalie
                           , untouchedTime = 500
                           , trials        = 2
--                           , showMonitor   = False
--                           , recordLogs    = True
                           , standartPace  = True
                           , giveBallToPlayer = 1 }

agentConf :: AgentConf
agentConf = defaultAgent { episodes = trials serverConf }



main :: IO ()
main = do

    let popsizeDefense = 20
        popsizeOffense = 20

--        defPopulation :: [Defense]
--        defPopulation = flip evalRand g $ genIndividuals popsizeDefense 

--        offPopulation :: [Offense]
--        offPopulation = flip evalRand g $ genIndividuals popsizeOffense

    runGA 

runGA :: IO ()
runGA = do
    cleanLog
    replicateM_ 1 startSimulation

-- | Main entry point for simulation
--   
startSimulation :: IO [Maybe HFOStates]
startSimulation = do

    let agentConf   = defaultAgent { episodes = trials serverConf }
        offenseConf = agentConf    { actions  = Left  defaultOffense , teamName = "base_left"  }
        defenseConf = agentConf    { actions  = Right defaultDefense , teamName = "base_right" }

--  Start the server
    runServer_ serverConf

--  Start the offensive agents
    replicateM_ (offenseAgents serverConf) $ runAgent offenseConf

--  Start the goalie (somehow hardcoded in HFO binary)
    (_, phagent) <- runAgent $ defenseConf { isGoalie = True }

--  Start the defensive agents (minus the goalie)
    replicateM_ (defenseAgents serverConf - 1) $ runAgent defenseConf

    aExit <- waitForProcess phagent
    dirtyExit
    getResults


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