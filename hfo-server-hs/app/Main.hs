module Main where

import System.IO                
import System.Exit              (exitSuccess)
import System.Process  
import Control.Monad
import Control.Concurrent.MVar
import HFO.Server               (ServerConf(..), defaultServer, ToFlags(..), runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, runAgent)
-- import System.TimeIt

main :: IO ()
main = do
    replicateM_ 2 startSimulation

-- | Main entry point for simulation
--   
startSimulation :: IO ()
startSimulation = do
    let serverConf = defaultServer { defenseAgents = 2
                                   --, defenseAgents = 1
                                   , untouchedTime = 1000
--                                   , showMonitor   = False
--                                   , recordLogs    = True
                                   , standartPace  = True }

    (_, phserver) <- runServer serverConf
    (_, phagent)  <- runAgent  defaultAgent
    (_, phagent2) <- runAgent  defaultAgent

    aExit <- waitForProcess phagent
    putStrLn $ " || hfo-genetic-server: Player exited with " ++ show aExit
    dirtyExit




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