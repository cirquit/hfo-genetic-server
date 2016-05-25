module Main where

import System.IO                
import System.Exit              (exitSuccess)
import System.Process  
import Control.Monad
import Control.Concurrent       (threadDelay)
import Control.Concurrent.MVar
import HFO.Server               (ServerConf(..), defaultServer, ToFlags(..), runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, runAgent)
-- import System.TimeIt

main :: IO ()
main = do
    replicateM_ 3 startSimulation

startSimulation :: IO ()
startSimulation = do
    let serverConf = defaultServer { offenseAgents = 1, untouchedTime = 1000 }

    (_, phserver) <- runServer serverConf
    sleep 2
    (_, phagent)  <- runAgent  defaultAgent

    exit <- waitForProcess phagent
    dirtyExitAfter 0



-- | really dirty hack to stop the execution of HFO & friends
-- @TODO: find a better solution (probably in System.Process) 
--
-- @TODO: rcssserver wont terminate, it has to be killed manually
dirtyExitAfter :: Int -> IO ()
dirtyExitAfter i = do
    sleep i
    _ <- rawSystem "killall" ["-9", "rcssserver"]
    _ <- rawSystem "killall" ["-9", "soccerwindow2"]
    _ <- rawSystem "killall" ["-9", "python"]
    _ <- rawSystem "killall" ["-9", "sample_player"]
    return ()


-- | delay execution for i seconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^6)