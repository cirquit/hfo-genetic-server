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
    let serverConf = defaultServer { offenseAgents = 1, untouchedTime = 1000 }

    (_, phserver) <- runServer serverConf
--    sleep 3
--    (_, phagent)  <- runAgent  defaultAgent

    
    dirtyExitAfter 5
    printResult phserver


-- | really dirty hack to stop the execution of HFO & friends
-- @TODO: find a better solution (probably in System.Process) 
--
dirtyExitAfter :: Int -> IO ()
dirtyExitAfter i = do
    sleep i
    _ <- rawSystem "killall" ["rcssserver"]
    _ <- rawSystem "killall" ["soccerwindow2"]
    _ <- rawSystem "killall" ["python"]
    _ <- rawSystem "killall" ["sample_player"]
    return ()


-- | delay execution for i seconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^6)


printResult :: ProcessHandle -> IO ()
printResult ph = do
    exit <- waitForProcess ph
    putStrLn $ "Process exited with " ++ show exit