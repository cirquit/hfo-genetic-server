{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Main where

import System.Process
import System.Random
import Control.Monad.Random
import System.Console.ANSI
import Data.Aeson

import Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.Text.IO as T (appendFile)
import           Data.Text    as T (pack)



import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation, writePrettyPopulation
                                , printPrettyPopulation, writePrettyPopulationTo)


import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection

-- | Tweak your startup configuration here
--
--   Half-Field Offense server binary configuration (see HFO.Server.Conf)
serverConf :: ServerConf
serverConf = defaultServer { untouchedTime = 50
                           , trials        = popSize * teamEpisodes
--                           , showMonitor   = False
--                           , standartPace  = True
                           , giveBallToPlayer = 9 }
--
--  Python agent script configuration (see HFO.Agent.Conf)
agentConf :: AgentConf
agentConf = defaultAgent { episodes = teamEpisodes }

-- | Genetic algorithms parameters
--
generations :: Int
generations    = 2 -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popSize :: Int
popSize        = 10 -- population size (for offense as well as defense teams)

teamEpisodes :: Int
teamEpisodes   = 10 -- amount of trials for every team

alpha :: Double
alpha = 0.30   -- % of best individuals will be selected - [0.0, 0.5] (if its >= 0.5 then we won't have any inherently new individuals)

beta  :: Double
beta  = 0.50   -- % of individuals that will be mutated  - [0.0, 1.0]

delta :: Int
delta = 15     -- by how many units will the distribution of actions be changed - [0,100]

-- resultsFile :: FilePath
-- resultsFile = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results.txt"


-- | Main entry point
--
main :: IO ()
main = do

--  color everything in red to differentiate between the output of the hfo-binary and python-agents
    -- setSGR [SetColor Foreground Vivid Black]
    -- setSGR [SetColor Background Vivid Magenta]

--  start with a seed
    let g = mkStdGen 31415926

        defPopulation :: [DefenseTeam]
        defPopulation = flip evalRand g $ genIndividuals popSize

        offPopulation :: [OffenseTeam]
        offPopulation = flip evalRand g $ genIndividuals popSize

    runGA defPopulation offPopulation generations


-- | Main loop for the genetic algorithm
--
runGA :: [DefenseTeam] -> [OffenseTeam] -> Int -> IO ()
runGA defense offense 0   = printPrettyPopulation defense offense >> writePrettyPopulation defense offense
runGA defense offense gen = do

--  Start the simulation for every pair of (defense <-> offense)
    (defenseTeams, offenseTeams) <- startSimulation (defense,offense)

-- -- Selection of alpha % best individuals
--    let defSelected = select alpha defense -- Teams
--        offSelected = select alpha offense -- Teams

----  Crossover of every selected defense and offense among each other (size is equivalent to the parentlist)
--    defChildren <- crossover defSelected
--    offChildren <- crossover offSelected

----  Mutation of beta % children by delta units
--    defMutated  <- mutate beta delta defChildren
--    offMutated  <- mutate beta delta offChildren

----  Repopulation with new individuals - these should amount to popSize - (popSize * alpha * 2)
----  because of parents (popSize * alpha) and children (popSize * alpha)
--    newDefense <- repopulate popSize (defSelected ++ defMutated)
--    newOffense <- repopulate popSize (offSelected ++ offMutated)

--    runGA newDefense newOffense (gen - 1)
    let path = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/results" ++ show gen ++ ".json"

    writePrettyPopulationTo path defenseTeams offenseTeams

--    runGA (select 1.0 defenseTeams) (select 1.0 offenseTeams) (gen - 1)
    runGA defenseTeams offenseTeams (gen - 1)

-- | Main entry point for simulation
--   
startSimulation :: ([DefenseTeam], [OffenseTeam]) -> IO ([DefenseTeam],[OffenseTeam])
startSimulation (defenseTeams, offenseTeams) = do

    clearLog

--  Write to log so the python agents can access it
    writePopulation defenseTeams offenseTeams

--  Start the server
    runServer_ serverConf

--  Start the offensive agents
    offphs <- runOffenseTeam agentConf

--  Start the defensive agents and return the handle from goalie
    defphs <- runDefenseTeam agentConf

--  If any player terminated, the simualtion is over
    waitForProcesses (offphs ++ defphs)

    putStrLn "Done Waiting..."

--  Securely terminate all running processes of the HFO instances + python scripts (which should not be running anyways)
    dirtyExit

--  Get simulation results
    readPopulation

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