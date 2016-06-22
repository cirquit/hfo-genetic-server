{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Main where

import System.Process
import System.Random
import Control.Monad.Random
import System.Console.ANSI

import qualified Data.Text.IO as T (appendFile)
import           Data.Text    as T (pack)

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses)
import HFO.StateParser          (getResults, cleanLog)


import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection

-- | Tweak your startup configuration here
--
--   Half-Field Offense server binary configuration (see HFO.Server.Conf)
serverConf :: ServerConf
serverConf = defaultServer { untouchedTime = 50
                           , trials        = 5
--                           , showMonitor   = False
--                           , standartPace  = True
                           , giveBallToPlayer = 11 }
--
--  Python agent script configuration (see HFO.Agent.Conf)
agentConf :: AgentConf
agentConf = defaultAgent { episodes = trials serverConf }

-- | Genetic algorithms parameters
--
generations :: Int
generations    = 5 -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popsizeDefense :: Int
popsizeDefense = 20

popsizeOffense :: Int
popsizeOffense = 20

alpha :: Double
alpha = 0.30   -- % of best individuals will be selected - [0.0, 0.5] (if its >= 0.5 then we won't have any inherently new individuals)

beta  :: Double
beta  = 0.50   -- % of individuals that will be mutated  - [0.0, 1.0]

delta :: Int
delta = 15     -- by how many units will the distribution of actions be changed - [0,100]

resultsFile :: FilePath
resultsFile = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results.txt"


-- | Main entry point
--
main :: IO ()
main = do

--  color everything in red to differentiate between the output of the hfo-binary and python-agents
    setSGR [SetColor Foreground Vivid Black]
    setSGR [SetColor Background Vivid Magenta]

--  start with a seed
    let g = mkStdGen 31415926

        defPopulation :: [DefenseTeam]
        defPopulation = flip evalRand g $ genIndividuals popsizeDefense 

        offPopulation :: [OffenseTeam]
        offPopulation = flip evalRand g $ genIndividuals popsizeOffense

    runGA defPopulation offPopulation generations

-- |  
--
runGA :: [DefenseTeam] -> [OffenseTeam] -> Int -> IO ()
runGA defense offense 0   = return ()
runGA defense offense gen = do

--  Start the simulation for every pair of (defense <-> offense)
    (defenseTeams, offenseTeams) <- unzipWithM' startSimulation (zip defense offense)

 -- Selection of alpha % best individuals
    let defSelected = select alpha defenseTeams
        offSelected = select alpha offenseTeams

--  Crossover of every selected defense and offense among each other (size is equivalent to the parentlist)
    defChildren <- crossover defSelected
    offChildren <- crossover offSelected


--  Mutation of beta % children by delta units
    defMutated  <- mutate beta delta defChildren
    offMutated  <- mutate beta delta offChildren

--  Repopulation with new individuals - these should amount to popSize - (popSize * alpha * 2)
--  because of parents (popSize * alpha) and children (popSize * alpha)
    newDefense <- repopulate popsizeDefense (defSelected ++ defMutated)
    newOffense <- repopulate popsizeOffense (offSelected ++ offMutated)

    T.appendFile resultsFile (T.pack $ unlines ["Generation: " ++ show gen, show newDefense, show newOffense])

--    print newDefense
--    print newOffense

    runGA newDefense newOffense (gen - 1)

-- | Main entry point for simulation
--   
startSimulation :: (DefenseTeam, OffenseTeam) -> IO (DefenseTeam, OffenseTeam)
startSimulation (defenseTeam, offenseTeam) = do

    cleanLog

--  Start the server
    runServer_ serverConf

--  Start the offensive agents
    offphs <- runOffenseTeam agentConf offenseTeam

--  Start the defensive agents and return the handle from goalie
    defphs <- runDefenseTeam agentConf defenseTeam

--  If any player terminated, the simualtion is over
    waitForProcesses (offphs ++ defphs)

--  Securely terminate all running processes of the HFO instances + python scripts (which should not be running anyways)
    dirtyExit

--  Get simulation results
    results <- getResults

--  Update the team fitness (with Bangs so we avoid lazy IO for sure)
    let (defScore, defList) = defFitness defenseTeam
        (offScore, offList) = offFitness offenseTeam

        defense = defenseTeam { defFitness = (defScore, defList ++ results) }
        offense = offenseTeam { offFitness = (offScore, offList ++ results) }

    return (defense, offense)


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