{-# LANGUAGE RecordWildCards #-}

module Main where

import System.IO
import System.Exit              (exitSuccess)
import System.Process
import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad
import Control.Concurrent
import Data.List                (sort)

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, runAgent, defaultOffense, defaultDefense
                                ,DefenseTeam(..), OffenseTeam(..), defaultDefenseTeam, defaultOffenseTeam
                                ,Defense(..), Offense(..), runDefenseTeam, runOffenseTeam)
import HFO.Parser               (getResults, cleanLog, HFOState(..))



import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection

-- | Tweak your startup configuration here
--
--   Half-Field Offense server binary configuration (see HFO.Server.Conf)
serverConf :: ServerConf
serverConf = defaultServer { untouchedTime = 100
                           , trials        = 2
--                           , showMonitor   = False
                           , standartPace  = True
                           , giveBallToPlayer = 9 }
--
--  Python agent script configuration (see HFO.Agent.Conf)
agentConf :: AgentConf
agentConf = defaultAgent { episodes = trials serverConf }

-- | Genetic algorithms parameters
--
generations :: Int
generations    = 5 -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popsizeDefense :: Int
popsizeDefense = 2

popsizeOffense :: Int
popsizeOffense = 2

alpha :: Double
alpha = 0.30   -- % of best individuals will be selected - [0.0, 1.0]

beta  :: Double
beta  = 0.20   -- % of individuals that will be mutated  - [0.0, 1.0]

delta :: Int
delta = 15     -- by how many units will the distribution of actions be changed - [0,100]


-- | Main entry point
--
main :: IO ()
main = do

    let g = mkStdGen 31415926

        defPopulation :: [DefenseTeam]
        defPopulation = flip evalRand g $ genIndividuals popsizeDefense 

        offPopulation :: [OffenseTeam]
        offPopulation = flip evalRand g $ genIndividuals popsizeOffense

    runGA defPopulation offPopulation generations

runGA :: [DefenseTeam] -> [OffenseTeam] -> Int -> IO ()
runGA defense offense gen = do

    (defenseTeams, offenseTeams) <- unzipWithM startSimulation (zip defense offense)
    print "Fukin done, mate"

-- | Main entry point for simulation
--   
startSimulation :: (DefenseTeam, OffenseTeam) -> IO (DefenseTeam, OffenseTeam)
startSimulation (defenseTeam, offenseTeam) = do

    cleanLog

--  Start the server
    runServer_ serverConf

--  Start the offensive agents
    runOffenseTeam agentConf offenseTeam

--  Start the defensive agents and return the handle from goalie
    processHandle <- runDefenseTeam agentConf defenseTeam

--  If goalie terminated, the simualtion is over
    aExit <- waitForProcess processHandle

--  Securely terminate all running processes of the HFO instances + python scripts (which should not be running anyways)
    dirtyExit

--  Get simulation results
    results <- getResults

--  Update the team fitness
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


g0 = mkStdGen 123123999
g1 = mkStdGen 123123123
g2 = mkStdGen 123123234
g3 = mkStdGen 123123345
g4 = mkStdGen 123123456
g5 = mkStdGen 123123567
g6 = mkStdGen 123123678
g7 = mkStdGen 123123789
g8 = mkStdGen 123123890
g9 = mkStdGen 123123901

runRandom f = map (evalRand f) [g0, g1, g2, g3, g4, g5, g6, g7, g8, g9]