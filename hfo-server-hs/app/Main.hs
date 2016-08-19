{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Main where

import System.Process
import System.Random
import Control.Monad.Random
import Data.Aeson

import Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.Text.IO   as T (appendFile)
import           Data.Text      as T (pack)
import           Control.Monad       (when)

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..)
                                ,sleep, Defense(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation
                                , printPrettyPopulation, writePrettyPopulationTo, readPopulationFrom)

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
                           , offenseAgents = 1
                           , defenseAgents = 0
                           , offenseNpcs   = 0
                           , defenseNpcs   = 1
--                           , showMonitor   = False
--                           , standartPace  = True
                           , giveBallToPlayer = 1   -- 1 should give it to the first player...with the number 7
                           }
--
--  Python agent script configuration (see HFO.Agent.Conf)
agentConf :: AgentConf
agentConf = defaultAgent { episodes = teamEpisodes }

-- | Genetic algorithms parameters
--
generations :: Int
generations    = 50 -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popSize :: Int
popSize        = 50 -- population size (for offense as well as defense teams)

teamEpisodes :: Int
teamEpisodes   = 15  -- amount of trials for every team

alpha :: Double
alpha = 0.35   -- % of best individuals will be selected - [0.0, 0.5] (if its >= 0.5 then we won't have any inherently new individuals)

beta  :: Double
beta  = 0.5    -- % of individuals that will be mutated  - [0.0, 1.0]

delta :: Int
delta = 20     -- by how many units will the distribution of actions be changed - [0,100]

lambda :: Double
lambda = 0.5   -- % of fieldseparations will be mutated  - [0.0, 1.0]

-- | Path to save all the intermediate results so we can easily start from the last population
--   if the simulation "broke"
--
intermediateResultsPath :: Int -> FilePath
intermediateResultsPath x = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results/results" ++ show x ++ ".json"


-- | Main entry point
--
main :: IO ()
main = do

--  start with a seed
    let g = mkStdGen 31415926

--        defPopulation :: [DefenseTeam]
--        defPopulation = flip evalRand g $ genIndividuals 0 -- popSize
--
--        offPopulation :: [OffenseTeam]
--        offPopulation = flip evalRand g $ genIndividuals popSize

    (defPopulation, offPopulation) <- readPopulationFrom (intermediateResultsPath 21)

    runGA defPopulation offPopulation generations


-- | Main loop for the genetic algorithm
--
runGA :: [DefenseTeam] -> [OffenseTeam] -> Int -> IO ()
runGA defense offense 0   = writePrettyPopulationTo (intermediateResultsPath 0) defense offense
runGA defense offense gen = do

--  Start the simulation for every pair of (defense <-> offense)
    (defenseTeams, offenseTeams) <- startSimulation (defense,offense)

    let savePath = intermediateResultsPath gen :: FilePath
    writePrettyPopulationTo savePath defenseTeams offenseTeams


 -- Selection of alpha % best individuals
    let defSelected = select alpha defenseTeams
        offSelected = select alpha offenseTeams

--  Crossover of every selected defense and offense among each other (size is equivalent to the parentlist)
    defChildren <- crossover defSelected
    offChildren <- crossover offSelected

--  Mutation of beta % children by delta units
    defMutated  <- mutate beta delta lambda defChildren
    offMutated  <- mutate beta delta lambda offChildren

--  Repopulation with new individuals - these should amount to popSize - (popSize * alpha * 2)
--  because of parents (popSize * alpha) and children (popSize * alpha)
    newDefense <- repopulate popSize (defSelected ++ defMutated)
    newOffense <- repopulate popSize (offSelected ++ offMutated)

    runGA newDefense newOffense (gen - 1)


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
--    defphs <- runDefenseTeam agentConf

--  If any player terminated, the simualtion is over
    waitForProcesses (offphs) -- ++ defphs)

    putStrLn "Done Waiting..."

--  Securely terminate all running processes of the HFO instances + python scripts (which should not be running anyways)
    dirtyExit

--  Get simulation results
    (def, off) <- readPopulation

--  if the simulation bugged out because of reasons, restart it with the new input
--  that is determined if the last invidiual has no simulation results
    if (null . snd . offFitness . last $ off)
        then do
            print "restarting simulation..."
            startSimulation (defenseTeams, offenseTeams)
        else do
            return (def, off)


-- | stops the execution of HFO & friends
--  
--  System.Process.terminateProcess can not be used because HFO itself spawns processes that somehow
--  are not grouped together. There are no ProcessHandles for those and we have to resort to an ugly solution (for now)
--
dirtyExit :: IO ()
dirtyExit = do
    sleep 500
    _ <- rawSystem "killall" ["-9", "rcssserver"]
    _ <- rawSystem "killall" ["-9", "soccerwindow2"]
    _ <- rawSystem "killall" ["-9", "python"]
    _ <- rawSystem "killall" ["-9", "sample_player"]
    return ()