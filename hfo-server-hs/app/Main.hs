{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Main where

import System.Process
import System.Random
import Control.Monad.Random
import Data.Aeson


import Control.Concurrent            (forkIO, ThreadId(), killThread)
import Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.Text.IO   as T (appendFile)
import           Data.Text      as T (pack)
import           Control.Monad       (when)

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer, serverWatcher)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..)
                                ,sleep, Defense(..), Offense(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation
                                ,printPrettyPopulation, writePrettyPopulationTo, readPopulationFrom)
import Evaluator                (getDataFromTo, getBestNPlayers)
import CrossEntropy             (DistributionSet(), createDistributionSet, getMeanAndStd
                                ,createDistributionSetFrom)

import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection
import Genetic.Permutation

-- | Tweak your startup configuration here
--
--   Half-Field Offense server binary configuration (see HFO.Server.Conf)
serverConf :: ServerConf
serverConf = defaultServer { untouchedTime  = 50
                           , framespertrial = 500
                           , noLogging      = True
                           , trials         = popSize * teamEpisodes
                           , offenseAgents  = 1
                           , defenseAgents  = 0
                           , offenseNpcs    = 0
                           , defenseNpcs    = 1
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
generations    = 1  -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popSize :: Int
popSize        = 5  -- population size (for offense as well as defense teams)

teamEpisodes :: Int
teamEpisodes   = 10000  -- amount of trials for every team

alpha :: Double
alpha = 0.25   -- % of best individuals will be selected - [0.0, 0.5] (if its >= 0.5 then we won't have any inherently new individuals)

coefficients :: Int
coefficients = 20  -- # of coefficients for every agent

mean :: Double
mean = 0         -- the (-mean, +mean) for the initial sampling

std :: Double
std = 1.5          -- the (-std, +std) for the initial sampling

-- | Path to save all the intermediate results so we can easily start from the last population
--   if the simulation "broke"
--
intermediateResultsPath :: Int -> FilePath
intermediateResultsPath x = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results/results" ++ show x ++ ".json"


-- | Main entry point
--
main :: IO ()
main = do

{-
--  start with a seed
    let g0 = mkStdGen 31415926
        g1 = mkStdGen 27182818
        g2 = mkStdGen 16180339

--        distributionSet :: DistributionSet
--        distributionSet = flip evalRand g0 $ createDistributionSet mean std coefficients
--
--        defPopulation :: [DefenseTeam]
--        defPopulation = flip evalRand g0 $ genIndividuals 0       distributionSet
--
--        offPopulation :: [OffenseTeam]
--        offPopulation = flip evalRand g2 $ genIndividuals popSize distributionSet

    (defPopulation, offPopulation) <- readPopulationFrom (intermediateResultsPath 15)

    runGA defPopulation offPopulation generations

-}
--   single evaluation has to be compiled to work...(just c++ server things)
    
    (_, off) <- getDataFromTo 1 300
    let best = getBestNPlayers off 5 :: [(OffenseTeam, Int)]
        players = map ((\x -> x { offFitness = ([], []) }) .fst . (best !!)) [0..4]

    startSimulation ([], players) >>= uncurry writePopulation

    print "Haskell: Done with all simulations!"

-- -}


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
    let offSelected = select alpha offenseTeams :: [OffenseTeam]

--  Calculate new normal distributions from the best players

    let offensePlayers = map op1 offSelected :: [Offense]
        newDistSet  = createDistributionSetFrom (getMeanAndStd offensePlayers)

--  Create new individuals with the new 
    offRepopulated <- repopulate popSize offSelected newDistSet

    runGA defense offRepopulated (gen - 1)


-- | Main entry point for simulation
--   
startSimulation :: ([DefenseTeam], [OffenseTeam]) -> IO ([DefenseTeam],[OffenseTeam])
startSimulation (defenseTeams, offenseTeams) = do

    clearLog

--  Write to log so the python agents can access it
    writePopulation defenseTeams offenseTeams

--  Start the server
    serverPh <- runServer serverConf

--  Start the offensive agents
    offphs <- runOffenseTeam agentConf

--  Start server watcher so buggy starts are intercepted (currently after 15 min)
    tid <- forkIO (serverWatcher serverPh)

--  Start the defensive agents and return the handle from goalie
--    defphs <- runDefenseTeam agentConf

--  If any player terminated, the simualtion is over
    waitForProcesses (offphs ++ [serverPh]) -- ++ defphs)

    putStrLn "Haskell: Done Waiting."

--  Securely terminate all running processes of the HFO instances + python scripts (which should not be running anyways) + threadWatcher if still running
    dirtyExit tid

--  Get simulation results
    (def, off) <- readPopulation

--  if the simulation bugged out because of reasons, restart it with the new input
--  that is determined if the last invidiual has no simulation results
    if (null . snd . offFitness . last $ off)
        then do
            print "Haskell: Restarting simulation."
            startSimulation (defenseTeams, offenseTeams)
        else do
            return (def, off)

-- | stops the execution of HFO & friends & serverWatcher
--  
--  System.Process.terminateProcess can not be used because HFO itself spawns processes that somehow
--  are not grouped together. There are no ProcessHandles for those and we have to resort to an ugly solution (for now)
--
dirtyExit :: ThreadId -> IO ()
dirtyExit tid = do
    killThread tid
    sleep 500
    _ <- rawSystem "killall" ["-9", "rcssserver"]
    _ <- rawSystem "killall" ["-9", "soccerwindow2"]
    _ <- rawSystem "killall" ["-9", "python"]
    _ <- rawSystem "killall" ["-9", "sample_player"]
    return ()
