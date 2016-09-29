{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Main where

import System.Process
import System.Random
import Control.Monad.Random
import Data.Aeson


import Data.Aeson.Encode.Pretty           (encodePretty)
import qualified Data.Text.IO        as T (appendFile)
import           Data.Text           as T (pack)
import           Control.Monad            (when)
import           Control.Concurrent       (forkIO, ThreadId, killThread)

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer, serverWatcher)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..)
                                ,sleep, Defense(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation
                                , printPrettyPopulation, writePrettyPopulationTo, readPopulationFrom)

import Evaluator                (getDataFromTo, getBestNPlayers)

import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection

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

--  Python agent script configuration (see HFO.Agent.Conf)
agentConf :: AgentConf
agentConf = defaultAgent { episodes = teamEpisodes }

-- | Genetic algorithms parameters
--
generations :: Int
generations    = 300  -- how many times does the GA loop (Simulation -> Selection -> Crossover -> Mutation)

popSize :: Int
popSize        = 5  -- population size (for offense as well as defense teams)

teamEpisodes :: Int
teamEpisodes   = 10000  -- amount of trials for every team

alpha :: Double
alpha = 0.25   -- % of best individuals will be selected - [0.0, 0.5] (if its >= 0.5 then we won't have any inherently new individuals)

beta  :: Double
beta  = 0.1  -- % of individuals that will be mutated  - [0.0, 1.0]

phi :: Double
phi = 3        -- (-phi, +phi) sample space for coefficients

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

        defPopulation :: [DefenseTeam]
        defPopulation = flip evalRand g0 $ genIndividuals 0       phi

        offPopulation :: [OffenseTeam]
        offPopulation = flip evalRand g1 $ genIndividuals popSize phi

--    (defPopulation, offPopulation) <- readPopulationFrom (intermediateResultsPath 21)

    runGA defPopulation offPopulation generations

-}

--   single evaluation has to be compiled to work...(just c++ server things)
    
    (_, off) <- getDataFromTo 1 300
    let best = getBestNPlayers off 5 :: [(OffenseTeam, Int)]
        players = map ((\x -> x { offFitness = ([], []) }) .fst . (best !!)) [0..4]

    startSimulation ([], players) >>= uncurry writePopulation

    print "Haskell: Done with all simulations!"





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

--  Mutation of beta % children
    defMutated  <- mutate beta phi defChildren
    offMutated  <- mutate beta phi offChildren

--  Repopulation with new individuals - these should amount to popSize - (popSize * alpha * 2)
--  because of parents (popSize * alpha) and children (popSize * alpha)
    newDefense <- repopulate popSize (defSelected ++ defMutated) phi
    newOffense <- repopulate popSize (offSelected ++ offMutated) phi

    runGA newDefense newOffense (gen - 1)



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


-- | stops the execution of HFO & friends
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