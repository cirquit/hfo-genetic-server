{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Evaluator where


import Data.Aeson

import Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.Text.IO as T (appendFile)
import           Data.Text    as T (pack)


import Text.Printf


import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer)
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..), HFOState(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation
                                , printPrettyPopulation, writePrettyPopulationTo, readPopulationFrom)


import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection


resultsFile = concat [ "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results/"
                     , "27_06_v" ++ show 2 ++ "/"
                     , "results" ++ show 1 ++ ".json"
                     ]


testServerConf :: ServerConf
testServerConf = defaultServer { untouchedTime = 50
                               , trials        = testGamesCount
--                               , showMonitor   = False
                               , standartPace  = True
                               , giveBallToPlayer = 9 }
--
testAgentConf :: AgentConf
testAgentConf = defaultAgent { episodes = testGamesCount }

testGamesCount = 5

startSingleSimulation :: DefenseTeam -> OffenseTeam -> IO (DefenseTeam, OffenseTeam)
startSingleSimulation defense offense = do

    clearLog

    writePopulation [defense] [offense]

--  Start the server
    runServer_ testServerConf

--  Start the offensive agents
    offphs <- runOffenseTeam testAgentConf

--  Start the defensive agents and return the handle from goalie
    defphs <- runDefenseTeam testAgentConf

--  If any player terminated, the simualtion is over
    waitForProcesses (offphs ++ defphs)

    uncurry (\[x] [y] -> (x,y)) <$> readPopulation


countFitness :: Either OffenseTeam DefenseTeam -> IO ()
countFitness team = do
        printf "%-20s: %-6f, %6f%%\n"  "Goals"             goals (roundTo ((goals / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "OutOfTime"         oot   (roundTo ((oot   / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "CapturedByDefense" cbd   (roundTo ((cbd   / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "OutOfBounds"       oob   (roundTo ((oob   / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "ServerDown"        sd    (roundTo ((sd    / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "Ingame"            ing   (roundTo ((ing   / len) * 100) 2)
        printf "%-20s: %-6f, %6f%%\n"  "Failed Parse"      err   (roundTo ((err   / len) * 100) 2)
        printf "%-20s: %-6f\n"         "Games"             len

    where

            (goals, oot, cbd, oob, sd, ing, err, len) = foldl go (0,0,0,0,0,0,0,0) maybeStates

            maybeStates :: [Maybe HFOState]
            maybeStates = case team of
                    Left offense -> snd $ offFitness offense
                    Right defense -> snd $ defFitness defense

            go :: (Double, Double, Double, Double, Double, Double, Double, Double)
               -> Maybe HFOState
               -> (Double, Double, Double, Double, Double, Double, Double, Double)
            go (a,b,c,d,e,f,g,len) (Just Goal)               = (a+1,b,  c,  d  ,e,  f,  g,   len + 1)
            go (a,b,c,d,e,f,g,len) (Just OutOfTime)          = (a,  b+1,c,  d  ,e,  f,  g,   len + 1)
            go (a,b,c,d,e,f,g,len) (Just CapturedByDefense)  = (a,  b,  c+1,d  ,e,  f,  g,   len + 1)
            go (a,b,c,d,e,f,g,len) (Just OutOfBounds)        = (a,  b,  c,  d+1,e,  f,  g,   len + 1)
            go (a,b,c,d,e,f,g,len) (Just ServerDown)         = (a,  b,  c,  d  ,e+1,f,  g,   len + 1)
            go (a,b,c,d,e,f,g,len) (Just Ingame)             = (a,  b,  c,  d  ,e,  f+1,g,   len + 1)
            go (a,b,c,d,e,f,g,len) _                         = (a,  b,  c,  d  ,e,  f,  g+1, len + 1)

            roundTo :: Double -> Int -> Double
            roundTo x n = (fromInteger $ round $ x * (10^n)) / (10.0^^n)