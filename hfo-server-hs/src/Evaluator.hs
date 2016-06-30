{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Evaluator where


import Data.Aeson

import Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.Text.IO   as T (appendFile)
import           Data.Text      as T (pack)
import           Data.List           (genericLength)
import           System.Process      (proc, createProcess, CreateProcess(..))

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


resultsFile n = concat [ "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results/"
                     , "29_06_v" ++ show 2 ++ "/"
                     , "results" ++ show n ++ ".json"
                     ]

graphsLogFile = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/graphs/info/"


plotEverything :: IO ()
plotEverything = createProcess cproc { cwd = cwd } >> return ()
    where

        cproc :: CreateProcess
        cproc = proc "./ploteverything" args

        args :: [String]
        args = []

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/graphs"


testServerConf :: ServerConf
testServerConf = defaultServer { untouchedTime = 50
                               , trials        = testGamesCount
--                               , showMonitor   = False
                               , standartPace  = True
                               , giveBallToPlayer = 9
                               }
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


countFitness :: Either OffenseTeam DefenseTeam -> Double
countFitness team =
        -- printf "%-20s: %-6f, %6f%%\n"  "Goals"             goals (roundTo ((goals / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "OutOfTime"         oot   (roundTo ((oot   / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "CapturedByDefense" cbd   (roundTo ((cbd   / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "OutOfBounds"       oob   (roundTo ((oob   / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "ServerDown"        sd    (roundTo ((sd    / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "Ingame"            ing   (roundTo ((ing   / len) * 100) 2)
        -- printf "%-20s: %-6f, %6f%%\n"  "Failed Parse"      err   (roundTo ((err   / len) * 100) 2)
        -- printf "%-20s: %-6f\n"         "Games"             len

        case team of
            Left  _ -> to2Percent goals
            Right _ -> to2Percent cbd
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

            to2Percent :: Double -> Double
            to2Percent n = (roundTo ((n / len) * 100) 2)

readInformationFromTo :: Int -> Int -> IO ()
readInformationFromTo n m = do
        (defenseTeams, offenseTeams) <- ioContent
        let defMax  = map (maxFitness  . Right) defenseTeams
            defMean = map (meanFitness . Right) defenseTeams
            offMax  = map (maxFitness  . Left)  offenseTeams
            offMean = map (meanFitness . Left)  offenseTeams

            genList = [1..(length defenseTeams)]

            defGamesCount = map (maxFitGamesCount . Right) defenseTeams
            offGamesCount = map (maxFitGamesCount . Left)  offenseTeams

            defenseContent = unlines $ zipWith (\x y -> show x ++ " " ++ show y) defMax defMean
            offenseContent = unlines $ zipWith (\x y -> show x ++ " " ++ show y) offMax offMean

            defenseMaxCount = let l1 = zipWith (\x y -> show x ++ " " ++ show y) defMax defGamesCount
                                  l2 = zipWith (\x y -> x ++ " " ++ show y) l1     genList
                              in unlines l2

            offenseMaxCount = let l1 = zipWith (\x y -> show x ++ " " ++ show y) offMax offGamesCount
                                  l2 = zipWith (\x y -> x ++ " " ++ show y) l1     genList
                              in unlines l2


        writeFile (graphsLogFile ++ "defenseContent.txt") defenseContent
        writeFile (graphsLogFile ++ "offenseContent.txt") offenseContent

        writeFile (graphsLogFile ++ "defenseMaxCount.txt") defenseMaxCount
        writeFile (graphsLogFile ++ "offenseMaxCount.txt") offenseMaxCount

        plotEverything

    where

        ioContent :: IO ([[DefenseTeam]], [[OffenseTeam]])
        ioContent = ((\(x,y) -> (reverse x, reverse y)) . unzip) <$> mapM (readPopulationFrom . resultsFile) [n .. m]

        maxFitness :: Either [OffenseTeam] [DefenseTeam] -> Double
        maxFitness (Left  offs) = (countFitness . Left)  . head . sortByDescFitness $ offs
        maxFitness (Right defs) = (countFitness . Right) . head . sortByDescFitness $ defs

        meanFitness :: Either [OffenseTeam] [DefenseTeam] -> Double
        meanFitness (Left  offs) = (foldr ((+) . countFitness . Left)  0.0 offs) / genericLength offs
        meanFitness (Right defs) = (foldr ((+) . countFitness . Right) 0.0 defs) / genericLength defs


        maxFitGamesCount :: Either [OffenseTeam] [DefenseTeam] -> Int
        maxFitGamesCount (Left  offs) = (games . Left)  . head . sortByDescFitness $ offs
        maxFitGamesCount (Right defs) = (games . Right) . head . sortByDescFitness $ defs

        games :: Either OffenseTeam DefenseTeam -> Int
        games (Left  off) = length . snd . offFitness $ off
        games (Right def) = length . snd . defFitness $ def