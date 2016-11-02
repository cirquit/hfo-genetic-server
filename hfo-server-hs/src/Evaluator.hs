{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Evaluator where

import Data.Aeson

import Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.Text.IO   as T (appendFile)
import           Data.Text      as T (pack)
import           Data.List           (genericLength, maximumBy, sortBy)
import           Data.Function       (on)
import           System.Process      (proc, createProcess, CreateProcess(..))

import           Text.Printf
import           Data.List.Split     (chunksOf)
import           Control.Category    ((>>>))
import           Data.Ord            (comparing)
import           Data.Foldable       (maximumBy)

import           Data.Conduit
import qualified Data.Conduit.List as CL

import HFO.Server               (ServerConf(..), defaultServer, runServer_, runServer, Team(..))
import HFO.Agent                (AgentConf(..), defaultAgent, DefenseTeam(..), OffenseTeam(..)
                                ,runDefenseTeam, runOffenseTeam, waitForProcesses, SerializedTeams(..)
                                , HFOState(..),Offense(..), Defense(..), ActionDist(..))
import HFO.StateParser          (clearLog, writePopulation, readPopulation
                                , printPrettyPopulation, writePrettyPopulationTo, readPopulationFrom)


import Genetic.Allele
import Genetic.Mutation
import Genetic.Crossover
import Genetic.Selection

resultsFile n = concat [ "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/results/"
                     , "goal-fitness/"
                     , "1v1/"
                     , "actiondist-25-trails/"
                     , "json-data/"
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
testServerConf = defaultServer { untouchedTime    = 50
                               , framespertrial   = 500
                               , noLogging        = True
                               , trials           = testGamesCount
--                               , showMonitor   = False
                               , offenseAgents    = 1
                               , defenseAgents    = 0
                               , offenseNpcs      = 0
                               , defenseNpcs      = 1
                               , standartPace     = True
                               , giveBallToPlayer = 1 -- gives the ball to the first player...with the number 7
                               }

--
testAgentConf :: AgentConf
testAgentConf = defaultAgent { episodes = testGamesCount }

testGamesCount = 5

startSingleSimulation :: OffenseTeam -> IO OffenseTeam
startSingleSimulation offense = do

    clearLog

--  reset the previous states
    let newOffense = offense { offFitness = ([],[]) }

    defense <- genIndividual :: IO DefenseTeam 

    writePopulation [defense] [newOffense]

--  Start the server
    runServer_ testServerConf

--  Start the offensive agents
    offphs <- runOffenseTeam testAgentConf

--  Start the defensive agents and return the handle from goalie
--    defphs <- runDefenseTeam testAgentConf

--  If any player terminated, the simualtion is over
    waitForProcesses offphs
--    waitForProcesses (offphs ++ defphs)
    uncurry (\[x] [y] -> y) <$> readPopulation




evaluate :: [[OffenseTeam]] -> IO ()
evaluate offTeams = do
        let infos           = foldl go [[],[],[],[]] offTeams
            bestFitness     = concat (infos !! 0)   -- best per generation               ~ [Double]
            meanFitness     = concat (infos !! 1)   -- mean per generation               ~ [Double]
--            bestBallActions = infos !! 2            -- best distribution per generation  ~ [[Shoot, Dribble, NoOp]]
--            meanBallActions = infos !! 3            -- mean distribution per generation  ~ [[Shoot, Dribble, NoOp]]
            bestActions     = infos !! 2            -- best distribution per generation  ~ [[Move, Intercept, Shoot, Dribble, NoOp]]
            meanActions     = infos !! 3            -- mean distribution per generation  ~ [[Move, Intercept, Shoot, Dribble, NoOp]]


        writeFile (graphsLogFile ++ "offenseFitness.dat")     (unlines $ zipWith (\x y -> show x ++ ' ':(show y)) bestFitness meanFitness)
        writeFile (graphsLogFile ++ "offenseActDist.dat")     (unlines $ map (unwords . map show) meanActions)
--        writeFile (graphsLogFile ++ "offenseBallActDist.dat") (unlines $ map (unwords . map show) meanBallActions)

    where
        go :: [[[Double]]] -> [OffenseTeam] -> [[[Double]]]
        go [bFit, mFit, bAct, mAct] offs = [ [curBestFitness]     : bFit
                                           , [curMeanFitness]     : mFit
--                                           , curBestBallActions   : bBAct
--                                           , curMeanBallActions   : mBAct
                                           , curBestActions       : bAct
                                           , curMeanActions       : mAct
                                           ]
            where
                curBestFitness :: Double
                curBestFitness = fromIntegral . classify $ sortedOffs !! 0

--                curBestBallActions :: [Double]
--                curBestBallActions =  getBallActions $ sortedOffs !! 0

                curBestActions :: [Double]
                curBestActions =  getActions     $ sortedOffs !! 0

                curMeanFitness :: Double
                curMeanFitness = (fromIntegral $ sum (map classify sortedOffs)) / teamCount

--                curMeanBallActions :: [Double]
--                curMeanBallActions = zipWith (flip (/)) (replicate 4 teamCount)
--                                   $ foldl (zipWith (+)) (repeat 0) (map getBallActions sortedOffs)

                curMeanActions :: [Double]
                curMeanActions = zipWith (flip (/))  (repeat teamCount)
                               $ foldl (zipWith (+)) (repeat 0) (map getActions sortedOffs)

--                getBallActions :: OffenseTeam -> [Double]
--                getBallActions = map (fromIntegral . snd) . ballActionDist . offBallActionDist . op1

                getActions :: OffenseTeam -> [Double]
                getActions = map (fromIntegral . snd) . actionDist . offActionDist . op1

                sortedOffs :: [OffenseTeam]
                sortedOffs = take teamCount $ sortByDescFitness offs  -- take 14 because we don't want the random generated individuals to influence the results

                teamCount :: Num a => a
                teamCount = 37



{-
-- get the distribution of all the actions for every of the 16th fields
--
getOffDistData :: Offense -> [[Int]]
getOffDistData off = [move, intercept, catch, noop, shoot, dribble, pass7, pass11]
    where
        move      = map (snd . (!! 0) . actionDist) (offActionDist off)
        intercept = map (snd . (!! 1) . actionDist) (offActionDist off)
        catch     = map (snd . (!! 2) . actionDist) (offActionDist off)
        noop      = map (snd . (!! 3) . actionDist) (offActionDist off)
        shoot     = map (snd . (!! 0) . ballActionDist) (offBallActionDist off)
        dribble   = map (snd . (!! 1) . ballActionDist) (offBallActionDist off)
        pass7     = map (snd . (!! 2) . ballActionDist) (offBallActionDist off)
        pass11    = map (snd . (!! 3) . ballActionDist) (offBallActionDist off)

saveDistData :: [[Int]] -> IO ()
saveDistData dists = do
    let move      = dists !! 0
        intercept = dists !! 1
        catch     = dists !! 2
        noop      = dists !! 3
        shoot     = dists !! 4
        dribble   = dists !! 5
        pass7     = dists !! 6
        pass11    = dists !! 7

        adjustFormat = unlines . map unwords . chunksOf 4 . map show

    writeFile (graphsLogFile ++ "move.dat") (adjustFormat move)
    writeFile (graphsLogFile ++ "intercept.dat") (adjustFormat intercept)
    writeFile (graphsLogFile ++ "catch.dat") (adjustFormat catch)
    writeFile (graphsLogFile ++ "noop.dat") (adjustFormat noop)
    writeFile (graphsLogFile ++ "shoot.dat") (adjustFormat shoot)
    writeFile (graphsLogFile ++ "dribble.dat") (adjustFormat dribble)
    writeFile (graphsLogFile ++ "pass7.dat") (adjustFormat pass7)
    writeFile (graphsLogFile ++ "pass11.dat") (adjustFormat pass11)

saveAvgMaxFitness :: Int -> Int -> IO ()
saveAvgMaxFitness n m = do
        (_, offs) <- getDataFromTo n m
        let maxFitness   = map (maximum . map classify . take 7) offs   -- we only take the first 7 teams because the rest is newly generated
            meanFitness  = map (mean    . map classify . take 7) offs   --
            gamesCount   = map (stateCount . maximumBy (comparing fst) . (\x -> zip (map classify x) x) . take 7) offs
            
            stateCount :: (Int, OffenseTeam) -> Int
            stateCount = length . snd . offFitness . snd
    
            combinedData = zipWith (\x y -> show x ++ ' ' : show y) maxFitness meanFitness -- gamesCount

        writeFile (graphsLogFile ++ "offenseFitness.dat") (unlines combinedData)


mean :: [Int] -> Int
mean l = sum l `div` len
    where
        len = length l

-}

getDataFromTo :: Int -> Int -> IO ([[DefenseTeam]], [[OffenseTeam]])
getDataFromTo n m = go <$> mapM (readPopulationFrom . resultsFile) [n .. m]
    where
        go :: [([DefenseTeam], [OffenseTeam])] -> ([[DefenseTeam]], [[OffenseTeam]])
        go = unzip

        rev :: ([a],[b]) -> ([a], [b])
        rev (x,y) = (reverse x, reverse y)

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

getBestNPlayers :: [[OffenseTeam]] -> Int -> [(OffenseTeam, Int)]
getBestNPlayers offs n = take n . sortBy (flip compare `on` snd) $ map go offs
    where
        go :: [OffenseTeam] -> (OffenseTeam, Int)
        go = maximumBy (compare `on` snd) . map (\x -> (x, classify x))

--readInformationFromTo :: Int -> Int -> IO ()
--readInformationFromTo n m = do
--        (defenseTeams, offenseTeams) <- ioContent
--        let defMax  = map (maxFitness  . Right) defenseTeams
--            defMean = map (meanFitness . Right) defenseTeams
--            offMax  = map (maxFitness  . Left)  offenseTeams
--            offMean = map (meanFitness . Left)  offenseTeams

--            genList = [1..(length defenseTeams)]

--            defGamesCount = map (maxFitGamesCount . Right) defenseTeams
--            offGamesCount = map (maxFitGamesCount . Left)  offenseTeams

--            bestDefActions = map (bestActions . Right) defenseTeams
--            bestOffActions = map (bestActions . Left)  offenseTeams 


--            defenseContent = unlines $ zipWith (\x y -> show x ++ " " ++ show y) defMax defMean
--            offenseContent = unlines $ zipWith (\x y -> show x ++ " " ++ show y) offMax offMean

--            defenseMaxCount = let l1 = zipWith (\x y -> show x ++ " " ++ show y) defMax defGamesCount
--                                  l2 = zipWith (\x y ->      x ++ " " ++ show y) l1     genList
--                              in unlines l2

--            offenseMaxCount = let l1 = zipWith (\x y -> show x ++ " " ++ show y) offMax offGamesCount
--                                  l2 = zipWith (\x y -> x ++ " " ++ show y) l1     genList
--                              in unlines l2

--            defenseActionsDist = unlines $ map (unwords . map show) bestDefActions
--            offenseActionsDist = unlines $ map (unwords . map show) bestOffActions

----        writeFile (graphsLogFile ++ "defenseContent.txt") defenseContent
--        writeFile (graphsLogFile ++ "offenseContent.txt") offenseContent

----        writeFile (graphsLogFile ++ "defenseMaxCount.txt") defenseMaxCount
--        writeFile (graphsLogFile ++ "offenseMaxCount.txt") offenseMaxCount

----        writeFile (graphsLogFile ++ "defenseActionsDist.txt") defenseActionsDist
--        writeFile (graphsLogFile ++ "offenseActionsDist.txt") offenseActionsDist

--        plotEverything

--    where

--        ioContent :: IO ([[DefenseTeam]], [[OffenseTeam]])
--        ioContent = ((\(x,y) -> (reverse x, reverse y)) . unzip) <$> mapM (readPopulationFrom . resultsFile) [n .. m]

--        maxFitness :: Either [OffenseTeam] [DefenseTeam] -> Double
--        maxFitness (Left  offs) = (countFitness . Left)  . head . sortByDescFitness $ offs
--        maxFitness (Right defs) = (countFitness . Right) . head . sortByDescFitness $ defs

--        meanFitness :: Either [OffenseTeam] [DefenseTeam] -> Double
--        meanFitness (Left  offs) = (foldr ((+) . countFitness . Left)  0.0 offs) / genericLength offs
--        meanFitness (Right defs) = (foldr ((+) . countFitness . Right) 0.0 defs) / genericLength defs

--        maxFitGamesCount :: Either [OffenseTeam] [DefenseTeam] -> Int
--        maxFitGamesCount (Left  offs) = (games . Left)  . head . sortByDescFitness $ offs
--        maxFitGamesCount (Right defs) = (games . Right) . head . sortByDescFitness $ defs

--        games :: Either OffenseTeam DefenseTeam -> Int
--        games (Left  off) = length . snd . offFitness $ off
--        games (Right def) = length . snd . defFitness $ def


--        bestActions :: Either [OffenseTeam] [DefenseTeam] -> [Double]
--        bestActions (Right def) = map (/ genericLength def)
--                                . foldl (zipWith (+)) [0,0,0,0,0,0]
--                                $ map bestDefenseActionDist def
--        bestActions (Left off) = map (/ genericLength off)
--                                . foldl (zipWith (+)) [0,0,0,0,0,0,0,0,0]
--                                $ map bestOffenseActionDist off



--        bestOffenseActionDist :: OffenseTeam -> [Double]
--        bestOffenseActionDist (OffenseTeam op1 op2 op3 op4 _)  = [ fromIntegral $ maximum (map getMoveProb      opList)
--                                                                 , fromIntegral $ maximum (map getInterceptProb opList)
--                                                                 , fromIntegral $ maximum (map getCatchProb     opList)
--                                                                 , fromIntegral $ maximum (map getNoOpProb      opList)
--                                                                 , fromIntegral $ maximum (map getShootProb     opList)
--                                                                 , fromIntegral $ maximum (map getDribProb      opList)
--                                                                 , fromIntegral $ maximum (map getPass7Prob     opList)
----                                                                 , fromIntegral $ maximum (map getPass8Prob     opList)
----                                                                 , fromIntegral $ maximum (map getPass9Prob     opList)
--                                                                 , fromIntegral $ maximum (map getPass11Prob    opList)
--                                                                 , fromIntegral $ maximum (map getMoveToProb     opList)
--                                                                 ]
--            where
--                opList = [op1, op2]
--        --        non-ballActions
--                getMoveProb       = snd . (\(x:_)         -> x) . fst . offActionDist
--                getInterceptProb  = snd . (\(_:x:_)       -> x) . fst . offActionDist
--                getCatchProb      = snd . (\(_:_:x:_)     -> x) . fst . offActionDist
--                getNoOpProb       = snd . (\(_:_:_:x:_)   -> x) . fst . offActionDist
--                getMoveToProb     = snd . (\(_:_:_:_:x:_) -> x) . fst . offActionDist

--        --        ballActions
--                getShootProb = snd . head            . fst . offBallActionDist
--                getDribProb  = snd . (\(_:x:_) -> x) . fst . offBallActionDist

--                getPass7Prob   = snd . (\(_:_:x:_) -> x) . fst . offBallActionDist
----                getPass8Prob   = snd . (\(_:_:_:x:_) -> x) . fst . offBallActionDist
----                getPass9Prob   = snd . (\(_:_:_:_:x:_) -> x) . fst . offBallActionDist
--                getPass11Prob  = snd . (\(_:_:_:x:_) -> x) . fst . offBallActionDist


--        bestDefenseActionDist :: DefenseTeam -> [Double]
--        bestDefenseActionDist (DefenseTeam dp1 dp2 dp3 dp4 _)  = [ fromIntegral $ maximum (map getMoveProb      opList)
--                                                                 , fromIntegral $ maximum (map getInterceptProb opList)
--                                                                 , fromIntegral $ maximum (map getCatchProb     opList)
--                                                                 , fromIntegral $ maximum (map getNoOpProb      opList)
--                                                                 ]
--            where
--                opList = [dp1, dp2, dp3, dp4]
--        --        non-ballActions
--                getMoveProb       = snd . (\(x:_)       -> x) . fst . defActionDist
--                getInterceptProb  = snd . (\(_:x:_)     -> x) . fst . defActionDist
--                getCatchProb      = snd . (\(_:_:x:_)   -> x) . fst . defActionDist
--                getNoOpProb       = snd . (\(_:_:_:x:_) -> x) . fst . defActionDist
