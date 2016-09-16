{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Genetic.Selection where

import System.Random
import Control.Monad.Random
import Control.Monad
import Data.List            (sort, sortBy, foldl', genericLength)
import Data.Ord             (comparing)


import HFO.Agent

-- | Selection class for the genetic algorithm
--
--   minimal definition: classify
--   maximum definition: classify, updateFitness
--
--   everything else will be derived automatically
--
class Selection a where

-- | classify the individual based on its performance
--
    classify :: a -> Int

-- | if we need to update a fitness while sorting
--   default: id
--
    updateFitness :: a -> a
    updateFitness = id

-- | derives itself based on classify
--
    compareFitness :: a -> a -> Ordering
    compareFitness x y = compare (classify x) (classify y)

-- | sorts the inviduals by fitness and updates fitness if necessary beforehand
--
    sortByAscFitness :: [a] -> [a]
    sortByAscFitness  = sortBy compareFitness . map updateFitness

    sortByDescFitness :: [a] -> [a]
    sortByDescFitness = sortBy (flip compareFitness) . map updateFitness


-- | select the best α percent of the population
--
    select :: Double -> [a] -> [a]
    select alpha xs = take best (sortByDescFitness xs)
        where best = round (alpha * genericLength xs)


instance Selection OffenseTeam where

--  ([X-Pos (Double)], [Maybe HFOStates])
--    
--  1) we compute the average x-position
--  2) we compute based on "fitness-function" the HFOStates and divide them by 4
--  at last we add 1) and 2) together
--
--  avg-case-scenario: avg.X-Pos is ~ 0.5 and we scored one goal out of 10 => 2 * 1 / 10
--  => 0.6
--  
--  I want to favor the goal-scoring as an avg 0.1(meter) space "win"
--
--  3) multiplication is only to avoid floating point madness
--
--  classify :: OffenseTeam -> Int
    classify OffenseTeam{..} =
            let (otherFitness, stateFitness) = offFitness
                summedotherFitness = (sum otherFitness)                                  / (genericLength otherFitness)
                summedStateFitness = 10 * (foldl' (flip ((+) . fitness)) 0 stateFitness) / (genericLength stateFitness)
            in  round $ (summedotherFitness + summedStateFitness) * 10000
        where
            fitness :: Maybe HFOState -> Double
            fitness (Just Goal)               = 1
            fitness (Just OutOfTime)          = 0
            fitness (Just CapturedByDefense)  = 0
            fitness (Just OutOfBounds)        = 0
            fitness (Just ServerDown)         = 0
            fitness (Just Ingame)             = 0
            fitness _                         = 0


instance Selection DefenseTeam where

--  ([X-Pos (Double)], [Maybe HFOStates])
--    
--  1) we compute the average x-position
--  2) we compute based on "fitness-function" the HFOStates and divide them by 4
--  at last we add 1) and 2) together
--
--  avg-case-scenario: avg.X-Pos is ~ 0.5 and we scored one goal => 1 / 4
--  => 0.75
--  
--  I want to favor the goal-scoring as an avg 0.25(meter) space "win"
--
--  3) multiplication is only to avoid floating point madness
--
--  classify :: DefenseTeam -> Int
    classify DefenseTeam{..} =
            let (otherFitness, stateFitness) = defFitness
                summedotherFitness = (sum otherFitness)                                 / (genericLength otherFitness)
                summedStateFitness = 2 * (foldl' (flip ((+) . fitness)) 0 stateFitness) / (genericLength stateFitness)
            in  round $ (summedotherFitness + summedStateFitness) * 10000
        where
            fitness :: Maybe HFOState -> Double
            fitness (Just CapturedByDefense)  = 1
            fitness (Just OutOfTime)          = 0
            fitness (Just Goal)               = 0
            fitness (Just OutOfBounds)        = 0
            fitness (Just ServerDown)         = 0
            fitness (Just Ingame)             = 0
            fitness _                         = 0

--  updateFitness :: DefenseTeam -> DefenseTeam
--    updateFitness defense = defense { defFitness = (classify defense, []) }
