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

--  classify :: OffenseTeam -> Int
    classify OffenseTeam{..} =
            let (previousFitness, states) = offFitness
            in  previousFitness + foldl' (flip ((+) . fitness)) 0 states
        where
            fitness :: Maybe HFOState -> Int
            fitness (Just Goal)               = 2
            fitness (Just OutOfTime)          = 1
            fitness (Just CapturedByDefense)  = 0
            fitness (Just OutOfBounds)        = 0
            fitness (Just ServerDown)         = 0
            fitness (Just Ingame)             = 0
            fitness _                         = 0


-- if we want to update the fitness gradually then it has to be a relative fitness, not an absolute
-- count of simulation results

--  updateFitness :: OffenseTeam -> OffenseTeam
--    updateFitness offense = offense { offFitness = (classify offense, []) }


instance Selection DefenseTeam where

--  classify :: DefenseTeam -> Int
    classify DefenseTeam{..} =
            let (previousFitness, states) = defFitness
            in  previousFitness + foldl' (flip ((+) . fitness)) 0 states
        where
            fitness :: Maybe HFOState -> Int
            fitness (Just CapturedByDefense)  = 2
            fitness (Just OutOfTime)          = 1
            fitness (Just Goal)               = 0
            fitness (Just OutOfBounds)        = 0
            fitness (Just ServerDown)         = 0
            fitness (Just Ingame)             = 0
            fitness _                         = 0

--  updateFitness :: DefenseTeam -> DefenseTeam
--    updateFitness defense = defense { defFitness = (classify defense, []) }
