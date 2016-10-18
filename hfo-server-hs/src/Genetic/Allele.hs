module Genetic.Allele where

import Control.Monad.Random
import System.Random
import Control.Monad

import Data.List (sort)

import HFO.Agent
import CrossEntropy (sampleOnceFromDistSet, DistributionSet())

import Genetic.Selection

-- | This defines the basic representation of an individual
--
-- | Cross Entropy individuals
-- 
--   This encoding uses normal distributions with mean and standart deviation to encode the coefficient space
--
--   i := Individuals
--   c_1-n := encoding for every individual
--
--   i / c | c_1 | c_2 | c_3 | ... | c_n
--   ----------------------------------- 
--   i_1   | 0.1 | -1  | 1.1 | ... 
--   i_2   | 1.9 | -2  | 0.4 | ... 
--   i_3   | 1.0 | 3.1 | 2.4 | ... 
--    .       .     .     .     .  
--    .       .     .     .     .  
--    .       .     .     .     .  
--   i_m   |     |     |     | ...
--
--   we have n normal distributions, so the c_i coefficients would be sampled from the i'th normal distribution
--
class Allele a where

-- | Creates a single individual
--
--  weights are sampled from the DistributionSet
--
    genIndividual :: MonadRandom r => DistributionSet -> r a

-- | Creates n individuals
--                                        n     
    genIndividuals :: (MonadRandom r) => Int -> DistributionSet -> r [a]
    genIndividuals n distSet = replicateM n (genIndividual distSet)


-- | Repopulates a list of individuals up to a maximal length and appends from behind
--
    repopulate :: (MonadRandom r) => Int -> [a] -> DistributionSet-> r [a]
    repopulate maxLength l distSet = do
        let len = length l

        if maxLength >= len
            then (l ++) <$> genIndividuals (maxLength - len) distSet
            else return l

-- | merges two lists of individuals (parent population and children)
--   deletes 
    merge :: [a] -> [a] -> [a]
    merge parents children = (take best parents) ++ children
        where best        = length parents - childLength
              childLength = length children

-- | Generate a single Offense individual
--
instance Allele Offense where

--  genIndividual :: MonadRandom r => DistributionSet -> r a
    genIndividual distSet = Offense <$> sampleOnceFromDistSet distSet

-- | Generate a single Defense individual
--
instance Allele Defense where

--  genIndividual :: MonadRandom r => DistributionSet -> r a
    genIndividual distSet = Defense <$> sampleOnceFromDistSet distSet

-- | Generate a whole 'Offense team' - individual
--
instance Allele OffenseTeam where

--  genIndividual :: MonadRandom r => DistributionSet -> r OffenseTeam
    genIndividual distSet = OffenseTeam <$> genPlayer <*> genPlayer
                                        <*> genPlayer <*> genPlayer
                                        <*> pure ([], [])
        where
            genPlayer :: MonadRandom r => r Offense
            genPlayer = genIndividual distSet

-- | Generate a whole 'Defense team' - individual
--
instance Allele DefenseTeam where

--  genIndividual :: MonadRandom r =>  DistributionSet -> DefenseTeam
    genIndividual distSet = DefenseTeam <$> genPlayer <*> genPlayer
                                        <*> genPlayer <*> genPlayer
                                        <*> pure ([], [])
        where
            genPlayer :: MonadRandom r => r Defense
            genPlayer = genIndividual distSet