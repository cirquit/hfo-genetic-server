module Genetic.Allele where

import Control.Monad.Random
import System.Random
import Control.Monad

import Data.List (sort)

import HFO.Agent

-- | This defines the basic representation of an individual
--
class Allele a where

-- | Creates a single individual
--
--  weights are initialized in the interval (-phi,+phi)

    genIndividual :: MonadRandom r => Double -> r a

-- | Creates n individuals
--                                        n       φ
    genIndividuals :: (MonadRandom r) => Int -> Double -> r [a]
    genIndividuals n phi = replicateM n (genIndividual phi)


-- | Repopulates a list of individuals up to a maximal length and appends from behind
--
--                                                   φ
    repopulate :: (MonadRandom r) => Int -> [a] -> Double -> r [a]
    repopulate maxLength l phi = do
        let len = length l

        if maxLength >= len
            then (l ++) <$> genIndividuals (maxLength - len) phi
            else return l

-- | Generate a single Offense individual
--
instance Allele Offense where

--  genIndividual :: MonadRandom r => Double -> r a
    genIndividual phi = Offense <$> (take coeffCount <$> getRandomRs (-phi, phi))
        where coeffCount = 20 :: Int

-- | Generate a single Defense individual
--
instance Allele Defense where

--  genIndividual :: MonadRandom r => Double -> r a
    genIndividual phi = Defense <$> ((take coeffCount) <$> getRandomRs (-phi, phi))
        where coeffCount = 20 :: Int

-- | Generate a whole 'Offense team' - individual
--
instance Allele OffenseTeam where

--  genIndividual :: MonadRandom r => Double -> r OffenseTeam
    genIndividual phi = OffenseTeam <$> genPlayer <*> genPlayer
                                    <*> genPlayer <*> genPlayer
                                    <*> pure ([], [])
        where 
            genPlayer :: MonadRandom r => r Offense
            genPlayer = genIndividual phi

-- | Generate a whole 'Defense team' - individual
--
instance Allele DefenseTeam where

--  genIndividual :: MonadRandom r => Double -> DefenseTeam
    genIndividual phi = DefenseTeam <$> genPlayer <*> genPlayer
                                    <*> genPlayer <*> genPlayer
                                    <*> pure ([], [])
        where 
            genPlayer :: MonadRandom r => r Defense
            genPlayer = genIndividual phi