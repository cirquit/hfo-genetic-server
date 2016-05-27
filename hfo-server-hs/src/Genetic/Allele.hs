module Genetic.Allele where

import Control.Monad.Random
import System.Random
import Control.Monad

-- | This defines the basic representation of an individual
--
class Allele a where

-- | Creates a single individual
--
    genIndividual :: MonadRandom r => r a

-- | Creates n individuals
-- 
    genIndividuals :: (MonadRandom r) => Int -> r [a]
    genIndividuals n = replicateM n genIndividual

