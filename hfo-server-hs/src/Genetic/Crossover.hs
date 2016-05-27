module Genetic.Crossover where

import System.Random
import Control.Monad.Random
import Control.Monad

-- | This defines the crossover method on individuals
--
class Crossover a where

-- | Basic combination for two indivduals (default: uniformly distributed)
--
    crossover :: MonadRandom r => a -> a -> r (a,a)
    crossover = uniformCO

-- | Uniform distribution of the allele between the parents
--
    uniformCO :: MonadRandom r => a -> a -> r (a,a)

-- | Cut the allele at 'n' points
--
--   This may be favorable in learner where the hyperparameter have a higher dependency on each other
    nPointCO :: (MonadRandom r, Integral i) => i -> a -> a -> r (a,a)


-- | classify the individual based on his performance
--
--   future use - combine preferred parents to generate better children (this has shown to support inbreeding extremly)
    classify :: (Num n) => a -> n


-- @TODO think about a nice representation where we can group individuals of the same type and combine them in a not
--       monomorphic list
--
-- class (Crossover a, Traversable t) => Evolution (t a) where
--
--    evolve :: t a -> t a
