module Genetic.Crossover where

import System.Random
import Control.Monad.Random
import Control.Monad
import Genetic.Selection

-- | This defines the crossover method on individuals
--
--   Dependent on Selection to know on which basis to classify the individuals
--
class Selection a => Crossover a where

-- | Basic combination for two individuals (default: uniformly distributed)
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




