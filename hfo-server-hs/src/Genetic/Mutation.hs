module Genetic.Mutation where

import Control.Monad.Random
import Control.Monad
import System.Random

-- | This class defines the mutate method on individuals
--
class Mutation a where

-- | How to mutate a single individual
    mutateI :: MonadRandom r => a -> r a

