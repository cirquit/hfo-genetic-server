module Genetic.Mutation where

import Control.Monad.Random
import Control.Monad
import System.Random

-- | This class defines the mutate method on individuals
--
class Mutation a where

    mutateI :: MonadRandom r => a -> r a

    mutate :: MonadRandom r => [a] -> r [a]
    mutate = mapM mutateI

    mutationProb :: Fractional a => a

--    mutation
