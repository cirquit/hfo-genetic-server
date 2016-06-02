module Genetic.Selection where

import System.Random
import Control.Monad.Random
import Control.Monad

class Selection a where

    -- | classify the individual based on its performance
--
--   future use - combine preferred parents to generate better children (this has shown to support inbreeding extremly)
    classify :: (Num n) => a -> n


