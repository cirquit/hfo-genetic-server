{-# LANGUAGE RecordWildCards, FlexibleInstances, ScopedTypeVariables #-}

module Genetic.Mutation
    ( Mutation(..)
    ) where

import Control.Monad.Random
import Control.Monad
import System.Random

import Debug.Trace

import Genetic.Allele
import Genetic.Crossover

import HFO.Agent

-- | This class defines the mutate method on individuals
--
class Mutation a where

-- | Mutate β percent of invidiuals
--
--                               β         φ
    mutate :: MonadRandom r => Double -> Double -> [a] -> r [a]
    mutate beta phi ps = mapM (mutateI beta phi) ps

-- | How to mutate a single individual
--
--   *) Mutate β percent of invidiuals
--   *) Mutate in (-φ,+φ) bounds
--
--                                 β          φ
    mutateI :: MonadRandom r => Double -> Double -> a -> r a

instance Mutation [Double] where
 
--  mutateI :: MonadRandom r => Double -> Double -> [Double] -> r [Double]
    mutateI beta phi l = mapM go l
        where 
              go :: MonadRandom r => Double -> r Double
              go el = (beta >=) <$> getRandomR (0.0, 1.0) >>= element el

              element :: MonadRandom r => Double -> Bool -> r Double
              element x True  = getRandomR (-phi, phi)
              element x False = return x


instance Mutation Offense where

--  mutateI :: MonadRandom r => Double -> Double -> Offense -> r Offense
    mutateI beta phi Offense{..} = Offense <$> mutateI beta phi offEncoding

instance Mutation Defense where

--  mutateI :: MonadRandom r => Double -> Double -> Defense -> r Defense
    mutateI beta phi Defense{..} = Defense <$> mutateI beta phi defEncoding

instance Mutation DefenseTeam where

--  mutateI :: MonadRandom r => Double -> Double -> DefenseTeam -> r DefenseTeam
    mutateI beta phi DefenseTeam{..} = DefenseTeam <$> mutateI beta phi goalie
                                                   <*> mutateI beta phi dp2
                                                   <*> mutateI beta phi dp3
                                                   <*> mutateI beta phi dp4
                                                   <*> pure ([], [])

instance Mutation OffenseTeam where

--  mutateI :: MonadRandom r => Double -> Double -> OffenseTeam -> r OffenseTeam
    mutateI beta phi OffenseTeam{..} = OffenseTeam <$> mutateI beta phi op1
                                                   <*> mutateI beta phi op2
                                                   <*> mutateI beta phi op3
                                                   <*> mutateI beta phi op4
                                                   <*> pure ([], [])