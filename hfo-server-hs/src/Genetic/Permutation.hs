{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Genetic.Permutation
    ( Permutation(..)
    ) where

import System.Random
import Control.Monad.Random
import Control.Monad
import Data.List             (foldl', transpose)

import HFO.Agent
import Genetic.Selection
import Genetic.Allele

import System.Random.Shuffle (shuffleM)

-- | This defines the permutation step of CoSyNE for a population
--
class Permutation a where

    permute :: MonadRandom r => [a] -> r [a]


instance Permutation Offense where

-- permute :: MonadRandom r => [Offense] -> r [Offense]
   permute offs = do
      let encodings = map offEncoding offs :: [[Double]]

       -- transposing the encoded weights for shuffling on a current weight level only
          transEnc  = transpose encodings   :: [[Double]]

      shuffled  <- mapM shuffleM transEnc   :: MonadRandom r => r [[Double]]

       -- transpose again to get the original encoding back
      let orgEncodings = transpose shuffled       :: [[Double]]
          newOffense   = map Offense orgEncodings :: [Offense]

      return newOffense


instance Permutation Defense where

-- permute :: MonadRandom r => [Defense] -> r [Defense]
   permute defs = do
      let encodings = map defEncoding defs :: [[Double]]

       -- transposing the encoded weights for shuffling on a current weight level only
          transEnc  = transpose encodings   :: [[Double]]

      shuffled <- mapM shuffleM transEnc    :: MonadRandom r => r [[Double]]

       -- transpose again to get the original encoding back
      let orgEncodings = transpose shuffled       :: [[Double]]
          newDefense   = map Defense orgEncodings :: [Defense]

      return newDefense


instance Permutation OffenseTeam where

--  permute :: MonadRandom r => [OffenseTeam] -> r [OffenseTeam]
    permute offteams = do
        -- create player lists corresponding to their position
        let (p1s,p2s,p3s,p4s) = foldl' (\(a,b,c,d) team -> (op1 team : a, op2 team : b, op3 team : c, op4 team : d)) ([],[],[],[]) offteams

        -- premute every player internally and return a list of new teams
        zipWith5M OffenseTeam (permute p1s) (permute p2s) (permute p3s) (permute p4s) (pure (repeat ([],[])))

instance Permutation DefenseTeam where

--  permute :: MonadRandom r => [DefenseTeam] -> r [DefenseTeam]
    permute defTeams = do
        -- create player lists corresponding to their position
        let (p1s,p2s,p3s,p4s) = foldl' (\(a,b,c,d) team -> (goalie team : a, dp2 team : b, dp3 team : c, dp4 team : d)) ([],[],[],[]) defTeams

        -- premute every player internally and return a list of new teams
        zipWith5M DefenseTeam (permute p1s) (permute p2s) (permute p3s) (permute p4s) (pure (repeat ([],[])))


-- | Helper function
--
zipWith5M :: forall a b c d e f m . Monad m => (a -> b -> c -> d -> e -> f) -> m [a] -> m [b] -> m [c] -> m [d] -> m [e] -> m [f]
zipWith5M f ma mb mc md me = go <$> ma <*> mb <*> mc <*> md <*> me
    where
        go :: [a] -> [b] -> [c] -> [d] -> [e] -> [f]
        go [] _  _  _  _  = []
        go _  [] _  _  _  = []
        go _  _  [] _  _  = []
        go _  _  _  [] _  = []
        go _  _  _  _  [] = []
        go (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : go as bs cs ds es


