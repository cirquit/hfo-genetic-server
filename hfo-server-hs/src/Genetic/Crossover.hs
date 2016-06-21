{-# LANGUAGE BangPatterns #-}

module Genetic.Crossover where

import System.Random
import Control.Monad.Random
import Control.Monad
import Genetic.Selection

import HFO.Agent

import System.Random.Shuffle (shuffleM)


-- | This defines the crossover method on individuals
--
--   Dependent on Selection to know on which basis to classify the individuals
--
class Selection a => Crossover a where

-- | Basic combination for two individuals (default: uniformly distributed)
--
    crossoverI :: MonadRandom r => a -> a -> r (a,a)
    crossoverI = uniformCO

-- | Uniform distribution of the allele between the parents
--
    uniformCO :: MonadRandom r => a -> a -> r (a,a)

-- | Cut the allele at 'n' points
--
--   This may be favorable in learner where the hyperparameter have a higher dependency on each other
    nPointCO :: (MonadRandom r, Integral i) => i -> a -> a -> r (a,a)
    nPointCO = undefined

-- | Main crossover function that uses crossoverI to create new individuals
--
--   *) currently only using the first child of two
--
--   the following should hold:
--
--      *) length (crossover l) = length l
--
--
    crossover :: MonadRandom r => [a] -> r [a]
    crossover l = do
          shuffled <- shuffleM l
          zipWithM (\x y -> fst <$> crossoverI x y) shuffled (reverse shuffled)

instance Crossover DefenseTeam where

--  uniformCO :: MonadRandom r => DefenseTeam -> DefenseTeam -> r (DefenseTeam, DefenseTeam)
    uniformCO (DefenseTeam fst1 fst2 fst3 fst4 _) (DefenseTeam snd1 snd2 snd3 snd4 _) = do

            let fstTeam = [fst1, fst2, fst3, fst4] :: [Defense]
                sndTeam = [snd1, snd2, snd3, snd4] :: [Defense]
                teams   = zip fstTeam sndTeam      :: [(Defense, Defense)]

            ([fst1',fst2',fst3',fst4'], [snd1',snd2',snd3',snd4']) <- unzipWithM switch teams

            let team1 = DefenseTeam fst1' fst2' fst3' fst4' (0, [])
                team2 = DefenseTeam snd1' snd2' snd3' snd4' (0, [])

            return (team1, team2)

instance Crossover OffenseTeam where

--  uniformCO :: MonadRandom r => OffenseTeam -> OffenseTeam -> r (OffenseTeam, OffenseTeam)
    uniformCO (OffenseTeam fst1 fst2 fst3 fst4 _) (OffenseTeam snd1 snd2 snd3 snd4 _) = do

            let fstTeam = [fst1, fst2, fst3, fst4] :: [Offense]
                sndTeam = [snd1, snd2, snd3, snd4] :: [Offense]
                teams   = zip fstTeam sndTeam      :: [(Offense, Offense)]

            ([fst1',fst2',fst3',fst4'], [snd1',snd2',snd3',snd4']) <- unzipWithM switch teams

            let team1 = OffenseTeam fst1' fst2' fst3' fst4' (0, [])
                team2 = OffenseTeam snd1' snd2' snd3' snd4' (0, [])

            return (team1, team2)


switch :: MonadRandom r => (a, a) -> r (a, a)
switch (x,y) = go <$> getRandomR (True, False)
    where go True  = (y,x)
          go False = (x,y)

-- | This should be a fold left without reversing the list
--
--   TODO: Test this or else the players are in reverse order
--
unzipWithM :: Monad m => ((a,b) -> m (c,d)) -> [(a,b)] -> m ([c], [d])
unzipWithM f = foldM go ([], [])
    where
        go (xs, ys) (x,y) = (\(c,d) -> (xs ++ [c], ys ++ [d])) <$> f (x,y)

-- | Strict unzipWithM (fold left without reversing the list)
--
unzipWithM' :: Monad m => ((a,b) -> m (c,d)) -> [(a,b)] -> m ([c], [d])
unzipWithM' f = go ([],[])
    where

        go (xs, ys) []            = return (xs,ys)
        go (xs, ys) ((a,b) : abs) = do
            (!x,!y) <- f (a,b)
            go (xs ++ [x], ys ++ [y]) abs