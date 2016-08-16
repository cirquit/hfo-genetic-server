{-# LANGUAGE BangPatterns #-}

module Genetic.Crossover
    ( Crossover(..)
    , normalizeDist
    ) where

import System.Random
import Control.Monad.Random
import Control.Monad
import Genetic.Selection

import HFO.Agent
import Genetic.Allele (generateDistributionFrom)

import System.Random.Shuffle (shuffleM)


-- | This defines the crossover method on individuals
--
--  minimal definition: uniformCO
--  maximal definition: uniformCO, nPointCO
--
--  we get a shuffled crossover for list for free

class Crossover a where

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
    crossover :: MonadRandom r => [a] -> r [a]
    crossover l = do
        shuffled <- shuffleM l
        zipWithM (\x y -> fst <$> crossoverI x y) shuffled (reverse shuffled)

-- | Crossover between two lists of the same individual
--
--   *) currently only using the first child of two
--
--  the following should hold:
--
--   *) let lengthA = length listA
--          lengthB = length listB
--      
--      length (crossoverBetween listA listB) = mininum [lengthA,lengthB]
--
    crossoverBetween :: MonadRandom r => [a] -> [a] -> r [a]
    crossoverBetween a b = zipWithM (\x y -> fst <$> crossoverI x y) a b


instance Crossover ActionDist where

--  uniformCO :: MonadRandom r => ActionDist -> ActionDist -> r (ActionDist, ActionDist)
    uniformCO (ActionDist distA genA) (ActionDist distB genB) = do

-- ## Generator Crossover
        -- calculate the average of the probabilities using the generator lists
        --let genAB = zipWith (\x y -> (x+y) `div` 2) genA genB
        --    dist  = generateDistributionFrom genAB

        --    actions = map fst distA    :: [Action]
        --    distAB  = zip actions dist :: [(Action, Int)]

        --    -- only one child with the current implementation
        --    result = ActionDist distAB genAB

        --return (result, result)

-- ## Simple Crossover without using the generator
        let dist       = zipWith (\(_,x) (_,y) -> x + y) distA distB

        --  normalize the distribution
            normDistAB = normalizeDist dist

            actions = map fst distA          :: [Action]
            distAB  = zip actions normDistAB :: [(Action, Int)]

        --  we don't have any generator so it stays empty, if we use this method mutation has to be adjusted
            result  = ActionDist distAB []

        return (result, result)


instance Crossover BallActionDist where

-- uniformCO :: MonadRandom r => BallActionDist -> BallActionDist -> r (BallActionDist, BallActionDist)
   uniformCO (BallActionDist distA genA) (BallActionDist distB genB) = do

        ---- calculate the average of the probabilities using the generator lists
        --let genAB = zipWith (\x y -> (x+y) `div` 2) genA genB
        --    dist  = generateDistributionFrom genAB

        --    actions = map fst distA   :: [BallAction]
        --    distAB = zip actions dist :: [(BallAction, Int)]

        --    -- only one child with the current implementation
        --    result = BallActionDist distAB genAB

        --return (result, result)

-- ## Simple Crossover without using the generator
        let dist       = zipWith (\(_,x) (_,y) -> x + y) distA distB

        --  normalize the distribution
            normDistAB = normalizeDist dist

            actions = map fst distA          :: [BallAction]
            distAB  = zip actions normDistAB :: [(BallAction, Int)]

        --  we don't have any generator so it stays empty, if we use this method mutation has to be adjusted
            result  = BallActionDist distAB []

        return (result, result)

instance Crossover Defense where

--  uniformCO :: MonadRandom r => Defense -> Defense -> r (Defense, Defense)
    uniformCO (Defense actionDistA) (Defense actionDistB) = do

            actionDistAB <- {- crossoverBetween -} fst <$> crossoverI actionDistA actionDistB

            -- only one child with the current implementation (maybe TODO)
            let result = Defense actionDistAB

            return (result, result)

instance Crossover Offense where

--  uniformCO :: MonadRandom r => Offense -> Offense -> r (Offense, Offense)
    uniformCO (Offense actionDistA ballActionDistA) (Offense actionDistB ballActionDistB) = do

            actionDistAB     <- {- crossoverBetween -} fst <$> crossoverI actionDistA     actionDistB
            ballActionDistAB <- {- crossoverBetween -} fst <$> crossoverI ballActionDistA ballActionDistB

            -- only one child with the current implementation (maybe TODO)
            let result = Offense actionDistAB ballActionDistAB

            return (result, result)


instance Crossover DefenseTeam where

--  uniformCO :: MonadRandom r => DefenseTeam -> DefenseTeam -> r (DefenseTeam, DefenseTeam)
    uniformCO (DefenseTeam fst1 fst2 fst3 fst4 _) (DefenseTeam snd1 snd2 snd3 snd4 _) = do

            let fstTeam = [fst1, fst2, fst3, fst4] :: [Defense]
                sndTeam = [snd1, snd2, snd3, snd4] :: [Defense]

            [p1, p2, p3, p4] <- crossoverBetween fstTeam sndTeam

            -- only one child with the current implementation (maybe TODO)
            let result = DefenseTeam p1 p2 p3 p4 ([], [])

            return (result, result)

instance Crossover OffenseTeam where

--  uniformCO :: MonadRandom r => OffenseTeam -> OffenseTeam -> r (OffenseTeam, OffenseTeam)
    uniformCO (OffenseTeam fst1 fst2 fst3 fst4 _) (OffenseTeam snd1 snd2 snd3 snd4 _) = do

            let fstTeam = [fst1, fst2, fst3, fst4] :: [Offense]
                sndTeam = [snd1, snd2, snd3, snd4] :: [Offense]

            [p1, p2, p3, p4] <- crossoverBetween fstTeam sndTeam

            -- only one child with the current implementation (maybe TODO)
            let result = OffenseTeam p1 p2 p3 p4 ([], [])

            return (result, result)

-- | TODO: Remove if not used anymore
--
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
            !(!x,!y) <- f (a,b)
            go (xs ++ [x], ys ++ [y]) abs


-- Kinda hacky way to ensure the 100-sum-rule 
--
-- Normalize the probabilities except for the last one
-- The last one will be computed by 100 - (sum of other probabilities)
-- 
-- That way we don't get any floating point errors
--
normalizeDist :: [Int] -> [Int]
normalizeDist dist = resDist ++ [rest]
    where

        rest    = 100 - (sum resDist) :: Int

        resDist = init fullNormDist :: [Int]

        fullNormDist = map (floor . (* 100) . (\x -> fromIntegral x / distSum)) dist :: [Int]

        distSum = fromIntegral (sum dist) :: Double