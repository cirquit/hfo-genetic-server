module Genetic.Allele where

import Control.Monad.Random
import System.Random
import Control.Monad

import Data.List (sort)

import HFO.Agent

-- | This defines the basic representation of an individual
--
class Allele a where

-- | Creates a single individual
--
--  weights are initialized in the interval [-phi,+phi]

    genIndividual :: MonadRandom r => Double -> r a

-- | Creates n individuals
-- 
    genIndividuals :: (MonadRandom r) => Int -> Double -> r [a]
    genIndividuals n phi = replicateM n (genIndividual phi)


-- | Repopulates a list of individuals up to a maximal length and appends from behind
--
    repopulate :: (MonadRandom r) => Int -> [a] -> Double -> r [a]
    repopulate maxLength l phi = do
        let len = length l

        if maxLength >= len
            then (l ++) <$> genIndividuals (maxLength - len) phi
            else return l

-- | Generate a single Offense individual
--
instance Allele Offense where

--  genIndividual :: MonadRandom r => Double -> r a
    genIndividual phi = Offense <$> (take coeffCount <$> getRandomRs (-phi, phi))
        where coeffCount = 20

-- | Generate a single Defense individual
--
instance Allele Defense where

--  genIndividual :: MonadRandom r => Double -> r a
    genIndividual phi = Defense <$> ((take coeffCount) <$> getRandomRs (-phi, phi))
        where coeffCount = 20

-- | Generate a whole 'Offense team' - individual
--
instance Allele OffenseTeam where

--  genIndividual :: MonadRandom r => Double -> r OffenseTeam
    genIndividual phi = OffenseTeam <$> genPlayer <*> genPlayer
                                    <*> genPlayer <*> genPlayer
                                    <*> pure ([], [])
        where genPlayer = genIndividual phi

-- | Generate a whole 'Defense team' - individual
--
instance Allele DefenseTeam where

--  genIndividual :: MonadRandom r => Double -> DefenseTeam
    genIndividual phi = DefenseTeam <$> genPlayer <*> genPlayer
                                    <*> genPlayer <*> genPlayer
                                    <*> pure ([], [])
        where genPlayer = genIndividual phi

-- | generates random Actions
--
--genActions :: MonadRandom r => r ([Action], Int)
--genActions = do
----    let boundsLength = 0.4

----    xBs <- roundTo 4 <$> getRandomR (-boundsLength, boundsLength)
----    yBs <- roundTo 4 <$> getRandomR (-boundsLength, boundsLength)

----    x   <- roundTo 4 <$> getRandomR (-1.0, 1.0)
----    y   <- roundTo 4 <$> getRandomR (-1.0, 1.0)

--    let res = [Move, Intercept, NoOp] -- MoveTo (x,y) (xBs, yBs)]
--    return (res, length res)

-- | generates random BallActions
--
--   WARNING!! - We can currently only pass to player 7 and 11 because we have enough NoOp Actions
--             - For 4 vs 4 we have to change this!
--
--genBallActions :: MonadRandom r => r ([BallAction], Int)
--genBallActions = do
--    -- p <- head . filter (/= 10) <$> getRandomRs (7,11)
--    let res = [Shoot, Dribble, BNoOp] --, GoalKick]-- Pass 7, Pass 11] --Pass 8, Pass 9, Pass 11]
--    return (res, length res) 

-- | creates a uniform distribution for 'n' Elements 
--
--                 (sort)   (zipWith)
-- Performance: O(n * log n) + O(n)
--
-- λ> let g = mkStdGen 123123
-- λ> flip evalRand g $ genUniformDistribution 5
-- [30,6,13,22,29]
-- λ> sum it
-- 100
--
-- TODO: Write tests
-- genUniformDistribution :: MonadRandom r => Int -> r [Int]
-- genUniformDistribution n = fst <$> genUniformDistributionGen n

-- | returns the uniform distribution with the generator list (needed for mutation)
--
--   the following should always hold
--
--   (xs, ys) <- genUniformDistributionGen n
--
--   *) length xs == (length ys) - 1
--
--   *) head ys == 0
--   *) last ys == 100
--
-- 
-- genUniformDistributionGen :: MonadRandom r => Int -> r ([Int], [Int])
-- genUniformDistributionGen n = do
--     rs <- sort . (0:) . (100:) . take (n-1) <$> getRandomRs (0,100)
--     return $ (generateDistributionFrom rs, rs)

-- | uses the generator list to create the uniform distribution
--
--   the following should always hold:
--  
--   let res = generateDistributionFrom xs
--
--   *)  length xs = (length res) + 1
--
-- generateDistributionFrom :: [Int] -> [Int]
-- generateDistributionFrom rs = zipWith (-) (tail rs) rs

-- rounding Doubles to specified number of digits
--
-- roundTo :: Int -> Double -> Double
-- roundTo n d = (fromInteger $ round $ d * (10^n)) / (10.0^^n)