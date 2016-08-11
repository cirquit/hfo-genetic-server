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
    genIndividual :: MonadRandom r => r a

-- | Creates n individuals
-- 
    genIndividuals :: (MonadRandom r) => Int -> r [a]
    genIndividuals n = replicateM n genIndividual


-- | Repopulates a list of individuals up to a maximal length and appends from behind
--
    repopulate :: (MonadRandom r) => Int -> [a] -> r [a]
    repopulate maxLength l = do
        let len = length l

        if maxLength >= len
            then (l ++) <$> genIndividuals (maxLength - len)
            else return l

-- | Generate a single Offense individual
--
-- n = length [minBound .. maxBound] :: [Action]
-- m = length [minBound .. maxBound] :: [BallAction]
--
--                (uniform Dist)      (zip)
-- Performance: O(n * log n) + O(n) + O(n)
--            + O(m * log m) + O(m) + O(m)
--
instance Allele Offense where

--  genIndividual :: MonadRandom r => r a
    genIndividual = do
        (actions, actionsLen)         <- genActions
        (ballActions, ballActionsLen) <- genBallActions

        let actionsDist     = {- replicateM 16 $ -} uncurry (ActionDist     . zip actions)     <$> genUniformDistributionGen actionsLen
            ballActionsDist = {- replicateM 16 $ -} uncurry (BallActionDist . zip ballActions) <$> genUniformDistributionGen ballActionsLen

        Offense <$> actionsDist <*> ballActionsDist

-- | Generate a single Defense individual
--
-- n = length [minBound .. maxBound] :: [Action]
--
--                (uniform Dist)      (zip)
-- Performance: O(n * log n) + O(n) + O(n)
--
instance Allele Defense where

--  genIndividual :: MonadRandom r => r a
    genIndividual = do
        (actions, actionsLen) <- genActions
        let actionsDist = {- replicateM 16 $ -} uncurry (ActionDist . zip actions) <$> genUniformDistributionGen actionsLen

        Defense <$> actionsDist

-- | Generate a whole 'Offense team' - individual
--
-- Performance: 4 * (genIndividual :: Offense)
--
instance Allele OffenseTeam where

--  genIndividual :: MonadRandom r => r OffenseTeam
    genIndividual = OffenseTeam <$> genIndividual <*> genIndividual <*> genIndividual <*> genIndividual <*> pure ([], [])

-- | Generate a whole 'Defense team' - individual
--
-- Performance: 4 * (genIndividual :: Offense)
--
instance Allele DefenseTeam where

--  genIndividual :: MonadRandom r => DefenseTeam
    genIndividual = DefenseTeam <$> genIndividual <*> genIndividual <*> genIndividual <*> genIndividual <*> pure ([], [])


-- | generates random Actions
--
genActions :: MonadRandom r => r ([Action], Int)
genActions = do
--    let boundsLength = 0.4

--    xBs <- roundTo 4 <$> getRandomR (-boundsLength, boundsLength)
--    yBs <- roundTo 4 <$> getRandomR (-boundsLength, boundsLength)

--    x   <- roundTo 4 <$> getRandomR (-1.0, 1.0)
--    y   <- roundTo 4 <$> getRandomR (-1.0, 1.0)

    let res = [Move, Intercept, NoOp] -- MoveTo (x,y) (xBs, yBs)]
    return (res, length res)

-- | generates random BallActions
--
--   WARNING!! - We can currently only pass to player 7 and 11 because we have enough NoOp Actions
--             - For 4 vs 4 we have to change this!
--
genBallActions :: MonadRandom r => r ([BallAction], Int)
genBallActions = do
    -- p <- head . filter (/= 10) <$> getRandomRs (7,11)
    let res = [Shoot, Dribble, Pass 7, Pass 11] --Pass 8, Pass 9, Pass 11]
    return (res, length res) 

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
genUniformDistribution :: MonadRandom r => Int -> r [Int]
genUniformDistribution n = fst <$> genUniformDistributionGen n

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
genUniformDistributionGen :: MonadRandom r => Int -> r ([Int], [Int])
genUniformDistributionGen n = do
    rs <- sort . (0:) . (100:) . take (n-1) <$> getRandomRs (0,100)
    return $ (generateDistributionFrom rs, rs)

-- | uses the generator list to create the uniform distribution
--
--   the following should always hold:
--  
--   let res = generateDistributionFrom xs
--
--   *)  length xs = (length res) + 1
--
generateDistributionFrom :: [Int] -> [Int]
generateDistributionFrom rs = zipWith (-) (tail rs) rs



-- rounding Doubles to specified number of digits
--
roundTo :: Int -> Double -> Double
roundTo n d = (fromInteger $ round $ d * (10^n)) / (10.0^^n)