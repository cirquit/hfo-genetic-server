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

        if maxLength > len
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
        let actions         = [minBound .. maxBound] :: [Action]
            ballActions     = [minBound .. maxBound] :: [BallAction]

            actionsLen      = 1 + fromEnum (maxBound :: Action)
            ballActionsLen  = 1 + fromEnum (maxBound :: BallAction)

            actionsDist     = uncurry ((,) . zip actions)     <$> genUniformDistributionGen actionsLen
            ballActionsDist = uncurry ((,) . zip ballActions) <$> genUniformDistributionGen ballActionsLen

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
        let actions         = [minBound .. maxBound] :: [Action]
            actionsLen      = 1 + fromEnum (maxBound :: Action)
            actionsDist     = uncurry ((,) . zip actions)      <$> genUniformDistributionGen actionsLen

        Defense <$> actionsDist

-- | Generate a whole 'Offense team' - individual
--
-- Performance: 4 * (genIndividual :: Offense)
--
instance Allele OffenseTeam where

--  genIndividual :: MonadRandom r => r OffenseTeam
    genIndividual = OffenseTeam <$> genIndividual <*> genIndividual <*> genIndividual <*> genIndividual <*> pure (0, [])

-- | Generate a whole 'Defense team' - individual
--
-- Performance: 4 * (genIndividual :: Offense)
--
instance Allele DefenseTeam where

--  genIndividual :: MonadRandom r => DefenseTeam
    genIndividual = DefenseTeam <$> genIndividual <*> genIndividual <*> genIndividual <*> genIndividual <*> pure (0, [])



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