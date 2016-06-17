{-# LANGUAGE RecordWildCards #-}

module Genetic.Mutation where

import Control.Monad.Random
import Control.Monad
import System.Random
import Data.List             (sort)

import Debug.Trace

import Genetic.Allele        (generateDistributionFrom)

import HFO.Agent

-- | This class defines the mutate method on individuals
--
class Mutation a where

-- | How to mutate a single individual
--
--
--   We are using the distribution generator list to always create a distribution that follows:
--
--   *) sum dist = 100
--   *) all (>= 0) dist = True
--
--   Mutation takes place as following:
--   
--   *) Mutate β percent of invidiuals
--   *) Mutate δ "value-changes" of an individual 

--   δ ~ this is sort of an absolute pearson correlation coefficient
--   δ = 0    ~ Nothing changes
--   δ = 100  ~ Everything changes
--
--   there will be δ "value-changes" to the invidual
--
--                generator  (initial distribution is [50,50])
--                   |
--   mutateI 10 [0,50,100] => we split 10 in 1 part, because 0 and 100 are our bounds => [+10]
--                         => we add to the first number after 0 => [0, 60, 100]
--                         => generateDistributionFrom => [40, 60]
--
--
--                generator  (initial distribution is [50,20,15,15])
--                   |
--   mutateI 20 [0,50,75,85,100] => we split 20 in 3 parts, because 0 and 100 are our bounds => [-6, +6, +6]
--                               => we add from the first number after 0 => [0, 46, 81, 91, 100]
--                               => generateDistributionFrom => [46,35,10,9]
--
--  TODO: *) many many tests
--        *) try to make splitDelta follow the sum-rule
--
--                               δ
    mutateI :: MonadRandom r => Int ->  a -> r a

-- | Mutate β percent of invidiuals
--
--                               β        δ
    mutate :: MonadRandom r => Double -> Int -> [a] -> r [a]
    mutate beta delta ps = mapM go ps
        where
            go individual = (beta >=) <$> getRandomR (0.0, 1.0) >>= \x -> if x then mutateI delta individual
                                                                               else return individual

instance Mutation Offense where

--  mutateI :: MonadRandom r => Int -> Offense -> r Offense
    mutateI delta Offense{..} = Offense <$> mutateActions offActionDist <*> mutateBallActions offBallActionDist

        where 
                mutateActions :: MonadRandom r => ([(Action, Int)], [Int]) -> r ([(Action, Int)], [Int])
                mutateActions (_, generator) = do
                    let actions    = [minBound .. maxBound] :: [Action]
                        actionsLen = 1 + fromEnum (maxBound :: Action)

                    summands <- splitDelta delta (actionsLen - 1)

                    let newGenerator  = mutateGenerator generator summands
                        newActionDist = zip actions $ generateDistributionFrom newGenerator

                    return (newActionDist, newGenerator)

                mutateBallActions :: MonadRandom r => ([(BallAction, Int)], [Int]) -> r ([(BallAction, Int)], [Int])
                mutateBallActions (_, generator)  = do
                    let ballActions    = [minBound .. maxBound] :: [BallAction]
                        ballActionsLen = 1 + fromEnum (maxBound :: BallAction)

                    summands       <- splitDelta delta (ballActionsLen - 1)

                    let newGenerator      = mutateGenerator generator summands
                        newBallActionDist = zip ballActions $ generateDistributionFrom newGenerator

                    return (newBallActionDist, newGenerator)

instance Mutation Defense where

--  mutateI :: MonadRandom r => Int -> Defense -> r Defense
    mutateI delta Defense{..} = Defense <$> mutateActions defActionDist

        where 
                mutateActions :: MonadRandom r => ([(Action, Int)], [Int]) -> r ([(Action, Int)], [Int])
                mutateActions (_, generator) = do
                    let actions    = [minBound .. maxBound] :: [Action]
                        actionsLen = 1 + fromEnum (maxBound :: Action)
                    
                    summands <- splitDelta delta (actionsLen - 1)

                    let newGenerator  = mutateGenerator generator summands
                        newActionDist = zip actions $ generateDistributionFrom newGenerator

                    return (newActionDist, newGenerator)

instance Mutation DefenseTeam where

--  mutateI :: MonadRandom r => Int -> DefenseTeam -> r DefenseTeam
    mutateI delta DefenseTeam{..} = DefenseTeam <$> mutateI delta goalie
                                                <*> mutateI delta dp2
                                                <*> mutateI delta dp3
                                                <*> mutateI delta dp4
                                                <*> pure (0, [])

instance Mutation OffenseTeam where

--  mutateI :: MonadRandom r => Int -> OffenseTeam -> r OffenseTeam
    mutateI delta OffenseTeam{..} = OffenseTeam <$> mutateI delta op1
                                                <*> mutateI delta op2
                                                <*> mutateI delta op3
                                                <*> mutateI delta op4
                                                <*> pure (0, [])

-- | splits delta (0-100) in n parts  (partition - number theory)
--
--   the following should hold (but does not for now):
--
--   *) sum . map abs $ splitGamma δ n == γ
--
--   Examples:
--
--      splitDelta 10 2 => 10 may be splitted in [-5, +5]
--
--      splitDelta 50 3 => 50 may be splitted in [-16, 16, 16]
--
--      splitDelta 100 4 => 100 may be splitted in [-25, +25, +25, -25]
--   
--   TODO: Major testing required here
--
--                              δ      n
splitDelta :: MonadRandom r => Int -> Int -> r [Int]
splitDelta delta parts = zipWith (*) partList . filter (/= 0) <$> getRandomRs (-1, 1)
    where
       partList = replicate parts $ delta `div` parts


-- | takes the generator from Genetic.Allele and the partitioned list of deltas
--   
--   *) sums both lists elementwise and adds the boundaries 0,100
--   *) normalizing every sum to range 0-100
--   *) sort the new generator
--
--   the following should hold:
--
--   *) head result = 0
--   *) last result = 100
--   *) all (<=100 && >= 0) result = True
--
-- TODO: tests
--
mutateGenerator :: [Int] -> [Int] -> [Int]
mutateGenerator generator deltas = sort $ zipWith (((max 0 . min 100) .) . (+)) generator (0:deltas ++ [100])