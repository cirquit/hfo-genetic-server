{-# LANGUAGE RecordWildCards #-}

module Genetic.Mutation
    ( Mutation(..)
    ) where

import Control.Monad.Random
import Control.Monad
import System.Random
import Data.List             (sort)

import Debug.Trace

import Genetic.Allele        (generateDistributionFrom)
import Genetic.Crossover     (normalizeDist)

import HFO.Agent

-- | This class defines the mutate method on individuals
--
class Mutation a where

-- | Mutate β percent of invidiuals
--
--                               β        δ       λ
    mutate :: MonadRandom r => Double -> Int -> Double -> [a] -> r [a]
    mutate beta delta lambda ps = mapM go ps
        where
            go individual = (beta >=) <$> getRandomR (0.0, 1.0) >>= \x -> if x then mutateI delta lambda individual
                                                                               else return individual

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
--   *) Mutate λ percent of seperations (of fields), currently 16

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
--                               δ       λ

    mutateI :: MonadRandom r => Int -> Double -> a -> r a

instance Mutation ActionDist where

--  mutateI :: MonadRandom r => Int -> Double -> ActionDist -> r ActionDist
    mutateI delta _ (ActionDist dist generator) = do

-- ## Generator Mutation
          --let actions    = map fst dist :: [Action]
          --    actionsLen = length  dist :: Int

          --summands <- splitDelta delta (actionsLen - 1)

          --let newGenerator  = mutateGenerator generator summands
          --    newActionDist = zip actions $ generateDistributionFrom newGenerator

          --return $ ActionDist newActionDist newGenerator

-- ## Simpler Mutation without generator
        let actions    = map fst dist :: [Action]
            actionsLen = length dist  :: Int

        summands <- splitDelta delta actionsLen

        let newDist = zipWith (\x y -> max 0 (x + y)) summands (map snd dist) :: [Int]

            normDist = normalizeDist newDist

            newActionDist = zip actions normDist :: [(Action, Int)]

    --  we don't have any generator so it stays empty, if we use this method crossover has to be adjusted
        return $ ActionDist newActionDist []


instance Mutation BallActionDist where

--  mutateI :: MonadRandom r => Int -> Double -> BallActionDist -> r BallActionDist
    mutateI delta _ (BallActionDist dist generator) = do
        --let ballActions    = map fst dist :: [BallAction]
        --    ballActionsLen = length  dist :: Int

        --summands <- splitDelta delta (ballActionsLen - 1)

        --let newGenerator      = mutateGenerator generator summands
        --    newBallActionDist = zip ballActions $ generateDistributionFrom newGenerator

        --return $ BallActionDist newBallActionDist newGenerator

        let actions    = map fst dist :: [BallAction]
            actionsLen = length dist  :: Int

        summands <- splitDelta delta actionsLen

        let newDist = zipWith (\x y -> max 0 (x + y)) summands (map snd dist) :: [Int]

            normDist = normalizeDist newDist

            newActionDist = zip actions normDist :: [(BallAction, Int)]

    --  we don't have any generator so it stays empty, if we use this method crossover has to be adjusted
        return $ BallActionDist newActionDist []

instance Mutation Offense where

--  mutateI :: MonadRandom r => Int -> Double -> Offense -> r Offense
    mutateI delta lambda Offense{..} = Offense <$> {- mutate lambda delta 0 -} mutateI delta 0 offActionDist
                                               <*> {- mutate lambda delta 0 -} mutateI delta 0 offBallActionDist

instance Mutation Defense where

--  mutateI :: MonadRandom r => Int -> Double -> Defense -> r Defense
    mutateI delta lambda Defense{..} = Defense <$> {- mutate lambda delta 0 -} mutateI delta 0 defActionDist

instance Mutation DefenseTeam where

--  mutateI :: MonadRandom r => Int -> Double -> DefenseTeam -> r DefenseTeam
    mutateI delta lambda DefenseTeam{..} = DefenseTeam <$> mutateI delta lambda goalie
                                                       <*> mutateI delta lambda dp2
                                                       <*> mutateI delta lambda dp3
                                                       <*> mutateI delta lambda dp4
                                                       <*> pure ([], [])

instance Mutation OffenseTeam where

--  mutateI :: MonadRandom r => Int -> Double -> OffenseTeam -> r OffenseTeam
    mutateI delta lambda OffenseTeam{..} = OffenseTeam <$> mutateI delta lambda op1
                                                       <*> mutateI delta lambda op2
                                                       <*> mutateI delta lambda op3
                                                       <*> mutateI delta lambda op4
                                                       <*> pure ([], [])

-- | splits delta (0-100) in n parts  (negative partition - number theory)
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
mutateGenerator :: [Int] -> [Int] -> [Int]
mutateGenerator generator deltas = sort $ zipWith (((max 0 . min 100) .) . (+)) generator (0:deltas ++ [100])


-- TODO think of a way to mutate this properly
--
-- mutatePositions :: [Action] -> IO [Action]
-- mutatePositions = foldM go []
--     where
--         go (MoveTo (x,y) (xBs,yBs)) as = as ++ [MoveTo (x,y) (xBs,yBs)]
--         go a as                        = as ++ [a]