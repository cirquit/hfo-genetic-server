{-# LANGUAGE RecordWildCards #-}

module Test where


import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic
import Test.QuickCheck
import System.Directory
import Data.List  (sort)
import Data.Aeson
import Control.Monad (replicateM)

import Genetic
import HFO
import HFO.ToFlags


-- | Arbitrary Instances for (every) possible data type
--
--   These don't have a MonadRandom instance so one has to use the QuickCheck functions for that
--
--
instance Arbitrary Action where

--  arbitrary :: Gen Action
    arbitrary = elements =<< fst <$> genTestActions

instance Arbitrary BallAction where

--  arbitrary :: Gen BallAction
    arbitrary = elements =<< fst <$> genTestBallActions

instance Arbitrary HFOState where

--  arbitrary :: Gen HFOState
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary ActionDist where

-- arbitrary :: Gen ActionDist
   arbitrary = do
        (actions, actionsLen) <- genTestActions
        uncurry (ActionDist . zip actions) <$> genTestDistribution actionsLen

instance Arbitrary BallActionDist where

-- arbitrary :: Gen BallActionDist
    arbitrary = do
        (ballActions, ballActionsLen) <- genTestBallActions
        uncurry (BallActionDist . zip ballActions) <$> genTestDistribution ballActionsLen

instance Arbitrary Defense where

--  arbitrary :: Gen Defense
    arbitrary = Defense <$> replicateM 16 arbitrary

instance Arbitrary Offense where

--  arbitrary :: Gen Offense
    arbitrary = Offense <$> replicateM 16 arbitrary <*> replicateM 16 arbitrary

instance Arbitrary OffenseTeam where

--  arbitrary :: Gen OffenseTeam
    arbitrary = OffenseTeam <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> ((,) <$> arbitrary `suchThat` (>= 0)
                                     <*> arbitrary)

instance Arbitrary DefenseTeam where

--  arbitrary :: Gen DefenseTeam
    arbitrary = DefenseTeam <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> ((,) <$> (arbitrary `suchThat` (>= 0))
                                     <*> (listOf $ oneof [Just <$> arbitrary, (return Nothing)]))

genTestActions :: Gen ([Action], Int)
genTestActions = do
--    let boundsLength = 0.4
--
--    xBs <- roundTo 4 <$> choose (-boundsLength, boundsLength)
--    yBs <- roundTo 4 <$> choose (-boundsLength, boundsLength)
--    x   <- roundTo 4 <$> choose (-1.0, 1.0)
--    y   <- roundTo 4 <$> choose (-1.0, 1.0)

    let res = [Move, Intercept, Catch, NoOp] -- , MoveTo (x,y) (xBs,yBs)]
    return (res, length res)

genTestBallActions :: Gen ([BallAction], Int)
genTestBallActions = do
--    p <- head . filter (/= 10) <$> infiniteListOf (choose (7,11))
    let res = [Shoot, Dribble, Pass 7, Pass 8, Pass 9, Pass 11]
    return (res, length res) 

genTestDistribution :: Int -> Gen ([Int], [Int])
genTestDistribution n = do
    rs <- sort . (0:) . (100:) . take (n-1) <$> infiniteListOf (choose (0,100))
    return $ (generateDistributionFrom rs, rs)

-- | All the Properties
--
--
flagOffenseAgent :: Bool
flagOffenseAgent =
    let agent = AgentConf { teamName  = "base_left"
                          , episodes  = 1
                          , aseed     = 123
                          , playerNumber = 0
                          , isOffense = True
                          }

    in toFlags_ agent == (["--team","base_left","--episodes","1","--seed","123","--playerNumber","0", "--isOffense"])

flagDefenseAgent :: Bool
flagDefenseAgent = 
    let agent = AgentConf { teamName  = "base_right"
                          , episodes  = 4
                          , aseed     = 456
                          , playerNumber = 3
                          , isOffense = False
                          }

    in toFlags_ agent == (["--team","base_right","--episodes","4","--seed","456","--playerNumber", "3"])


-- | test if the distribution always amounts summed to 100
--
actionDistOffenseGeneration     :: Offense -> Bool
actionDistOffenseGeneration     Offense{..} = all actionDistSumRule     offActionDist

ballActionDistOffenseGeneration :: Offense -> Bool
ballActionDistOffenseGeneration Offense{..} = all ballActionDistSumRule offBallActionDist

actionDistDefenseGeneration     :: Defense -> Bool
actionDistDefenseGeneration     Defense{..} = all actionDistSumRule     defActionDist

actionDistSumRule     :: ActionDist -> Bool
actionDistSumRule         ActionDist{..} = foldr ((+) . snd) 0 actionDist == 100

ballActionDistSumRule :: BallActionDist -> Bool
ballActionDistSumRule BallActionDist{..} = foldr ((+) . snd) 0 ballActionDist == 100


-- | check if the mutation violates the distribution sum rule
--
mutationOffenseDist :: Offense -> Property
mutationOffenseDist offense = monadicIO $ do
    let delta  = 15
        lambda = 0.5
    mutated <- run $ mutateI delta lambda offense 
    assert (actionDistOffenseGeneration mutated
        &&  ballActionDistOffenseGeneration mutated)

mutationDefenseDist :: Defense -> Property
mutationDefenseDist defense = monadicIO $ do
    let delta  = 15
        lambda = 0.5
    mutated <- run $ mutateI delta lambda defense
    assert (actionDistDefenseGeneration mutated)

-- | json serialization tests
--
jsonPropAction :: Action -> Bool
jsonPropAction       action = eitherDecode (encode action)  == (Right action)

jsonPropBallAction :: BallAction -> Bool
jsonPropBallAction  baction = eitherDecode (encode baction) == (Right baction)

jsonPropActionDist :: ActionDist -> Bool
jsonPropActionDist   actionD = eitherDecode (encode actionD)  == (Right actionD)

jsonPropBallActionDist :: BallActionDist -> Bool
jsonPropBallActionDist bactionD = eitherDecode (encode bactionD) == (Right bactionD)

jsonPropHFOState :: HFOState -> Bool
jsonPropHFOState      state = eitherDecode (encode state)   == (Right state)

jsonPropDefense :: Defense -> Bool
jsonPropDefense         def = eitherDecode (encode def)     == (Right def)

jsonPropOffense :: Offense -> Bool
jsonPropOffense         off = eitherDecode (encode off)     == (Right off)

jsonPropDefenseTeam :: DefenseTeam -> Bool
jsonPropDefenseTeam defTeam = eitherDecode (encode defTeam) == (Right defTeam)

jsonPropOffenseTeam :: OffenseTeam -> Bool
jsonPropOffenseTeam offTeam = eitherDecode (encode offTeam) == (Right offTeam)

jsonPropSerializedTeams :: SerializedTeams -> Bool
jsonPropSerializedTeams teams = eitherDecode (encode teams) == (Right teams)