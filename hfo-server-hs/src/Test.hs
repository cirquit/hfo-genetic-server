{-# LANGUAGE RecordWildCards #-}

module Test where


import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic
import Test.QuickCheck
import System.Directory
import Data.List  (sort)
import Data.Aeson


import Genetic
import HFO
import HFO.ToFlags
import HFO.StateParser


-- | Arbitrary Instances for (every) possible data type
--
--   These don't have a MonadRandom instance so one should use the QuickCheck functions for that
--
--
instance Arbitrary HFOState where

--  arbitrary :: Gen HFOState
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary Defense where

--  arbitrary :: Gen Defense
    arbitrary = do

        let actions     = [minBound .. maxBound] :: [Action]
            actionsLen  = 1 + fromEnum (maxBound :: Action)
            actionsDist = uncurry ((,) . zip actions) <$> genTestDistribution actionsLen

        Defense <$> actionsDist

instance Arbitrary Offense where

--  arbitrary :: Gen Offense
    arbitrary = do

        let actions         = [minBound .. maxBound] :: [Action]
            ballActions     = [minBound .. maxBound] :: [BallAction]

            actionsLen      = 1 + fromEnum (maxBound :: Action)
            ballActionsLen  = 1 + fromEnum (maxBound :: BallAction)

            actionsDist     = uncurry ((,) . zip actions)     <$> genTestDistribution actionsLen
            ballActionsDist = uncurry ((,) . zip ballActions) <$> genTestDistribution ballActionsLen

        Offense <$> actionsDist <*> ballActionsDist

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


genTestDistribution :: Int -> Gen ([Int], [Int])
genTestDistribution n = do
    rs <- sort . (0:) . (100:) . take (n-1) <$> infiniteListOf (choose (0,100))
    return $ (generateDistributionFrom rs, rs)

-- | All the Properties
--
--
flagPropOffenseAgent :: Offense -> Bool
flagPropOffenseAgent off =
    let agent = AgentConf { teamName = "base_left"
                          , isGoalie = False
                          , aseed    = 123
                          , episodes = 1
                          , actions  = Left off
                          }
        dist     = map (show . snd) . fst . offActionDist     $ off 
        ballDist = map (show . snd) . fst . offBallActionDist $ off

    in toFlags_ agent == (["--team","base_left","--episodes","1","--seed","123","--offactions"] ++ dist ++ ballDist)


flagPropDefenseAgent :: Defense -> Bool
flagPropDefenseAgent def = 
    let agent = AgentConf { teamName = "base_right"
                          , isGoalie = True
                          , aseed    = 456
                          , episodes = 4
                          , actions  = Right def
                          }
        dist = map (show . snd) . fst . defActionDist     $ def

    in toFlags_ agent == (["--team","base_right","--goalie","--episodes","4","--seed","456","--defactions"] ++ dist)


-- | test if the distribution always amounts summed to 100
--
actionDistOffenseGeneration :: Offense -> Bool
actionDistOffenseGeneration  Offense{..} = foldr ((+) . snd) 0 (fst offActionDist) == 100

ballActionDistOffenseGeneration :: Offense -> Bool
ballActionDistOffenseGeneration Offense{..} = foldr ((+) . snd) 0 (fst offBallActionDist) == 100

actionDistDefenseGeneration :: Defense -> Bool
actionDistDefenseGeneration Defense{..} = foldr ((+) . snd) 0 (fst defActionDist) == 100


-- | check if the mutation violates the distribution sum rule
--
mutationOffenseDist :: Offense -> Property
mutationOffenseDist offense = monadicIO $ do
    mutated <- run $ mutateI 15 offense 
    assert (actionDistOffenseGeneration mutated
        &&  ballActionDistOffenseGeneration mutated)

mutationDefenseDist :: Defense -> Property
mutationDefenseDist defense = monadicIO $ do
    mutated <- run $ mutateI 15 defense
    assert (actionDistDefenseGeneration mutated)

-- | json serialization tests
--
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