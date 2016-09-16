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


-- | Global config for tests

-- Creation bounds for the coefficients (-phi, phi)
phi :: Double
phi = 2 

instance Arbitrary HFOState where

--  arbitrary :: Gen HFOState
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary Defense where

--  arbitrary :: Gen Defense
    arbitrary = Defense . take 20 <$> (listOf $ choose (-phi, phi)) -- limit to length 20 for faster tests

instance Arbitrary Offense where

--  arbitrary :: Gen Offense
    arbitrary = Offense . take 20 <$> (listOf $ choose (-phi, phi)) -- limit to length 20 for faster tests
 
instance Arbitrary OffenseTeam where

--  arbitrary :: Gen OffenseTeam
    arbitrary = OffenseTeam <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> ((,) <$> (listOf $ elements [-1,-0.9..1])
                                     <*> arbitrary)

instance Arbitrary DefenseTeam where

--  arbitrary :: Gen DefenseTeam
    arbitrary = DefenseTeam <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> ((,) <$> (listOf $ elements [-1,-0.9..1])
                                     <*> (listOf $ oneof [Just <$> arbitrary, (return Nothing)]))

instance Arbitrary SerializedTeams where

--  arbitrary :: Gen SerializedTeams
    arbitrary = SerializedTeams <$> (take 5 <$> listOf arbitrary) -- limit to 5 so the tests dont take too long
                                <*> (take 5 <$> listOf arbitrary)
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


-- | Allele bounds check
--
creationBounds :: [Double] -> Bool
creationBounds = all (\x -> x <= phi && x >= (-phi))

offenseBounds :: Offense -> Bool
offenseBounds Offense{..} = creationBounds offEncoding

defenseBounds :: Defense -> Bool
defenseBounds Defense{..} = creationBounds defEncoding

-- | Check for bounds and length violation
--
--  Rule #1: forall element in offense: x <= phi && x >= -phi
--  Rule #2: length after mutation stays the same
mutationOffense :: Offense -> Property
mutationOffense offense = monadicIO $ do
     let beta      = 0.1
         offLength = length $ offEncoding offense
     mutated <- run $ mutateI beta phi offense 
     let mutLength = length $ offEncoding mutated
     assert $  offenseBounds mutated
            && offLength == mutLength

-- | Check for bounds and length violation
--
--  Rule #1: forall element in offense: x <= phi && x >= -phi
--  Rule #2: length after mutation stays the same
mutationDefense :: Defense -> Property
mutationDefense defense = monadicIO $ do
     let beta      = 0.1
         defLength = length $ defEncoding defense

     mutated <- run $ mutateI beta phi defense

     let mutLength = length $ defEncoding mutated

     assert $  defenseBounds mutated
            && defLength == mutLength

-- | Check for bounds and length violation
--
--  Rule #1: forall element in offense: x <= phi && x >= -phi
--  Rule #2: length after crossover is the minimum of the parents length
crossoverOffense :: Offense -> Offense -> Property
crossoverOffense offA offB = monadicIO $ do
    let offALength = length $ offEncoding offA
        offBLength = length $ offEncoding offB
        minLength  = min offALength offBLength

    (childA, childB) <- run $ crossoverI offA offB

    assert $ offenseBounds childA
          && offenseBounds childB
          && length (offEncoding childA) == minLength
          && length (offEncoding childB) == minLength

-- | Check for bounds and length violation
--
--  Rule #1: forall element in offense: x <= phi && x >= -phi
--  Rule #2: length after crossover is the minimum of the parents length
crossoverDefense :: Defense -> Defense -> Property
crossoverDefense defA defB = monadicIO $ do
    let defALength = length $ defEncoding defA
        defBLength = length $ defEncoding defB
        minLength  = min defALength defBLength

    (childA, childB) <- run $ crossoverI defA defB

    assert $ defenseBounds childA
          && defenseBounds childB
          && length (defEncoding childA) == minLength
          && length (defEncoding childB) == minLength

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