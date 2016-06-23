{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric, DeriveAnyClass, OverloadedStrings, FlexibleInstances #-}

module HFO.Agent.Data where

import HFO.StateParser (HFOState(..))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text

-- | All possible actions for an agent WITHOUT the possession of the ball
--
data Action  = Move           -- high level move based on strategy (whatever this might be - TODO)
             | Intercept      -- intercept the ball
             | Catch          -- goalie only  (this may be a little bit ugly)
             | NoOp           -- no operation
--             | MoveTo Int Int -- x in [-1,1], y in [-1,1]
--             | Dash Int Int   -- power in [0,100], direction in [-180,180]
--             | Turn Int       -- direction in [-180,180]
--             | Attract
    deriving (Show, Enum, Bounded, Generic, FromJSON, ToJSON, Eq)

-- | All possible actions for an agent WITH the possesion of the ball
--
data BallAction  = Shoot                  -- shoot in (possibly in looking direction)
                 | Dribble                -- dribble in whatever direction?...
--                 | Pass Int             -- pass to teammate in [0,11]
--                 | Kick   Int Int       -- power in [0,100], direction in [-180, 180]
--                 | KickTo Int Int Int   -- x in [-1,1], y in [-1,1], power in [0,3]
--                 | DribbleTo Int Int    -- x in [-1,1], y in [-1,1]
    deriving (Show, Enum, Bounded, Generic, FromJSON, ToJSON, Eq)

-- | Wrapper for defense action distribution
--
--   The following should always be True:
--
--      1) foldr ((+) . snd) 0 (fst . defActionsDist) == 100
--
--   The second part of the tuple is the generator list for the distribution (created via Genetic.Allele.uniformDistributionGen)
--   This is needed for a semi-random mutation
--
data Defense = Defense { defActionDist :: ([(Action, Int)], [Int]) }
    deriving (Show, Eq)

instance ToJSON Defense where

    toJSON (Defense (actions, generator)) = object [
        "defActions"          .= actions
      , "defActionsGenerator" .= generator
      ]

instance FromJSON Defense where

    parseJSON (Object o) = do 
        actions   <- o .: "defActions"
        generator <- o .: "defActionsGenerator"
        return $ Defense (actions, generator)

-- | Wrapper for offense action distribution
--
--   The following should always be True:
--
--       1) foldr ((+) . snd) 0 (fst . offActionsDist)     == 100
--       2) foldr ((+) . snd) 0 (fst . offBallActionsDist) == 100
--
--   The second part of the tuple is the generator list for the distribution (created via Genetic.Allele.uniformDistributionGen)
--   This is needed for a semi-random mutation
--
data Offense = Offense { offActionDist :: ([(Action, Int)], [Int]), offBallActionDist :: ([(BallAction, Int)], [Int]) }
    deriving (Show, Eq)

instance ToJSON Offense where

    toJSON (Offense (actions, generator) (ballActions, ballGenerator)) = object [
        "offActions"              .= actions
      , "offActionsGenerator"     .= generator
      , "offBallActions"          .= ballActions
      , "offBallActionsGenerator" .= ballGenerator
      ]

instance FromJSON Offense where

    parseJSON (Object o) =  do 
        actions       <- o .: "offActions"
        generator     <- o .: "offActionsGenerator"
        ballActions   <- o .: "offBallActions"
        ballGenerator <- o .: "offBallActionsGenerator"
        return $ Offense (actions, generator) (ballActions, ballGenerator)


-- | Wrapper for Teams
--
--   The Half-Field Offense Task defines 4 offensive and 3 defensive players + goalie
--
--  *) The fitness is evaluated once in Selection
--
--              already      new states from
--              calculated     simulation
--                  |          /
--                  |         |
--                (Int, [HFOState])
--
data OffenseTeam = OffenseTeam { op1        :: Offense
                               , op2        :: Offense
                               , op3        :: Offense
                               , op4        :: Offense
                               , offFitness :: (Int, [Maybe HFOState])
                               }
    deriving (Show, Eq)

instance ToJSON OffenseTeam where

    toJSON OffenseTeam{..} = object [
       "op1" .= op1
      ,"op2" .= op2
      ,"op3" .= op3
      ,"op4" .= op4
      ,"offFitness"       .= fst offFitness
      ,"offFutureFitness" .= snd offFitness
      ]

instance FromJSON OffenseTeam where

    parseJSON (Object o) = do
        op1 <- o .: "op1"
        op2 <- o .: "op2"
        op3 <- o .: "op3"
        op4 <- o .: "op4"
        offFitness       <- o .: "offFitness"
        offFutureFitness <- o .: "offFutureFitness"
        return $ OffenseTeam op1 op2 op3 op4 (offFitness, offFutureFitness)

data DefenseTeam = DefenseTeam { goalie     :: Defense
                               , dp2        :: Defense
                               , dp3        :: Defense
                               , dp4        :: Defense
                               , defFitness :: (Int, [Maybe HFOState])}
    deriving (Show, Eq)

instance ToJSON DefenseTeam where

    toJSON DefenseTeam{..} = object [
       "goalie" .= goalie
      ,"dp2" .= dp2
      ,"dp3" .= dp3
      ,"dp4" .= dp4
      ,"defFitness"       .= fst defFitness
      ,"defFutureFitness" .= snd defFitness
      ]


instance FromJSON DefenseTeam where

    parseJSON (Object o) = do
        goalie <- o .: "goalie"
        dp2    <- o .: "dp2"
        dp3    <- o .: "dp3"
        dp4    <- o .: "dp4"
        defFitness       <- o .: "defFitness"
        defFutureFitness <- o .: "defFutureFitness"
        return $ DefenseTeam goalie dp2 dp3 dp4 (defFitness, defFutureFitness)

-- | Defaults
--
-- (testing purposes only)
defaultDefense :: Defense
defaultDefense = Defense { defActionDist = ([(Move, 50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100]) }

-- (testing purposes only)
defaultOffense :: Offense
defaultOffense = Offense { offActionDist     = ([(Move,  50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100])
                         , offBallActionDist = ([(Shoot, 50), (Dribble,   50)],                          [0, 50, 100])
                         }

-- (testing purposes only)
defaultDefenseTeam :: DefenseTeam
defaultDefenseTeam  = DefenseTeam { goalie     = defaultDefense
                                  , dp2        = defaultDefense
                                  , dp3        = defaultDefense
                                  , dp4        = defaultDefense
                                  , defFitness = (0, [])
                                  }

-- (testing purposes only)
defaultOffenseTeam ::OffenseTeam
defaultOffenseTeam = OffenseTeam { op1        = defaultOffense
                                 , op2        = defaultOffense
                                 , op3        = defaultOffense
                                 , op4        = defaultOffense
                                 , offFitness = (0, [])
                                 }