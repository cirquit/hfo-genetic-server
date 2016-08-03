{-# LANGUAGE RecordWildCards, BangPatterns, OverloadedStrings, FlexibleInstances #-}

module HFO.Agent.Data where

import Data.Aeson
import Data.Aeson.Utils
import Data.Aeson.Types
import Data.Text as T
import GHC.Exts         -- (fromList)

import HFO.Agent.Actions
-- | All the possible states that our server can have
--
--   The states are parsed To/FromJSON based on the show instances defined and copied by the python library
--
data HFOState = Ingame
              | Goal
              | CapturedByDefense
              | OutOfBounds
              | OutOfTime
              | ServerDown
    deriving (Eq, Bounded, Enum)

instance ToJSON HFOState where

    toJSON s = String . T.pack . show $ s

instance FromJSON HFOState where

    parseJSON (String s) = do
        case toMState s of
            (Just x) -> return x
            Nothing  -> fail $ "Could not parse " ++ T.unpack s

    parseJSON o          = fail $ "Could not parse this object type - expected String (HFOState)" ++ show o

instance Show HFOState where

    show Ingame            = "IN_GAME"
    show Goal              = "GOAL"
    show CapturedByDefense = "CAPTURED_BY_DEFENSE"
    show OutOfBounds       = "OUT_OF_BOUNDS"
    show OutOfTime         = "OUT_OF_TIME"
    show ServerDown        = "SERVER_DOWN"

toMState :: Text -> Maybe HFOState
toMState "IN_GAME"              = Just Ingame
toMState "GOAL"                 = Just Goal
toMState "CAPTURED_BY_DEFENSE"  = Just CapturedByDefense
toMState "OUT_OF_BOUNDS"        = Just OutOfBounds
toMState "OUT_OF_TIME"          = Just OutOfTime
toMState "SERVER_DOWN"          = Just ServerDown
toMState _                      = Nothing


-- | Wrapper for defense action distribution
--
--   The following should always be True:
--
--      1) foldr ((+) . snd) 0 (fst . defActionsDist) == 100
--
--   The second part of the tuple is the generator list for the distribution (created via Genetic.Allele.uniformDistributionGen)
--   This is needed for a semi-random mutation
--
-- TODO: Segment the field in ~ 20 Fields and the corresponding actiondistribution
--
-- Coordinates: (x,y)
--
--  (Y)
--
-- -1     --------   -------   -------   --------
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |
-- -0.5  |_______-x,-y_______|_______+x,-y________|
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |__
--       |         |         |         |          |  |
--       |         |         |         |          |  |
--  0     --------   -------   --------   -------    |
--       |         |         |         |          |  |
--       |         |         |         |          |__|
--       |         |         |         |          |
--       |         |         |         |          |
--  0.5  |_______-x,+y_______|_______+x,+y________|
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |
--       |         |         |         |          |
--  1      -------   -------   -------   -------
--      -1       -0.5        0        0.5         1  (X)

--  x: (-1, -0.5) (-0.5, 0) (0, 0.5) (0.5, 1.0)
--  y: (-1, -0.5) (-0.5, 0) (0, 0.5) (0.5, 1.0)
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
data Offense = Offense { offActionDist     :: ([(Action, Int)],     [Int])
                       , offBallActionDist :: ([(BallAction, Int)], [Int]) }
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

-- | This data type is only used to create a better JSON representation to parse with Python
--
data SerializedTeams = SerializedTeams { defenseTeams :: [DefenseTeam], offenseTeams :: [OffenseTeam] }
    deriving (Show, Eq)

instance ToJSON SerializedTeams where

    toJSON SerializedTeams{..} = object [
        "defenseTeams" .= defenseTeams
      , "offenseTeams" .= offenseTeams
      ]

instance FromJSON SerializedTeams where

    parseJSON (Object o) = do
        defenseTeams <- o .: "defenseTeams"
        offenseTeams <- o .: "offenseTeams"
        return $ SerializedTeams defenseTeams offenseTeams

-- | Defaults
--
-- (testing purposes only)
defaultDefense :: Defense
defaultDefense = Defense { defActionDist = ([(Move, 50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100]) }

-- (testing purposes only)
defaultOffense :: Offense
defaultOffense = Offense { offActionDist     = ([(Move,  50), (Intercept, 20), (Catch, 15), (NoOp, 15)], [0, 50, 70, 85, 100])
                         , offBallActionDist = ([(Shoot, 50), (Dribble,   10), (Pass 7, 10)
                                                                             , (Pass 8, 10)
                                                                             , (Pass 9, 10)
                                                                             , (Pass 11,10)], [0,50,60,70,80,90,100])
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

-- (testing purposes only)
defaultTeams :: SerializedTeams
defaultTeams = SerializedTeams { defenseTeams = [defaultDefenseTeam]
                               , offenseTeams = [defaultOffenseTeam]
                               }