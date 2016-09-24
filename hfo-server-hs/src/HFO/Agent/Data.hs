{-# LANGUAGE RecordWildCards, BangPatterns, OverloadedStrings, FlexibleInstances #-}

module HFO.Agent.Data where

import Data.Aeson
import Data.Aeson.Utils
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Exts

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

toMState :: T.Text -> Maybe HFOState
toMState "IN_GAME"              = Just Ingame
toMState "GOAL"                 = Just Goal
toMState "CAPTURED_BY_DEFENSE"  = Just CapturedByDefense
toMState "OUT_OF_BOUNDS"        = Just OutOfBounds
toMState "OUT_OF_TIME"          = Just OutOfTime
toMState "SERVER_DOWN"          = Just ServerDown
toMState _                      = Nothing


-- | Wrapper for defense action distributions

--   Segment the field in ~ 16 Fields and the corresponding distribution for every action
--
--   Coordinates: (x,y)
--
--  (Y)
--
--  -1     --------   -------   -------   --------
--        |         |         |         |         |
--        |         |         |         |         |
--        |   a00   |   a01   |   a02   |   a03   |
--        |         |         |         |         |
--  -0.5  |_______-x,-y_______|_______+x,-y_______|
--        |         |         |         |         |
--        |         |         |         |         |
--        |   a04   |   a05   |   a06   |   a07   |__
--        |         |         |         |         |  |
--        |         |         |         |         |  |
--   0     --------   -------   --------   -------   |
--        |         |         |         |         |  |
--        |         |         |         |         |__|
--        |   a08   |   a09   |   a10   |   a11   |
--        |         |         |         |         |
--   0.5  |_______-x,+y_______|_______+x,+y_______|
--        |         |         |         |         |
--        |         |         |         |         |
--        |   a12   |   a13   |   a14   |   a15   |
--        |         |         |         |         |
--        |         |         |         |         |
--   1      -------   -------   -------   -------
--       -1       -0.5        0        0.5         1  (X)

-- For now we will pass the distributions for every field from top-left to bottom right
-- [a00,a01...a15]
--
-- !! If this order changes we have to update the python code too !!

data Defense = Defense { defActionDist :: ActionDist }
    deriving (Show, Eq)

instance ToJSON Defense where

    toJSON (Defense defActionDist) = object [
        "defActionDist" .= defActionDist
      ]

instance FromJSON Defense where

    parseJSON (Object o) = do
        defActionDist   <- o .: "defActionDist"
        return $ Defense defActionDist

-- | Wrapper for offense action distribution
-- 
--   *) Same order of actionDists to segments of the field as in Defense
--   *) We have two distributions, because we either have the ball, or not
--   
data Offense = Offense { offActionDist     :: ActionDist
--                       , offBallActionDist :: BallActionDist
                       }
    deriving (Show, Eq)

instance ToJSON Offense where

    toJSON (Offense offActionDist)  {- offBallActionDist) -} = object [
        "offActionDist"     .= offActionDist
--      , "offBallActionDist" .= offBallActionDist
      ]

instance FromJSON Offense where

    parseJSON (Object o) =  do 
        offActionDist      <- o .: "offActionDist"
--        offBallActionsDist <- o .: "offBallActionDist"
        return $ Offense offActionDist -- offBallActionsDist


-- | Wrapper for Teams
--
--   The Half-Field Offense Task defines 4 offensive and 3 defensive players + goalie
--
--              max-X-pos    
--              while holding   new states from
--               the ball         simulation
--                    |              |
--                    |              |
--                ([Double], [HFOState])
--
data OffenseTeam = OffenseTeam { op1        :: Offense
                               , op2        :: Offense
                               , op3        :: Offense
                               , op4        :: Offense
                               , offFitness :: ([Double], [Maybe HFOState])
                               }
    deriving (Show, Eq)

instance ToJSON OffenseTeam where

    toJSON OffenseTeam{..} = object [
       "op1" .= op1
      ,"op2" .= op2
      ,"op3" .= op3
      ,"op4" .= op4
      ,"offPosFitness"   .= (map show $ fst offFitness)
      ,"offStateFitness" .= snd offFitness
      ]

instance FromJSON OffenseTeam where

    parseJSON (Object o) = do
            op1 <- o .: "op1"
            op2 <- o .: "op2"
            op3 <- o .: "op3"
            op4 <- o .: "op4"
            offFitness       <- map readDouble <$> o .: "offPosFitness"
            offFutureFitness <- o .: "offStateFitness"
            return $ OffenseTeam op1 op2 op3 op4 (offFitness, offFutureFitness)
        where
            readDouble :: String -> Double
            readDouble = read

data DefenseTeam = DefenseTeam { goalie     :: Defense
                               , dp2        :: Defense
                               , dp3        :: Defense
                               , dp4        :: Defense
                               , defFitness :: ([Double], [Maybe HFOState])}
    deriving (Show, Eq)

instance ToJSON DefenseTeam where

    toJSON DefenseTeam{..} = object [
       "goalie" .= goalie
      ,"dp2" .= dp2
      ,"dp3" .= dp3
      ,"dp4" .= dp4
      ,"defPosFitness"   .= (map show $ fst defFitness)
      ,"defStateFitness" .= snd defFitness
      ]


instance FromJSON DefenseTeam where

    parseJSON (Object o) = do
            goalie <- o .: "goalie"
            dp2    <- o .: "dp2"
            dp3    <- o .: "dp3"
            dp4    <- o .: "dp4"
            defFitness       <- map readDouble <$> o .: "defPosFitness"
            defFutureFitness <- o .: "defStateFitness"
            return $ DefenseTeam goalie dp2 dp3 dp4 (defFitness, defFutureFitness)
        where
            readDouble :: String -> Double
            readDouble = read

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

{-

-- | Defaults
--
-- (testing purposes only)
defaultDefense :: Defense
defaultDefense = Defense { defActionDist = {- take 16 (repeat -} seg01 }
    where
        seg01 :: ActionDist
        seg01 = ActionDist { actionDist       = [(Move, 50), (Intercept, 20), (NoOp, 30)]
                           , actionGenerator  = [0, 50, 70, 100]
                           }

-- (testing purposes only)
defaultOffense :: Offense
defaultOffense = Offense { offActionDist     = {- take 16 (repeat -} seg01
                         , offBallActionDist = {- take 16 (repeat -} seg01Ball
                         }
    where
        seg01 :: ActionDist
        seg01 = ActionDist { actionDist       = [(Move, 50), (Intercept, 20), (NoOp, 30)]
                           , actionGenerator  = [0, 50, 70, 100]
                           }
        seg01Ball :: BallActionDist
        seg01Ball = BallActionDist { ballActionDist      = [(Shoot,50), (Dribble,10), (Pass 7,10), (Pass 11,10)]
                                   , ballActionGenerator = [0,50,60,70,80,90,100]
                                   }

-- (testing purposes only)
defaultDefenseTeam :: DefenseTeam
defaultDefenseTeam  = DefenseTeam { goalie     = defaultDefense
                                  , dp2        = defaultDefense
                                  , dp3        = defaultDefense
                                  , dp4        = defaultDefense
                                  , defFitness = ([], [])
                                  }

-- (testing purposes only)
defaultOffenseTeam ::OffenseTeam
defaultOffenseTeam = OffenseTeam { op1        = defaultOffense
                                 , op2        = defaultOffense
                                 , op3        = defaultOffense
                                 , op4        = defaultOffense
                                 , offFitness = ([], [])
                                 }

-- (testing purposes only)
defaultTeams :: SerializedTeams
defaultTeams = SerializedTeams { defenseTeams = [defaultDefenseTeam]
                               , offenseTeams = [defaultOffenseTeam]
                               }
-}