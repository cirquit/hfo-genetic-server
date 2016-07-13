{-# LANGUAGE RecordWildCards, BangPatterns, OverloadedStrings, FlexibleInstances #-}

module HFO.Agent.Actions where

import Data.Aeson
import Data.Aeson.Utils
import Data.Aeson.Types
import Data.Text as T hiding (map)
import GHC.Exts         -- (fromList)

import qualified Data.Vector as V

-- | All possible actions for an agent WITHOUT the possession of the ball
--
--   To/FromJSON instances also based on the Show instances which are copied from the python library
-- 
data Action  = Move                                    -- high level move based on strategy (whatever this might be - TODO)
             | Intercept                               -- intercept the ball
             | Catch                                   -- goalie only  (this may be a little bit ugly)
             | NoOp                                    -- no operation
             | MoveTo (Double,Double) (Double, Double) -- (x,y) (xBounds, yBounds) x € [-1,1], y € [-1,1]
--             | Dash Int Int                          -- power in [0,100], direction in [-180,180]
--             | Turn Int                              -- direction in [-180,180]
--             | Attract
    deriving (Eq, Show)


instance ToJSON Action where

    toJSON (MoveTo (x,y) (xBs, yBs)) = object [
          "action"    .= (String . toActionText $ (MoveTo (x,y) (xBs,yBs)))
        , "arguments" .= Array (fromList [Number $ fromFloatDigits x,   Number $ fromFloatDigits y,
                                          Number $ fromFloatDigits xBs, Number $ fromFloatDigits yBs])
        ]
    toJSON a = object [
          "action"    .= (String $ toActionText a)
        , "arguments" .= Array (fromList [])
        ]

instance FromJSON Action where

    parseJSON (Object o) = do
        action    <- o .: "action"
        arguments <- o .: "arguments"
        case toMAction action arguments of
            (Just x) -> return x
            Nothing  -> fail $ "Could not parse this object type - expected valid Action" ++ show o

toActionText :: Action -> Text
toActionText Move         = "MOVE"
toActionText Intercept    = "INTERCEPT"
toActionText Catch        = "CATCH"
toActionText NoOp         = "NOOP"
toActionText (MoveTo _ _) = "MOVE_TO"

toMAction :: Text -> V.Vector Value -> Maybe Action
toMAction "MOVE"      _ = Just Move
toMAction "INTERCEPT" _ = Just Intercept
toMAction "CATCH"     _ = Just Catch
toMAction "NOOP"      _ = Just NoOp
toMAction "MOVE_TO"    l = let (Number mx : Number my : Number mxBy : Number myBs : _) = V.toList l
                               [x, y, xBs, yBs] = map (toDouble . floatingOrInteger) [mx,my,mxBy,myBs]
                           in Just $ MoveTo (x,y) (xBs, yBs)
toMAction _           _ = Nothing

toDouble :: Either Double Integer -> Double
toDouble (Left  d) = d
toDouble (Right i) = fromInteger i




-- | All possible actions for an agent WITH the possesion of the ball
--
data BallAction  = Shoot                  -- shoot in (possibly in looking direction)
                 | Dribble                -- dribble in whatever direction?...
                 | Pass Integer                 -- pass to teammate in [0,11]
--                 | Kick   Int Int       -- power in [0,100], direction in [-180, 180]
--                 | KickTo Int Int Int   -- x in [-1,1], y in [-1,1], power in [0,3]
--                 | DribbleTo Int Int    -- x in [-1,1], y in [-1,1]
    deriving (Eq, Show)

instance ToJSON BallAction where

    toJSON (Pass i) = object [
        "ballAction"    .= (String . toBallActionText $ (Pass i))
      , "ballArguments" .= Array (fromList [Number $ fromInteger i])
        ]
    toJSON a = object [
        "ballAction"    .= (String . toBallActionText $ a)
      , "ballArguments" .= Array (fromList [])
        ]

instance FromJSON BallAction where

    parseJSON (Object o) = do
        ballAction    <- o .: "ballAction"
        ballArguments <- o .: "ballArguments"
        case toMBallAction ballAction ballArguments of
            (Just x) -> return x
            Nothing  -> fail $ "Could not parse this object type - expected valid BallAction" ++ show o

toBallActionText :: BallAction -> Text
toBallActionText Shoot    = "SHOOT"
toBallActionText Dribble  = "DRIBBLE"
toBallActionText (Pass _) = "PASS"

toMBallAction :: Text -> V.Vector Value -> Maybe BallAction
toMBallAction "SHOOT"   _        = Just Shoot
toMBallAction "DRIBBLE" _        = Just Dribble
toMBallAction "PASS"    l        = let (Number i):_ = V.toList l in
     case floatingOrInteger i of
          Left float -> Just $ Pass (round float)  -- this is just to convert from Double to Int, it should never change the value
          Right int  -> Just $ Pass int
toMBallAction _        _                  = Nothing
