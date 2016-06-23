{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module HFO.StateParser where

import qualified Data.Text.IO as T (readFile, writeFile)
import           Data.Text    as T (pack, unpack, lines, Text())

import Data.Aeson

data HFOState = Ingame
              | Goal
              | CapturedByDefense
              | OutOfBounds
              | OutOfTime
              | ServerDown
    deriving Eq

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

logpath :: FilePath
logpath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/goalie-log.txt"


-- | Cleans the log for future simulations
--
cleanLog :: IO ()
cleanLog = T.writeFile logpath ""


-- | Using strict readFile/writeFile from text-package to get the results and concat them into a list of states
--  
--   *) Nothing if there is no parse for the line (should not happen anyways if the pythonagent is correct)
-- 
-- This will be deprecated because we store the serialized teams in as JSON
--
getResults :: IO [Maybe HFOState]
getResults = map toMState . T.lines <$> T.readFile logpath

