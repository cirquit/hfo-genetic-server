{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module HFO.StateParser where

-- import System.Directory
-- import System.IO

-- import Prelude hiding (readFile, writeFile, lines)

import qualified Data.Text.IO as T (readFile, writeFile)
import           Data.Text    as T (unpack, lines, Text(..))

data HFOState = Ingame
              | Goal
              | CapturedByDefense
              | OutOfBounds
              | OutOfTime
              | ServerDown

instance Show HFOState where

    show Ingame            = "IN_GAME"

    show Goal              = "GOAL"

    show CapturedByDefense = "CAPTURED_BY_DEFENSE"

    show OutOfBounds       = "OUT_OF_BOUNDS"

    show OutOfTime         = "OUT_OF_TIME"

    show ServerDown        = "SERVER_DOWN"


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
getResults :: IO [Maybe HFOState]
getResults = map toMState . T.lines <$> T.readFile logpath

    where

        toMState :: Text -> Maybe HFOState
        toMState "IN_GAME"              = Just Ingame
        toMState "GOAL"                 = Just Goal
        toMState "CAPTURED_BY_DEFENSE"  = Just CapturedByDefense
        toMState "OUT_OF_BOUNDS"        = Just OutOfBounds
        toMState "OUT_OF_TIME"          = Just OutOfTime
        toMState "SERVER_DOWN"          = Just ServerDown
        toMState _                      = Nothing