module HFO.Parser where

import System.Directory
import System.IO


data HFOStates = Ingame
               | Goal
               | CapturedByDefense
               | OutOfBounds
               | OutOfTime
               | ServerDown

instance Show HFOStates where

    show Ingame            = "IN_GAME"

    show Goal              = "GOAL"

    show CapturedByDefense = "CAPTURED_BY_DEFENSE"

    show OutOfBounds       = "OUT_OF_BOUNDS"

    show OutOfTime         = "OUT_OF_TIME"

    show ServerDown        = "SERVER_DOWN"


logpath :: FilePath
logpath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/goalie-log.txt"


cleanLog :: IO ()
cleanLog = withFile logpath WriteMode doNothing
    where doNothing = \_ -> return ()


getResults :: IO [Maybe HFOStates]
getResults = map toMState . lines <$>readFile logpath
    where

        toMState :: String -> Maybe HFOStates
        toMState "IN_GAME"              = Just Ingame
        toMState "GOAL"                 = Just Goal
        toMState "CAPTURED_BY_DEFENSE"  = Just CapturedByDefense
        toMState "OUT_OF_BOUNDS"        = Just OutOfBounds
        toMState "OUT_OF_TIME"          = Just OutOfTime
        toMState "SERVER_DOWN"          = Just ServerDown
        toMState _                      = Nothing




