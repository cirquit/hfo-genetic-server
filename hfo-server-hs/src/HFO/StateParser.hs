{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module HFO.StateParser
  ( clearLog
  , writePopulation
  , readPopulation
  ) where

import           Data.ByteString.Lazy as B (readFile, writeFile, ByteString())


import HFO.Agent.Data  -- (SerializedTeams(), OffenseTeam(), DefenseTeam())
import Data.Aeson

logPath :: FilePath
logPath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/communication.json"

-- | Cleans the log for future simulations
--
clearLog :: IO ()
clearLog = B.writeFile logPath ""

-- | This encodes the whole population of defense and offense teams into a JSON object so
--   we can parse it nicely through our wrapper SerializedTeams with Python
--
writePopulation :: [DefenseTeam] -> [OffenseTeam] -> IO ()
writePopulation defs = B.writeFile logPath . encode . SerializedTeams defs


readPopulation :: IO ([DefenseTeam], [OffenseTeam])
readPopulation = do
    content <- B.readFile logPath
    case eitherDecode content of
        Right (SerializedTeams defense offense) -> return (defense, offense)
        Left  err                               -> error $ "Could not parse the JSON with error: " ++ err
