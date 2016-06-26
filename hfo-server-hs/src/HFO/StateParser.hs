{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module HFO.StateParser
  ( clearLog
  , writePopulation
  , readPopulation
  , writePrettyPopulation
  , writePrettyPopulationTo
  , printPrettyPopulation
  ) where

import Data.ByteString.Lazy       as B (readFile, writeFile, ByteString())
import Data.ByteString.Lazy.Char8 as B (putStrLn)


import HFO.Agent.Data  -- (SerializedTeams(), OffenseTeam(), DefenseTeam())
import Data.Aeson
import Data.Aeson.Encode.Pretty

logPath :: FilePath
logPath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/communication.json"

resultPath :: FilePath
resultPath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/results.json"


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


writePrettyPopulation :: [DefenseTeam] -> [OffenseTeam] -> IO ()
writePrettyPopulation defs = B.writeFile resultPath . encodePretty . SerializedTeams defs

printPrettyPopulation :: [DefenseTeam] -> [OffenseTeam] -> IO ()
printPrettyPopulation defs = B.putStrLn . encodePretty . SerializedTeams defs

-- | debug purposes
--
writePrettyPopulationTo :: FilePath -> [DefenseTeam] -> [OffenseTeam] -> IO ()
writePrettyPopulationTo fp defs = B.writeFile fp . encodePretty . SerializedTeams defs

