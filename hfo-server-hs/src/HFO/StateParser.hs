{-# LANGUAGE RecordWildCards, BangPatterns, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module HFO.StateParser
  ( clearLog
  , writePopulation
  , readPopulation
  , readPopulationFrom
  , writePrettyPopulationTo
  , printPrettyPopulation
  ) where

import Data.ByteString.Lazy       as B (readFile, writeFile, ByteString())
import Data.ByteString.Lazy.Char8 as B (putStrLn)
import System.Directory                (getModificationTime, getDirectoryContents)
import System.FilePath.Posix           ((</>))
import Data.List                       (sortBy)
import Data.Ord                        (comparing)
import Control.Monad                   (when)

import HFO.Agent.Data  -- (SerializedTeams(), OffenseTeam(), DefenseTeam())
import Data.Aeson
import Data.Aeson.Encode.Pretty

logPath :: FilePath
logPath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/communication/communication.json"

communicationPath :: FilePath
communicationPath = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/communication/"


-- | Cleans the log for future simulations
--
clearLog :: IO ()
clearLog = B.writeFile logPath ""

-- | This encodes the whole population of defense and offense teams into a JSON object so
--   we can parse it nicely through our wrapper SerializedTeams with Python
--
writePopulation :: [DefenseTeam] -> [OffenseTeam] -> IO ()
writePopulation defs = B.writeFile logPath . encode . SerializedTeams defs


-- | reads the population from the latest communicationFile because the server
--   does not always terminate agents
--
--   thats why we have to look which one terminated the first and wrote the updated json
--   file so we can go forward with the simulation
--
readPopulation :: IO ([DefenseTeam], [OffenseTeam])
readPopulation = do
    fp <- getNewestFile
    readPopulationFrom fp

-- | reads the population and decodes it in two lists of teams
--
readPopulationFrom :: FilePath -> IO ([DefenseTeam], [OffenseTeam])
readPopulationFrom fp = do
    content <- B.readFile fp
    case eitherDecode content of
        Right (SerializedTeams defense offense) -> return (defense, offense)
        Left  err                               -> error $ "Could not parse the JSON with error: " ++ err


printPrettyPopulation :: [DefenseTeam] -> [OffenseTeam] -> IO ()
printPrettyPopulation defs = B.putStrLn . encodePretty . SerializedTeams defs


-- | debug purposes
--
writePrettyPopulationTo :: FilePath -> [DefenseTeam] -> [OffenseTeam] -> IO ()
writePrettyPopulationTo fp defs = B.writeFile fp . encodePretty . SerializedTeams defs


-- | returns the latest modified file in the communication folder
--
--   if there is are no files (this should never happen) we exit with an error
-- 
getNewestFile :: IO FilePath
getNewestFile = do

--  get every file in communication folder without ".", ".."
    content     <- filter (`notElem` [".", ".."]) <$> getDirectoryContents communicationPath

--  if there are no files - terminate everything, as this should never happen
    when (null content) (error "HFO.StateParser.getNewestFile: There are no files in the 'communicationPath'")

--  get modificationTime for every file and store the list as (filepath, modTime)
    contentTime <- (zip content) <$> mapM (getModificationTime . (communicationPath ++)) content

--  sort by descending time and get the first element
    let ((fp, _):_) = sortBy (flip $ comparing snd) contentTime
    return $ communicationPath ++ fp
