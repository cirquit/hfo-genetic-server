{-# LANGUAGE RecordWildCards #-}

module HFO.Server where

import System.Process
import System.Exit     (ExitCode(..))
import System.IO       (Handle(..))

import HFO.ToFlags     (ToFlags(..))

-- | Current implementations of teams in HFO
--
data Team = Base | Helios

instance Show Team where

    show Helios = "helios"
    show Base   = "base"

-- | For the following settings a negative value means unlimited
--
--   *) framesPerTrail
--   *) frames
--   *) trials
--   *) untouchedTime

data ServerConf = ServerConf
    { framespertrial   :: Int        -- | after this many frames there will be a timeout and the server shuts down
    , seed             :: Int        -- | set custom seed for prng
    , showMonitor      :: Bool       -- | run with a visual output
    , port             :: Int        -- | server communicates through this port
    , untouchedTime    :: Int        -- | end trail if ball untouched
    , trials           :: Int        -- | number of trials to run
    , frames           :: Int        -- | number of frames to run for
    , offenseAgents    :: Int        -- | number of agents to run (TODO: check bounds)
    , defenseAgents    :: Int        -- | number of agents to run (TODO: check bounds)
    , offenseNpcs      :: Int        -- | number of agents to run (TODO: check bounds)
    , defenseNpcs      :: Int        -- | number of agents to run (TODO: check bounds)
    , offenseTeam      :: Maybe Team -- | base/helios (TODO: check relations with other settings)
    , defenseTeam      :: Maybe Team -- | base/helios (TODO: check relations with other settings)
    , standartPace     :: Bool       -- | slows the game to a more human paced steptime
    , logdir           :: FilePath   -- | log directory (TODO: create if missing)
    , noLogging        :: Bool       -- | disable logging
    , fullState        :: Bool       -- | server provides full state information
    , messageSize      :: Int        -- | max size limit for communication
    , ballMinX         :: Double     -- | [0..1] 
    , ballMaxX         :: Double     -- | [0..1]
    , recordLogs       :: Bool       -- | TODO: look up the logs/create parser
    , giveBallToPlayer :: Int        -- | (index-based?), if negative, nobody gets it. of greater than number of offense players -> random
    }

instance ToFlags ServerConf where

    toFlags ServerConf{..} = concat $
                                    [ showMonitor', fpt', port', seed', frames', logdir', standartPace'
                                    , trials', noLogging', fullState', ballMaxX', ballMinX'
                                    , recordLogs', untouchedTime', offenseAgents', defenseAgents'
                                    , offenseNpcs', defenseNpcs', offenseTeam', defenseTeam'
                                    , giveBallToPlayer', messageSize']
        where

            showMonitor' | showMonitor = []
                         | otherwise   = ["--headless"]

            fpt'      = ["--frames-per-trial", show framespertrial]

            port'     = ["--port", show port]

            seed'     = ["--seed", show seed]

            frames'   = ["--frames", show frames]

            trials'   = ["--trials", show trials]

            untouchedTime' = ["--untouched-time", show untouchedTime]

            logdir'  = ["--log-dir", logdir]

            standartPace' | standartPace = ["--no-sync"]
                          | otherwise    = []

            noLogging' | noLogging = ["--no-logging"]
                       | otherwise = []

            fullState' | fullState = ["--fullstate"]
                       | otherwise = []

            ballMinX' = ["--ball-x-min", show ballMinX]

            ballMaxX' = ["--ball-x-max", show ballMaxX]

            recordLogs' | recordLogs = ["--record"]
                        | otherwise  = []

            offenseAgents' | offenseAgents <= 0 = []
                           | otherwise          = ["--offense-agents", show offenseAgents]

            defenseAgents' | defenseAgents <= 0 = []
                           | otherwise          = ["--defense-agents", show defenseAgents]

            offenseNpcs'   | offenseNpcs <= 0 = []
                           | otherwise        = ["--offense-npcs", show offenseNpcs]

            defenseNpcs'   | defenseNpcs <= 0 = []
                           | otherwise        = ["--defense-npcs", show defenseNpcs]

            offenseTeam'   | Just team <- offenseTeam = ["--offense-team", show team]
                           | otherwise                = []

            defenseTeam'   | Just team <- defenseTeam = ["--defense-team", show team]
                           | otherwise                = []

            giveBallToPlayer' = ["--offense-on-ball", show giveBallToPlayer]

            messageSize' = ["--message-size", show messageSize]

-- | Deterministic default settings
--   
--   Still needs to set players
--
defaultServer :: ServerConf
defaultServer = ServerConf
    { showMonitor      = True
    , trials           = -1
    , frames           = -1
    , framespertrial   = 1000
    , untouchedTime    = 100
    , offenseAgents    = 0
    , defenseAgents    = 0
    , offenseNpcs      = 0
    , defenseNpcs      = 0
    , offenseTeam      = Just Base
    , defenseTeam      = Just Base
    , standartPace     = False
    , port             = 6000
    , noLogging        = False
    , logdir           = "log/"
    , recordLogs       = False
    , giveBallToPlayer = 0
    , fullState        = False
    , seed             = 31415926
    , messageSize      = 128     -- | TODO: check default value
    , ballMinX         = 0
    , ballMaxX         = 0.2
    }


-- | run server binary with the specified config
--   
runServer :: ServerConf -> IO ProcessHandle
runServer conf = getInfo <$> createProcess cproc { cwd = cwd } -- , std_err = CreatePipe }
    where

        cproc :: CreateProcess
        cproc = proc "./bin/HFO" args

        args :: [String]
        args = toFlags_ conf

        getInfo :: (a, b, Maybe Handle, ProcessHandle) -> ProcessHandle
        getInfo = (\(_,_,_,ph) -> ph)

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/HFO"

-- | runServer without Handle/ProcessHandle
--
runServer_ :: ServerConf -> IO ()
runServer_ conf = runServer conf >> return ()
