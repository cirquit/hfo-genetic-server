{-# LANGUAGE RecordWildCards #-}

module HFO.Server.Conf where

import HFO.ToFlags     (ToFlags(..))

-- | All possible flags for the Half-Field Offense server encoded as attributes
--
--   These parameters will be transferred via commandline arguments to the Half-Field Offense binary
--
--   For the following settings a negative value means unlimited
--
--      **) framesPerTrail
--      **) frames
--      **) trials
--      **) untouchedTime
--
data ServerConf = ServerConf
    { framespertrial   :: Int        -- | after this many frames there will be a timeout and the server shuts down
    , seed             :: Int        -- | set custom seed for prng
    , showMonitor      :: Bool       -- | run with a visual output
    , port             :: Int        -- | server communicates through this port
    , untouchedTime    :: Int        -- | end trail if ball untouched
    , trials           :: Int        -- | number of trials to run
    , frames           :: Int        -- | number of frames to run for
    , offenseAgents    :: Int        -- | number of agents to run (0..11)  ------> No more than 11 agents per side
    , defenseAgents    :: Int        -- | number of agents to run (0..11)  +++++/+++++
    , offenseNpcs      :: Int        -- | number of agents to run (0..11)  ----/      \
    , defenseNpcs      :: Int        -- | number of agents to run (0..11)  +++++++++++> No more than 11 agents per side
    , offenseTeam      :: Maybe Team -- | base/helios (this choice affects the NPCs)
    , defenseTeam      :: Maybe Team -- | base/helios (this choice affects the NPCs)
    , standartPace     :: Bool       -- | slows the game to a more human paced steptime
    , logdir           :: FilePath   -- | log directory (this does not work at all, logs are corrupted)
    , noLogging        :: Bool       -- | disable logging
    , fullState        :: Bool       -- | server provides full state information
    , messageSize      :: Int        -- | max size limit for communication
    , ballMinX         :: Double     -- | [0..1] (TODO: create simple legend)
    , ballMaxX         :: Double     -- | [0..1] (TODO: create simple legend)
    , recordLogs       :: Bool       -- | TODO: look up the logs/create parser
    , giveBallToPlayer :: Int        -- | (index-based?), if negative, nobody gets it. of greater than number of offense players -> random
    }

-- | Current implementations of teams in HFO
--
data Team = Base | Helios

instance Show Team where

    show Helios = "helios"
    show Base   = "base"

-- | ToFlag Instace that will be used by 'createProcess' in HFO.Server.runServer
--   to start the server binary with the desired command line arguments
--
--   *) Nothing here should be changed here as this is the full set of arguments that are accepted
--
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

-- | Default settings with fixed seed
--   
--   This configuration is fitted for the Half-Field Offense Task (4 offense + 4 defense)
--
defaultServer :: ServerConf
defaultServer = ServerConf
    { showMonitor      = True
    , trials           = -1
    , frames           = -1
    , framespertrial   = 1000
    , untouchedTime    = 100
    , offenseAgents    = 4
    , defenseAgents    = 4
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
