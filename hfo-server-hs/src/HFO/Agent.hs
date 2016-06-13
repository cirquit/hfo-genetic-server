{-# LANGUAGE RecordWildCards #-}

module HFO.Agent where

import System.IO                (Handle(..))
import System.Process
import Control.Concurrent       (threadDelay)
import Debug.Trace              (trace)
import Data.Foldable


import HFO.ToFlags              (ToFlags(..))

data Action  = Move           -- high level move based on strategy (whatever this might be - TODO)
--             | MoveTo Int Int -- x in [-1,1], y in [-1,1]
--             | Dash Int Int   -- power in [0,100], direction in [-180,180]
             | Intercept      -- intercept the ball
             | Catch          -- goalie only  (this may be a little bit ugly)
             | NoOp           -- no operation
--             | Turn Int       -- direction in [-180,180]
--             | Attract
    deriving Show

data BallAction  = Shoot                -- shoot in (possibly in looking direction)
--                 | Pass Int             -- pass to teammate in [0,11]
--                 | Kick   Int Int       -- power in [0,100], direction in [-180, 180]
--                 | KickTo Int Int Int   -- x in [-1,1], y in [-1,1], power in [0,3]
                 | Dribble              -- dribble in whatever direction?...
--                 | DribbleTo Int Int    -- x in [-1,1], y in [-1,1]
    deriving Show

data Defense = Defense { defActionDist :: [(Action, Int)] }
    deriving Show

data Offense = Offense { offActionDist :: [(Action, Int)], offBallActionDist :: [(BallAction, Int)] }
    deriving Show

defaultDefense :: Defense
defaultDefense = Defense { defActionDist = [(Move, 50), (Intercept, 20), (Catch, 15), (NoOp, 15)]}

defaultOffense :: Offense
defaultOffense = Offense { offActionDist     = [(Move,  50), (Intercept, 20), (Catch, 15), (NoOp, 15)]
                         , offBallActionDist = [(Shoot, 50), (Dribble,   50)]
                         }

-- data Team = OffenseTeam { op1 :: Offense, op2 :: Offense, op3 :: Offense }
--           | DefenseTeam { dp1 :: Defense, dp2 :: Defense, dp3 :: Defense, goalie :: Defense }
-- 
-- instance Foldable Team where
--     
--     foldr f acc (OffenseTeam op1 op2 op3)        = foldr f acc [op1, op2, op3]
-- 
--     foldr f acc (DefenseTeam dp1 dp2 dp3 goalie) = foldr f acc [goalie, op1, op2, op3]


data AgentConf = AgentConf
    { teamName   :: String -- this is currently 'base_left' (offense) and 'base_right' (defense) only
    , isGoalie   :: Bool
    , episodes   :: Int    -- how many episodes are played
    , aseed      :: Int    -- seed for the rng in python
    , actions    :: Either Offense Defense
    }

-- | Default settings
--
defaultAgent :: AgentConf
defaultAgent = AgentConf
    { teamName = "base_left"
    , isGoalie = False
    , aseed     = 123
    , episodes = 1
    , actions  = Left defaultOffense
    }

instance ToFlags AgentConf where

    toFlags AgentConf{..} = concat [teamName', isGoalie', episodes', seed', actions']
        where

            teamName' = ["--team", teamName]

            isGoalie'
                | isGoalie  = ["--goalie"]
                | otherwise = [""]

            episodes'
                | episodes > 0 = ["--episodes", show episodes]
                | otherwise    = error $ "Agent.AgentConf.episodes: This value should never be below 1, it's: " ++ show episodes

            actions' =
                case actions of
                    Left  (Offense actions ballActions) -> ["--offactions"] ++ map (show . snd) actions ++ map (show . snd) ballActions
                    Right (Defense actions            ) -> ["--defactions"] ++ map (show . snd) actions

            seed' = ["--seed", show aseed]

-- | run agent python script with the corresponding configuration
--
runAgent :: AgentConf -> IO (Handle, ProcessHandle)
runAgent conf = sleep 1 >> getInfo <$> createProcess cproc { cwd = cwd, std_err = CreatePipe }
    where

        cproc :: CreateProcess
        cproc = proc "./genetic-agent.py" args

        args :: [String]
        args = toFlags_ conf

        getInfo :: (a, b, Maybe Handle, ProcessHandle) -> (Handle, ProcessHandle)
        getInfo = (\(_,_,Just err,ph) -> (err, ph))

        cwd :: Maybe FilePath
        cwd = Just "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py"

-- | delay execution for i seconds
--
sleep :: Int -> IO ()
sleep i = threadDelay (i * 10^6)