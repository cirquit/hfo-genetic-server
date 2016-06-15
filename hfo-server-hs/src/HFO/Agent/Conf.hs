{-# LANGUAGE RecordWildCards #-}

module HFO.Agent.Conf where

import HFO.ToFlags              (ToFlags(..))
import HFO.Agent.Data           (Offense(..), Defense(..), defaultOffense)

-- | Top level agent configuration
--
--   These parameters will be transferred via commandline arguments to the python agent
--
data AgentConf = AgentConf
    { teamName   :: String                  -- this is currently 'base_left' (offense) and 'base_right' (defense) only
    , isGoalie   :: Bool
    , episodes   :: Int                     -- how many episodes are played
    , aseed      :: Int                     -- seed for the rng in python
    , actions    :: Either Offense Defense
    }

-- | ToFlag Instace that will be used by 'createProcess' in HFO.Agent.runAgent
--   to start the agent with the desired command line arguments
--
--   *) For every change here, the ArgumentParser in hfo-agent-py/genetic-agent.py has to be updated
--
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


-- | Defaults
--
-- (testing purposes only)
defaultAgent :: AgentConf
defaultAgent = AgentConf
    { teamName = "base_left"
    , isGoalie = False
    , aseed    = 123
    , episodes = 1
    , actions  = Left defaultOffense
    }

