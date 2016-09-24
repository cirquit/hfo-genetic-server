{-# LANGUAGE RecordWildCards #-}

module HFO.Agent.Conf where

import HFO.ToFlags              (ToFlags(..))
import HFO.Agent.Data           (Offense(..), Defense(..)) -- , defaultOffense)

-- | Top level agent configuration
--
--   These parameters will be transferred via commandline arguments to the python agent
--
data AgentConf = AgentConf
    { teamName     :: String                  -- this is currently 'base_left' (offense) and 'base_right' (defense) only
    , episodes     :: Int                     -- how many episodes are played
    , aseed        :: Int                     -- seed for the rng in python
    , playerNumber :: Int                     -- [0-3], because our teams have hardcoded 4 members
    , isOffense    :: Bool
    }

-- | ToFlag Instace that will be used by 'createProcess' in HFO.Agent.runAgent
--   to start the agent with the desired command line arguments
--
--   *) For every change here, the ArgumentParser in hfo-agent-py/genetic-agent.py has to be updated
--
instance ToFlags AgentConf where

    toFlags AgentConf{..} = concat [teamName', episodes', seed', playerNumber', isOffense']
        where

            teamName' = ["--team", teamName]

            episodes'
                | episodes > 0 = ["--episodes", show episodes]
                | otherwise    = error $ "Agent.AgentConf.episodes: This value should never be below 1, it's: " ++ show episodes

            seed' = ["--seed", show aseed]

            playerNumber' = ["--playerNumber", show playerNumber]

            isOffense'
                | isOffense  = ["--isOffense"]
                | otherwise = [""]

-- | Defaults
--
-- (testing purposes only)
defaultAgent :: AgentConf
defaultAgent = AgentConf
    { teamName     = "base_left"
    , aseed        = 123
    , episodes     = 1
    , playerNumber = 0
    , isOffense    = True
    }

