#!/usr/bin/env python
# encoding: utf-8

import random
import thread
import argparse
import sys
from pprint import pprint


from hfo import *
from coloredoutput import info, warn
from common        import stateToString, actionToString
from agentaction   import getAction
from jsonparser    import parseJSON, updateJSON, writeJSON, getActionDistribution


# command line parser for the configurations + genome encoding (TODO)
#
def runParser():

    parser = argparse.ArgumentParser();
    parser.add_argument("--team",         dest = "team");
    parser.add_argument("--episodes",     dest = "episodes",     type=int);
    parser.add_argument("--seed ",        dest = "seed",         type=int);
    parser.add_argument("--playerNumber", dest = "playerNumber", type=int);
    parser.add_argument("--isOffense",    dest = "isOffense",    action = "store_true", default = False);

    result = parser.parse_args()
    return result;

#    parser.add_argument("--isGoalie", dest = 'isGoalie',        action = "store_true",   default = False)
#    parser.add_argument("--offactions", dest = "offense_actions", type = int, nargs = "+", default = [])
#    parser.add_argument("--defactions", dest = "defense_actions", type = int, nargs = "+", default = [])

# TODO test with: ./genetic-agent.py --team base_left --episodes 1 --seed 123 --playerNumber 0 --isOffense

# and with: ./bin/HFO --offense-agents 1 

# Absolute paths for the formations and log files
#
formationsPath = "/home/rewrite/Documents/Project-Repos/HFO/bin/teams/base/config/formations-dt"
logPath        = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/communication.json"

# Main entry point
#
def main():

    jsonData = parseJSON(logPath)
#    pprint(data)

    # parse the command line arguments
    options = runParser()

    teamname     = options.team         # String
    episodes     = options.episodes     # positive Int
    seed         = options.seed         # Int
    playerNumber = options.playerNumber # Int
    isOffense    = options.isOffense    # Bool

    isGoalie     = (not isOffense) and playerNumber == 0 # Bool

    random.seed(seed)

    # Create the HFO Environment
    hfo = HFOEnvironment()

    # Connect to the server with the specified feature set
    hfo.connectToServer(HIGH_LEVEL_FEATURE_SET, formationsPath, 6000, "localhost", teamname, isGoalie)

    # check if we have the same amount of offense as defense teams
    offTeamCount = len(jsonData["offenseTeams"])
    assert offTeamCount == len(jsonData["defenseTeams"])

    # for every team
    for teamIndex in xrange(offTeamCount):

        # action distribution of the player I represent
        # playerDist :: Dict String Int
        playerDist = getActionDistribution(jsonData, teamIndex, isOffense, playerNumber)

        # Main game loop
        for episode in xrange(episodes):
            status = IN_GAME
            while status == IN_GAME:
                state = hfo.getState()
                action = getAction(state, isOffense, playerDist)
                hfo.act(action)
                status = hfo.step()

            # Goalie logs every result in the json object
            if isGoalie:
                 jsonData = updateJSON(jsonData, status, teamIndex)

    # when we are done with every team, write the updated json object to the log
    writeJSON(jsonData, logpath)


if __name__ == "__main__":
    main()
