#!/usr/bin/env python
# encoding: utf-8

# Custom test: ./genetic-agent.py --team base_left --episodes 1 --seed 123 --playerNumber 0 --isOffense
#              ./genetic-agent.py --team base_left --episodes 1 --seed 123 --playerNumber 1 --isOffense
#              ./bin/HFO --offense-agents 2 --defense-npcs 2 --defense-team base --no-sync --offense-on-ball 1 

# Custom test2 : ./bin/HFO --offense-agents 1 --defense-agents 1 --no-sync --offense-on-ball 1
#                ./genetic-agent.py --team base_left --episodes 1 --seed 123 --playerNumber 0 --isOffense
#                ./genetic-agent.py --team base_right --episodes 1 --seed 123 --playerNumber 0

# Custom test3 : ./bin/HFO --offense-agents 1 --defense-npcs 1 --no-sync --offense-on-ball 1
#                ./genetic-agent.py --team base_left --episodes 1 --seed 123 --playerNumber 0 --isOffense


import random
import thread
import argparse
import sys
from pprint import pprint


from hfo import *
from coloredoutput import info, warn
from common        import stateToString, actionToString, getMaxXPos, getGoalOpeningAngle
from agentaction   import getAction, toAngleHF, toAngleLF
from jsonparser    import parseJSON, updateJSON, writeJSON, getActionDistribution



def runParser():
    '''
    command line parser for the configurations + genome encoding (TODO)
    '''

    parser = argparse.ArgumentParser();
    parser.add_argument("--team",         dest = "team");
    parser.add_argument("--episodes",     dest = "episodes",     type=int);
    parser.add_argument("--seed ",        dest = "seed",         type=int);
    parser.add_argument("--playerNumber", dest = "playerNumber", type=int);
    parser.add_argument("--isOffense",    dest = "isOffense",    action = "store_true", default = False);

    result = parser.parse_args()
    return result;


# Absolute paths for the formations and log files
#
formationsPath = "/home/rewrite/Documents/Project-Repos/HFO/bin/teams/base/config/formations-dt"
logPath        = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/communication/communication.json"


def getMyLogPath(isOffense, index):
    '''
    getMyLogPath :: Bool -> Int -> FilePath
    '''
    if isOffense:
        return "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/communication/communication" + str(index) + "off.json"
    else:
        return "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/communication/communication" + str(index) + "def.json"



def main():
    '''
    Main entry point
    '''

    jsonData = parseJSON(logPath)

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
    # hfo.connectToServer(HIGH_LEVEL_FEATURE_SET, formationsPath, 6000, "localhost", teamname, isGoalie)
    hfo.connectToServer(HIGH_LEVEL_FEATURE_SET, formationsPath, 6000, "localhost", teamname, isGoalie)

    # check if we have the same amount of offense as defense teams
    offTeamCount = len(jsonData["offenseTeams"])
#    assert offTeamCount == len(jsonData["defenseTeams"])

    currentTeam = -1

#   added for fitness evaluation, will be stored for every episode in the json
    xPos  = -1

#   added for fitness evaluation, will be stored for every episode in the json
    goalOpeningAngle = 0

    for currentEpisode in xrange(offTeamCount * episodes):

        # switch to next team if the current team played all episodes
        # reset the maximum X-Position
        if (currentEpisode % episodes) == 0:
            currentTeam = currentTeam + 1

        # action distribution of the player I represent
        # playerDist :: ActionJSON (defined in jsonparser.py)
        playerDist = getActionDistribution(jsonData, currentTeam, isOffense, playerNumber)

        # reset maximum x-position
        xPos = -1

        # reset maximum goal opening
        goalOpeningAngle = 0.0

        # Main game loop
        state = IN_GAME
        while state == IN_GAME:
            state  = hfo.getState()
            action = getAction(state, isOffense, playerDist)
            action.execute(env = hfo, state = state)
            # xPos = getMaxXPos(state, xPos)
            goalOpeningAngle = getGoalOpeningAngle(state, goalOpeningAngle)
            state  = hfo.step()

        # Goalie logs every result in the json object
        jsonData = updateJSON(jsonData, state, currentTeam, goalOpeningAngle)

    # when we are done with every team, write the updated json object to the log
    myLogPath = getMyLogPath(isOffense, playerNumber)
    writeJSON(jsonData, myLogPath)


if __name__ == "__main__":
    main()
