#!/usr/bin/env python

import json

from common import stateToString

#   loads the json object with all the individuals that shall be evaluated
#
#   parseJSON :: FilePath -> JSON
def parseJSON(filepath):

    data = []

    with open(filepath) as file:
        data = json.load(file)

    return data


#   if the player is offense - both actions and ballactions are being concatinated to a big dict
#
#   the returning dict can be accessed as follows
#
#   actionsindex for offense [0-5]
#   actionsindex for defense [0-3]
# 
#   data[actionindex][0] = actionname
#   data[actionindex][1] = distribution
#
#
#   getActionDistribution ::  JSON     -> Int    -> Bool       -> [0-3] -> Dict String Int
def getActionDistribution(jsonData, teamIndex, isOffense, playerNumber):

    if isOffense:

        player = ""
        if playerNumber == 0:
            player = "op1"
        elif playerNumber == 1:
            player = "op2"
        elif playerNumber == 2:
            player = "op3"
        else:
            player = "op4"

        return jsonData["offenseTeams"][teamIndex][player]["offActions"] + jsonData["offenseTeams"][teamIndex][player]["offBallActions"]

    else:

        player = ""
        if playerNumber == 0:
            player = "goalie"
        elif playerNumber == 1:
            player = "dp2"
        elif playerNumber == 2:
            player = "dp3"
        else:
            player = "dp4"

        return jsonData["defenseTeams"][teamIndex][player]["defActions"]



#   update the json object with all the individuals with the state for the simulation
#
#   this should only be called if the simulation ended
#
#   current restriction is that we can only have teams with the same index play vs each other,
#   because the goalie can not know vs which team it plays
#
#   updateJSON :: JSON -> GameState -> Int -> JSON
def updateJSON(jsonData, state, teamIndex):

    strstate = stateToString(state)
    jsonData["offenseTeams"][teamIndex]["offFutureFitness"].append(strstate)
#    jsonData["defenseTeams"][teamIndex]["defFutureFitness"].append(strstate)
    return jsonData


#   writes the updated JSON to the file so Haskell can use the information about the simulations
#
#   deletes everything beforehand
#
def writeJSON(jsonData, filepath):

    with open(filepath, 'w') as file:
        json.dump(jsonData, file)
        file.flush()

