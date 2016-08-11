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
#   actionindex for defense:
#       result[0-15]                       returns the actiondistribution for the 16 separations of the field
#       result[0]["actionDist"][0-3]       for every action possible (currently MOVE, INTERCEPT, CATCH, NOOP)
#
#   actionindex for offense:
# 
#       result[0-15]                       returns the actiondistribution for the 16 separations of the field
#       result[0]["actionDist"][0-3]       for every action possible (currently MOVE, INTERCEPT, CATCH, NOOP)
#
#       result[16-31]                      returns the ballActionDistribution for the 16 separations of the field
#       result[16]["ballActionDist"][0-3]  for every ballAction possible (currently SHOOT, DRIBBLE, PASS, PASS)
#
#   getActionDistribution ::  JSON -> Int      -> Bool   -> [0-3]         -> ActionJSON
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

#        return jsonData["offenseTeams"][teamIndex][player]["offActionDist"] + jsonData["offenseTeams"][teamIndex][player]["offBallActionDist"]
        return jsonData["offenseTeams"][teamIndex][player]["offActionDist"]["actionDist"] + jsonData["offenseTeams"][teamIndex][player]["offBallActionDist"]["ballActionDist"]

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

#        return jsonData["defenseTeams"][teamIndex][player]["defActionDist"]
        return jsonData["defenseTeams"][teamIndex][player]["defActionDist"]["actionDist"]



#   update the json object with all the individuals with the state for the simulation
#
#   this should only be called if the simulation ended
#
#   current restriction is that we can only have teams with the same index play vs each other,
#   because the goalie can not know vs which team it plays
#
#   updateJSON :: JSON -> GameState -> Int -> JSON
def updateJSON(jsonData, state, teamIndex, curMaxXPos):

    strstate = stateToString(state)
    jsonData["offenseTeams"][teamIndex]["offStateFitness"].append(strstate)
    jsonData["offenseTeams"][teamIndex]["offPosFitness"].append(curMaxXPos)
#    jsonData["defenseTeams"][teamIndex]["defStateFitness"].append(strstate)
    return jsonData


#   writes the updated JSON to the file so Haskell can use the information about the simulations
#
#   deletes everything beforehand
#
def writeJSON(jsonData, filepath):

    with open(filepath, 'w') as file:
        json.dump(jsonData, file)
        file.flush()

