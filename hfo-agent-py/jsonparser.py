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


def get_rnn_encoding(jsonData, teamIndex, isOffense, playerNumber):
    '''
    '''
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

        return jsonData["offenseTeams"][teamIndex][player]["rnnEncoding"]

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

        return jsonData["defenseTeams"][teamIndex][player]["rnnEncoding"]


def updateJSON(jsonData, state, teamIndex, additionalFitness):
    '''
    update the json object with all the individuals with the state for the simulation
 
    this should only be called if the simulation ended
 
    current restriction is that we can only have teams with the same index play vs each other,
    because the goalie can not know vs which team it plays
 
    updateJSON :: JSON -> GameState -> Int -> JSON
    '''

    print("Getting goalAngle as number: {0}, as string: {1}").format(additionalFitness, str(additionalFitness))

    strstate = stateToString(state)
    jsonData["offenseTeams"][teamIndex]["offStateFitness"].append(strstate)
    jsonData["offenseTeams"][teamIndex]["offPosFitness"].append(str(additionalFitness)) # we cast the float to string to avoid JSON problems
#    jsonData["defenseTeams"][teamIndex]["defStateFitness"].append(strstate)
    return jsonData


def writeJSON(jsonData, filepath):
    '''
    writes the updated JSON to the file so Haskell can use the information about the simulations

    deletes everything beforehand
    '''

    with open(filepath, 'w') as file:
        json.dump(jsonData, file)
        file.flush()

