#!/usr/bin/env python
# encoding: utf-8

import random
import thread
import argparse
import sys

from hfo import *

from common import statusToString, actionToString
from agentaction import getAction

# command line parser for the configurations + genome encoding (TODO)
#
def runParser():

    parser = argparse.ArgumentParser();
    parser.add_argument("--team",       dest = "team");
    parser.add_argument("--episodes",   dest = "episodes", type=int);
    parser.add_argument("--goalie",     dest = 'isGoalie', action = "store_true", default = False)
    parser.add_argument("--seed ",      dest = "seed",     type=int);
    parser.add_argument("--offactions", dest = "offense_actions", type = int, nargs = "+", default = [])
    parser.add_argument("--defactions", dest = "defense_actions", type = int, nargs = "+", default = [])

    result = parser.parse_args()

    return result;


# Absolute paths for the formations and log files
#
formationsPath = "/home/rewrite/Documents/Project-Repos/HFO/bin/teams/base/config/formations-dt"
logPath        = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/goalie-log.txt"

# Main entry point
#
def main():

    # parse the command line arguments
    options = runParser()

    teamname = options.team        # String
    isGoalie = options.isGoalie    # Bool
    episodes = options.episodes    # positive Int
    seed     = options.seed        # Int

    random.seed(seed)

    # Create the HFO Environment
    hfo = HFOEnvironment()

    # Connect to the server with the specified feature set
    hfo.connectToServer(HIGH_LEVEL_FEATURE_SET, formationsPath, 6000, "localhost", teamname, isGoalie)

    # Main interaction loop
    for episode in xrange(episodes):
        status = IN_GAME
        while status == IN_GAME:
            state = hfo.getState()
            action = getAction(state, options)
            hfo.act(action)
            status = hfo.step()

        # Goalie logs every result to a file
        if isGoalie:
            log = open(logPath, "a")
            log.write(statusToString(status) + "\n")
            log.close()

if __name__ == "__main__":
    main()