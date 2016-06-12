#!/usr/bin/env python
# encoding: utf-8

# Before running this program, first Start HFO server:
# $> ./bin/HFO --no-sync --offense-agents 1

import random
import thread
import argparse
import sys

from hfo import *

# command line parser for the configurations + genome encoding (TODO)
#
def runParser():

  parser = argparse.ArgumentParser();
  parser.add_argument("-t", "--team",     dest = "team");
  parser.add_argument("-e", "--episodes", dest = "episodes",     type=int);
  parser.add_argument("-g", "--goalie",   action = "store_true", dest = 'isGoalie', default = False)
  result = parser.parse_args()

  return result;


# custom GameStatus to String functions because the hfo.py returns scrambled strings
#
def statusToString(status):

  if status == IN_GAME:
    return "IN_GAME"

  if status == GOAL:
    return "GOAL"

  if status == CAPTURED_BY_DEFENSE:
    return "CAPTURED_BY_DEFENSE"

  if status == OUT_OF_BOUNDS:
    return "OUT_OF_BOUNDS"

  if status == OUT_OF_TIME:
    return "OUT_OF_TIME"

  if status == SERVER_DOWN:
    return "SERVER_DOWN"

  return ("genetic-agent.py.toString: error, no parse for status - " + str(status))


formationsPath = "/home/rewrite/Documents/Project-Repos/HFO/bin/teams/base/config/formations-dt"
logPath        = "/home/rewrite/Documents/Project-Repos/hfo-genetic-server/hfo-agent-py/goalie-log.txt"

def main():

  # parse the command line arguments
  options = runParser();

  teamname = options.team        # String
  isGoalie = options.isGoalie    # Bool
  episodes = options.episodes    # positive Int

  # Create the HFO Environment
  hfo = HFOEnvironment()

  # Connect to the server with the specified feature set
  hfo.connectToServer(HIGH_LEVEL_FEATURE_SET, formationsPath, 6000, "localhost", teamname, isGoalie)

  for episode in xrange(episodes):
    status = IN_GAME
    while status == IN_GAME:
      state = hfo.getState()
      if state[5] == 1: # State[5] is 1 when the player can kick the ball
        hfo.act(random.choice([SHOOT, DRIBBLE]))
      else:
        hfo.act(MOVE)
      status = hfo.step()
    
    if isGoalie:
      log = open(logPath, "a")
      log.write(statusToString(status) + "\n")
      log.close()

if __name__ == "__main__":
  main()