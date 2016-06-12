#!/usr/bin/env python
# encoding: utf-8

from hfo import *

# custom GameStatus to String function because the hfo.py returns scrambled strings
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

  return ("genetic-agent.py.statusToString: error, no parse for status - " + str(status))

# custom Action to String function because the hfo.py returns scrambled strings
#
def actionToString(action):

  if action == MOVE:
    return "MOVE"

  if action == INTERCEPT:
    return "INTERCEPT"

  if action == CATCH:
    return "CATCH"

  if action == NOOP:
    return "NOOP"

  if action == SHOOT:
    return "SHOOT"

  if action == DRIBBLE:
    return "DRIBBLE"

  return ("genetic-agent.py.actionToString: error, no parse for action - " + str(action))
