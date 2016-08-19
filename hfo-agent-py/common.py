#!/usr/bin/env python
# encoding: utf-8

from hfo import *


def stateToString(state):
    '''
    custom GameStatus to String function because the hfo.py returns scrambled strings
    '''
    if state == IN_GAME:
      return "IN_GAME"

    if state == GOAL:
      return "GOAL"

    if state == CAPTURED_BY_DEFENSE:
      return "CAPTURED_BY_DEFENSE"

    if state == OUT_OF_BOUNDS:
      return "OUT_OF_BOUNDS"

    if state == OUT_OF_TIME:
      return "OUT_OF_TIME"

    if state == SERVER_DOWN:
      return "SERVER_DOWN"

    return ("genetic-agent.py.statusToString: error, no parse for status - " + str(state))



def actionToString(action):
    '''
      custom Action to String function because the hfo.py returns scrambled strings
    '''

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

    if action == MOVE_TO:
      return "MOVE_TO"

    return ("genetic-agent.py.actionToString: error, no parse for action - " + str(action))

def toAngleHF(encoded):
    '''
      angle decoding in HIGH FEATURE STATE

      opening angles are encoded as (0, 3.1415926) and normalized to (-1, 1)
      to transfrom back: add 1, divide by 2, multiply by 3.1415926
    '''

    return ((encoded + 1) / 2) * 3.1415926

def toAngleLF(feature1, feature2):
    '''
      angle decoding in LOW FEATURE STATE

      angles are encoded as sin(µ) and cos(µ) where µ is the original angle
      to transfrom back: multiply sign of feature1 on acos(feature2), and we get radians
                         then divide by 3.1415926, multiply 180 to get the angle
    '''

    if feature1 >= 0: sign = 1
    else:             sign = -1

    return (sign * acos(feature2) / 3.1415926) * 180


def getMaxXPos(state, xPos):
    '''
        returns the all times maximum X-Position with the ball
        using HIGH FEATURE STATE
    '''
    ballPossession = state[5]

    if (ballPossession == 1):
        newXPos = state[0]
        return max(newXPos, xPos);

    return xPos

def getGoalOpeningAngle(state, goalOpeningAngle):
    '''
        returns the all times maximum X-Position with the ball
        using HIGH FEATURE STATE
    '''
    ballPossession = state[5]

    if (ballPossession == 1):
        newGOA = state[8]
        return max(newGOA, goalOpeningAngle);

    return goalOpeningAngle



