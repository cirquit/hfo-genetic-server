#!/usr/bin/env python
# encoding: utf-8

import random
from hfo import *
from common import actionToString

# getAction returns the action based on their probabilities and in what current state we are
#
#   getAction :: State -> Bool -> Dict String Int -> IO Action
def getAction(state, isOffense, actionsDict):

  # pick from actions based on their probability
  #
  # chooseFrom :: [(Action, Int)] -> IO Action
    def chooseFrom(actions):
        r = random.randint(0,100)
        for (action, prob) in actions:
            if r <= prob:
                return action
            else:
                r = r - prob
        raise Exception("None of the actions were used - r: " + str(r))

  # ballPossession :: Int (0 or 1)
    ballPossession = state[5] # does the agent have the ball

  # if we are a defense player, we only have 4 possible actions to choose from
    if (not isOffense):
        actions = [ (Action(actionenum = MOVE),      actionsDict[0][1])
                  , (Action(actionenum = INTERCEPT), actionsDict[1][1])
                  , (Action(actionenum = CATCH),     actionsDict[2][1])
                  , (Action(actionenum = NOOP),      actionsDict[3][1])
                  ]
        return chooseFrom(actions)

  # if we are an offense player, we have 4 possible actions + 2 ball actions to choose from
    else:
        actions = [ (Action(actionenum = MOVE),      actionsDict[0][1])
                  , (Action(actionenum = INTERCEPT), actionsDict[1][1])
                  , (Action(actionenum = CATCH),     actionsDict[2][1])
                  , (Action(actionenum = NOOP),      actionsDict[3][1])
                  ]

        ballActions = [ (Action(actionenum = SHOOT),   actionsDict[4][1])
                      , (Action(actionenum = DRIBBLE), actionsDict[5][1])
                      , (Action(actionenum = PASS, passTo = 7),  actionsDict[6][1])
                      , (Action(actionenum = PASS, passTo = 8),  actionsDict[7][1])
                      , (Action(actionenum = PASS, passTo = 9),  actionsDict[8][1])
                      , (Action(actionenum = PASS, passTo = 11), actionsDict[9][1])
                      ]

      # if we have the ball, choose from the ball actions
        if ballPossession == 1: return chooseFrom(ballActions)
      # otherwise, choose from the normal actions
        else: return chooseFrom(actions)


# Action object for every possible hfoaction
# needed to store optional arguments for the actions like playernumber, or coordiantes
#
class Action(object):

    def __init__(self, actionenum = MOVE, passTo = 11, xCoord = 0, yCoord = 0, power = 1, direction = 100 ):
        
        self.actionenum = actionenum   # this can be any possible action 
        self.passTo     = passTo       # only used in PASS, possible numbers [7,8,9,11]
        self.xCoord     = xCoord       # not used yet
        self.yCoord     = yCoord       # not used yet
        self.power      = power        # not used yet
        self.direction  = direction    # not used yet

    def execute(self, env):

        if self.actionenum == MOVE:      env.act(MOVE);
        if self.actionenum == INTERCEPT: env.act(INTERCEPT);
        if self.actionenum == CATCH:     env.act(CATCH);
        if self.actionenum == NOOP:      env.act(NOOP);
        if self.actionenum == SHOOT:     env.act(SHOOT);
        if self.actionenum == DRIBBLE:   env.act(DRIBBLE);
        if self.actionenum == PASS:      env.act(PASS, self.passTo);


# gamestates for the future (TODO)

#  xpos           = state[0] # x-position                     type:Int, range:[-1..1]
#  ypos           = state[1] # y-position                     type:Int, range:[-1..1]
#  orient         = state[2] # orientation
#  ballProximity  = state[3] # how far is the distance to the ball 
#  ballAngle      = state[4] # angle to the ball
#  goalCenterProximity = state[6]
#  goalCenterAngle     = state[7]
#  goalOpeningAngle    = state[8]

# Coordinates
#
# -1,-1--------- --------- 1,-1 
#     |         |         |     
#     |         |         |     
#     |  -x,-y  |  +x,-y  |     
#    _|         |         |_    
#   | |_________|_________| |   
#   |_|         |         |_|   
#     |         |         |     
#     |  -x,+y  |  +x,+y  |     
#     |         |         |     
#     |         |         |     
# -1,1 --------   ------- 1,1   

# TODO angles

