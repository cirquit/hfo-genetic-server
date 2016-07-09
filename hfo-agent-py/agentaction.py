#!/usr/bin/env python
# encoding: utf-8

import random
from hfo import *
from common import actionToString

# getAction returns the action based on their probabilities and in what current state we are
#
#   getAction :: State -> Bool -> [(String, [(Dict ActionInfo (Either String Int), Int)]] -> IO Action
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
        actions = [ (Action(actionenum = MOVE),                                                actionsDict[0][1])
                  , (Action(actionenum = INTERCEPT),                                           actionsDict[1][1])
                  , (Action(actionenum = CATCH),                                               actionsDict[2][1])
                  , (Action(actionenum = NOOP),                                                actionsDict[3][1])
                  , (Action(actionenum = MOVE_TO, arguments = actionsDict[4][0]["arguments"]), actionsDict[4][1])
                  ]
        return chooseFrom(actions)

  # if we are an offense player, we have 4 possible actions + 2 ball actions to choose from
    else:
        actions = [ (Action(actionenum = MOVE),      actionsDict[0][1])
                  , (Action(actionenum = INTERCEPT), actionsDict[1][1])
                  , (Action(actionenum = CATCH),     actionsDict[2][1])
                  , (Action(actionenum = NOOP),      actionsDict[3][1])
                  , (Action(actionenum = MOVE_TO, arguments = actionsDict[4][0]["arguments"]), actionsDict[4][1])
                  ]

        ballActions = [ (Action(actionenum = SHOOT),                                                 actionsDict[5][1])
                      , (Action(actionenum = DRIBBLE),                                               actionsDict[6][1])
                      , (Action(actionenum = PASS, arguments = actionsDict[7][0]["ballArguments"]),  actionsDict[7][1])
                      , (Action(actionenum = PASS, arguments = actionsDict[8][0]["ballArguments"]),  actionsDict[8][1])
#                      , (Action(actionenum = PASS, arguments = actionsDict[8][0]["ballArguments"]),  actionsDict[8][1])
#                      , (Action(actionenum = PASS, arguments = actionsDict[9][0]["ballArguments"]),  actionsDict[9][1])
                      ]

      # if we have the ball, choose from the ball actions
        if ballPossession == 1: return chooseFrom(ballActions)
      # otherwise, choose from the normal actions
        else: return chooseFrom(actions)


# Action object for every possible hfoaction
# needed to store optional arguments for the actions like playernumber, or coordiantes
#
class Action(object):

    def __init__(self, actionenum = MOVE, arguments = []):
        
        self.actionenum = actionenum   # this can be any possible action 
        self.arguments  = arguments

    def execute(self, env):

        if self.actionenum == MOVE:      env.act(MOVE);
        if self.actionenum == INTERCEPT: env.act(INTERCEPT);
        if self.actionenum == CATCH:     env.act(CATCH);
        if self.actionenum == NOOP:      env.act(NOOP);
        if self.actionenum == SHOOT:     env.act(SHOOT);
        if self.actionenum == DRIBBLE:   env.act(DRIBBLE);
        if self.actionenum == PASS:      print env.act(PASS, self.arguments[0]);
        if self.actionenum == MOVE_TO:
             x   = self.arguments[0]
             y   = self.arguments[1]
             xBs = self.arguments[2]
             yBs = self.arguments[3]

             if x <= xBs: xTo = random.uniform(x,xBs)
             else:        xTo = random.uniform(xBs,x)

             if y <= yBs: yTo = random.uniform(y,yBs)
             else:        yTo = random.uniform(yBs,y)

             print("moving to x:{0} y:{1}").format(xTo, yTo)
             env.act(MOVE_TO, xTo, yTo);


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

