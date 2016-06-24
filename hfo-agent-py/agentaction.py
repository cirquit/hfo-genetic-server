#!/usr/bin/env python
# encoding: utf-8

import random
from hfo import *
from common import actionToString

# getAction returns the action based on their probabilities and in what current state we are
#
#   getAction :: State -> Bool -> Dict String Int -> IO Action
def getAction(state, isOffense, actions):

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
    if not isOffense:
        actions = [ (MOVE,      actions[0][1])
                  , (INTERCEPT, actions[1][1])
                  , (CATCH,     actions[2][1])
                  , (NOOP,      actions[3][1])
                  ]
        return chooseFrom(actions)

  # if we are an offense player, we have 4 possible actions + 2 ball actions to choose from
    else:
        actions = [ (MOVE,      actions[0][1])
                  , (INTERCEPT, actions[1][1])
                  , (CATCH,     actions[2][1])
                  , (NOOP,      actions[3][1])
                  ]

        ballActions = [ (SHOOT,   actions[4][1])
                      , (DRIBBLE, actions[5][1])
                      ]

      # if we have the ball, choose from the ball actions
        if ballPossession == 1: return chooseFrom(ballActions)
      # otherwise, choose from the normal actions
        else: return chooseFrom(actions)


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

