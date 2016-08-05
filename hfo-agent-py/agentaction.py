#!/usr/bin/env python
# encoding: utf-8

import random
from hfo import *
from common import actionToString

def getSubfieldIndex(state):
    '''
    returns the index of the current subfield the player is on (indexing from top-left to bottom-right)

    Segment the field in ~ 16 Fields

    Coordinates: (x,y)

    (Y)

    -1     --------   -------   -------   --------
          |         |         |         |         |
          |         |         |         |         |
          |   a00   |   a01   |   a02   |   a03   |
          |         |         |         |         |
    -0.5  |_______-x,-y_______|_______+x,-y_______|
          |         |         |         |         |
          |         |         |         |         |
          |   a04   |   a05   |   a06   |   a07   |__
          |         |         |         |         |  |
          |         |         |         |         |  |
     0     --------   -------   --------   -------   |
          |         |         |         |         |  |
          |         |         |         |         |__|
          |   a08   |   a09   |   a10   |   a11   |
          |         |         |         |         |
     0.5  |_______-x,+y_______|_______+x,+y_______|
          |         |         |         |         |
          |         |         |         |         |
          |   a12   |   a13   |   a14   |   a15   |
          |         |         |         |         |
          |         |         |         |         |
     1      -------   -------   -------   -------
         -1       -0.5        0        0.5         1  (X)

    automatic out of bounds checking through open intervals:
        ypos < 1 check is not needed becaue every other case is dealt with, even if ypos > 1
        we want to give the "best" approximation of the subfield index

        the same applies to the last 'xpos if'
    '''
  # x cooridante :: Double (-1..1)
    xpos = state[0] 

  # y cooridante :: Double (-1..1)
    ypos = state[1] 

    if (xpos < -0.5):
        if (ypos < -0.5): return 0;
        if (ypos <  0.0): return 4;
        if (ypos <  0.5): return 8;
        return 12;

    if (xpos < 0):
        if (ypos < -0.5): return 1;
        if (ypos <  0):   return 5;
        if (ypos <  0.5): return 9;
        return 13;

    if (xpos < 0.5):
          if (ypos < -0.5): return 2;
          if (ypos <  0.0): return 6;
          if (ypos <  0.5): return 10;
          return 14;

    if (ypos < -0.5): return 3;
    if (ypos <  0.0): return 7;
    if (ypos <  0.5): return 11;
    return 15;


def getAction(state, isOffense, actionJSON):
    ''' getAction returns the action based on their probabilities and in what current state we are
    
        getAction :: State -> Bool -> ActionJSON -> IO Action

        ActionJSON is defined in jsonparser.py
    '''

    # pick from actions based on their probability
    #  
    # chooseFrom :: [(Action, Int)] -> IO Action
    #
    def chooseFrom(actions):
        r = random.randint(0,100)
        for (action, prob) in actions:
            if r <= prob:
                return action
            else:
                r = r - prob
        raise Exception("None of the actions were used - r: " + str(r))

  # ballPossession :: Int (0 or 1) - (False or True)
    ballPossession = state[5]

  # subfieldIndex :: Int (0-15)
    subfieldIndex = getSubfieldIndex(state)

  # print("currently on subfield #{0}\n").format(subfieldIndex)

  # if we are a defense player, we only have 4 possible actions to choose from
    if (not isOffense):
        actionDist = actionJSON[subfieldIndex]["actionDist"]

        actions = [ (Action(actionenum = MOVE),      actionDist[0][1])
                  , (Action(actionenum = INTERCEPT), actionDist[1][1])
                  , (Action(actionenum = CATCH),     actionDist[2][1])
                  , (Action(actionenum = NOOP),      actionDist[3][1])
  #                , (Action(actionenum = MOVE_TO, arguments = actionJSON[4][0]["arguments"]), actionJSON[subfieldIndex]["actionDist"] [4][1])
                  ]
        return chooseFrom(actions)

  # if we are an offense player, we have 4 possible actions + 2 ball actions to choose from
    else:
        actionDist = actionJSON[subfieldIndex]["actionDist"]

        actions = [ (Action(actionenum = MOVE),      actionDist[0][1])
                  , (Action(actionenum = INTERCEPT), actionDist[1][1])
                  , (Action(actionenum = CATCH),     actionDist[2][1])
                  , (Action(actionenum = NOOP),      actionDist[3][1])
#                  , (Action(actionenum = MOVE_TO, arguments = actionJSON[4][0]["arguments"]), actionJSON[subfieldIndex]["actionDist"][4][1])
                  ]

        # the 0-15 index of actionJSON are the normal actions for every subfield, 16-31 are the ballActions for every subfield
        ballActionDist = actionJSON[subfieldIndex + 16]["ballActionDist"]
        ballActions = [ (Action(actionenum = SHOOT),                                                   ballActionDist[0][1])
                      , (Action(actionenum = DRIBBLE),                                                 ballActionDist[1][1])
                      , (Action(actionenum = PASS, arguments = ballActionDist[2][0]["ballArguments"]), ballActionDist[2][1])
                      , (Action(actionenum = PASS, arguments = ballActionDist[3][0]["ballArguments"]), ballActionDist[3][1])
#                      , (Action(actionenum = PASS, arguments = actionJSON[8][0]["ballArguments"]),  actionJSON[8][1])
#                      , (Action(actionenum = PASS, arguments = actionJSON[9][0]["ballArguments"]),  actionJSON[9][1])
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
        if self.actionenum == PASS:      env.act(PASS, self.arguments[0]);
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