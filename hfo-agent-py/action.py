import random
from math import acos
from hfo import *

# new Action Enums, they start from index 15, to be sure not to collide with the predefined Actions in hfo.py
GOALKICK = 16 # smart kick in the middle from goalie and furthest goalpost (max -power)

# Action object for every possible hfoaction
# needed to store optional arguments for the actions like playernumber, or coordiantes
#
class Action(object):

    def __init__(self, actionenum = MOVE, arguments = []):
        
        self.actionenum = actionenum   # this can be any possible action 
        self.arguments  = arguments

    def execute(self, env, state = []):

        # print("Player rot: {0}").format(state[2]*180)
        # print("Goal opening angle: {0}").format(toAngleHF(state[8]))

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
        if self.actionenum == GOALKICK:

# opening angles are encoded as (0, 3.1415926) and normalized to (-1, 1)
# to transfrom back: add 1, divide by 2, multiply by 3.1415926
#
            goalOpeningAngle = (state[8] + 1) / 2 * 3.1415926
            print("kicking to {0}").format(state[8])
            env.act(KICK, 100, 0);








# gamestates for the future (TODO)

#  xpos           = state[0] # x-position                     type:Int, range:[-1..1]
#  ypos           = state[1] # y-position                     type:Int, range:[-1..1]
#  orient         = state[2] # orientation
#  ballProximity  = state[3] # how far is the distance to the ball 
#  ballAngle      = state[4] # angle to the ball
#  goalCenterProximity = state[6]
#  goalCenterAngle     = state[7]
#  goalOpeningAngle    = state[8]


