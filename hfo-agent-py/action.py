import random
from math import acos
from hfo import *

# new Action Enums, they start from index 15, to be sure not to collide with the predefined Actions in hfo.py
GOALKICK = 16 # smart kick in the middle from goalie and furthest goalpost (max -power)


class Action(object):
    '''
        Action object for every possible hfoaction
        needed to store optional arguments for the actions like playernumber, or coordiantes
    '''

    def __init__(self, actionenum = MOVE, arguments = []):
        
        self.actionenum = actionenum   # this can be any possible action 
        self.arguments  = arguments

    def execute(self, env, state = []):

        # print("Player rot: {0}").format(state[2]*180)
        # print("Goal opening angle: {0}").format(toAngleHF(state[8]))

        ballPossession = True if state[5] == 1 else False

        if    self.actionenum == MOVE:                        print("Chose: {0}").format("MOVE"); env.act(MOVE); 
        elif (self.actionenum == INTERCEPT) & ballPossession: print("Chose: {0}").format("INTERCEPT"); env.act(INTERCEPT);
        elif  self.actionenum == CATCH:                       print("Chose: {0}").format("CATCH"); env.act(CATCH);
        elif  self.actionenum == NOOP:                        print("Chose: {0}").format("NOOP"); env.act(NOOP);
        elif (self.actionenum == SHOOT)     & ballPossession: print("Chose: {0}").format("SHOOT"); env.act(SHOOT)
        elif  self.actionenum == DRIBBLE:                     print("Chose: {0}").format("DRIBBLE"); env.act(DRIBBLE);
        elif (self.actionenum == PASS)      & ballPossession: print("Chose: {0}").format("PASS"); env.act(PASS, self.arguments[0])
        elif  self.actionenum == MOVE_TO:
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
        elif self.actionenum == GOALKICK:

# opening angles are encoded as (0, 3.1415926) and normalized to (-1, 1)
# to transfrom back: add 1, divide by 2, multiply by 3.1415926
#
            goalOpeningAngle = (state[8] + 1) / 2 * 3.1415926
            print("kicking to {0}").format(state[8])
            env.act(KICK, 100, 0);

        
        #    If nothing else works, don't do anything
        else:
            env.act(NOOP) 