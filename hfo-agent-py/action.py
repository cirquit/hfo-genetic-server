
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

    def execute(self, env, playernumber, state = []):

        # print("Player rot: {0}").format(state[2]*180)
        # print("Goal opening angle: {0}").format(toAngleHF(state[8]))

        ballPossession = True if state[5] == 1 else False

        if    self.actionenum == MOVE:                        print("p{1}: {0}").format("MOVE", playernumber); env.act(MOVE); 
        elif (self.actionenum == INTERCEPT) & ballPossession: print("p{1}: {0}").format("INTERCEPT", playernumber); env.act(INTERCEPT);
        elif  self.actionenum == CATCH:                       print("p{1}: {0}").format("CATCH", playernumber); env.act(CATCH);
        elif  self.actionenum == NOOP:                        print("p{1}: {0}").format("NOOP", playernumber); env.act(NOOP);
        elif (self.actionenum == SHOOT)     & ballPossession: print("p{1}: {0}").format("SHOOT", playernumber); env.act(SHOOT)
        elif  self.actionenum == DRIBBLE:                     print("p{1}: {0}").format("DRIBBLE", playernumber); env.act(DRIBBLE);
        elif (self.actionenum == PASS)      & ballPossession:
            if playernumber == 0: # server thinks i'm number 7
                print("p{1}: {0}").format("PASS to 11", playernumber); env.act(PASS, 11)
            else:                 # server thinks i'm number 11
                print("p{1}: {0}").format("PASS to 7", playernumber); env.act(PASS, 7)

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
            print("PYAgent: Action could not be carried out! NOOP")
            env.act(NOOP) 

