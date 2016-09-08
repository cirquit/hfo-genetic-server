'''
    example of my lstm-based neural net for a value-function(state -> action) mapping in HFO
    weights are encoded with DCT to lower the chromosome size for the GA (http://people.idsia.ch/~tino/papers/koutnik.gecco10.pdf)
'''

import numpy  as np
import random

from keras.models  import Sequential
from keras.layers  import LSTM, Dense
from keras         import backend     as K
from scipy.fftpack import dct

from math          import acos
from hfo           import *
from action        import *
from common        import *

'''
    we made the configuration of the rnn global so everything is bound in one single module if
    one wishes to make changes
'''

def_i_lstm  = 8  # how big is the feature space
def_o_lstm  = 12
def_o_dense = 5  # we choose from 5 actions

def create_model_from_data(factors, i_lstm = def_i_lstm, o_lstm = def_o_lstm, o_dense = def_o_dense):
    '''
        main generation function that's called in genetic-agent to generate the model
        based on the encoding

        return a keras model
    '''

    model = create_model(i_lstm = i_lstm, o_lstm = o_lstm, o_dense = o_dense)
    model = update_model_from_data(model, factors, i_lstm = i_lstm, o_lstm = o_lstm, o_dense = o_dense)
    return model


def update_model_from_data(model, factors, i_lstm = def_i_lstm, o_lstm = def_o_lstm, o_dense = def_o_dense):
    '''
        main updating function that's called in genetic-agent to update the model with new
        factors when a new player comes into play

        return a keras model
    '''
    weight_size = calculate_weightsize(i_lstm = i_lstm, o_lstm = o_lstm, o_dense = o_dense)
    weights     = create_weights(factors, n = weight_size)
    model       = fill_rnnmodel(model, weights, i_lstm = i_lstm, o_lstm = o_lstm, o_dense = o_dense)

    return model

def get_action(model, state):
    '''
        Uses the model and gamestate to predict the next action
        returns an Action object (action.py)
    '''

    xpos                = state[0]
    ypos                = state[1]
    orient              = state[2]
    ballProximity       = state[3]
    ballAngle           = state[4]
    goalCenterProximity = state[6]
    goalCenterAngle     = state[7]
    goalOpeningAngle    = state[8]

    # creating custom state to test the implementation
    custom_state = [xpos, ypos, orient, ballProximity, ballAngle, goalCenterProximity, goalCenterAngle, goalOpeningAngle]

    # reshaping to the form lstms want to
    reshaped_input = gamestate_to_input(custom_state)

    # get a prediction
    prediction = model.predict(reshaped_input)

    # fix floating point error
    last_prediction = 1 - (prediction[0][0] + prediction[0][1] + prediction[0][2] + prediction[0][3])

    a1 = (Action(MOVE),      prediction[0][0])
    a2 = (Action(INTERCEPT), prediction[0][1])
    a3 = (Action(CATCH),     prediction[0][2])
    a4 = (Action(SHOOT),     prediction[0][3])
    a5 = (Action(DRIBBLE),   last_prediction)

    def choose_from(actions):
        r = random.uniform(0,1)
        for (action, prob) in actions:
            if r <= prob:
                return action
            else:
                r = r - prob
        raise Exception("None of the actions were used - r: " + str(r))

    return choose_from([a1,a2,a3,a4,a5])



def create_model(i_lstm = def_i_lstm, o_lstm = def_o_lstm, o_dense = def_o_dense):
    '''
        Simple model to approximate a value function, using a lstm and dense layer
        Softmax at the end to get a probability (0-1) for given action
    '''
    model = Sequential()
    model.add(LSTM(output_dim = o_lstm, input_dim = i_lstm, input_length = 1))
    model.add(Dense(output_dim = o_dense, activation = "softmax"))

    return model

def concat_list(l):
    '''
        concat from Haskell

        > [item | sublist <- l, item <- sublist]
        > concat_lists([[1], [2,3]])
        [1,2,3]

        implementation from SO, because this was tested as the fastest one
    '''
    return [item for sublist in l for item in sublist]

def fill_rnnmodel(model, weights, i_lstm = def_i_lstm, o_lstm = def_o_lstm, o_dense = def_o_dense):
    '''
        model   = a model that consists of an lstm and dense layer
        weights = a list of weights that the model will be filled
        i_lsmt  = number of inputs for the lstm layer
        o_lstm  = number of outputs for the lstm layer (should be the same as i_dense)
        i_dense = number of inputs for the dense layer (should be the same as o_lstm)
        o_dense = number of outputs for the dense layer

        returns the model with the updated weights

        TODO: This is currently a simple state machine, made for the purpose to be easily
              reworkable and not adjusted for proficient work. There is some boilerplate.
    '''

    assert isinstance(weights, list), "weights should be a list"
    assert isinstance(i_lstm, int),   "i_lstm should be an int"
    assert isinstance(o_lstm, int),   "o_lstm should be an int"
    assert isinstance(o_dense, int),  "o_dense should be an int"

    # assert that the weight size matches the weightlist length
    weight_size = calculate_weightsize(i_lstm = i_lstm, o_lstm = o_lstm, o_dense = o_dense)
    assert weight_size == len(weights), "expected " + str(weight_size) + " weights, got " + str(len(weights))

    # globals
    result_list = []
    # input of the dense layer is the same as the output of the lstm layer
    i_dense     = o_lstm

    def lstm_complete():
        return (len(result_list) >= 12)

    def dense_complete():
        return (len(result_list) == 14)


    # LSTM layer has the following dependencies based on the input/output
    # the keras implementation of LSTM needs three numpy arrays in a list, for every weight matrix (4x)
    #
    # 4 times [arr1, arr2, arr3] concatinated
    # 
    # input_length defines the length of arr1
    # output_length defines the length of arr2 and arr3
    #               defines the length of the inner arrays of arr1 and arr2
    #
    # example: i_lstm = 1, o_lstm = 3
    # 
    # arr1 = [[1,1,1]]
    # arr2 = [[1,1,1], [2,2,2], [3,3,3]]
    # arr3 = [1,1,1]
    # 
    # the resulting accepted list should have the form
    # [arr1, arr2, arr3, arr1, arr2, arr3, arr1, arr2, arr3, arr1, arr2, arr3]
    #

    arr1_length       = i_lstm
    arr1_inner_length = o_lstm
    arr2_length       = o_lstm
    arr2_inner_length = o_lstm
    arr3_length       = o_lstm

    result_list = []

    # we have 3 arrays times 4:
    for i in xrange(12):

        # arr1
        if (i % 3) == 0:

            inner_lists = []
            # inner lists for arr1
            for j in xrange(arr1_length):

                # slice new weights
                cur_list = weights[:arr1_inner_length]
                # append weights
                inner_lists.append(cur_list)
                # remove used weights
                weights  = weights[arr1_inner_length:]

            # form into numpy array and append to the resulting list
            result_list.append(np.array(inner_lists, dtype="float32"))

        # arr2
        elif (i % 3) == 1:

            inner_lists = []
            # inner lists for arr2
            for j in xrange(arr2_length):

                # slice new weights
                cur_list = weights[:arr2_inner_length]
                # append weights
                inner_lists.append(cur_list)
                # remove used weights
                weights  = weights[arr2_inner_length:]

            # form into numpy array and append to the resulting list
            result_list.append(np.array(inner_lists, dtype="float32"))

        # arr3
        else:

            # slice new weights
            cur_list = weights[:arr3_length]
            # remove used weights
            weights  = weights[arr3_length:]
            # form into numpy array and append to the resulting list
            result_list.append(np.array(cur_list, dtype="float32"))


    # Dense layer has the following dependencies based on the input/output
    # the keras implementation of a Dense layer needs two numpy arrays in a list, for the weights and the biases
    #
    # [arr1, arr2]
    #
    # input_length defines the length of arr1
    # output_length defines the length of arr2
    #               defines the length of the inner arrays of arr1
    #
    # example: i_dense = 2, o_dense = 4
    #
    # arr1 = [[1,2,3,4], [1,2,3,4]]
    # arr2 = [1,1,1,1]
    # the resulting accepted list should have the form
    # [arr1, arr2]
    #

    # arr1

    arr1_length       = i_dense
    arr1_inner_length = o_dense
    arr2_length       = o_dense

    # for arr1 and arr2
    for i in range(2):

        # arr1
        if (i % 2) == 0:

            inner_lists = []
            # inner lists for arr1
            for j in xrange(arr1_length):

                # slice new weights
                cur_list = weights[:arr1_inner_length]
                # remove used weights
                weights  = weights[arr1_inner_length:]
                # append weights
                inner_lists.append(cur_list)
            
            # form into numpy array and append to the resulting list
            result_list.append(np.array(inner_lists, dtype="float32"))

        # arr2
        else:

            # slice new weights
            cur_list = weights[:arr2_length]
            # remove used weights
            weights  = weights[arr2_length:]
            # form into numpy array and append to the resulting list
            result_list.append(np.array(cur_list, dtype="float32"))

    assert len(weights) == 0, "After the numpy array is formed, all the weights should've been used, but there are " + str(len(weights)) + " left"

    # set new weights
    model.set_weights(result_list)

    return model


def calculate_weightsize(i_lstm, o_lstm, o_dense):
    '''
        calculates how many weights one should create if we have the following model:

        model = Sequential()
        model.add(LSTM(output_dim = o_lstm, input_dim = i_lstm, input_length = 1))
        model.add(Dense(output_dim = o_dense))
    '''

    # input of the dense layers takes exactly the output of the lstm layer
    i_dense = o_lstm

    # calculate the size of weights
    lstm_layer_size  = 4 * (i_lstm + o_lstm + 1) * o_lstm  # 4 * (i_lstm * o_lstm + o_lstm * o_lstm + o_lstm)
    dense_layer_size = (i_dense + 1) * o_dense             # i_dense * o_dense + o_dense
    weight_size      = lstm_layer_size + dense_layer_size

    return weight_size

def create_weights(factors, n):
    '''
        factors = a list of numbers (nonempty)
        n       = length of the resulting weights (>=1)

        returns a list of floating point values converted with scipy.fftpack.dct
    '''
    return dct(factors, n=n).tolist()


def gamestate_to_input(state):
    '''
        state transformer for the lstm format: [sample, timesteps, features]

        TODO: Maybe we can pool the state information and increase the timesteps...
    '''
    return np.reshape(state, (1, 1, len(state)))