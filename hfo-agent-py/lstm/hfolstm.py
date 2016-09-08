'''
example of my lstm-based nn for value-function(state -> action) mapping in HFO
encoded with DCT, decoded with IDCT
'''

import numpy as np


from keras.models  import Sequential
from keras.layers  import LSTM, Dense
from keras         import backend     as K
from scipy.fftpack import dct

def create_model():
    '''
        Simple model to approximate a value function

        state-vector consists of:
            * current x-pos
            * current y-pos
            * ball x-pos
            * ball y-pos
            * goal opening angle (between goalie and goalpost)

        action-vector:
            * Move
            * Shoot
            * Dribble
            * NoOp

        lstm layer has 864 weights
        dense layer has 52 weights
        softmax at the end to get a probability (0-1) for given action
    '''
    model = Sequential()
    model.add(LSTM(output_dim = 12, input_dim = 5, input_length = 1))
#   model.add(LSTM(output_dim = 3, input_dim = 1, input_length = 1))

#    model.add(Dense(output_dim = 4, activation = "softmax"))
#    model.add(LSTM(output_dim = 3, input_dim = 1, input_length = 2))
    model.add(Dense(output_dim = 4, activation = "softmax"))

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

def fill_rnnmodel(model, weights, i_lstm, o_lstm, o_dense):
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
            for j in xrange(arr1_legth):

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
    model.set_weights(np.array(result_list, dtype="float32"))

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