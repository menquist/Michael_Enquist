### net.py - File containing all the Single-Hidden Layer MLP code
### code adapted from http://machinelearningmastery.com/implement-backpropagation-algorithm-scratch-python/.

### The neural net is created using the simple 'array/vector' programmng style
### an object oriented network would be more general but harder to implement.

from random import random
from random import seed
from datetime import datetime
import numpy as np





### initialize_network() initializes the neural network with small random weights in [-0.5, 0.5]
def initialize_network(n_inputs, n_hidden, n_outputs):
    initialize_network.prevDeltaWeights = [0] * ( n_inputs * n_hidden + n_hidden + n_outputs * n_hidden + n_outputs)
    #seed(datetime.now())
    seed(1)
    network = list() # network is initially an empty list
    hidden_layer = [{'weights': [(random() - 0.5) for _ in range(n_inputs + 1)]} for _ in range(n_hidden)]
    network.append(hidden_layer)
    output_layer = [{'weights': [(random() - 0.5)  for _ in range(n_hidden + 1)]} for _ in range(n_outputs)]
    network.append(output_layer)
    return network

### calculate activation
def activate(weights, inputs):
    activation = weights[-1]
    for i in range(len(weights)-1):
        activation += weights[i] * inputs[i]
        return activation

### Transfer neuron activation
def transfer(activation):
	return 1.0 / (1.0 + np.exp(-activation))

### Forward propagate input to a network output
def forward_propagate(network, row):
	inputs = row
	for layer in network: # outer loop to go through each layer
		new_inputs = []
		for neuron in layer: # inner loop to go through each neuron per layer
			activation = activate(neuron['weights'], inputs)
			neuron['output'] = transfer(activation)
			new_inputs.append(neuron['output'])
		inputs = new_inputs
	return inputs

### Calculate the derivative of an neuron output
def transfer_derivative(output):
	return output * (1.0 - output)

### Backpropagate error and store in neurons.
### here we really see the power of Python in allowing us to manipulate
### complicated data structures with ease
def backward_propagate_error(network, expected):
	for i in reversed(range(len(network))): # traverse the network from output layer first
		layer = network[i]
		errors = list()
		if i != len(network)-1: # hidden layer error computation
			for j in range(len(layer)):
				error = 0.0
				for neuron in network[i + 1]:
					error += (neuron['weights'][j] * neuron['delta'])
				errors.append(error)
		else: # output layer error computation
			for j in range(len(layer)):
				neuron = layer[j]
				errors.append(expected[j] - neuron['output'])
		for j in range(len(layer)): # compute deltas for output layer
			neuron = layer[j]
			neuron['delta'] = errors[j] * transfer_derivative(neuron['output'])


### apply weight update
def update_weights(network, row, l_rate, m_rate, first):


	k = 0;
	for i in range(len(network)):
		inputs = row[:-1]
		if i != 0:
			inputs = [neuron['output'] for neuron in network[i - 1]]
		for neuron in network[i]:
			for j in range(len(inputs)):

				deltaW = l_rate * neuron['delta'] * inputs[j]
				+ m_rate * initialize_network.prevDeltaWeights[k]

				initialize_network.prevDeltaWeights[k] = deltaW

				neuron['weights'][j] += deltaW
				k += 1

			deltaW = l_rate * neuron['delta'] + m_rate * initialize_network.prevDeltaWeights[k]
			initialize_network.prevDeltaWeights[k] = deltaW
			neuron['weights'][-1] += deltaW
			k += 1


### Train a network for a fixed number of epochs
def train_network(network, train, l_rate, m_rate, n_epoch, n_outputs):
	for epoch in range(n_epoch):
		sum_error = 0
		for row in train:
			outputs = forward_propagate(network, row)
			expected = [0 for i in range(n_outputs)]
			expected[row[-1]] = 1
			sum_error += sum([(expected[i]-outputs[i])**2 for i in range(len(expected))])
			backward_propagate_error(network, expected)
			update_weights(network, row, l_rate,m_rate, epoch)
		print('>epoch=%d, lrate=%.3f, error=%.3f' % (epoch, l_rate, sum_error))