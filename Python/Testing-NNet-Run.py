### __run__.py - create and run Neural net on dataset


from src.net import *

### initialize_network(n_inputs, n_hidden, n_outputs)

# example dataset
dataset = [[2.7810836,2.550537003,0],
	[1.465489372,2.362125076,0],
	[3.396561688,4.400293529,0],
	[1.38807019,1.850220317,0],
	[3.06407232,3.005305973,0],
	[7.627531214,2.759262235,1],
	[5.332441248,2.088626775,1],
	[6.922596716,1.77106367,1],
	[8.675418651,-0.242068655,1],
	[7.673756466,3.508563011,1]]


n_inputs = len(dataset[0]) - 1
n_outputs = len(set([row[-1] for row in dataset]))
n_neurons = 2
epochs = 200

### neural net momentum hyperparameter
momentum_rate = 0.9
l_rate = 0.5

network = initialize_network(n_inputs, n_neurons, n_outputs)
train_network(network, dataset, l_rate, momentum_rate, epochs, n_outputs)
for layer in network:
	print(layer)