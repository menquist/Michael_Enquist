#'/github/Enquist_Mike/Enquist_Mike/AQM Assignments/NNET-Python'

#df = pd.read_csv('/Users/Enquist//github/Enquist_Mike/Enquist_Mike/AQM Assignments/NNET-Python/seeds_dataset.csv',header=None).dropna()

#df.head()
#df.describe()


retval = os.getcwd()

os.chdir( "github/Enquist_Mike/Enquist_Mike/AQM Assignments/" )

retval = os.getcwd()

### I am using reference from "How to Implement the Backpropagation Algorithm From Scratch In Python"
# By Jason Brownlee


from random import seed
from random import randrange
from random import random
from csv import reader
from math import exp

#Importing and reading a CSV file

def Importing_csv(filename):
	dataset = list()
	with open(filename, 'r') as file:
		csv_reader = reader(file)
		for row in csv_reader:
			if not row:
				continue
			dataset.append(row)
	return dataset

# Convert string column to float
def string_to_float(dataset, column):
    for row in dataset:
        row[column] = float(row[column].strip())
 
# Convert string column to integer
def column_to_integer(dataset, column):
    convert = [row[column] for row in dataset]
    unique = set(convert)
    Review = dict()
    for i, value in enumerate(unique):
        Review[value] = i
    for row in dataset:
        row[column] = Review[row[column]]
    return Review
 
# Find the min and max values for each column
def max_min_values(dataset):
    minimum_maximum = list()
    arrange = [[min(column), max(column)] for column in zip(*dataset)]
    return arrange
 
# Rescale dataset columns to the range 0-1
def Lag_Dataset(dataset, minimum_maximum):
    for row in dataset:
        for i in range(len(row)-1):
            row[i] = (row[i] - minimum_maximum[i][0]) / (minimum_maximum[i][1] - minimum_maximum[i][0])
 
# Split a dataset into k folds
def K_Fold_Split(dataset, n_number_of_folds):
    split_data = list()
    copy_data = list(dataset)
    fold_size = int(len(dataset) / n_number_of_folds)
    for i in range(n_number_of_folds):
        fold = list()
        while len(fold) < fold_size:
            index = randrange(len(copy_data))
            fold.append(copy_data.pop(index))
        split_data.append(fold)
    return split_data

# Initialize a network
def initialize_network(n_inputs, n_hidden, n_outputs):
	network = list()
	hidden_layer = [{'weights':[random() for i in range(n_inputs + 1)]} for i in range(n_hidden)]
	network.append(hidden_layer)
	output_layer = [{'weights':[random() for i in range(n_hidden + 1)]} for i in range(n_outputs)]
	network.append(output_layer)
	return network

# Calculate neuron activation for an input
def Input_Weights(weights, inputs):
    Calculate = weights[-1]
    for i in range(len(weights)-1):
        Calculate += weights[i] * inputs[i]
    return Calculate

# Transfer neuron activation
def transfer(Calculate):
    return 1.0/(1.0+exp(-Calculate))

# Forward propagate input to a network output
def forward_propagation(network, row):
    inputs = row
    for layer in network:
        new_inputs = []
        for neuron in layer:
            Calculate = Input_Weights(neuron['weights'], inputs)
            neuron['output'] = transfer(Calculate)
            new_inputs.append(neuron['output'])
        inputs=new_inputs
    return inputs

# Calculate the derivative of a neuron output
def transfer_derivative(output):
    return output*(1.0-output)

# Backpropagate error and store in neurons
def backward_propagate_error(network, expected):
    for i in reversed(range(len(network))):
        layer = network[i]
        errors = list()
        if i != len(network)-1:
            for j in range(len(layer)):
                error = 0.0
                for neuron in network[i + 1]:
                    error += (neuron['weights'][j] * neuron['delta'])
                errors.append(error)
        else:
            for j in range(len(layer)):
                neuron = layer[j]
                errors.append(expected[j] - neuron['output'])
        for j in range(len(layer)):
            neuron = layer[j]
            neuron['delta'] = errors[j] * transfer_derivative(neuron['output'])



# Update network weights with error
def update_weights(network, row, l_rate):
    for i in range(len(network)):
        inputs = row[:-1]
        if i != 0:
            inputs = [neuron['output'] for neuron in network[i-1]]
        for neuron in network[i]:
            for j in range(len(inputs)):
                neuron['weights'][j] += l_rate * neuron ['delta'] * inputs[j]
            neuron['weights'][-1] += l_rate*neuron['delta']
            
# Train a network for a fixed number of epochs
def train_network(network, train, l_rate, n_epoch, n_output_layers):
    for epoch in range(n_epoch):
        sum_error = 0
        for row in train:
            outputs = forward_propagation(network, row)
            expected = [0 for i in range(n_output_layers)]
            expected[row[-1]] = 1
            sum_error += sum([(expected[i]-outputs[i])**2 for i in range(len(expected))])
            backward_propagate_error(network, expected)
            update_weights(network, row, l_rate)
        #print('>epoch=%d, lrate=%.3f, error=%.3f' % (epoch, l_rate, sum_error))
        
# Make a prediction with a network
def predict(network, row):
    outputs = forward_propagation(network,row)
    return outputs.index(max(outputs))
 
# Calculate accuracy percentage
def accuracy_metric(actual, predicted):
    correct = 0
    for i in range(len(actual)):
        if actual[i] == predicted[i]:
            correct += 1
    return correct / float(len(actual)) * 100.0

# Backpropagation Algorithm With Stochastic Gradient Descent
def back_propagation(train, test, l_rate, n_epoch, n_hidden):
	n_inputs = len(train[0]) - 1
	n_outputs = len(set([row[-1] for row in train]))
	network = initialize_network(n_inputs, n_hidden, n_outputs)
	train_network(network, train, l_rate, n_epoch, n_outputs)
	predictions = list()
	for row in test:
		prediction = predict(network, row)
		predictions.append(prediction)
	return(predictions)


# Evaluate an algorithm using a cross validation split
def evaluate_algorithm(dataset, algorithm, n_number_of_folds, *args):
    folds = K_Fold_Split(dataset, n_number_of_folds)
    scores = list()
    for fold in folds:
        train_set = list(folds)
        train_set.remove(fold)
        train_set = sum(train_set, [])
        test_set = list()
        for row in fold:
            row_copy = list(row)
            test_set.append(row_copy)
            row_copy[-1] = None
        predicted = algorithm(train_set, test_set, *args)
        actual = [row[-1] for row in fold]
        accuracy = accuracy_metric(actual, predicted)
        scores.append(accuracy)
    return scores

# Test Backprop on Seeds dataset
seed(1)
# load and prepare data
filename = 'seeds_dataset.csv'
dataset = Importing_csv(filename)

for i in range(len(dataset[0])-1):
    string_to_float(dataset, i)
    
    
    
# convert class column to integers
column_to_integer(dataset, len(dataset[0])-1)

# normalize input variables
minmax = max_min_values(dataset)
Lag_Dataset(dataset, minmax)
# evaluate algorithm
n_number_of_folds = 5
l_rate = 0.3
n_epoch = 500
n_hidden_layers = 5
scores = evaluate_algorithm(dataset, back_propagation, n_number_of_folds, l_rate, n_epoch, n_hidden_layers)
print('Scores: %s' % scores)
print('Mean Accuracy: %.3f%%' % (sum(scores)/float(len(scores))))




