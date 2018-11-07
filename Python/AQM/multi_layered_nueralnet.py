# Library with routines useful to clean the dataset

import pandas as pd
import numpy as np
import random

# Clean dataset: assuming last column represents the class. 
# Input is a dataframe, output is two dataframes, the x representing
# the independent variable, and the y representing the classes
def cleanData(df):
	# x is the inputs: drop last column of dataset, which is the class
	n_inputs = df.shape[1] - 1
	x = df.copy()
	x = x.drop(df.columns[n_inputs],axis=1)
	# Save last column of dataset which is the class
	y = df[[n_inputs]]
	y.columns = ['class']
	# From the class columns create an output dataframe of n columns,
	# where n is also the number of classes
	y = setClasses(y)
	# Rescale input data to be between 0 and 1
	x = rescale(x)
	# Shuffle data randomly
	x,y = shuffle(x,y)
	
	return x,y

# Rewrite y if there's more than 1 class, each row becomes a vector of
# n_classes elements
# input = dataframe, output = dataframe
def setClasses(y):
	# Find number of classes (number of unique values in the y dataframe)
	class_name = y['class'].unique()
	n_classes = len(y['class'].unique())
	print "Number of classes: " + str(n_classes)
	# If less than 2 classes, nothing to be done
	if (n_classes < 2):
		return y
	# If more than 1 class, then fix the data
	n_samples = y.shape[0]
	columns = []
	values = []
	for i in range(n_classes):
		columns.append(class_name[i])
		values.append(0.0)
	c = pd.DataFrame(columns = columns)
	for j in range(n_samples):
		c.loc[c.shape[0]] = values
		for i in range(n_classes):
			if (class_name[i] == y.loc[j].values):
				c[columns[i]].loc[j] = 1.0
			continue
	return c

# Rescale data from 0 to 1
# input = dataframe, output = dataframe
def rescale(df):
	nr, nc = df.shape
	cmax = df.max(axis = 0)
	cmin = df.min(axis = 0)
	df2 = pd.DataFrame(columns = df.columns.values)
	for i in range(df.shape[0]):
		rr = df.loc[i].values
		rr = (rr - cmin) / (cmax - cmin) 
		df2.loc[df2.shape[0]] = rr
		
	return df2

# Shuffle randomly the entries of the dataframes in input
def shuffle(x,y):
	# create random index
	rindex =  np.array(random.sample(xrange(x.shape[0]), x.shape[0]))
	x = x.ix[rindex].reset_index(drop = True)
	y = y.ix[rindex].reset_index(drop = True)
	return x,y

# Split the dataset in train and cross validation samples
def getTrainAndCVsets(x,y, p_train = 0.8):
	# Number of train samples
	n_train_samples = int(p_train * y.shape[0])
	# Generate random indices 
	idx_train = np.array(random.sample(xrange(x.shape[0]), n_train_samples))
	# Create train and cross-validation datasets
	xtrain = x.ix[idx_train].reset_index(drop=True)
	xcv = x.drop(idx_train).reset_index(drop=True)
	ytrain = y.ix[idx_train].reset_index(drop=True)
	ycv = y.drop(idx_train).reset_index(drop=True)
	
	return xtrain, xcv, ytrain, ycv
	
#################################################################################	
import numpy as np
import pandas as pd
import random

import lib_mlnnet as libnn
import lib_data as libd


# Read data,
df = pd.read_csv("iris_dataset.csv", sep = ',')
# Clean data
random.seed(2)
x,y = libd.cleanData(df)

# Split data in training and cv sets (80% training and 20% cross-validation)
xtrain, xcv, ytrain, ycv = libd.getTrainAndCVsets(x,y, p_train = 0.8)

# Initialize network
n_inputs = xtrain.shape[1]
n_outputs = ytrain.shape[1]
nnet = libnn.network(n_inputs = n_inputs, n_outputs = n_outputs,
	n_hidden = [2], mom = 0.9, lrate = 0.5)

# Train network
train_error, cv_error = libnn.train(nnet, xtrain, ytrain, xcv, ycv, maxIter = 500, 
	verbose = True, reshuffle = True)
print ("Accuracy: " + str(libnn.getAccuracy(nnet,xcv,ycv)) + " %")

libnn.plotErrorVsEpoch([train_error],[cv_error],logx = True, logy = True)

#98.0677	
####################################################################################
########################################################################################
import numpy as np
import pandas as pd
import time
from collections import OrderedDict
import matplotlib.pyplot as plt
from pylab import *
import lib_data as libd


class network:
	def __init__(self, n_inputs, n_outputs, 
		n_hidden = [2], 
		lrate = 0.5, mom = 0.0):
		
		print ("Initializing network...")
		# Pre-initialize all variables
		self.l_hidden = -1
		self.lrate = lrate
		self.momentum = mom
		self.nodes = OrderedDict()
		self.W = OrderedDict()
		self.W.dim = OrderedDict()
		self.dW = OrderedDict()
		self.dW.prev = OrderedDict()
		self.inputs = OrderedDict()
		self.outputs = OrderedDict()
		self.delta = OrderedDict()
		
		# Initialize network structure
		self.initLayers(n_inputs,n_outputs,n_hidden)
		
		# Initialize weights
		self.initWeights()
		
		# Initialize deltas
		self.initDeltas()
		
		return
	
	def initLayers(self,n_inputs,n_outputs,n_hidden):
		self.l_hidden = len(n_hidden)
		self.nodes = OrderedDict()
		self.nodes['i'] = n_inputs
		for l in range(self.l_hidden):
			self.nodes['h' + str(l)] = n_hidden[l]
		self.nodes['o'] = n_outputs
		
		print (" *** Layers: ")
		print (self.nodes.keys())
		print (" *** Nodes per layer: ")
		print (self.nodes.values())
		
		return
	
	def initWeights(self):
		
		np.random.seed(0)
		
		for l in range(self.l_hidden+1):
			if (l == 0):
				self.W['i->h'+str(l)] = \
					np.random.rand(self.nodes['i']+1,
					self.nodes['h'+str(l)])
				if (l == (self.l_hidden - 1)):
					self.W['h'+str(l) + '->o'] = \
						np.random.rand(self.nodes['h'+str(l)]+1,
						self.nodes['o'])
					continue
				else:
					continue
			if (l == (self.l_hidden) and (l != 0)):
				self.W['h'+str(l-1) + '->o'] = \
					np.random.rand(self.nodes['h'+str(l-1)]+1,
					self.nodes['o'])
				continue
			self.W['h'+str(l-1)+'->h'+str(l)] = \
				np.random.rand(self.nodes['h'+str(l-1)]+1,
					self.nodes['h' + str(l)])
		
		for i in range(len(self.W.values())):
			self.W.dim[self.W.keys()[i]] = self.W[self.W.keys()[i]].shape
		
		self.dW = self.W.copy()
		self.dW.prev = self.dW.copy()
		
		print (" *** Weights matrices initialized: ")
		print (self.W.keys())
		print (" *** Shape of weights matrices: ")
		print (self.W.dim.values())
		
		return
	
	def initDeltas(self):
		
		nlayers = self.l_hidden
		for l in range(0,nlayers+1):
			if (l == 0):
				self.delta['i<-h0'] = np.zeros((self.nodes['h0'],1),dtype=float)
				#print str(l) + '  i<-h0'
			if ((l == 0) and (l == nlayers )):
				self.delta['h0<-o'] = np.zeros((self.nodes['o'],1),dtype=float)
				#print str(l) + '  h0<-o'
			if ((l>0) and (l < nlayers )):
				self.delta['h'+str(l-1)+'<-h'+str(l)] = np.zeros((self.nodes['h'+str(l)],1),dtype=float)
				#print str(l) + '  h'+str(l-1)+'<-h'+str(l)
			if (l == nlayers):
				self.delta['h'+str(l-1)+'<-o'] = np.zeros((self.nodes['o'],1),dtype=float)
				#print str(l) + '  h'+str(l-1)+'<-o'
		
		return

class testNetwork:
	def __init__(self):
		self.nodes = OrderedDict()
		self.W = OrderedDict()
		self.W.dim = OrderedDict()
		self.dW = OrderedDict()
		self.inputs = OrderedDict()
		self.outputs = OrderedDict()
		self.lrate = 0.5
		self.delta = OrderedDict()
		
		self.initLayers(2,2,[2])
		
		self.initWeights()
		
		self.initDeltas()
		return
	
	def initLayers(self,n_inputs,n_outputs,n_hidden):
		self.l_hidden = len(n_hidden)
		self.nodes = OrderedDict()
		self.nodes['i'] = n_inputs
		for l in range(self.l_hidden):
			self.nodes['h' + str(l)] = n_hidden[l]
		self.nodes['o'] = n_outputs
		
		print (" *** Layers: ")
		print (self.nodes.keys())
		print (" *** Nodes per layer: ")
		print (self.nodes.values())
		
		return
	
	def initWeights(self):
		
		for l in range(self.l_hidden+1):
			if (l == 0):
				self.W['i->h'+str(l)] = \
					np.zeros((self.nodes['i']+1,
					self.nodes['h'+str(l)]),dtype=float)
				self.dW['i->h'+str(l)] = \
					np.zeros((self.nodes['i']+1,
					self.nodes['h'+str(l)]),dtype=float)
				if (l == (self.l_hidden - 1)):
					self.W['h'+str(l) + '->o'] = \
						np.zeros((self.nodes['h'+str(l)]+1,
						self.nodes['o']),dtype=float)
					self.dW['h'+str(l) + '->o'] = \
						np.zeros((self.nodes['h'+str(l)]+1,
						self.nodes['o']),dtype=float)
					continue
				else:
					continue
			if (l == (self.l_hidden) and (l != 0)):
				self.W['h'+str(l-1) + '->o'] = \
					np.zeros((self.nodes['h'+str(l-1)]+1,
					self.nodes['o']),dtype=float)
				self.dW['h'+str(l-1) + '->o'] = \
					np.zeros((self.nodes['h'+str(l-1)]+1,
					self.nodes['o']),dtype=float)
				continue
			self.W['h'+str(l-1)+'->h'+str(l)] = \
				np.zeros((self.nodes['h'+str(l-1)]+1,
					self.nodes['h' + str(l)]),dtype=float)
			self.dW['h'+str(l-1)+'->h'+str(l)] = \
				np.zeros((self.nodes['h'+str(l-1)]+1,
					self.nodes['h' + str(l)]),dtype=float)
		
		for i in range(len(self.W.values())):
			self.W.dim[self.W.keys()[i]] = self.W[self.W.keys()[i]].shape
		
		w1 = 0.15; w2 = 0.20; w3 = 0.25; w4 = 0.30; b1 = 0.35
		self.W['i->h0'][0,0] = w1; self.W['i->h0'][1,0] = w2;
		self.W['i->h0'][0,1] = w3; self.W['i->h0'][1,1] = w4;
		self.W['i->h0'][2,0] = b1; self.W['i->h0'][2,1] = b1;
		
		w5 = 0.40; w6 = 0.45; w7 = 0.5; w8 = 0.55; b2 = 0.60
		self.W['h0->o'][0,0] = w5; self.W['h0->o'][1,0] = w6;
		self.W['h0->o'][0,1] = w7; self.W['h0->o'][1,1] = w8;
		self.W['h0->o'][2,0] = b2; self.W['h0->o'][2,1] = b2;
		
		print (" *** Weights matrices initialized: ")
		print (self.W.keys())
		print (" *** Shape of weights matrices: ")
		print (self.W.dim.values())
		
		return
	
	def initDeltas(self):
		
		#nlayers = self.l_hidden
		#self.delta['h0<-o'] = np.zeros((self.nodes['o'],1),dtype=float)
		#for l in range(nlayers,0,-1):
		#	self.delta['i<-h'+str(l-1)] = np.zeros((self.nodes['h0'],1),dtype=float)
		
		nlayers = self.l_hidden
		for l in range(0,nlayers+1):
			if (l == 0):
				self.delta['i<-h0'] = np.zeros((self.nodes['h0'],1),dtype=float)
				#print str(l) + '  i<-h0'
			if ((l == 0) and (l == nlayers )):
				self.delta['h0<-o'] = np.zeros((self.nodes['o'],1),dtype=float)
				#print str(l) + '  h0<-o'
			if ((l>0) and (l < nlayers )):
				self.delta['h'+str(l-1)+'<-h'+str(l)] = np.zeros((self.nodes['h'+str(l)],1),dtype=float)
				#print str(l) + '  h'+str(l-1)+'<-h'+str(l)
			if (l == nlayers):
				self.delta['h'+str(l-1)+'<-o'] = np.zeros((self.nodes['o'],1),dtype=float)
				#print str(l) + '  h'+str(l-1)+'<-o'
		
		return

# Sigmoid activation function
def activate(o):
	return 1.0 / (1.0 + np.exp(-o))

# Derivative of sigmoid function
def d_activate(o):
	return o * (1.0 - o)

# Augment the input vector with the bias unit
def addUnitBias(iv):
	return np.append(iv,1.0)

# Perform linear combination of inputs
def collapse(W,i):
	i = addUnitBias(i)
	return np.dot(i,W)

# Forward propagate input in the network
def forwardProp(net,xx):
	
	net.inputs['i'] = xx
	for l in range(net.l_hidden+1):
		# Forward first layer
		if (l == 0):
			net.inputs['h'+str(l)] = collapse(net.W['i->h'+str(l)],net.inputs['i'])
			net.outputs['h'+str(l)] = activate(net.inputs['h'+str(l)])
		if ((l>0) and (l<net.l_hidden)):
			net.inputs['h'+str(l)] = collapse(net.W['h'+str(l-1)+'->h'+str(l)],net.outputs['h'+str(l-1)])
			net.outputs['h'+str(l)] = activate(net.inputs['h'+str(l)])
		if (l == (net.l_hidden)):
			net.inputs['o'] = collapse(net.W['h'+str(l-1)+'->o'],net.outputs['h'+str(l-1)])
			net.outputs['o'] = activate(net.inputs['o'])
	
	return

# Backward propagation
def backwardProp(net,xx,yy):
	
	# Save previous weights (needed in momentum update)
	net.dW.prev = net.dW.copy()
	
	# Calculate the delta vectors
	nlayers = net.l_hidden
	for l in range(nlayers,-1,-1):
		if (l == nlayers):
			net.delta['h'+str(l-1)+'<-o'] = - (yy - net.outputs['o']) * d_activate(net.outputs['o'])
			#print str(l) + '  h'+str(l-1)+'<-o  ' + str(net.delta['h'+str(l-1)+'<-o'].shape)
		if (l > 0):
			if (l < nlayers-1):
				for i in range(net.nodes['h'+str(l)]):
					net.delta['h'+str(l-1)+'<-h'+str(l)][i] = np.dot(net.W['h'+str(l)+'->h'+str(l+1)][i,:], 
						net.delta['h'+str(l)+'<-h'+str(l+1)]) \
						* d_activate(net.outputs['h'+str(l)][i])
				#print str(l) + '  h'+str(l-1)+'<-h'+str(l) + ' ' + str(net.delta['h'+str(l-1)+'<-h'+str(l)].shape)
			if (l == nlayers - 1):
				for i in range(net.nodes['h'+str(l)]):
					net.delta['h'+str(l-1)+'<-h'+str(l)][i] = np.dot(net.W['h'+str(l)+'->o'][i,:], 
						net.delta['h'+str(l)+'<-o']) \
						* d_activate(net.outputs['h'+str(l)][i])
				#print str(l) + '  h'+str(l-1)+'<-h'+str(l) + ' ' + str(net.delta['h'+str(l-1)+'<-h'+str(l)].shape)
		if (l == 0):
			if (l == nlayers - 1):
				for i in range(net.nodes['h'+str(l)]):
					net.delta['i<-h'+str(l)][i] = np.dot(net.W['h'+str(l)+'->o'][i,:],
						net.delta['h'+str(l)+'<-o']) \
						* d_activate(net.outputs['h'+str(l)][i])
				#print str(l) + '  i<-h'+str(l) + ' ' + str(net.delta['i<-h'+str(l)].shape)
			else:
				for i in range(net.nodes['h'+str(l)]):
					net.delta['i<-h'+str(l)][i] = np.dot(net.W['h'+str(l)+'->h'+str(l+1)][i,:],
						net.delta['h'+str(l)+'<-h'+str(l+1)]) \
						* d_activate(net.outputs['h'+str(l)][i])
				#print str(l) + '  i<-h'+str(l) + ' ' + str(net.delta['i<-h'+str(l)].shape)
	
	# Calculate delta weights
	
	for l in range(nlayers+1):
		if (l == 0):
			biasOut_1 = addUnitBias(net.inputs['i'])
			for i in range(net.W['i->h'+str(l)].shape[0]):
				for j in range(net.W['i->h'+str(l)].shape[1]):
					net.dW['i->h'+str(l)][i,j] = net.delta['i<-h'+str(l)][j] * biasOut_1[i]
		if ((l > 0) and (l < nlayers)):
			biasOut_1 = addUnitBias(net.outputs['h'+str(l-1)])
			for i in range(net.W['h'+str(l-1)+'->h'+str(l)].shape[0]):
				for j in range(net.W['h'+str(l-1)+'->h'+str(l)].shape[1]):
					net.dW['h'+str(l-1)+'->h'+str(l)][i,j] = net.delta['h'+str(l-1)+'<-h'+str(l)][j] * biasOut_1[i]
		if (l == nlayers):
			biasOut_1 = addUnitBias(net.outputs['h'+str(l-1)])
			for i in range(net.W['h'+str(l-1)+'->o'].shape[0]):
				for j in range(net.W['h'+str(l-1)+'->o'].shape[1]):
					net.dW['h'+str(l-1)+'->o'][i,j] = net.delta['h'+str(l-1)+'<-o'][j] * biasOut_1[i]
	
	return

# Update weights of network
def updateWeights(net):
	
	for key in net.W:
		net.W[key] = net.W[key] - net.lrate * net.dW[key] - net.momentum * net.dW.prev[key]
	
	return

# Get total error of the network
def getError(nnet,x,y):
	bias = np.ones((x.shape[0],1),dtype=float) # bias column vector
	in_des_mtx = np.hstack((x,bias)) # input design matrix
	out1 = activate(np.dot(in_des_mtx,nnet.W['i->h0']))
	
	nlayers = nnet.l_hidden
	for key in (nnet.W):
		if key == 'i->h0':
			continue
		layer_des_mtx = np.hstack((out1,bias))
		out1 = activate(np.dot(layer_des_mtx,nnet.W[key]))
	derr = out1 - y
	error = sum(np.sqrt(np.sum(derr.values * derr.values,axis = 1) / (1.0 * y.shape[0])))
	return error

# Calculate accuracy of the network
def getAccuracy(nnet,x,y):
	error = getError(nnet,x,y)
	accuracy = 100.0 * (1.0 - (1.0 * error / (1.0 * x.shape[0])))
	return accuracy

# Numerical gradient check (check for backprop)
def numGradCheck(nnet, eps = 1E-5):
	
	# Generate synthetic data
	xx = [0.3,0.1,0.95,0.35,0.08]
	yy = [1.0,0.0,0.0,0.0]
	
	# Iterate over all layers
	tot = 0.0
	ii = 0
	nlayers = nnet.l_hidden
	for key in nnet.dW:
		for i in range(nnet.W[key].shape[0]):
			for j in range(nnet.W[key].shape[1]):
				forwardProp(nnet,xx)
				backwardProp(nnet,xx,yy)
				dW_bp = nnet.dW[key][i,j]
				# Get upper numerical
				nnet.W[key][i,j] = nnet.W[key][i,j] + eps
				forwardProp(nnet,xx)
				backwardProp(nnet,xx,yy)
				yup = nnet.outputs['o']
				errup = 0.5 * sum((yup - yy) * (yup - yy))
				# Get lower numerical
				nnet.W[key][i,j] = nnet.W[key][i,j] - 2.0 * eps
				forwardProp(nnet,xx)
				backwardProp(nnet,xx,yy)
				ylo = nnet.outputs['o']
				errlo = 0.5 * sum((ylo - yy) * (ylo - yy))
				# Calculate derivative
				dW_num = (errup - errlo) / (2.0 * eps)
				# Calculate mismatch
				delta = abs(dW_num - dW_bp)
				tot = tot + delta
				# Refix the original weight
				nnet.W[key][i,j] = nnet.W[key][i,j] + eps
				ii = ii + 1
	
	return (tot / (1.0 * ii))

# Plot error vs epoch
def plotErrorVsEpoch(train_error_list,cv_error_list, hidden_list = [2], 
	logx = False, logy = False, custom_labels = False):
	
	color = ["blue","black","green","orange","red","magenta","lime","yellow","darkgreen","gray","aqua"]
	n_curves = len(train_error_list)
	#if (len(train_error_list) != len(hidden_list)):
	#	print "Error 2, mismatch in size of input lists!"
	#	exit()
	
	fig = figure()
	sub = fig.add_subplot(111)
	
	for i in range(n_curves):
		if (not custom_labels):
			if (len(hidden_list) > 1):
				label = "Hidden nodes #: " + ",".join(str(x) for x in hidden_list[i])
			else:
				label = "Hidden nodes #: " + str(hidden_list[i])
		else:
			label = str(hidden_list[i])
		sub.plot(np.arange(len(train_error_list[i])),train_error_list[i],lw=3.0,ls='-',color=color[i],label=(label+" (train)"))
		sub.plot(np.arange(len(cv_error_list[i])),cv_error_list[i],lw=2.0,ls='--',color=color[i],label=(label+" (cv)"))
	sub.set_xlabel("Epoch")
	sub.set_ylabel("Error")
	
	if (logx):
		sub.set_xscale("log")
	if (logy):
		sub.set_yscale("log")
	
	# Shrink current axis by 20%
	box = sub.get_position()
	sub.set_position([box.x0, box.y0, box.width * 0.8, box.height])
	
	# Put a legend to the right of the current axis
	sub.legend(loc='center left', bbox_to_anchor=(1, 0.5),fontsize=10)
	
	plt.show(block = False)
	print "Press enter to continue..."
	raw_input()
	return

def train(net, xtrain, ytrain, xcv, ycv, maxIter = 200, verbose = True, reshuffle = False):
	
	# Number of data samples
	nsamples = xtrain.shape[0]
	# Initialize error arrays
	train_error = range(maxIter); cv_error = range(maxIter);
	
	t0 = time.clock()
	# Loop over epochs
	for it in range(maxIter):
		
		# Reshuffle data
		if (reshuffle):
			xtrain, ytrain = libd.shuffle(xtrain,ytrain)
		
		# Loop over data
		for m in range(nsamples):
			xx = xtrain.loc[m].values
			yy = ytrain.loc[m].values
			
			forwardProp(net,xx)
			backwardProp(net,xx,yy)
			updateWeights(net)
		
		# At each epoch calculate training and cv error
		train_error[it] = getError(net,xtrain,ytrain)
		cv_error[it] = getError(net,xcv,ycv)
		
		# Print to shell
		if (verbose):
			print ("Epoch: " + str(it) + " ** Tr-Error: " + str(train_error[it]) + \
				" ** Cv-Error: " + str(cv_error[it]))
	
	print "Time to train the network: " + str(time.clock() - t0) + " s"
	
	return train_error, cv_error
	