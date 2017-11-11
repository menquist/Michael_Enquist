import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

# import data
faithful = pd.read_csv('faithful.csv')
faithful.columns = ['eruptions', 'waiting']

# view data
#plt.scatter(faithful.eruptions, faithful.waiting)
#plt.title('Old Faithful Data Scatterplot')
#plt.xlabel('Length of eruption (minutes)')
#plt.ylabel('Time between eruptions (minutes)')

faith = np.array(faithful)

def normalizeData(x):
    x_norm = np.zeros((np.shape(x)[0], np.shape(x)[1]))
    for i in range(x.shape[1]):
        x_norm[:,i] = (x[:,i] - x[:,i].min()) / (x[:,i].max() - x[:,i].min())
    return x_norm

def initializeClusters(K):
    mu_init = np.random.rand(K,2)
    return mu_init

def expectationStep(x, mu):
    # return array of closest centroids
    distances = np.sqrt(((x - mu[:, np.newaxis])**2).sum(axis=2))
    return np.argmin(distances, axis = 0)


def maximizationStep(x, r, K):
    # return optimal centroid locations
    return np.array([x[r == k].mean(axis=0) for k in range(K.shape[0])])

def kMeans(x, K):
    x_norm = normalizeData(x)
    m = initializeClusters(K)

    change = 1
    while change > 0.00001:
        r = expectationStep(x_norm, m)
        m1 = maximizationStep(x_norm, r, m)
        J0 = ((x_norm - m1[:, np.newaxis])**2).sum(axis=2).sum()
        J1 = ((x_norm - m[:, np.newaxis])**2).sum(axis=2).sum()
        change = np.abs(J1 - J0)
        m = m1
        print(change)
    return {'r':r, 'm':m }


runKmeans = kMeans(faith, 2)
data = normalizeData(faith)

plt.scatter(data[:,0], data[:,1], c = runKmeans['r'], cmap='viridis_r')
plt.scatter(runKmeans['m'][:, 0], runKmeans['m'][:, 1], c='r', s=100)
plt.show()