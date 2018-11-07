/github/Enquist_Mike/Enquist_Mike/AQM Assignments/K-means-Python
%pylab inline
import pandas as pd
matplotlib.style.use('ggplot')
from mpl_toolkits.mplot3d import Axes3D

df = pd.read_csv('/Users/Enquist//github/Enquist_Mike/Enquist_Mike/AQM Assignments/K-means-Python/Oil.Comp.Prices.csv', sep = ',')

df.head()
df.describe()

# Function: K Means
# Based on pseudocode from http://stanford.edu/~cpiech/cs221/handouts/kmeans.html 
# -------------
# K-Means is an algorithm that takes in a dataset and a constant
# k and returns k centroids (which define clusters of data in the
# dataset which are similar to one another).
def kmeans(dataSet, k):
	
    if k <= 0:
        message = "Check your k value. Should be an integer larger than zero."
        return message
    
    # Initialize centroids randomly
    # numFeatures = dataSet.getNumFeatures()
    # centroids = getRandomCentroids(numFeatures, k)
    centroids = getCentroids(dataSet, k=k)
    
    # Initialize book keeping vars.
    iterations = 0
    oldCentroids = zeros((k,len(dataSet[0])))
    
    # Run the main k-means algorithm
    while not shouldStop(oldCentroids, centroids, iterations):
        # Save old centroids for convergence test. Book keeping.
        oldCentroids = centroids
        iterations += 1
        
        # Assign labels to each datapoint based on centroids
        labels = getLabels(dataSet, centroids)
        
        # Assign centroids based on datapoint labels
        centroids = getCentroids(dataSet, labels, k)
        
    # We can get the labels too by calling getLabels(dataSet, centroids)
    print iterations
    return centroids
    
    # Function: Should Stop
# -------------
# Returns True or False if k-means is done. K-means terminates either
# because it has run a maximum number of iterations OR the centroids
# stop changing.
def shouldStop(oldCentroids, centroids, iterations):
    if iterations > MAX_ITERATIONS: return True
    return ((oldCentroids - centroids) == 0).all()

# Function: Get Labels
# -------------
# Returns a label for each piece of data in the dataset. 
def getLabels(dataSet, centroids):
    # For each element in the dataset, choose the closest centroid. 
    # Make that centroid the element's label.
    labels = zeros(len(dataSet))
    distances = np.empty((len(centroids),len(dataSet)))
    distances[:] = np.NAN
    for i in range(0,len(centroids)):
        distances[i] = sqrt(((dataSet - centroids[i])**2).sum(axis=1))
    distances = transpose(distances)
    for i in range(0,len(distances)):
        closest_centroid = min(distances[i])
        interim_label = [k for k, j in enumerate(distances[i]) if j == closest_centroid]
        labels[i] = int(interim_label[0]+1)
    return labels
    
# Function: Get Centroids
# -------------
# Returns k random centroids, each of dimension n.
def getCentroids(dataSet, labels=None, k=1):
    k_centroids_temp = zeros((k,2*len(dataSet[0])))
    k_centroids = zeros((k,len(dataSet[0])))
    
    # Randomly generate k centroids
    # Note: assumes no negative coordinates
    # x,y,z values between 0 and max in dataSet
    if labels == None:
    #if k_centroids_temp[i][len(dataSet[0])] == 0:
        max_data = dataSet.max()
        for i in range(0,len(k_centroids_temp)):
            for k in range(0,len(dataSet[0])):
                k_centroids[i][k] = random.random()*max_data
        return k_centroids

    # Each centroid is the geometric mean of the points that
    # have that centroid's label. Important: If a centroid is empty (no points have
    # that centroid's label) you should randomly re-initialize it.
    for i in range(0,len(dataSet)):
        for j in range(0,len(dataSet[0])):
            k_centroids_temp[labels[i]-1][j] += dataSet[i][j]
            k_centroids_temp[labels[i]-1][len(dataSet[0])+j]+=1

    for i in range(0,len(k_centroids_temp)):
        for j in range(0,len(dataSet[0])):
            k_centroids_temp[i][j] = k_centroids_temp[i][j]/k_centroids_temp[i][j+len(dataSet[0])]
            k_centroids[i][j] = k_centroids_temp[i][j]
        
        if k_centroids_temp[i][len(dataSet[0])] == 0:
            max_data = dataSet.max()
            for k in range(0,len(dataSet[0])):
                k_centroids[i][k] = random.random()*max_data
                
    return k_centroids
    
data = ones((499,3))
print len(data[0])

print data

k=5
k_centroids = zeros((k,2*len(data[0])))
print k_centroids

labels1 = np.random.randint(1,5, size=499)
sorted(labels1)


print data

data = np.random.randint(1,5,size=(499, 3))

getCentroids(data, labels=labels1, k=5)

testcentroids = getCentroids(data, labels=labels1, k=5)
print data
print testcentroids

distances1 = np.empty((len(testcentroids),len(data)))
distances1[:] = np.NAN
for i in range(0,len(testcentroids)):
    distances1[i] = sqrt(((data - testcentroids[i])**2).sum(axis=1))
print distances1


testlabels = getLabels(data, testcentroids)
print testlabels

distances2 = transpose(distances1)
print distances2
[i for i, j in enumerate(distances2[2]) if j == 0.0]

MAX_ITERATIONS = 25
kmeantest = kmeans(data,k=4)
print data
print kmeantest

labelstest = getLabels(data, kmeantest)
print labelstest

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
colours = ['r','b','k','g','c']

for i in range(0,len(data)):
    ax.scatter(data[i][0],data[i][1],data[i][2], c=colours[int(labelstest[i]-1)],marker='o')
for i in range(0,len(kmeantest)):
    ax.scatter(kmeantest[i][0],kmeantest[i][1],kmeantest[i][2], c=colours[i],marker='+')

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
plt.show

tetra = empty((3,len(df)))
tetra[0] = df['ECA.TO.csv']
tetra[1] = df['SU.TO.csv']
tetra[2] = df['HSE.TO.csv']
tetra = transpose(tetra)
print tetra

MAX_ITERATIONS = 25
tetratest = kmeans(tetra,k=4)
#print tetra
print tetratest
labelstetra = getLabels(tetra, tetratest)
print labelstetra

fig = plt.figure()
ax1 = fig.add_subplot(111, projection='3d')
colours = ['r','b','k','g','c']

for i in range(0,len(tetra)):
    ax1.scatter(tetra[i][0],tetra[i][1],tetra[i][2], c=colours[int(labelstetra[i]-1)],marker='o')
for i in range(0,len(tetratest)):
    ax1.scatter(tetratest[i][0],tetratest[i][1],tetratest[i][2], c=colours[i],marker='+')

ax1.set_xlabel('X')
ax1.set_ylabel('Y')
ax1.set_zlabel('Z')

plt.show

#Encana is red, Suncor is blue, Husky is black. Encana and Suncor are cluster closely together
# For a more objective measure, I should try to experiment by increasing values of k and graph various metrics
#(indices) of the quality of the resulting clustering’s 


