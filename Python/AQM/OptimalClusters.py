import numpy as np
import matplotlib.pyplot as plt
import pickle


class OptimalClusters:

	def __init__(self, means, bts, max_K, N, start_index, bootstrap_n):
		self.means = means
		self.bts = bts
		self.max_K = max_K
		self.N = N
		self.start_index = start_index
		self.bootstrap_n = bootstrap_n
		self.find_optimal_k(means, bts, max_K, N, start_index, bootstrap_n)

	@staticmethod
	def find_optimal_k(means, bts, max_K, N, start_index, bootstrap_n):
		"""
		find optimal K using Pseudo-F for bootstrapped KMeans
    Pseudo-F = (sum(within cluster variance) / (K+1)) / (sum(betwen cluster variance) / 2*(N - K + 1))
    This function also plots an 'Elbow plot' for finding the elbow point for the optimal K
    This function is also 'embarrassingly parallelizable' and quite slow if not parallelized:
    The within cluster variance calculation is the slowest step.
    The exercise is left to the reader to implement a parallel version of this function.
		:param bts:
		:param max_K: MAX K used for optimal K sweep
		:param N: number of bootstrap samples (YOU CAN PLAY AROUND WITH THIS)
		:return: None
		"""
		optimal_k = None
		wit_cluster_var = []
		btw_cluster_var = []

		# find the within cluster variance for each bootstrap set for each K
		for i in range(max_K - 1):
			bts_val = []
			# iterate each bootstrap
			for j in range(N):
				temp = []
				# get cluster centroid
				cents = means[i][j].cluster_centers_
				# calculate each sample's distance from its assigned centroid
				labels = means[i][j].predict(bts[j].values[:, start_index:])
				# iterate each item in bootstrap sample
				for l in range(len(bts[j])):
					temp.append(np.linalg.norm(bts[j].values[l, start_index:] - cents[labels[l]]))

				# average the sum of within cluster variance over (N-K)
				val = sum(temp) / (bootstrap_n - (i + 2))
				print("K = " + str(i + 2) + " within cluster var = " + str(val))
				bts_val.append(val)

			wit_cluster_var.append(bts_val)

		# compute the between cluster variances (between the Kmeans centroids)
		for i in range(max_K - 1):
			bts_val = []
			# iterate through each K
			for j in range(N):
				temp = []
				# get cluster centroids
				cents = means[i][j].cluster_centers_
				cents_mean = cents[0]

				# find population vector centroid
				for k in range(len(cents) - 1):
					cents_mean = cents_mean + cents[k + 1]
				cents_mean = cents_mean / float(len(cents))

				# calculate btw cluster centroid variance
				for k in range(len(cents)):
					temp.append(np.linalg.norm(cents[k] - cents_mean))

				# average the sum of btw cluster variance over (K - 1)
				val = sum(temp) / ((i + 2) - 1)
				print("K = " + str(i + 2) + " btw cluster var = " + str(val))
				bts_val.append(val)

			btw_cluster_var.append(bts_val)

		pf = []

		# calcluate F-stat ratio across each bootstrap
		for i in range(len(btw_cluster_var)):
			temp = []
			for j in range(len(btw_cluster_var[i])):
				temp.append(btw_cluster_var[i][j] / wit_cluster_var[i][j])

			pf.append(temp)

		# debug output and save pf to file
		print("Pseudo F:")
		print(pf)
		pickle.dump(pf, open("pf.p", "wb"))

		# pf boxplot
		plt.figure()
		plt.boxplot(pf)

		# scatter plot of points to fit within the boxes
		for n in range(max_K - 1):
			for o in range(len(pf[n])):
				y = pf[n][o]
				x = n + 1
				plt.plot(x, y, 'r.', alpha=0.2)

		ticks = list(np.linspace(2, max_K, max_K - 1))
		plt.xticks(list(np.linspace(1, max_K - 1, max_K - 1)), [str(int(s)) for s in ticks])
		plt.xlabel('Number of clusters')
		plt.ylabel('Pseudo-F statistic')
		plt.show()

		return None
