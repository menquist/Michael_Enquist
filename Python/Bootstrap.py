import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from Optimalclusters import Optimalclusters


class Bootstrap:

	def __init__(self, df, size, N):
		self.df = df
		self.size = size
		self.N = N

	def get_bootstraps(self):
		"""
		Get bootstrap samples in df format, size = size of
		each bootstrap sample, N = number of bootstrap samples
		"""
		bootstrapped_dfs = []
		for i in range(self.N):
			# randomly select rows from original DF until size is met
			temp = pd.DataFrame()
			for j in range(self.size):
				# uniformly sample with replacement from dataset (there will be duplicate rows)
				rand_index = np.random.randint(0, len(self.df))
				# append row to new bootstrap sample dataframe
				temp = temp.append(self.df.iloc[rand_index, :])[self.df.columns.tolist()]

			# this list is a collection of the bootstrap sample dfs
			bootstrapped_dfs.append(temp)

		return bootstrapped_dfs

	@staticmethod
	def kmeans_bootstrap(bts, ksweep, bootstrap_n, start_index, k_opt, max_k):
		"""
		In the final implementation, K_means_bts contains the labels for all bootstraps for the optimal K
		for all methods
		:return:
		"""
		K_means = []
		K_means_bts = []
		print("running K means")

		if ksweep is True:
			### Set K = 8 and do the sweep.
			#max_k = 8
			start = 0

			for i in range(start, max_k - 1):

				temp = []
				temp2 = []
				for j in range(bootstrap_n):
					temp.append(KMeans(n_clusters=i + 2, init='k-means++', n_jobs=1, n_init=10, max_iter=400))
					temp2.append(temp[j].fit_predict(bts[j].values[:,start_index:]))
					print("K = " + str(i + 2) + " N = " + str(j))

				K_means.append(temp)
				K_means_bts.append(temp2)

			print("running optimal K")
			OptimalClusters(K_means, bts, max_K=max_k, N=bootstrap_n, start_index=start_index, bootstrap_n=bootstrap_n)

			return(K_means_bts)

		else:
			max_k = k_opt
			start = k_opt - 2

			for i in range(start, max_k - 1):

				temp = []
				temp2 = []
				for j in range(bootstrap_n):
					temp.append(KMeans(n_clusters=i + 2, init='k-means++', n_jobs=1, n_init=10, max_iter=400))
					temp2.append(temp[j].fit_predict(bts[j].values[:,start_index:]))
					print("K = " + str(i + 2) + " N = " + str(j))

				K_means.append(temp)
				K_means_bts.append(temp2)

			return K_means_bts


