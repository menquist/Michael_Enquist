import pickle
from sklearn.cluster import KMeans
from sklearn.mixture import GaussianMixture
from sklearn.cluster import AgglomerativeClustering

class RunAlgos:

	def __init__(self, k_opt, bootsrap_n, start_index, bts, k_means_bts):
		self.k_opt = k_opt
		self.bootsrap_n = bootsrap_n
		self.start_index = start_index
		self.bts = bts
		self.k_means_bts = k_means_bts

	def run_KMeans(self):
		# start = MAX_K
		print("running KMeans")
		# KMeans clustering
		K_means = []
		K_means_bts = []

		for i in range(self.k_opt-2, self.k_opt - 1):

			temp = []
			temp2 = []
			for j in range(self.bootsrap_n):
				temp.append(KMeans(n_clusters=i + 2, init='k-means++', n_jobs=1, n_init=10, max_iter=400))
				temp2.append(temp[j].fit_predict(self.bts[j].values[:, self.start_index:]))
				print("K = " + str(i + 2) + " N = " + str(j))

			K_means.append(temp)
			K_means_bts.append(temp2)

			pickle.dump(K_means, open("KMeans.p", "wb"))
			return temp2

	def run_GMM(self):
		# start = MAX_K
		print("running GMM")
		# GMM clustering
		GMM = []
		GMM_bts = []

		for i in range(self.k_opt-2, self.k_opt - 1):
			temp = []
			temp2 = []
			for j in range(self.bootsrap_n):
				temp.append(GaussianMixture(n_components=i + 2))
				temp[j].fit(self.bts[j].values[:, self.start_index:])
				temp2.append(temp[j].predict(self.bts[j].values[:, self.start_index:]))
				print("K = " + str(i + 2) + " N = " + str(j))

			GMM.append(temp)
			GMM_bts.append(temp2)

			pickle.dump(GMM, open("GMM.p", "wb"))
			return temp2

	def run_Agglomerative(self):
		wards = []
		wards_bts = []

		for i in range(self.k_opt-2, self.k_opt - 1):
			temp = []
			temp2 = []
			for j in range(self.bootsrap_n):
				temp.append(AgglomerativeClustering(n_clusters=i + 2))
				temp2.append(temp[j].fit_predict(self.bts[j].values[:, self.start_index:]))
				print("K = " + str(i + 2) + " N = " + str(j))

			wards.append(temp)
			wards_bts.append(temp2)

			pickle.dump(wards, open("wards.p", "wb"))
			return temp2
