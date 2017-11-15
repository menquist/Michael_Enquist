import pickle
import pandas as pd
from collections import Counter
import matplotlib.pyplot as plt
from HungarianAlgo import *
import seaborn as sns

class Consensus:

	def __init__(self, alg_out1, alg_out2, alg_out3, bts, data, start_index, opt_k, labels):
		self.alg_out1 = alg_out1
		self.alg_out2 = alg_out2
		self.alg_out3 = alg_out3
		self.bts = bts
		self.data = data
		self.start_index = start_index
		self.opt_k = opt_k
		self.labels = labels

	def match_labels(self):
		K_means_bts = self.append_all(self.alg_out1, self.alg_out2, self.alg_out3)
		ref_label = K_means_bts[0][0]

		final_labels = []
		final_labels_cm = []

		for i in range(len(K_means_bts)):
			for j in range(len(K_means_bts[i])):
				update_labels = Hungarian(K_means_bts[0][j], ref_label).hungarian()
				print(update_labels['matched_labels'])
				final_labels.append(update_labels['matched_labels'])
				print(update_labels['new_cm'])
				final_labels_cm.append(update_labels['new_cm'])

	def combine_results(self):
		K_means_bts = self.append_all(self.alg_out1, self.alg_out2, self.alg_out3)
		labels_with_indices = []

		# append the named index to each final label
		for i in range(len(K_means_bts[0])):
			indices = self.bts[i].values[:, 0]
			temp = []
			for j in range(len(K_means_bts)):

				temp.append(
					pd.DataFrame(indices).join(pd.DataFrame(K_means_bts[j][i]), lsuffix='name', rsuffix='class', how='inner'))

			labels_with_indices.append(temp)

		# iterate through the dfs and find the class labels for each row in the original df

		original_df_labels = []
		str_list = self.data.values[:, 0]
		for i in range(len(self.data)):
			# iterate across methods (3)
			original_df_labels.append([str_list[i]])
			for j in range(len(K_means_bts)):
				temp2 = []
				# iterate across the bootstraps
				for k in range(len(K_means_bts[0])):
					foo = labels_with_indices[k][j][labels_with_indices[k][j]['0name'].str.contains(str_list[i])].values[:,
					      self.start_index]
					if foo.size != 0:
						temp2.append(foo[0])

				original_df_labels[i].append(temp2)

		# count the class labels for each row in original df
		original_df_labels_freqs = []
		for i in range(len(self.data)):
			original_df_labels_freqs.append([str_list[i]])
			for j in range(len(K_means_bts)):
				# iterate across methods (3)
				temp = Counter(original_df_labels[i][j + 1]).keys()
				temp2 = Counter(original_df_labels[i][j + 1]).values()
				original_df_labels_freqs[i].append(dict(zip(temp, temp2)))

		# calculate proportions here
		mats = []

		for j in range(len(K_means_bts)):
			temp_mat = []
			for i in range(len(self.data)):
				total_occurences = float(sum(original_df_labels_freqs[i][j + 1].values()))
				temp = []
				for k in range(self.opt_k):
					if k in original_df_labels_freqs[i][j + 1]:
						temp.append(original_df_labels_freqs[i][j + 1][k] / total_occurences)
					else:
						temp.append(0.0)
				temp_mat.append(temp)
			mats.append(temp_mat)
		return mats

	def consensus(self, mats):
		# finally, get the consensus from each algorithm
		consensus = []
		for i in range(len(self.data)):
			temp = []
			for j in range(self.opt_k):
				temp2 = []
				for k in range(len(mats)):
					temp2.append(mats[k][i][j])

				temp2 = sum(temp2) / float(self.opt_k)
				temp.append(temp2)

			consensus.append(temp)
		# save final consensus to CSV
		consensus = pd.DataFrame(consensus)
		consensus.insert(0, 'name', self.labels)
		consensus.to_csv('consensus.csv')
		return consensus


	@staticmethod
	def heatmaps(mats):
		## heatmap/Matrix plots for each algorithm
		titles = ['KMeans', 'GMM Clustering', 'Ward Agglomerative']

		for i in range(len(titles)):
			sns_plt = sns.clustermap(np.asarray(mats[i][0:]).transpose(), cmap="mako", robust=True)
			sns_plt.savefig(titles[i])
			sns_plt
			plt.title(titles[i])
			plt.show()

		# for i in range(len(titles)):
		# 	plt.figure()
		# 	plt.title(titles[i])
		# 	plt.xlabel('First 20 samples in numeric order')
		# 	plt.ylabel('Class label')
		# 	plt.yticks([0, 1, 2])
		# 	plt.imshow(np.asarray(mats[i][0:50]).transpose(), interpolation='nearest', cmap=plt.cm.ocean)
		# 	plt.colorbar()
		# 	plt.axes().set_aspect('auto')
		# plt.show()

	@staticmethod
	def append_all(alg_out1, alg_out2, alg_out3):
		foo = []
		foo.append(alg_out1)
		foo.append(alg_out2)
		foo.append(alg_out3)
		pickle.dump(foo, open("bts.p", "wb"))
		return foo