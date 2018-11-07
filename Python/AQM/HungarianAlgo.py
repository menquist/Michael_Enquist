from munkres import Munkres
from sklearn.metrics import confusion_matrix
import numpy as np

class Hungarian:

	def __init__(self, labels, reference):
		self.labels = labels
		self.reference = reference
		self.num_labels = len(np.unique(reference))


	def __cost_matrix(self):
		uc1 = np.unique(self.labels)
		uc2 = np.unique(self.reference)
		l1 = uc1.size
		l2 = uc2.size
		assert (l1 == l2 and np.all(uc1 == uc2))

		m = np.ones([l1, l2])
		for i in range(l1):
			it_i = np.nonzero(self.labels == uc1[i])[0]
			for j in range(l2):
				it_j = np.nonzero(self.reference == uc2[j])[0]
				m_ij = np.intersect1d(it_j, it_i)
				m[i, j] = -m_ij.size
		return m

	def hungarian(self):

		def __translate_clustering(clst, mapper):
			return np.array([mapper[i] for i in clst])

		cost_matrix = self.__cost_matrix()
		indices = Munkres().compute(cost_matrix)
		mapper = {old: new for (old, new) in indices}
		matched_labels = __translate_clustering(self.labels, mapper)
		new_cm = confusion_matrix(self.reference, matched_labels, labels=range(self.num_labels))
		return {'matched_labels' : matched_labels, 'new_cm' : new_cm}


