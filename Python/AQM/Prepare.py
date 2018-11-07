import pandas as pd

class Prepare:

	def __init__(self, s, size):
		self.s = s
		self.size = size

	@staticmethod
	def generate_names(s, size):
		"""
		Generate names for each row in the Iris dataset
		"""
		labels = []

		for i in range(size):
			labels.append(s + str(i))

		return labels

	def names_join(self, data):
		"""
		Join the names column to the dataframe
		"""
		labels = self.generate_names(self.s, len(data))
		labels = pd.DataFrame(labels)
		df = data
		df.insert(0, 'name', labels)
		return {"df" : df, "labels" : labels}
