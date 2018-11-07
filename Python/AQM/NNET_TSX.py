
retval = os.getcwd()

os.chdir( "/Users/Enquist/R_Stuff/Stock_Analysis/")

retval = os.getcwd()

from random import seed
from random import randrange
from random import random
from csv import reader
from math import exp


#Importing and reading a CSV file
def load_csv(filename):
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
    
    
# Test Backprop on Seeds dataset
seed(1)
# load and prepare data
filename = 'TSX_NNET.csv'
dataset = load_csv(filename)

for i in range(len(dataset[0])-1):
    string_to_float(dataset, i)
    
    
    
# convert class column to integers
column_to_integer(dataset, len(dataset[0])-1)

# normalize input variables
minmax = max_min_values(dataset)
