import urllib
    
# define URLs
test_data_url = "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/testdata.txt"
train_data_url = "https://dl.dropboxusercontent.com/u/8082731/datasets/UMICH-SI650/training.txt"
    
# define local file names
test_data_file_name = 'test_data.csv'
train_data_file_name = 'train_data.csv'
    
# download files using urlib
test_data_f = urllib.urlretrieve(test_data_url, test_data_file_name)
train_data_f = urllib.urlretrieve(train_data_url, train_data_file_name)

import pandas as pd
    
test_data_df = pd.read_csv(test_data_file_name, header=None, delimiter="\t", quoting=3)
test_data_df.columns = ["Text"]
train_data_df = pd.read_csv(train_data_file_name, header=None, delimiter="\t", quoting=3)
train_data_df.columns = ["Sentiment","Text"]

train_data_df.shape
test_data_df.shape
train_data_df.head()
test_data_df.head()

train_data_df.Sentiment.value_counts()
import numpy as np 
np.mean([len(s.split(" ")) for s in train_data_df.Text])

#Importing Porpus
import re, nltk
from sklearn.feature_extraction.text import CountVectorizer        
from nltk.stem.porter import PorterStemmer

#######
# based on http://www.cs.duke.edu/courses/spring14/compsci290/assignments/lab02.html
stemmer = PorterStemmer()
def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def tokenize(text):
    # remove non letters
    text = re.sub("[^a-zA-Z]", " ", text)
    # tokenize
    tokens = nltk.word_tokenize(text)
    # stem
    stems = stem_tokens(tokens, stemmer)
    return stems
######## 

vectorizer = CountVectorizer(
    analyzer = 'word',
    tokenizer = tokenize,
    lowercase = True,
    stop_words = 'english',
    max_features = 85
)


corpus_data_features = vectorizer.fit_transform(
    train_data_df.Text.tolist() + test_data_df.Text.tolist())

corpus_data_features_nd = corpus_data_features.toarray()
corpus_data_features_nd.shape

vocab = vectorizer.get_feature_names()
print vocab

# Sum up the counts of each vocabulary word
dist = np.sum(corpus_data_features_nd, axis=0)
    
# For each, print the vocabulary word and the number of times it 
# appears in the data set
for tag, count in zip(vocab, dist):
    print count, tag

from sklearn.cross_validation import train_test_split
    
# remember that corpus_data_features_nd contains all of our 
# original train and test data, so we need to exclude
# the unlabeled test entries
X_train, X_test, y_train, y_test  = train_test_split(
        corpus_data_features_nd[0:len(train_data_df)], 
        train_data_df.Sentiment,
        train_size=0.85, 
        random_state=1234)

from sklearn.linear_model import LogisticRegression
    
log_model = LogisticRegression()
log_model = log_model.fit(X=X_train, y=y_train)

y_pred = log_model.predict(X_test)

from sklearn.metrics import classification_report
print(classification_report(y_test, y_pred))

# train classifier
log_model = LogisticRegression()
log_model = log_model.fit(X=corpus_data_features_nd[0:len(train_data_df)], y=train_data_df.Sentiment)
    
# get predictions
test_pred = log_model.predict(corpus_data_features_nd[len(train_data_df):])
    
# sample some of them
import random
spl = random.sample(xrange(len(test_pred)), 15)
    
# print text and labels
for text, sentiment in zip(test_data_df.Text[spl], test_pred[spl]):
    print sentiment, text

