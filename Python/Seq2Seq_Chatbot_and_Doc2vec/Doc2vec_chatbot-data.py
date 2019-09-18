#!/usr/bin/env python
# coding: utf-8

# In[1]:


#Import all the dependencies
import gensim
import pandas as pd
import re, nltk
import numpy as np
import os
import xlrd


# In[2]:


from gensim.models.doc2vec import Doc2Vec, TaggedDocument
from nltk.tokenize import word_tokenize


# In[3]:


# Importing the dataset
#lines = pd.read_csv('Chatbot_data.csv')

cwd = os.getcwd()
cwd
file_name = "Chatbot-dat.xlsx"
#lines = pd.read_excel(file_name, sheet_name="documents")

#file_name = "doc2vec-dat.xlsx"
xls = pd.ExcelFile(file_name)
df1 = pd.read_excel(xls,"documents")
df1['ORI Docs'] = df1['Text']
df = df1

import string
from string import printable
st = set(printable)
df['Text'] = df['Text'].apply(lambda x: ''.join([" " if i not in st else i for i in x]))

df['Text'] = df['Text'].apply(lambda x: x.lower())

def remove_punctuations(text):
    for punctuation in string.punctuation:
        text = text.replace(punctuation, ' ')
    return text
df['Text'] = df['Text'].apply(remove_punctuations)

to_remove = "0123456789"
df.Text = [s.translate(str.maketrans('','', to_remove)) for s in df.Text]

corpus = df[['Text']].values
corpus = np.concatenate(corpus).ravel().tolist()
corpus


# In[4]:


tagged_data = [TaggedDocument(words = word_tokenize(_d.lower()), tags =[str(i)]) for i, _d in enumerate(corpus)]
tagged_data 


# In[5]:


max_epochs = 100 
vec_size = 300
alpha = .025

model = Doc2Vec(size =vec_size,
                alpha = alpha,
                min_alpha = .0001,
                min_count = 1,
                dm = 1)

model.build_vocab(tagged_data)

for epoch in range(max_epochs):
    print('iteration {0}'.format(epoch))
    model.train(tagged_data,
               total_examples=model.corpus_count,
               epochs=model.iter)
    model.alpha -= .00002
    model.min_alpha = model.alpha
   
model.save("Doc2Vec COT(Sentences).model")
print("model saved")


# In[ ]:




