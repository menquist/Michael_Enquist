#!/usr/bin/env python
# coding: utf-8

# In[56]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale 
get_ipython().run_line_magic('matplotlib', 'inline')


# In[46]:


# In R, I exported the dataset from package 'ISLR' to an Excel file
df = pd.read_excel('Default.xlsx')

# Note: factorize() returns two objects: a label array and an array with the unique values.
# We are only interested in the first object. 

df['student2'] = df.student.factorize()[0]
df['default2'] = df.default.factorize()[0]

df.head(3)

# removing zero LTI 
df = df[df['LTI']!= 0]
df.head(3)


# In[47]:



X = df[['balance','income', 'Age', 'LTI','Months Delayed', 'student2']].values

y = df.default2.values

plt.figure(figsize=(10, 6))
plt.scatter(X[y == 0][:, 0], X[y == 0][:, 1], color='b', label='0')
plt.scatter(X[y == 1][:, 0], X[y == 1][:, 1], color='r', label='1')
plt.legend();


# In[48]:


from sklearn.linear_model import Lasso

def lasso_regression(data, predictors, alpha, models_to_plot={}):
    #Fit the model
    lassoreg = Lasso(alpha=alpha,normalize=True, max_iter=1e5)
    lassoreg.fit(X,y)
    y_pred = lassoreg.predict(X)
    
    #Check if a plot is to be made for the entered alpha
    if alpha in models_to_plot:
        plt.subplot(models_to_plot[alpha])
        plt.tight_layout()
        plt.plot(y,y_pred)
        #plt.plot(data['x'],data['y'],'.')
        plt.title('Plot for alpha: %.3g'%alpha)
    
    #Return the result in pre-defined format
    rss = sum((y_pred-y**2))
    ret = [rss]
    ret.extend([lassoreg.intercept_])
    ret.extend(lassoreg.coef_)
    return ret


# In[50]:



#Define the alpha values to test
alpha_lasso = [1e-15, 1e-10, 1e-8, 1e-5,1e-4, 1e-3,1e-2, 1, 5, 10]

#Initialize the dataframe to store coefficients
col = ['rss','intercept'] +  (df.columns[3:9].values.tolist())
ind = ['alpha_%.2g'%alpha_lasso[i] for i in range(0,10)]
coef_matrix_lasso = pd.DataFrame(index=ind, columns=col)

#Define the models to plot
models_to_plot = {1e-10:231, 1e-5:232,1e-4:233, 1e-3:234, 1e-2:235, 1:236}

#Iterate over the 10 alpha values:
for i in range(10):
    coef_matrix_lasso.iloc[i,] = lasso_regression(X, y, alpha_lasso[i], models_to_plot)


# In[52]:


coef_matrix_lasso


# #### This tells us that the model complexity decreases with increase in the values of alpha. 

# In[53]:


coef_matrix_lasso.apply(lambda x: sum(x.values==0),axis=1)


# #### We can observe that even for a small value of alpha, a significant number of coefficients are zero. This also explains the horizontal line fit for alpha=1 in the lasso plots, its just a baseline model! This phenomenon of most of the coefficients being zero is called ‘sparsity‘. Although lasso performs feature selection, this level of sparsity is achieved in special cases only which we’ll discuss towards the end.

# In[57]:


alphas = 10**np.linspace(10,-2,100)*0.5

lasso = Lasso()
coefs = []

for a in alphas:
    lasso.set_params(alpha=a)
    lasso.fit(scale(X), y)
    coefs.append(lasso.coef_)

ax = plt.gca()
ax.plot(alphas, coefs)
ax.set_xscale('log')
ax.set_xlim(ax.get_xlim()[::-1])  # reverse axis
plt.axis('tight')
plt.xlabel('alpha')
plt.ylabel('weights')
plt.title('Ridge coefficients as a function of the regularization');


# In[60]:



from sklearn.linear_model import LassoCV
lassocv = LassoCV(alphas=None, cv=10, max_iter=10000)
lassocv.fit(scale(X), y)


# In[61]:


lassocv.alpha_


# In[64]:


lasso.set_params(alpha=lassocv.alpha_)
lasso.fit(scale(X), y)
#mean_squared_error(y_test, lasso.predict(scale(X_test)))
# Some of the coefficients are now reduced to exactly zero.
pd.Series(lasso.coef_, index=(df.columns[3:9].values))


# ### 
