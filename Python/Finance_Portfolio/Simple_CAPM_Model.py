#!/usr/bin/env python
# coding: utf-8

# ### A simple least squares regression on the CAPM model, and derive the values of α and βi by running the following code in Python: APPLE and S&P 500

# In[2]:



""" Linear regression with SciPy """
from scipy import stats
import pandas as pd
import numpy as np

# APPLE and GSPC returns

apple = pd.read_csv('AAPL.csv')
GSPC = pd.read_csv('MARKET_RETURNS.csv')
Join = pd.merge(apple,GSPC,on='date').dropna()

stock_returns = Join['rate'] #.values.tolist()    
mkt_returns =  Join['rate.NYSE'] #.values.tolist()   

beta, alpha, r_value, p_value, std_err =     stats.linregress(stock_returns, mkt_returns)
print(beta, alpha)

""" Calculating the SML """
rf = 0.05
mrisk_prem = 0.085
risk_prem = mrisk_prem * beta
print("Risk premium:", risk_prem)

expected_stock_return = rf + risk_prem
print("Expected stock return:", expected_stock_return)

