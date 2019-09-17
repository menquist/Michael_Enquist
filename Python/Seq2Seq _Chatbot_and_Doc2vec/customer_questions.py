#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 23 11:21:37 2018

@author: michael
"""

import pandas as pd
our_list = [] # create empty list
col = ["Name","Sex","Age","Martial Status","Family Income","Phone Number","Legal Status"] 
Question1 = (input('Please Enter Name?: '))
Question2 = (input('What is your gender?: '))
Question3 = (input('What is your Age?: '))
Question4 = (input('What is your martial status?: '))
Question5 = (input('What is your Family Income?: '))
Question6 = (input('What is your Phone Number?: '))
Question7 = (input('What is your Legal Status?: '))

our_list.append(Question1)
our_list.append(Question2)
our_list.append(Question3)
our_list.append(Question4)
our_list.append(Question5)
our_list.append(Question6)
our_list.append(Question7)
our_list

#our_list = ['Thanks You', 'Its fine no problem', 'Are you sure']

#create new df 
df = pd.DataFrame(data=[our_list],columns=col)
print (df)

# DF TO CSV
df.to_csv('Know_your_customers.csv', sep=',')