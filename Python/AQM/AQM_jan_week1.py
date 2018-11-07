

import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt


# Import Data
apc_df = pd.read_table('/Users/Enquist/Desktop/apc_2011_subset.txt',sep='\t', low_memory=False).dropna()

print(apc_df.head())

print(apc_df.describe())

# Merge Tables
df1 = pd.DataFrame({'HPI':[80,85,88,85],
                    'Int_rate':[2, 3, 2, 2],
                    'US_GDP_Thousands':[50, 55, 65, 55]},
                   index = [2001, 2002, 2003, 2004])

df2 = pd.DataFrame({'HPI':[80,85,88,85],
                    'Int_rate':[2, 3, 2, 2],
                    'US_GDP_Thousands':[50, 55, 65, 55]},
                   index = [2005, 2006, 2007, 2008])

df3 = pd.DataFrame({'HPI':[80,85,88,85],
                    'Unemployment':[7, 8, 9, 6],
                    'Low_tier_HPI':[50, 52, 50, 53]},
                   index = [2001, 2002, 2003, 2004])


print(pd.merge(df1,df3,on='HPI'))

print(pd.merge(df1,df2, on=['HPI','Int_rate']))

df4 = pd.merge(df1,df3, on='HPI')
print(df4)
df4.set_index('HPI',inplace=True)

df1.set_index('HPI',inplace=True)
df3.set_index('HPI',inplace=True)
joined = df1.join(df3)
print(joined)

print(df3.join(df1))

df1 = pd.DataFrame({
                    'Int_rate':[2, 3, 2, 2],
                    'US_GDP_Thousands':[50, 55, 65, 55],
                    'Year':[2001, 2002, 2003, 2004]
                    })

df3 = pd.DataFrame({
                    'Unemployment':[7, 8, 9, 6],
                    'Low_tier_HPI':[50, 52, 50, 53],
                    'Year':[2001, 2003, 2004, 2005]})

merged = pd.merge(df1,df3,on='Year')
merged
print(df1)
print(df3)

merged = pd.merge(df1,df3,on='Year',how='left')
merged
# Create tables
df1 = pd.DataFrame({'col1':[80,85,88,85],
                    'col2':[2,3,2,2]}
)

# String
var1 = 'Hello World!'
var1.find('l',5)

raw_data = {'regiment': ['Nighthawks', 'Nighthawks', 'Nighthawks', 'Nighthawks', 'Dragoons', 'Dragoons', 'Dragoons', 'Dragoons', 'Scouts', 'Scouts', 'Scouts', 'Scouts'], 
        'company': ['1st', '1st', '2nd', '2nd', '1st', '1st', '2nd', '2nd','1st', '1st', '2nd', '2nd'], 
        'name': ['Miller', 'Jacobson', 'Ali', 'Milner', 'Cooze', 'Jacon', 'Ryaner', 'Sone', 'Sloan', 'Piger', 'Riani', 'Ali'], 
        'preTestScore': [4, 24, 31, 2, 3, 4, 24, 31, 2, 3, 2, 3],
        'postTestScore': [25, 94, 57, 62, 70, 25, 94, 57, 62, 70, 62, 70]}
df = pd.DataFrame(raw_data, columns = ['regiment', 'company', 'name', 'preTestScore', 'postTestScore'])
df
groupby_company = df['preTestScore'].groupby(df['company'])
list(groupby_company)
groupby_company.describe()

groupby_company.mean()

# Visualization
ts = pd.Series(np.random.randn(1000), index=pd.date_range('1/1/2000', periods=1000))
ts = ts.cumsum()

df = pd.DataFrame(np.random.randn(1000, 4), index=ts.index, columns=list('ABCD'))
df = df.cumsum()
df
ts.plot()

plt.figure()
df.ix[5].plot(kind='bar')
df.ix[5].plot.bar()
plt.axhline(0,color='k')

df5 = pd.DataFrame(np.random.rand(10, 4), columns=['a', 'b', 'c', 'd'])
df5.plot.bar()

df5.plot.barh(stacked=True)


# Excercise 1

BUSLINE_143 = apc_df[apc_df['Line'].str.contains('143')]

BUSLINE_041 = apc_df[apc_df['Line'].str.contains('041')]

BUSLINE_160 = apc_df[apc_df['Line'].str.contains('160')]

# BUSLINE_143 = convertTime(BUSLINE_143,'ActualArriveTime','ActualArriveTime')
# Plot

plt.plot(pd.to_datetime(BUSLINE_143.ActualArriveTime), BUSLINE_143.OnsLoadCompensated, lw = 0, marker = '*')
plt.plot(pd.to_datetime(BUSLINE_041.ActualArriveTime), BUSLINE_041.OnsLoadCompensated, lw = 0, marker = '*')
plt.plot(pd.to_datetime(BUSLINE_160.ActualArriveTime), BUSLINE_160.OnsLoadCompensated, lw = 0, marker = '*')


# Excercise 2 Translink Weather

yvr_weather = pd.read_table('/Users/Enquist/Desktop/yvr_weather_09-2011.csv',sep='\t', low_memory=False).dropna()

print(yvr_weather.head())

yvr_weather['DateTime'] = yvr_weather["Date/Time"].apply(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H:%M'))
yvr_weather['DateTimeRounded'] = yvr_weather['DateTime']

print(yvr_weather.head())




