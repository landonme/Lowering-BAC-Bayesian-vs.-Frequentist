# -*- coding: utf-8 -*-
"""
Created on Sat Nov  2 00:06:14 2019

@author: Lando
"""


import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

os.chdir("C:\\Users\\Lando\\Desktop\\Econ 7801 Proj")
df = pd.read_csv('Datasets\\DDACTS_download.csv')
df.columns
df.columns = map(str.lower, df.columns)
df['dtime'] = pd.to_datetime(df['crash_datetime'])
df['dt'] = df['dtime'].dt.date
df['wk'] = df.dtime.dt.strftime('%Y-%W')
df["wk_st"] = df['dtime'].apply(lambda x: (x - timedelta(days=x.dayofweek))).dt.date


## DUI Crashes by Week..
dc = df[df.dui=='Y'].groupby('wk_st').agg({'crash_id' : 'count'})
dc.columns = ['crashes']
dc.index = pd.to_datetime(dc.index)
# dc = dc.asfreq('w')

## Plot
dc.crashes.plot()

## Is the Series Stationary??

import statsmodels.tsa.stattools as sts
# Rune Augmented DF Test
sts.adfuller(dc.crashes
             , autolag="BIC")
'''
H0: There is a unit root

(-15.942021632204177,
 7.521868943584307e-29, -- P-Value
)
## We reject Hnot and conclude the series is stationary
'''

## Check for some seasonality
# Additive
from statsmodels.tsa.seasonal import seasonal_decompose
s_dec_additive = seasonal_decompose(dc.crashes, model='additive')
s_dec_additive.plot()
    # Suggests no Seasonality

# Multiplicative
s_dec_mul = seasonal_decompose(dc.crashes, model='multiplicative')
s_dec_mul.plot()
    # Again suggests no seasonality

## Try including a deterministic trend..
import statsmodels.api as sm
dc['trend'] = [num for num in range(1,dc.shape[0]+1, 1)]
dc['trend2'] = dc['trend']**2
model = sm.OLS(dc.crashes, dc[['trend', 'trend2']])
results = model.fit()
print(results.summary())



# Autocorrelation Functions
import statsmodels.graphics.tsaplots as sgt
# Plot ACF
sgt.plot_acf(dc.crashes, lags = 40, zero=False)
plt.title("ACF Chrashes")


# Plot PACF
sgt.plot_pacf(dc.crashes, lags=40, zero=False, method = ("ols"))
plt.title("PACF Crashes")
    # Anything outside the shaded area is significant...










