

library(lubridate)
library(dplyr)
library(stats)
library(tseries)
library(urca)
library(ggplot2)
library(forecast)
library(uroot)
#library(ggtheme)

#Data Setup..
#==============================================================
setwd("C:/Users/Lando/Desktop/Econ 7801 Proj/")
dta = read.csv("Datasets/crash_data_14_19.csv", stringsAsFactors = FALSE) #DDACTS_download.csv
names(dta) = tolower(names(dta))
#  names(dta)
# dta$wk = as.Date(dta$wk, format = "%m/%d/%Y")
dta$date = as.Date(dta$date, format = "%m/%d/%Y")
dta = dta %>% mutate(month = floor_date(date, "month"), wk = floor_date(date, "week"))


## Create Dataframes for monthly/daily/weekly data
df = dta %>% mutate(
  dui_num = ifelse(dui == "Y",1,0),
  non_dui = ifelse(dui == "N",1,0)
  ) %>% group_by(wk) %>% 
  summarise(cr = sum(dui_num),
            cr_other = sum(non_dui)) %>% 
  filter(wk <= as.Date('2019-12-31') & wk >= as.Date('2014-01-01'))

## Add some variables
# Week
df$trend = seq(1,nrow(df),1)
df$lcr = log(df$cr)
df$lcro = log(df$cr_other)
# include a dummy for post-intervention
df = df %>% mutate(
  post = ifelse(wk > as.Date("2018-12-29"),1,0),
  wk_num = as.factor(week(wk))
)

# write.csv(df, 'weekly_dui_crashes.csv')

# Creating TS Objects

# Weekly
start_dt = as.Date('2014-01-05')
stop_dt = as.Date('2019-12-31')
wk_pre_ts = df %>% filter(wk < as.Date('2018-12-30') & wk >= start_dt) %>% dplyr::select(lcr) %>%
  ts(freq=52, start=decimal_date(start_dt)) # '2015-01-04', '2014-01-05'

wk_full_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=52, start=decimal_date(start_dt))

wk_post_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(post) %>% 
  as.matrix()



## Plot
#==============================================================

## 2015-2019 Weekly DUI
ggplot(data=df, aes(x=wk, y=cr, group=1)) +
  geom_line(color="firebrick") + #+ geom_point() +
  xlab("week") + ylab("log crashes") +
  ggtitle("Log of DUI Related Crashes in Utah") +
  theme_bw()



## Checking for Seasonality
# I'll create Week Dummies and check for significance..
mm_df = model.matrix(~ 0 + wk_num, df) %>%
  as.data.frame() %>%
  bind_cols(df) %>% 
  select(wk, cr, contains("wk_num")) %>% select(-wk_num, -wk_num53, -wk_num52)


lm_seas = lm(cr ~ . , mm_df %>% select(-wk))
summary(lm_seas)


## Using Uroot
bbplot(wk_full_ts)
bb3D(wk_full_ts)

## Unit Root Tests
#==============================================================

## Augmented DF Test
# Ho: Unit Root
adf = ur.df(wk_pre_ts, type = c("drift"), 
            selectlags = c("AIC"))
summary(adf)

## DF-GLS Test
# H.: Unit Root
dfgls = ur.ers(wk_pre_ts,
               model = "constant",
               type="DF-GLS"
               #    lag.max = 15
    )

summary(dfgls)

## Hegy Test for Seasonal Data
#  deterministic = c(constant, trend, seasonal dummys)
#  Under Ho: Seasonal Unit Root Exists
ht = hegy.test(wk_pre_ts, deterministic = c(1,0,0), lag.method = "BIC")
summary(ht)
hegy.regressors(wk_pre_ts)

# Try taking 1 season difference
wk_pre_ts_ds1 = diff(wk_pre_ts, lag = 52, differences = 1)

ht = hegy.test(wk_pre_ts_ds1, deterministic = c(1,0,0), lag.method = "AIC")
summary(ht)


## Quick alternative to get number of differences needed
forecast::ndiffs(wk_pre_ts, alpha=0.05, test="adf", max.d=2)
forecast::nsdiffs(wk_pre_ts, max.D=1) # test=c("ocsb","ch")



## Is there a Deterministic trend??
model_trend = lm(cr ~ trend , df %>% filter(wk < as.Date('2018-12-30')))
summary(model_trend)



# Weekly
start_dt = as.Date('2015-01-04')
stop_dt = as.Date('2019-04-01')
wk_pre_ts = df %>% filter(wk < as.Date('2018-12-30') & wk >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=52, start=decimal_date(start_dt)) # '2017-01-01' '2016-01-02' '2015-01-04', '2014-01-05'

wk_full_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=52, start=decimal_date(start_dt))

wk_post_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(post) %>% 
    as.matrix()
#  ts(freq=365.25/7, start=decimal_date(ymd("2015-01-04")))


# Start Model Weekly..
#==========================================================================================================
library(lmtest)
#library(forecast)
auto = auto.arima(wk_full_ts, stepwise = FALSE, approximation = FALSE, xreg = wk_post_ts,
                  seasonal.test = "hegy"
                  )
auto


## Models Pre-Intervention
mod1_pre = Arima(wk_pre_ts, order = c(0,0,0), seasonal = c(1,1,0))
mod2_pre = Arima(wk_pre_ts, order = c(2,0,3)) # auto.arima with no seasonal.test
mod3_pre = Arima(wk_pre_ts, order = c(0,0,0), seasonal = c(1,1,0)) # auto.arima with seasonal.test="hegy"
mod4_pre = Arima(wk_pre_ts, order = c(0,1,2), seasonal = c(0,0,0)) # auto.arima with seasonal.test="hegy"

summary(mod1_pre); coeftest(mod1_pre)
summary(mod2_pre); coeftest(mod2_pre)
summary(mod3_pre)
summary(mod4_pre); coeftest(mod4_pre)

## Models Full Dataset
mod1 = Arima(wk_full_ts, order = c(0,0,0), seasonal = c(1,1,0), xreg = wk_post_ts)
mod2 = Arima(wk_full_ts, order = c(2,0,3), xreg = wk_post_ts)
mod3 = Arima(wk_full_ts, order = c(0,0,0), seasonal = c(0,1,0), xreg = wk_post_ts)
mod4 = Arima(df$cr, order = c(3,0,2), xreg = df$post)


summary(mod1); coeftest(mod1)
summary(mod2); coeftest(mod2)
summary(mod3); coeftest(mod3)
summary(mod4)



## Diagnostic Tests
#===================================
#Q Statistic # Box-Ljung test
## Ho: Independent Errors (white noise) -- WE want to fail to reject..
get_q = function(model, start = 5, stop = 25, by = 5, fitdf){
  x = c()
  y = c()
  i = 1
  for (val in seq(start,stop,by)) {
    new = Box.test(model$residuals, lag = val, type = "Ljung-Box", fitdf = fitdf)
    y[[i]] = val
    x[[i]] = new$p.value
    i = i+1
  }
  return(do.call(rbind.data.frame, Map(cbind, y, x)))
}

get_q(mod1, fitdf = 1) # fitdf should be P+Q i belive..


# Plot Residuals
fitvalues = wk_full_ts - mod1$residuals
plot(fitvalues, mod1$residuals)
plot(mod1$residuals)
hist(mod1$residuals)



AutoCorrelation <- pacf(df$lcr, plot = FALSE)
plot(AutoCorrelation, main = "PACF of Log DUI Crashes")


### Try a function to cycle through models..

nm = c(3,0,3)
seas = c(2,3,3)


run_arimas = function(df, nm, seas){
  for (p in 0:nm[1]) for(d in 0:nm[2]) for(q in 0:nm[3]) for (P in 0:seas[1]) for(D in 0:seas[2]) for(Q in 0:seas[3]) {
    norm_part = c(p,d,q)
    seas_part = c(P,D,Q)
    mod = Arima(wk_pre_ts, order = norm_part, seasonal = seas_part)
    aic = mod$aic
    bic = mod$bic
    qStat = get_q(mod, p+q+P+Q)
    coefs = coeftest(mod)
   
  }


}



ac = acf(wk_pre_ts_ds1, plot = FALSE, lag.max = 206)
plot(ac , main = "")


# PACF
pcf = pacf(wk_pre_ts_ds1, plot = FALSE, lag.max = 206)
plot(pcf, main = "")




## MONTHLY
#===========================================================================================================


df_month = dta %>% mutate(
  dui_num = ifelse(dui == "Y",1,0),
  non_dui = ifelse(dui == "N",1,0),
) %>% group_by(month) %>%
  summarise(cr = sum(dui_num),
            cr_other = sum(non_dui)) %>% 
  filter(month >= as.Date("2014-01-01") & month < as.Date('2019-12-31'))


# Month
df_month$trend = seq(1,nrow(df_month),1)
df_month$lcr = log(df_month$cr)
df_month$lcro = log(df_month$cr_other)
# include a dummy for post-intervention
df_month = df_month %>% mutate(
  post = ifelse(month > as.Date("2018-12-29"),1,0)
)



# Monthly
mo_pre_ts = df_month %>% filter(month < as.Date('2019-01-01') & month >= as.Date('2015-01-01')) %>% dplyr::select(cr) %>%
  ts(freq=12, start=2015)


start_dt = as.Date('2015-01-01')
stop_dt = as.Date('2019-06-30')

mo_pre_ts = df_month %>% filter(month < as.Date('2018-12-30') & month >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=12, start=decimal_date(start_dt)) # '2015-01-04', '2014-01-05'

mo_full_ts = df_month %>% filter(month < stop_dt & month >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=12, start=decimal_date(start_dt))

mo_post_ts = df_month %>% filter(month < stop_dt & month >= start_dt) %>% dplyr::select(post) %>% 
  as.matrix()

dim(mo_post_ts) == dim(mo_full_ts)

## A quick check on seasonality..
decomp = decompose(mo_pre_ts)
plot(decomp)
plot(mo_pre_ts)


## Unit Root Tests
#==============================================================
####### ON DUI
#=============
## Augmented DF Test
# Ho: Unit Root
adf = ur.df(df_month$cr, type = c("drift"), 
            selectlags = c("BIC"))
summary(adf)

## DF-GLS Test
# H.: Unit Root
dfgls = ur.ers(df_month$cr,
               model = "constant",
               type="DF-GLS"
               #    lag.max = 15
)
summary(dfgls)

## KPSS Test
# Ho: Stationary
kpss = ur.kpss(df_month$lcr, type = "mu")
summary(kpss)

## Is there a deterministic trend??
model_trent = lm(cr ~ trend, df_month %>% filter(month < as.Date('2019-01-01')))
summary(model_trent)


###
##   Test is without a doubt stationary
###

## Is there a trend??
model_trent = lm(cr ~ trend + I(trend^2), df_month %>% filter(month < as.Date('2019-01-01') & month >= as.Date('2015-01-01')))
summary(model_trent)



## Hegy Test for Seasonal Data
# deterministic = c(constant, trend, seasonal dummys)
# Under Ho: Seasonal Unit Root Exists
ht = hegy.test(mo_pre_ts, deterministic = c(1,0,0))
summary(ht)




# Start Model
#================================================================
auto = auto.arima(mo_pre_ts, stepwise = FALSE, approximation = FALSE,
                  seasonal.test = "hegy")
auto



## Models Pre-Intervention
mod1_pre = Arima(mo_pre_ts, order = c(2,1,0), seasonal = c(0,1,1))
mod2_pre = Arima(cr_pre, order = c(0,0,1))
mod3_pre = Arima(cr_pre, order = c(2,0,2))

summary(mod1_pre)
summary(mod2_pre)
summary(mod1_pre)


## Models Full Dataset
mod1 = Arima(mo_full_ts, order = c(2,1,0), seasonal = c(0,1,1), xreg = mo_post_ts)
mod2 = Arima(df_month$lcr, order = c(2,0,1), xreg = df_month$post)
mod3 = Arima(df_month$lcr, order = c(2,0,2), xreg = df_month$post)
mod4 = Arima(df_month$lcr, order = c(3,0,2), xreg = df_month$post)

summary(mod1); coeftest(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

## Diagnostic Tests
#===================================
#Q Statistic # Box-Ljung test
## Ho: Independent Errors (white noise) -- WE want to fail to reject..
print(Box.test(mod1$residuals, lag = 10, type = "Ljung-Box", fitdf = 4)) # we should subtract P,Q, & 1 (from the constant)
print(Box.test(mod1$residuals, lag = 15, type = "Ljung-Box", fitdf = 4)) # fitdf = P + Q
print(Box.test(mod1$residuals, lag = 20, type = "Ljung-Box", fitdf = 4)) # fitdf = P + Q

# Plot Residuals
fitvalues = df$cr - mod1$residuals
plot(fitvalues, mod1$residuals)
plot(mod1$residuals)
hist(mod1$residuals)



AutoCorrelation <- pacf(df$lcr, plot = FALSE)
plot(AutoCorrelation, main = "PACF of Log DUI Crashes")






# Daily
# =======================================================================================================


df_day = dta %>% mutate(
  dui_num = ifelse(dui == "Y",1,0),
  non_dui = ifelse(dui == "N",1,0)
) %>% group_by(date) %>%
  summarise(cr = sum(dui_num),
            cr_other = sum(non_dui)) %>% 
  filter(date >= as.Date("2014-01-01") & date <= as.Date('2019-12-31'))

# Day
df_day$trend = seq(1,nrow(df_day),1)
df_day$lcr = log(df_day$cr)
df_day$lcro = log(df_day$cr_other)
# include a dummy for post-intervention
df_day = df_day %>% mutate(
  post = ifelse(date > as.Date("2018-12-29"),1,0)
)


# Daily
d_pre_ts = df_day %>% filter(date < as.Date('2018-12-30') & date >= as.Date('2015-01-01')) %>% dplyr::select(cr) %>%
  ts(freq=365, start=decimal_date(ymd("2015-01-01")))




