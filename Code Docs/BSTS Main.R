


library(lubridate)
library(dplyr)
library(stats)
library(tseries)
library(urca)
library(ggplot2)
library(forecast)
library(uroot)
library(bsts)
library(reshape2)
library(knitr)
library(CausalImpact)

setwd("C:/Users/Lando/Desktop/Econ 7801 Proj/")
df = read.csv('Datasets/weekly_dui_crashes.csv',  stringsAsFactors = FALSE)

start_dt = as.Date('2015-01-04')
stop_dt = as.Date('2019-03-31')
wk_pre_ts = df %>% filter(wk < as.Date('2018-12-30') & wk >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=52, start=decimal_date(start_dt)) # '2015-01-04', '2014-01-05'

wk_full_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(cr) %>%
  ts(freq=52, start=decimal_date(start_dt))

wk_full_other = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(cr_other) %>%
  ts(freq=52, start=decimal_date(start_dt))

wk_post_ts = df %>% filter(wk < stop_dt & wk >= start_dt) %>% dplyr::select(post) %>% 
  as.matrix()

wk_full_ts = log(wk_full_ts)
wk_pre_ts = log(wk_pre_ts)
wk_full_other = log(wk_full_other)



## Plot Dataset
ggplot(data=df %>% filter(wk >= as.Date('2015-01-01') & wk <= as.Date('2019-04-01')),
       aes(x=as.Date(wk), y=lcr, group=1)) +
  geom_line(color="blue") + #+ geom_point() +
  ylab("log crashes") +
  ggtitle("Log Weekly DUI Related Crashes in Utah") +
  theme(axis.title.x=element_blank()) + theme_bw()




## Build Models for Out-Of-Sample Metrics
### Get out of sample evaluation metrics..
### ========================================

## Cut dataset.
y.1 = wk_full_ts[1:190] # Used to train models
y.2 = wk_full_ts[191:209] # Used to compare models

## LocalLinear and Seasonality
ss <- AddLocalLinearTrend(list(), y.1)
ss <- AddSeasonal(ss, y.1, nseasons = 52)
model1_pre <- bsts(y.1,
                   state.specification = ss, niter = 500)

## Seasonal Only
ss2 <- AddSeasonal(list(), y.1, nseasons = 52)
model2_pre <- bsts(y.1,
                   state.specification = ss2, niter = 500)

## Trend Only
ss3 <- AddLocalLinearTrend(list(), y.1)
model3_pre <- bsts(y.1,
                   state.specification = ss3, niter = 500)

## AddSemilocalLinearTrend
ss4 <- AddSemilocalLinearTrend(list(), y.1)
model4_pre <- bsts(y.1,
                   state.specification = ss4, niter = 500)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), y.1)
ss5 <- AddSeasonal(ss5, y.1, nseasons = 52)
model5_pre <- bsts(y.1,
                   state.specification = ss5, niter = 500)

## AddSemilocalLinearTrend & Seasonality & AR
ss6 <- AddSemilocalLinearTrend(list(), y.1)
ss6 <- AddSeasonal(ss6, y.1, nseasons = 52)
ss6 <- AddAr(ss6, y.1, lags = 6)
model6_pre <- bsts(y.1,
                   state.specification = ss6, niter = 500)

## AddSemilocalLinearTrend & Seasonality & AR
ss7 <- AddSemilocalLinearTrend(list(), y.1)
ss7 <- AddAr(ss7, y.1, lags = 6)
model7_pre <- bsts(y.1,
                   state.specification = ss7, niter = 500)




# Get MAE & RMSE on Holdout:
get.error.metrics.oos = function(model){
  preds = predict(model, horizon = length(y.2))
  errors = preds$mean - y.2
  mae = mean(abs(errors))
  rmse = sqrt(mean(errors^2))
  return(as.data.frame(list("MAE_hld" = round(mae,4), "RMSE_hld"= round(rmse,4))))
}

m1.metrics_pre = get.error.metrics.oos(model1_pre)
m2.metrics_pre = get.error.metrics.oos(model2_pre)
m3.metrics_pre = get.error.metrics.oos(model3_pre)
m4.metrics_pre = get.error.metrics.oos(model4_pre)
m5.metrics_pre = get.error.metrics.oos(model5_pre)
m6.metrics_pre = get.error.metrics.oos(model6_pre)
m7.metrics_pre = get.error.metrics.oos(model7_pre)
all.df = rbind(m1.metrics_pre, m2.metrics_pre, m3.metrics_pre, m4.metrics_pre, m5.metrics_pre, m6.metrics_pre, m7.metrics_pre)
row.names(all.df) = c('Model1', 'Model2', 'Model3', 'Model4', 'Model5', "Model6", "Model7")
kable(all.df)





## Full Dataset
## =====================================================================
## LocalLinear and Seasonality
ss <- AddLocalLinearTrend(list(), wk_full_ts)
ss <- AddSeasonal(ss, wk_full_ts, nseasons = 52)
model1 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss, niter = 500)

## Seasonal Only
ss2 <- AddSeasonal(list(), wk_full_ts, nseasons = 52)
model2 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss2, niter = 500)

## Trend Only
ss3 <- AddLocalLinearTrend(list(), wk_full_ts)
model3 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss3, niter = 500)

## AddSemilocalLinearTrend
ss4 <- AddSemilocalLinearTrend(list(), wk_full_ts)
model4 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss4, niter = 500)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), wk_full_ts)
ss5 <- AddSeasonal(ss5, wk_full_ts, nseasons = 52)
model5 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss5, niter = 500)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), wk_full_ts)
ss5 <- AddSeasonal(ss5, wk_full_ts, nseasons = 52)
model5 <- bsts(wk_full_ts ~ wk_post_ts,
               state.specification = ss5, niter = 500)



# Get Rsq, HGOF, MAE & RMSE on full dataset.
get.metrics = function(model){
  errors = bsts.prediction.errors(model)$in.sample
  mae = mean(abs(errors))
  rmse = sqrt(mean(errors^2))
  s = summary(model)
  rsq = round(as.numeric(s[3]),4)
  hgof = round(as.numeric(s[4]),4)
  return(as.data.frame(list("Rsquared" = rsq,"HarveyGOF" = hgof, "MAE" = round(mae,4), "RMSE"= round(rmse,4))))
}

m1.metrics.2 = get.metrics(model1)
m2.metrics.2 = get.metrics(model2)
m3.metrics.2 = get.metrics(model3)
m4.metrics.2 = get.metrics(model4)
m5.metrics.2 = get.metrics(model5)
m6.metrics.2 = get.metrics(model6)
all.df.2 = rbind(m1.metrics.2, m2.metrics.2, m3.metrics.2, m4.metrics.2, m5.metrics.2, m6.metrics.2)
row.names(all.df.2) = c('Model1', 'Model2', 'Model3', 'Model4', 'Model5', 'Model6')
cbind(all.df.2, all.df)


## Posterior Predictive Checks on Model 4 & 5

library(bayesplot)
library(dplyr)

## Some In-Package Plots
par(mfrow=c(2,2))

# 4 Whole
plot(pred4)
title("Figure 4.1")
lines(as.numeric(c(y.1,y.2), lty = 2, lwd = 2), col = "firebrick2")

# 4 Predictions
# One Option for Zooming In
plot(pred4, plot.original = 2)
title("Figure 4.2")
lines(y, col = "firebrick2")

# 5 Whole
plot(pred5, ylim = c(2.5, 4.5)) # 
title("Figure 4.3")
lines(as.numeric(c(y.1,y.2)), col = "firebrick2")

# 5 Predictions
# One Option for Zooming In
plot(pred5, plot.original = 2)
title("Figure 4.2")
lines(y, col = "firebrick2")















# Evaluate Coefficient (Model 5)
sum = summary(model5)[6]
kable(sum, digits = 4)

# Get 95% Credible Interval
lower = sum$coefficients[1,3] + qt(.025, 250)*sum$coefficients[1,4]
upper = sum$coefficients[1,3] - qt(.025, 250)*sum$coefficients[1,4]
# 95% HPD Interval
c(lower, sum$coefficients[1,3], upper)

# Evaluate Coefficient (Model 4)
sum = summary(model4)[6]
kable(sum, digits = 4)

# Get 95% Credible Interval
lower = sum$coefficients[1,3] + qt(.025, 250)*sum$coefficients[1,4]
upper = sum$coefficients[1,3] - qt(.025, 250)*sum$coefficients[1,4]
# 95% HPD Interval
c(lower, sum$coefficients[1,3], upper)




## Approach 2 -- CausalImpact Package
## ===============================================================================
# First, prepare dataset
pre.period <- c(1, 209)
post.period <- c(210, length(wk_full_ts))
y = wk_full_ts
y[post.period[1] : post.period[2]] <- NA
y.post = wk_full_ts[post.period[1] : post.period[2]]

# Build model
ss <- AddSemilocalLinearTrend(list(), y)
ss <- AddSeasonal(ss5, y, nseasons = 52)
model <- bsts(formula = y ~  wk_full_other, state.specification = ss, niter = 500)

# Feed bsts model to CausalImpact
impact <- CausalImpact(bsts.model = model,
                       post.period.response = y.post)

# Get summary
summary(impact)


plot(impact, "original") +
  coord_cartesian(xlim=c(200, 222)) + labs() +
  ggtitle("Figure 7: CausalImpact Analysis") +
  theme_classic()
  




