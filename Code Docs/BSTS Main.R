

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

#==============================================================
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

#   cbind(wk_full_ts, lag1, lag2, lag3)

#  AR Components to state space
#  ss <- AddAr(list(), lags = 3, sigma.prior = SdPrior(3.0, 1.0))


#ss <- AddAr(list(), lags = 3, y = wk_full_ts)


##  Also, ss <- AddLocalLevel(list(), y)


## Build Multiple Models and Use CompareBSTSModels to determine best..

## Build Models and get an out of sample evaluation.


## LocalLinear and Seasonality
ss <- AddLocalLinearTrend(list(), wk_full_ts)
#ss <- AddStaticIntercept(ss, wk_full_ts)
ss <- AddSeasonal(ss, wk_full_ts, nseasons = 52)
model1 <- bsts(formula = wk_full_ts ~  wk_post_ts,
              state.specification = ss, niter = 1000)


## Seasonal Only
#ss2 <- AddStaticIntercept(list(), wk_full_ts)
ss2 <- AddSeasonal(list(), wk_full_ts, nseasons = 52)
model2 <- bsts(formula = wk_full_ts ~  wk_post_ts,
               state.specification = ss2, niter = 1000)


## Trend Only
ss3 <- AddLocalLinearTrend(list(), wk_full_ts)
#ss3 <- AddStaticIntercept(ss3, wk_full_ts)
model3 <- bsts(formula = wk_full_ts ~  wk_post_ts,
               state.specification = ss3, niter = 1000)

## AddSemilocalLinearTrend
ss4 <- AddSemilocalLinearTrend(list(), wk_full_ts)
model4 <- bsts(formula = wk_full_ts ~  wk_post_ts,
               state.specification = ss4, niter = 1000)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), wk_full_ts)
ss5 <- AddSeasonal(ss5, wk_full_ts, nseasons = 52)
model5 <- bsts(formula = wk_full_ts ~  wk_post_ts,
               state.specification = ss5, niter = 1000)

CompareBstsModels(list("Trend and Seasonal" = model1,
                       "Seasonal Only" = model2,
                       "trend Only" = model3))


# Get Mean Absolute Errors:
get.error.metrics = function(model){
  errors = bsts.prediction.errors(model)$in.sample
  mae = mean(abs(errors))
  rmse = sqrt(mean(errors^2))
  return(as.data.frame(list("MAE" = round(mae,4), "RMSE"= round(rmse,4))))
  }

m1.metrics = get.error.metrics(model1)
m2.metrics = get.error.metrics(model2)
m3.metrics = get.error.metrics(model3)
m4.metrics = get.error.metrics(model4)
m5.metrics = get.error.metrics(model5)
rbind(m1.metrics, m2.metrics, m3.metrics, m4.metrics, m5.metrics)
  # Model 4 is the best.





### Get Posterior Predictive Probability Graphs..
# ===============================================
library(bayesplot)
color_scheme_set("brightblue")

y = as.vector(wk_full_ts)

get_pp = function(model, series){
  err = model$one.step.prediction.errors
  dims = dim(err)
  # Fill matrix to get predictions from errors
  ppd = matrix(nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      ppd[i,j] = series[j] + err[i,j]
    }
  }
  return(ppd)
}

# Model 5
ppd5 = get_pp(model5, wk_full_ts)
ppc_dens_overlay(y, ppd5)

# Model 4
ppd4 = get_pp(model4, wk_full_ts)
ppc_dens_overlay(y, ppd4)






### Get out of sample evaluation metrics..
### ===============================================================

## Cut dataset.
y.1 = wk_full_ts[1:199]
y.2 = wk_full_ts[200:209]

## LocalLinear and Seasonality
ss <- AddLocalLinearTrend(list(), wk_full_ts)
#ss <- AddStaticIntercept(ss, wk_full_ts)
ss <- AddSeasonal(ss, wk_full_ts, nseasons = 52)
model1 <- bsts(y.1,
               state.specification = ss, niter = 1000)


## Seasonal Only
#ss2 <- AddStaticIntercept(list(), wk_full_ts)
ss2 <- AddSeasonal(list(), wk_full_ts, nseasons = 52)
model2 <- bsts(y.1,
               state.specification = ss2, niter = 1000)

## Trend Only
ss3 <- AddLocalLinearTrend(list(), wk_full_ts)
#ss3 <- AddStaticIntercept(ss3, wk_full_ts)
model3 <- bsts(y.1,
               state.specification = ss3, niter = 1000)

## AddSemilocalLinearTrend
ss4 <- AddSemilocalLinearTrend(list(), wk_full_ts)
model4 <- bsts(y.1,
               state.specification = ss4, niter = 1000)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), wk_full_ts)
ss5 <- AddSeasonal(ss5, wk_full_ts, nseasons = 52)
model5 <- bsts(y.1,
               state.specification = ss5, niter = 1000)


# Get Mean Absolute Errors:
get.error.metrics.oos = function(model){
  preds = predict(model, horizon = 10)
  errors = preds$mean - y.2
  mae = mean(abs(errors))
  rmse = sqrt(mean(errors^2))
  return(as.data.frame(list("MAE" = round(mae,2), "RMSE"= round(rmse,2))))
}

m1.metrics = get.error.metrics.oos(model1)
m2.metrics = get.error.metrics.oos(model2)
m3.metrics = get.error.metrics.oos(model3)
m4.metrics = get.error.metrics.oos(model4)
m5.metrics = get.error.metrics.oos(model5)
all.df = rbind(m1.metrics, m2.metrics, m3.metrics, m4.metrics, m5.metrics)
row.names(all.df) = c('Model1', 'Model2', 'Model3', 'Model4', 'Model5')
all.df
# Model 4 is the best.

summary(model4)

# Let's see if it's better with or without the binary
ss5 <- AddSemilocalLinearTrend(list(), wk_full_ts)
ss5 <- AddSeasonal(ss5, wk_full_ts, nseasons = 52)
model5 <- bsts(wk_full_ts ~ wk_post_ts, state.specification = ss5, niter = 1000)
model5.2 <- bsts(wk_full_ts, state.specification = ss3, niter = 1000)

CompareBstsModels(list("Without Binary" = model5, "with Binary" = model5.2))
m5.metrics = get.error.metrics(model3.1)
m5.2.metrics = get.error.metrics(model3.2)
rbind(m3.1.metrics, m3.2.metrics)


# Get Inclusion Probabilities. This tells us the likelihood the variable should be in the model..
plot(model3.2, "coef")

# And some other things.
plot(model5)
plot(model5, "components")
plot(model5, "coefficients")
plot(model5, "predictors")


summary(model5)[3]
summary(model5)[4]

summary(model4)

## Causal Impact Analysis
## ======================================================================
library(CausalImpact)

## first, I'll us just the series.
pre.period <- c(1, 209)
post.period <- c(210, length(wk_full_ts))
y = log(wk_full_ts)
y[post.period[1] : post.period[2]] <- NA
y.post = log(wk_full_ts[post.period[1] : post.period[2]])


ss <- AddSemilocalLinearTrend(list(), y)
ss <- AddSeasonal(ss5, y, nseasons = 52)
model <- bsts(formula = y, state.specification = ss, niter = 500)
impact <- CausalImpact(bsts.model = model,
                       post.period.response = y.post)

summary(impact, "report")
plot(impact)





## Now, try with a control.

## Trend Only
ss <- AddLocalLinearTrend(list(), wk_full_ts)
ss <- AddStaticIntercept(ss, wk_full_ts)
model4 <- bsts(formula = y ~  wk_full_other,
               state.specification = ss, niter = 500)

summary(model4)
plot(model4, "coef")


impact <- CausalImpact(bsts.model = model4,
                       post.period.response = y.post)

  

plot(impact)
summary(impact)
summary(impact, "report")












## Appendix of stuff...
## =================================================

### Predict
#p <- predict.bsts(model, horizon = 12, burn = burn, quantiles = c(.025, .975))

### Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(10^as.numeric(-colMeans(model$one.step.prediction.errors[-(1:burn),])+y),  
    10^as.numeric(p$mean)),
  # actual data and dates 
  as.numeric(AirPassengers),
  as.Date(time(AirPassengers)))
names(d2) <- c("Fitted", "Actual", "Date")

### MAPE (mean absolute percentage error)
MAPE <- filter(d2, year(Date)>1959) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

### 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  10^as.numeric(p$interval[1,]),
  10^as.numeric(p$interval[2,]), 
  subset(d2, year(Date)>1959)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")






### Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
  b <- b[abs(b) > 0]
  if (length(b) > 0) 
    return(mean(b))
  return(0)
}




### Get the average coefficients when variables were selected (non-zero slopes)
coeff <- data.frame(melt(apply(model$coefficients[-(1:burn),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
ggplot(data=coeff, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  xlab("") + ylab("") + ggtitle("Average coefficients")

colMeans(model$coefficients)

### Inclusion probabilities -- i.e., how often were the variables selected 
inclusionprobs <- melt(colMeans(model$coefficients[-(1:burn),] != 0))
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")




















