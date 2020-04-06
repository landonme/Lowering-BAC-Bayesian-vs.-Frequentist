


library(bayesplot)


## Cut dataset.
y.1 = wk_full_ts[1:150] # Used to train models
y.2 = wk_full_ts[151:209] # Used to compare models


## AddSemilocalLinearTrend
ss4 <- AddSemilocalLinearTrend(list(), y.1)
model4_pre <- bsts(y.1,
                   state.specification = ss4, niter = 500)

## AddSemilocalLinearTrend
ss5 <- AddSemilocalLinearTrend(list(), y.1)
ss5 <- AddSeasonal(ss5, y.1, nseasons = 52)
model5_pre <- bsts(y.1,
                   state.specification = ss5, niter = 500)


pred4 <- predict(model4_pre, horizon = dim(as.matrix(y.2))[1], burn = 100, quantiles = c(0.15, 0.85))
pred_dist4 = pred4$distribution
ppc_dens_overlay(y.2, pred_dist4)

pred5 <- predict(model5_pre, horizon = dim(as.matrix(y.2))[1], burn = 1)
pred_dist5 = pred5$distribution
ppc_dens_overlay(y.2, pred_dist5)

## Some In-Package Plots
plot(pred4, xlim(150,200))
lines(as.numeric(c(y.1,y.2), lty = 2, lwd = 2), col = "firebrick2")

plot(pred5, ylim = c(2.5, 4.5)) # 
lines(as.numeric(c(y.1,y.2)), col = "firebrick2")

PlotDynamicDistribution(pred_dist5, quantile.step = 0.001, ylim = c(2.5, 4.5))
lines(y.2, col = "firebrick2")



## Residual Plots
par(mfrow = c(2,2))

r5 <- residuals(model5_pre)
#par(mfrow = c(1,2))
qqdist(r5)   ## A bit of departure in the upper tail
AcfDist(r5, ylim = c(-0.3, 0.3))  

## Residual Plots
r4 <- residuals(model4_pre)
qqdist(r4)   ## A bit of departure in the upper tail
AcfDist(r4, ylim = c(-0.3, 0.3))





### Get Posterior Predictive Probability Graphs..
# ===============================================

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
