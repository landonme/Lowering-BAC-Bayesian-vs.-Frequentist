





## Posterior Predictive Checks


library(bayesplot)
library(dplyr)


# ## Cut dataset.
# y.1 = wk_full_ts[1:150] # Used to train models
# y.2 = wk_full_ts[151:209] # Used to compare models



## Posterior Predictive Checks on Holdout
## ==============================================

### Not ono-step ahead
pred4 <- predict(model4_pre, horizon = dim(as.matrix(y.2))[1], burn = SuggestBurn(.1, model4_pre), quantiles = c(0.15, 0.85))
pred_dist4 = pred4$distribution
ppc_dens_overlay(y.2, pred_dist4)

pred5 <- predict(model5_pre, horizon = dim(as.matrix(y.2))[1], burn = SuggestBurn(.1, model5_pre))
pred_dist5 = pred5$distribution
ppc_dens_overlay(y.2, pred_dist5[50:70,])

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


## Some In-Package Plots
par(mfrow=c(1,2))

# 4 Predictions
# One Option for Zooming In

plot(pred4, plot.original = 5, ylab = "Log DUI collisions", xlab = "week #")
title("Figure 4.1: Model 4", font.main= 1)
lines(y, col = "firebrick2")

# 5 Predictions
# One Option for Zooming In
plot(pred5, plot.original = 5, ylab = NA, xlab = "week #")
title("Figure 4.2: Model 5", font.main= 1)
lines(y, col = "firebrick2")

#title("Figure 4", outer = TRUE, font.main = 2)



# Another Option for Zooming in
PlotDynamicDistribution(pred_dist5, quantile.step = 0.001, ylim = c(2.5, 4.5))
lines(y.2, col = "firebrick2")


### Testing Theories about the posterior predictive probability
# 1. Come up with a test statistic T that has power to diagnose
# violations of whatever assumption you are testing.
# 2. Calculate T for the observed data y: T(y)
# 3. Calculate T for each y
# rep draw from the posterior predictive
# distribution: T(y
#                 rep|y)
# 4. Calculate the fraction of times T(y
#                                      rep|y) > T(y). This is an
# estimate of the posterior predictive p-value.




### Same Thing, but we'll do one-step ahead errors on the holdout.
bsts.holdout.prediction.errors(model5_pre, y.2, burn = 1)






## Residual Plots
## ==============================================
par(mfrow = c(2,2))
## Residual Plots
r4 <- residuals(model4_pre)
r5 <- residuals(model5_pre)


#par(mfrow = c(1,2))
qqdist(r4)   ## A bit of departure in the upper tail
title("Figure 5.1: Model 4")
qqdist(r5)   ## A bit of departure in the upper tail
title("Figure 5.2: Model 5")
AcfDist(r4, ylim = c(-0.3, 0.3))
title("Figure 5.3: Model 4")
AcfDist(r5, ylim = c(-0.3, 0.3))  
title("Figure 5.4: Model 5")




mtext(side=3, line=3, at=-0.07, adj=0, cex=1, mytitle)



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

## GET PPD
ppd4 = get_pp(model4, wk_full_ts)
ppd5 = get_pp(model5, wk_full_ts)


# Dense Overlay
par(mfrow = c(2,1))
dense.4 = ppc_dens_overlay(y, ppd4)
dense.5 = ppc_dens_overlay(y, ppd5)
bayesplot_grid(dense.4, dense.5)


ppc_boxplot(y, ppd5[300:308,])
ppc_dens(y, ppd5[40:42,])
ppc_freqpoly(y, ppd5[40:42,])



library(gridExtra)
p4 = ppc_error_scatter_avg_vs_x(y, ppd4, x = seq(1:length(y))) + ggtitle("Figure 4.5: Model 4") + xlab("week #") + theme_classic()
p5 = ppc_error_scatter_avg_vs_x(y, ppd5, x = seq(1:length(y))) + ggtitle("Figure 4.6: Model 5") + xlab("week #") + theme_classic()
grid.arrange(p4, p5, ncol = 1)






ppc_stat(y, ppd5, stat = "mean")
ppc_stat(y, ppd4, stat = "mean")

ppc_intervals(y, ppd5)
ppc_ribbon(y, ppd5)






## Try plotting simulated RMSEs

# calculate posterior of rmse values

rmse <- function(y,yhat){sqrt(mean((yhat-y)^2))}
mae <- function(y,yhat){mean(abs(yhat-y))}
rmse_dist_5 =  apply(ppd5,1,rmse,y=y)
mae.5 =  apply(ppd5,1,mae,y=y)
rmse_dist_4 =  apply(ppd4,1,rmse,y=y)
mae.4 =  apply(ppd4,1,mae,y=y)



rd4 = data.frame(rmse_dist_4)
names(rd4) = c("rmse")
rd4$Model = "Model 4"
rd4$mae = mae.4
rd5 = data.frame(rmse_dist_5)
names(rd5) = c("rmse")
rd5$Model = "Model 5"
rd5$mae = mae.5
rdc = rbind(rd4, rd5)

ggplot(rdc,aes(x=rmse)) + 
  geom_histogram(data=subset(rdc,Model == "Model 4"),aes(fill = Model), alpha = 0.5) +
  geom_histogram(data=subset(rdc,Model == "Model 5"),aes(fill = Model), alpha = 0.5) +
  scale_fill_discrete("") +
  ggtitle("Figure 6: One-Step-Ahead RMSE") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

  
ggplot(rdc,aes(x=mae)) + 
  geom_histogram(data=subset(rdc,Model == "Model 4"),aes(fill = Model), alpha = 0.5) +
  geom_histogram(data=subset(rdc,Model == "Model 5"),aes(fill = Model), alpha = 0.5) +
  scale_fill_discrete("") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))








dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(dat,aes(x=xx)) + 
  geom_histogram(data=subset(dat,yy == 'a'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'b'),fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)



















