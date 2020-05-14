# Code written by Sachit Anil Kumar for ETF5231
# Student Number 29392624

# Loading all required packages / libraries
library(readxl)
library(tidyverse)
library(forecast)
library(ggplot2)
if(!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}

# Reading relevant data from excel file onto a time series object
wa.liquor.ts <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>% 
  pull("29392624") %>% ts(start = c(1982,4), frequency = 12)
# wa.liquor.ts

# Time Series Plot - Turnover Against Time
autoplot(wa.liquor.ts) + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA")
# Growing trend, increasing variance, large seasonality etc. observed
# Large slump in mid 2008 (GFC?)



################################################################################
################################# ASSIGNMENT 2 ################################# 
################################################################################

################################## QUESTION 1 ##################################


# Time Series Plot - Turnover Against Time
autoplot(wa.liquor.ts) + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA")
# Growing trend, increasing variance, large seasonality etc. observed
# Large slump in mid 2008 (GFC?)

# Seasonal Plot - Turnover Against Time
ggseasonplot(wa.liquor.ts, year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("Turnover ($mn)") + 
  ggtitle("Seasonal Plot: Turnover of Liquor Retailing in WA")
# Growing trend, large seasonality etc. observed

# Seasonal Polar Plot - Turnover Against Time
ggseasonplot(wa.liquor.ts, polar=TRUE) + 
  ylab("Turnover ($mn)") +
  ggtitle("Seasonal Polar Plot: Turnover of Liquor Retailing in WA")
# Growing trend, large seasonality etc. observed

# Seasonal Sub-Series Plot - Turnover Against Time
ggsubseriesplot(wa.liquor.ts) + 
  ylab("Turnover ($mn)") + 
  ggtitle("Seasonal Sub-Series Plot: Turnover of Liquor Retailing in WA")
# Growing trend, large seasonality etc. observed

# Autocorrelation Plot - ACF Against Number of Lags
ggAcf(wa.liquor.ts) +
  ggtitle("Autocorrelation Plot: Turnover of Liquor Retailing in WA")
# High correlation - Especially at lag 12 (1 yr)

# Lag Plot - Lagged Turnovers Against Turnovers
gglagplot(wa.liquor.ts, do.lines = FALSE) +
  ggtitle("Lag Plot: Turnover of Liquor Retailing in WA")
# High correlation - Especially at lag 12 (1 yr)

################################## QUESTION 2 ##################################

autoplot(wa.liquor.ts)

# Time Series Plot - Turnover Against Time - Square Root Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=1/2)) +
  ylab(paste("BoxCox(Turnover, 1/2)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Square Root Transform")

# Time Series Plot - Turnover Against Time - Cube Root Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=1/3)) +
  ylab(paste("BoxCox(Turnover, 1/3)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Cube Root Transform")

# Time Series Plot - Turnover Against Time - Log Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=0)) +
  ylab(paste("BoxCox(Turnover, 0)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Log Transform")

# Time Series Plot - Turnover Against Time - Inverse Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=-1)) +
  ylab(paste("BoxCox(Turnover, -1)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Inverse Transform")

# Optimal BoxCox lambda value
BoxCox.lambda(wa.liquor.ts)

# Time Series Plot - Turnover Against Time - Optimized Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=BoxCox.lambda(wa.liquor.ts))) +
  ylab(paste("BoxCox(Turnover, 0.013)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Optimized Transform")

################################## QUESTION 3 ##################################

# Split into Training & Test Sets
wa.liquor.ts.train1 <- window(wa.liquor.ts, end = c(2014,12))
wa.liquor.ts.test1 <- window(wa.liquor.ts, start = c(2015,1))

# Plotting both Training and Test Data Together
autoplot(wa.liquor.ts.train1, series = "Training") +
  autolayer(wa.liquor.ts.test1, series = "Test") + 
  ylab("Turnover ($mn)") +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA")

################################## QUESTION 4 ##################################

# Get Back-Transformed Seasonal Naive forecasts
wa.liquor.ts.snaive <- snaive(wa.liquor.ts.train1, 
                              h = length(wa.liquor.ts.test1), lambda = 0)

# Display forecasts in console
wa.liquor.ts.snaive$mean

# Plot the forecasts
autoplot(wa.liquor.ts.train1, series = "Training") +
  autolayer(wa.liquor.ts.test1, series = "Test") +
  autolayer(wa.liquor.ts.snaive, series = "Seasonal Naive", PI = F) +
  ylab("Turnover ($mn)") +
  xlab("Year") + 
  ggtitle("Forecasts of Liqour Retailing in WA using Seasonal Naive Method")

######

# Change end and start months so as not to have Dec as the last observation
wa.liquor.ts.train2 <- window(wa.liquor.ts, end = c(2014,11))
wa.liquor.ts.test2 <- window(wa.liquor.ts, start = c(2014,12))

# Get Back-Transformed Drift forecasts
wa.liquor.ts.drift <- rwf(wa.liquor.ts.train2, 
                          h = length(wa.liquor.ts.test2), drift = T, lambda = 0)

# Plot the forecasts
autoplot(wa.liquor.ts.train2, series = "Training") +
  autolayer(wa.liquor.ts.test2, series = "Test") +
  autolayer(wa.liquor.ts.drift, series = "Drift Method", PI = F) +
  ylab("Turnover ($mn)") +
  xlab("Year") + 
  ggtitle("Forecasts of Liqour Retailing in WA using Drift Method")


# Get different measures of fit and tabulate them
tab <- matrix(NA,ncol=4,nrow=2)
tab[1,] <- accuracy(wa.liquor.ts.snaive, wa.liquor.ts.test1)[2,c(2,3,5,6)]
tab[2,] <- accuracy(wa.liquor.ts.drift, wa.liquor.ts.test2)[2,c(2,3,5,6)]

colnames(tab) <- c("RMSE","MAE","MAPE","MASE")
rownames(tab) <- c("Seasonal Naive Method", "Drift Method")
knitr::kable(tab, digits=2, booktabs=TRUE)
# Seasonal Naive Chosen

################################## QUESTION 5 ##################################

# Display plots to check assumptions of residuals
# Conduct Ljung-Box Test of Autocorrelation
checkresiduals(wa.liquor.ts.snaive)

################################## QUESTION 6 ##################################

# Get Seasonal Naive Forecasts for 2017-2018 
wa.liquor.ts.snaive.fc <- snaive(wa.liquor.ts, h = 24, lambda = 0)

wa.liquor.ts.snaive.fc$mean

# Plotting Forecasts with Observed Data
autoplot(wa.liquor.ts, series = "Observed Sales") +
  autolayer(wa.liquor.ts.snaive.fc, series = "Forecasted Sales", PI = F) +
  ylab("Turnover ($mn)") +
  xlab("Year") + 
  ggtitle("Forecasts of Liqour Retailing in WA using Seasonal Naive Method")



################################################################################
################################# ASSIGNMENT 3 ################################# 
################################################################################

################################## QUESTION 1 ##################################

# Time Series Plot - Turnover Against Time
autoplot(wa.liquor.ts) + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA")
# Growing Trend with Increasing Slope
# Increasing Variance & Large Seasonality - Multiplicative Seasonality?

# Deecomposing the Time Series and Plotting for Clarity
wa.liquor.decomp <- stl(wa.liquor.ts, s.window = 15)
autoplot(wa.liquor.decomp) +
  ggtitle("Turnover of Liquor Retailing in WA - Decomposed")
# Growing Trend with Increasing Slope - Additive Trend
# Increasing Variance & Large Seasonality - Multiplicative Seasonality?
# Increasing Errors - Multiplicative Errors?

################################## QUESTION 2 ##################################

# Fitting MAM Model
wa.liquor.fit.mam <- ets(wa.liquor.ts, model = "MAM", damped = FALSE)
summary(wa.liquor.fit.mam)

# Plotting the Model
autoplot(wa.liquor.fit.mam) + 
  ggtitle("Turnover of Liquor Retailing in WA - Modeled using MAM ETS Model")

################################## QUESTION 3 ##################################

# Plotting Residuals from MAM Model
checkresiduals(wa.liquor.fit.mam)

################################## QUESTION 4 ##################################

# Letting R Choose an Appropriate ETS Model 
wa.liquor.fit.auto <- ets(wa.liquor.ts)
summary(wa.liquor.fit.auto)

################################## QUESTION 5 ##################################

# Fitting MAdM Model
wa.liquor.fit.madm <- ets(wa.liquor.ts, model = "MAM", damped = TRUE)
summary(wa.liquor.fit.madm)

# Plotting the Model
autoplot(wa.liquor.fit.madm) + 
  ggtitle("Turnover of Liquor Retailing in WA - Modeled using MAdM ETS Model")

# Checking Residuals of MAdM model
checkresiduals(wa.liquor.fit.madm)

################################## QUESTION 6 ##################################

# Forecasting using MAM Model
wa.liquor.fc.mam <- forecast(wa.liquor.fit.mam, h = 24)

# wa.liquor.fc.mam

# Plotting the Forecasts
autoplot(wa.liquor.ts, series = "Observed") +
  autolayer(wa.liquor.fc.mam, series = "Forecasts using (M,A,M) Model") + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Forecasted using (M,A,M) Model")

################################## QUESTION 7 ##################################

# Splitting Away the Last Few Years
wa.liquor.ts.recent <- window(wa.liquor.ts, start = c(2012,1))

# Forecasting using MAdM Model
wa.liquor.fc.madm <- forecast(wa.liquor.fit.madm, h = 24)

# Plotting both Forecasts Along with Recent Data
autoplot(wa.liquor.ts.recent, series = "Observed Recent Data") +
  autolayer(wa.liquor.fc.mam, series = "Forecasts using (M,A,M) Model", PI = F) + 
  autolayer(wa.liquor.fc.madm, series = "Forecasts using (M,Ad,M) Model", PI = F) + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Comparing Forecasts")

# Plotting MAM Forecasts Along with Recent Data
plot1 <- autoplot(wa.liquor.ts.recent, series = "Observed Recent Data") +
  autolayer(wa.liquor.fc.mam, series = "Forecasts using (M,A,M) Model") + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Forecasted using (M,A,M) Model") +
  theme(legend.position = "bottom") +
  ylim(50,250)

# Plotting MAdM Forecasts Along with Recent Data
plot2 <- autoplot(wa.liquor.ts.recent, series = "Observed Recent Data") +
  autolayer(wa.liquor.fc.madm, series = "Forecasts using (M,Ad,M) Model") + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Forecasted using (M,Ad,M) Model") +
  theme(legend.position = "bottom") +
  ylim(50,250)

# Plotting Both MAM and MAdM Forecasts Side by Side
grid.arrange(plot1, plot2, ncol=2)



################################################################################
################################# ASSIGNMENT 4 ################################# 
################################################################################

################################## QUESTION 1 ##################################

# Optimal BoxCox lambda value
BoxCox.lambda(wa.liquor.ts)    # 0.013

# Time Series Plot - Turnover Against Time - Log Transformation
autoplot(BoxCox(wa.liquor.ts,lambda=0)) +
  ylab(paste("BoxCox(Turnover, 0)")) +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Log Transform")

# Doing Seasonal Differencing
BoxCox(wa.liquor.ts,lambda=0) %>% nsdiffs
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% autoplot() + 
  ylab("Turnover -> Log Transform -> Seasonal Differencing") +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA -> Log Transform -> Seasonal Differencing")

# Checking Stationarity
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% ggtsdisplay() 

# Doing First-Order Differencing after Seasonal
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% ndiffs()
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% diff() %>% autoplot() +
  ylab("Turnover -> Log Transform -> Seasonal Differencing -> First-Order Differencing") +
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA -> Log Transform -> Seasonal & First-Order Differencing")

# Checking Stationarity
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% diff() %>% ggtsdisplay() +
  ggtitle("ACF and PACF of stationary data")
# Now stationary

#####

wa.liquor.ts %>% diff(12) %>% diff() %>% ggtsdisplay()

#####

################################## QUESTION 2 ##################################

# ACF and PACF for Appropriate ARIMA Model
BoxCox(wa.liquor.ts,lambda=0) %>% diff(12) %>% diff() %>% ggtsdisplay() 
# ACF too messy
# PACF - 2 spikes at the top - AR(2)
# PACF - Either 1 or 3 seasonal spikes - AR(1) or AR(3) --- choose later

# 2 possible models
fit1 <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,0), 
               seasonal=c(1,1,0), include.constant = T)
fit2 <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,0), 
               seasonal=c(3,1,0), include.constant = T)

fit1 %>% summary()
fit2 %>% summary() # lower AICc - choose this

################################## QUESTION 3 ##################################

# Check White Noise-ness of Residuals 
fit1 %>% checkresiduals()   # Not WNsy at all
fit1 %>% summary()

# Check White Noise-ness of Residuals 
fit2 %>% checkresiduals()   # Not WNsy at all
fit2 %>% summary()

################################## QUESTION 4 ##################################

# fit.a = fit2 
# Plus four possible alternatives
fit.a <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,0), seasonal=c(3,1,0))
fit.b <- Arima(wa.liquor.ts, lambda=0, order=c(3,1,0), seasonal=c(3,1,0), include.constant = T)
fit.c <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,0), seasonal=c(3,1,1), include.constant = T)
fit.d <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,0), seasonal=c(3,1,2), include.constant = T)
fit.e <- Arima(wa.liquor.ts, lambda=0, order=c(2,1,1), seasonal=c(3,1,2), include.constant = T)

summary(fit.a)
summary(fit.b)
summary(fit.c)
summary(fit.d)
summary(fit.e)

# check residuals of all models
fit.d %>% checkresiduals()

fit.a %>% checkresiduals()
fit.b %>% checkresiduals()
fit.c %>% checkresiduals()
fit.e %>% checkresiduals()

################################## QUESTION 5 ##################################

# Run auto.arima for R to find best suitable
auto.arima(wa.liquor.ts, lambda = 0) -> fit.auto

fit.auto %>% summary() #(1,0,3)(1,1,2)[12] -1133.75
fit.auto %>% checkresiduals()

################################## QUESTION 6 ##################################

# Run auto.arima for R to find best suitable - all considered
auto.arima(wa.liquor.ts, lambda = 0, stepwise=FALSE, approximation=FALSE) -> fit.auto.all

fit.auto.all %>% summary() #(1,0,3)(0,1,1)[12] -1133.91
fit.auto.all %>% checkresiduals()

################################## QUESTION 7 ##################################

# Using test set to find RMSE
getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc<-forecast(fit,h=h)
  return(round(accuracy(fc,test)[2,"RMSE"],4))
}

# Get RMSE of three models considered
getrmse(wa.liquor.ts,h=24,order=c(2,1,0),seasonal=c(3,1,2),include.constant = T,lambda=0)
getrmse(wa.liquor.ts,h=24,order=c(1,0,3),seasonal=c(1,1,2),include.constant = T,lambda=0)
getrmse(wa.liquor.ts,h=24,order=c(1,0,3),seasonal=c(0,1,1),include.constant = T,lambda=0)

################################## QUESTION 8 ##################################

# Making forecasts and plotting them
wa.liquor.fc.arima.d <- fit.d %>% forecast(h=24)
wa.liquor.fc.arima.d %>% autoplot() +
  ylab("Turnover ($mn)") +
  xlab("Year") + 
  ggtitle("Turnover Forecasts using ARIMA(2,1,0)(3,1,2)[12] Model")

################################## QUESTION 9 ##################################

# Read entire data
wa.liquor.ts.full <- read_xlsx("RetailDataIndividualFull.xlsx", skip=3) %>% 
  pull("29392624") %>% ts(start = c(1982,4), frequency = 12)

autoplot(wa.liquor.ts.full) + 
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Turnover of Liquor Retailing in WA - Full")

################################## QUESTION 10 #################################

# ASSG 2 - Seasonal Naive - wa.liquor.ts.snaive.fc
# ASSG 3 - ETS(M,A,M) - wa.liquor.fc.mam
# ASSG 4 - ARIMA(2,1,0)(3,1,2)[12] with drift - wa.liquor.fc.arima.d
# wa.liquor.ts.recent

autoplot(wa.liquor.ts.recent, series = "Observed") +
  autolayer(wa.liquor.ts.snaive.fc, PI=F, series="Seasonal Naive") +
  autolayer(wa.liquor.fc.mam, PI=F, series="ETS(M,A,M)") +
  autolayer(wa.liquor.fc.arima.d, PI=F, series="ARIMA(2,1,0)(3,1,2)[12] with Drift") +
  guides(colour=guide_legend(title = "Forecast Model")) +
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Forecasts from Different Models along with Recent Data")

################################## QUESTION 11 #################################

# 2017-2018 data
wa.liquor.test <- wa.liquor.ts.full %>% window(start = c(2017,1))

# Get different measures of fit and tabulate them
tab <- matrix(NA,ncol=3,nrow=3)
tab[1,] <- accuracy(wa.liquor.ts.snaive.fc, wa.liquor.test)[2,c(2,5,6)]
tab[2,] <- accuracy(wa.liquor.fc.mam, wa.liquor.test)[2,c(2,5,6)]
tab[3,] <- accuracy(wa.liquor.fc.arima.d, wa.liquor.test)[2,c(2,5,6)]

colnames(tab) <- c("RMSE","MAPE","MASE")
rownames(tab) <- c("Seasonal Naive", "ETS(M,A,M)", "ARIMA(2,1,0)(3,1,2)[12] with Drift")
knitr::kable(tab, digits=2, booktabs=TRUE)

################################## QUESTION 12 #################################

# Make same ARIMA model with entire data
fit.arima.full <- Arima(wa.liquor.ts.full, lambda=0, order=c(2,1,0), 
                        seasonal=c(3,1,2), include.constant = T)

# Forecast using that model
wa.liquor.fc.full <- fit.arima.full %>% forecast(h=24, level=80)

# Recent Data
wa.liquor.full.recent <- wa.liquor.ts.full %>% window(start = c(2014,1))

# Final Plot
autoplot(wa.liquor.full.recent, series = "Observed Recent") +
  autolayer(wa.liquor.fc.full,series = "Forecasted", PI = T) +
  ylab("Turnover ($mn)") + 
  xlab("Year") + 
  ggtitle("Forecasts for 2019-2020 along with Recent Data")
  