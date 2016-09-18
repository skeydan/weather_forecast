# loads ggplot2, tibble, tidyr, readr, purrr, and dplyr
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(gridExtra)
library(forecast)
library(stlplus)
library(tseries)

df <- read_csv('data/GlobalLandTemperaturesByCity.csv')
df <- rename(df, avg_temp = AverageTemperature, avg_temp_95p = AverageTemperatureUncertainty,
             city = City, country = Country, lat = Latitude, long = Longitude)
df

# explore dataframe
distinct(df, country) %>% arrange(country)
germany <- df %>% filter(country == 'Germany')

distinct(germany, city) %>% arrange(city) %>% print(n=81)
germany %>% group_by(city) %>% summarise(count = n(), avg = mean(avg_temp))

munich <- germany %>% filter(city == 'Munich')

munich1950 <- munich %>% filter(year(dt) > 1949)
munich1950 <- munich1950 %>% filter(!is.na(avg_temp))

#############################
# look for trend - visually #
#############################

p1950 <- ggplot(munich1950, aes(dt, avg_temp)) + geom_point() + xlab("") + ylab("avg monthly temp")
p1950 <- p1950 + theme_solarized_2()
p1950 <- p1950 + stat_smooth()
p1950

start_time <- as.Date("1992-01-01")
end_time <- as.Date("2013-08-01")
limits <- c(start_time,end_time)
p1992 <- p1950 + (scale_x_date(limits=limits))
p1992

grid.arrange(p1950, p1992, ncol=2)


#############################
#   construct timeseries    #
#############################

ts_1950 <- ts(munich1950$avg_temp, start = c(1950,1), end=c(2013,8), frequency = 12)

# To subset a ts object and preserve the date information use the window() function
ts_1992 <- window(ts_1950, start=c(1992,1))

plot(ts_1950)
plot(ts_1992)

# additional plots
monthplot(ts_1950)
seasonplot(ts_1950) 


#############################
#       decompose           #
#############################

# STL = Seasonal and Trend decomposition using Loess
# stlplus https://github.com/hafen/stlplus

# default for t.window: nextodd(ceiling((1.5*period) / (1-(1.5/s.window)))
# 21

fit1 <- stlplus(ts_1950, s.window="periodic")
fit2 <- stlplus(ts_1992, s.window="periodic")
grid.arrange(plot(fit1), plot(fit2), ncol = 2)

fit1 <- stlplus(ts_1950, s.window="periodic", t.window=51)
fit2 <- stlplus(ts_1992, s.window="periodic", t.window=51)
grid.arrange(plot(fit1), plot(fit2), ncol = 2)


#############################
# exponential smoothing     #
#############################

# exponential smoothing: Holt-Winters seasonal method
for (ts in list(ts_1950, ts_1992)) {
  
  fit <- hw(ts)
  print(fit$model)
  print(fit$mean)
  
  tsdisplay(ts)
  res <- fit$residuals
  tsdisplay(res)
  
  plot(fit,ylab="avg monthly temp", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
  lines(fitted(fit), col="blue", lty=2)
  lines(fit$mean, type="o", col="cyan")
  
  states <- cbind(fit$model$states[,1:3])
  colnames(states) <- c("level","slope","seasonal")
  plot(states, xlab="Year")
  
}


#############################
#            ets            #
#############################

# ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
#     gamma=NULL, phi=NULL, additive.only=FALSE, lambda=NULL,
#     lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98),
#     opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3,
#     bounds=c("both","usual","admissible"),
#     ic=c("aicc","aic","bic"), restrict=TRUE)

# The first letter denotes the error type ("A", "M" or "Z")
# the second letter denotes the trend type ("N","A","M" or "Z")
# and the third letter denotes the season type ("N","A","M" or "Z")
# In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected

# forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#          level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
#          npaths=5000, PI=TRUE, lambda=object$lambda, ...)

for (ts in list(ts_1950, ts_1992)) {
  fit <- ets(ts)
  summary(fit)
  plot(fit)
  plot(forecast(fit, h=36))
  accuracy(fit)
}

#############################
#        auto.arima         #
#############################


for (ts in list(ts_1950, ts_1992)) {
  # ADF
  # null hypothesis: non-stationary  
  print(adf.test(ts))
  # KPSS: test for level stationarity
  # null hypothesis: stationary  
  print(kpss.test(ts))
  # for non-seasonal ts
  print(ndiffs(ts))
  
  # for seasonal differencing
  print(nsdiffs(ts))
}

# p= order of the autoregressive part;
# d= degree of first differencing involved;
# q= order of the moving average part. 

# Series: ts 
# ARIMA(1,0,5)(0,0,2)[12] with non-zero mean 
# AIC=3696.94   AICc=3697.24   BIC=3743.33
#                        ME     RMSE      MAE      MPE     MAPE     MASE       ACF1
# Training set -0.001132392 2.675279 2.121494 23.30576 116.9652 1.136345 0.03740769
#
# ARIMA(1,0,3)(0,0,2)[12] with non-zero mean 
# AIC=1293.31   AICc=1293.88   BIC=1321.8
#                      ME     RMSE      MAE      MPE     MAPE     MASE        ACF1
# Training set 0.03144757 2.793657 2.264211 54.00268 198.7575 1.158018 0.007559646
#
# ARIMA(5,0,0) with non-zero mean 
# AIC=3341.93   AICc=3342.08   BIC=3374.4
#                        ME     RMSE     MAE      MPE     MAPE      MASE        ACF1
# Training set -0.003981795 2.129288 1.68801 1.361263 119.0598 0.9041562 -0.09655045
# 
# ARIMA(3,0,1) with non-zero mean 
# AIC=1137.59   AICc=1137.93   BIC=1158.96
#                        ME    RMSE      MAE      MPE     MAPE      MASE       ACF1
# Training set -0.005873045 2.08691 1.649352 42.94602 128.6583 0.8435524 -0.1386232

for (ts in list(ts_1950, ts_1992)) {
  # fit <- auto.arima(ts)
  fit <- auto.arima(ts, stepwise=FALSE, approximation=FALSE)
  summary(fit)
  #The forecast intervals for ARIMA models are based on assumptions that the residuals
  # are uncorrelated and normally distributed. If either of these are assumptions do not hold,
  #then the forecast intervals may be incorrect. 
  res <- fit$residuals
  acf(res)
  plot(density(res))
  plot(fit)
  plot(forecast(fit,h=36),include=80)
}

for (ts in list(ts_1950, ts_1992)) {
  tsdisplay(ts)
  tsdisplay(diff(ts,12))
  tsdisplay(diff(ts,1))
  tsdisplay(diff(diff(ts,12)))
}  