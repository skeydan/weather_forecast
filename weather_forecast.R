library(tidyverse)
library(lubridate)
library(scales)
library(nlme)
library(forecast)
library(tseries)
library(gridExtra)
library(stlplus)

###############################################################################################

# daily data
# tibbles
daily_1997_2015 <- read_csv('data/munich_1997_2015.csv')
daily_2016 <- read_csv('data/munich_2016.csv')

ggplot(daily_1997_2015, aes(day, mean_temp)) + geom_point() + xlab("") + ylab("temperature (daily average)") +
  scale_x_date(labels=date_format("%b %y")) + stat_smooth()

# time series
daily_1997_2015_ts <- ts(daily_1997_2015$mean_temp, start = c(1997,1), frequency = 365)
plot(daily_1997_2015_ts)

# monthly data
# tibbles
daily_1997_2015$month <- floor_date(daily_1997_2015$day, "month")
monthly_1997_2015 <- daily_1997_2015 %>% group_by(month) %>% summarise(mean_temp = mean(mean_temp))
monthly_1997_2015

ggplot(monthly_1997_2015, aes(month, mean_temp)) + geom_point() + xlab("") + ylab("temperature (daily average)") +
  scale_x_date(labels=date_format("%b %y")) + stat_smooth()

# time series
monthly_1997_2015_ts <- ts(monthly_1997_2015$mean_temp, start = c(1997,1), frequency = 12)
plot(monthly_1997_2015_ts)

monthplot(monthly_1997_2015_ts)
ggmonthplot(monthly_1997_2015_ts)

seasonplot(monthly_1997_2015_ts) 
ggseasonplot(monthly_1997_2015_ts) 

#############################
#       decompose           #
#############################

fit1 <- stlplus(monthly_1997_2015_ts, s.window="periodic")
fit2 <- stlplus(monthly_1997_2015_ts, s.window="periodic", t.window=51)
grid.arrange(plot(fit1), plot(fit2), ncol = 2)

#############################
# exponential smoothing     #
#############################

fit <- hw(monthly_1997_2015_ts)
print(fit$model)
print(fit$mean)

tsdisplay(monthly_1997_2015_ts)
res <- fit$residuals
tsdisplay(res)

plot(fit,ylab="avg monthly temp", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit), col="blue", lty=2)
lines(fit$mean, type="o", col="cyan")

states <- cbind(fit$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal")
plot(states, xlab="Year")


#############################
#            ets            #
#############################

fit <- ets(monthly_1997_2015_ts)
summary(fit)
plot(fit)
plot(forecast(fit, h=36))
accuracy(fit)


#############################
#        auto.arima         #
#############################

# ADF
# null hypothesis: non-stationary  
adf.test(na.exclude(monthly_1997_2015_ts))
# KPSS: test for level stationarity
# null hypothesis: stationary  
kpss.test(na.exclude(monthly_1997_2015_ts))
# for non-seasonal ts
ndiffs(monthly_1997_2015_ts)
  
# for seasonal differencing
nsdiffs(monthly_1997_2015_ts)

fit <- auto.arima(monthly_1997_2015_ts)
#ARIMA(0,0,4)(0,0,2)[12] with zero mean     
#AIC=1147.72   AICc=1148.22   BIC=1171.72
#                   ME     RMSE      MAE      MPE    MAPE     MASE      ACF1
#Training set 1.275131 3.059881 2.417012 10.06209 85.7372 1.283957 -0.141014

fit <- auto.arima(monthly_1997_2015_ts, stepwise=FALSE, approximation=FALSE)
#ARIMA(3,0,1) with non-zero mean 
# AIC=964.86   AICc=965.24   BIC=985.43
#                      ME     RMSE      MAE      MPE     MAPE      MASE       ACF1
#Training set -0.03199412 2.053964 1.634621 20.44746 69.55984 0.8683374 -0.1041259
summary(fit)

#The forecast intervals for ARIMA models are based on assumptions that the residuals
# are uncorrelated and normally distributed. If either of these are assumptions do not hold,
#then the forecast intervals may be incorrect. 
res <- fit$residuals
acf(na.exclude(res))
plot(density(res))
plot(fit)
plot(forecast(fit,h=36),include=80)

tsdisplay(ts)
tsdisplay(diff(ts,12))
tsdisplay(diff(ts,1))
tsdisplay(diff(diff(ts,12)))
  

###############################################################################################

# lm, trend only
t <- time(monthly_temp_1997_2015)
c <- cycle(monthly_temp_1997_2015)
lm_1997_2015_monthly <- lm(monthly_temp_1997_2015 ~ t)

summary(lm_1997_2015_monthly)
confint(lm_1997_2015_monthly)
AIC(lm_1997_2015_monthly)

plot(resid(lm_1997_2015_monthly))
acf(resid(lm_1997_2015_monthly))
pacf(resid(lm_1997_2015_monthly))

# glm
gls_1997_2015_monthly <-gls(monthly_temp_1997_2015 ~ t, na.action = na.exclude, cor = corAR1(0.8))
summary(gls_1997_2015_monthly)
confint(gls_1997_2015_monthly)
AIC(gls_1997_2015_monthly)

# lm with seasons
t <- time(monthly_temp_1997_2015)
c <- cycle(monthly_temp_1997_2015)
lm_1997_2015_monthly_s <- lm(monthly_temp_1997_2015 ~ 0 + t + factor(c))
summary(lm_1997_2015_monthly_s)
confint(lm_1997_2015_monthly_s)
AIC(lm_1997_2015_monthly_s)

# predict
new_time <- seq(2016, len= 2*12, by = 1/12)
new_df <- tibble(time = new_time, season = rep(1:12, 2))
predicted <- predict(lm_1997_2015_monthly_s, new_df)[1:24]

# harmonic model
SIN <- COS <- matrix(nr = length(monthly_temp_1997_2015), nc=6)  
for (i in 1:6) {
  COS[,i] <-cos(2 * pi * i * time(monthly_temp_1997_2015))
  SIN[,i] <-sin(2 * pi * i * time(monthly_temp_1997_2015))
}  

t <- (time(monthly_temp_1997_2015) -  mean(time(monthly_temp_1997_2015))) / sd(time(monthly_temp_1997_2015))

lm_1997_2015_monthly_s_h <- lm (monthly_temp_1997_2015 ~ t + I(t^2) + COS[,1]  + COS[,2]  + COS[,3]  + COS[,4] +
                                  COS[,5]  + COS[,6] + SIN[,1]  + SIN[,2]  + SIN[,3]  + SIN[,4] + SIN[,5]  + SIN[,6])
summary(lm_1997_2015_monthly_s_h)
AIC(lm_1997_2015_monthly_s_h)

lm_1997_2015_monthly_s_h <- lm (monthly_temp_1997_2015 ~ t + I(t^2) + COS[,1])
summary(lm_1997_2015_monthly_s_h)
AIC(lm_1997_2015_monthly_s_h)

