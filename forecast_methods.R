library(tidyverse)
library(lubridate)
library(scales)
library(nlme)

# general: accuracies on test and training sets
#          plot forecast


# STL decomposition

# Holt-Winters 

# Arima

# lm


###############################################################################################

# daily data
# tibbles
daily_1997_2015 <- read_csv('munich_1997_2015.csv')
daily_2016 <- read_csv('munich_2016.csv')

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

# time series
monthly_1997_2015_ts <- ts(monthly_1997_2015$mean_temp, start = c(1997,1), frequency = 12)
plot(monthly_1997_2015_ts)

ggplot(monthly_1997_2015, aes(day, mean_temp)) + geom_point() + xlab("") + ylab("temperature (daily average)") +
  scale_x_date(labels=date_format("%b %y")) + stat_smooth()

###############################################################################################

# lm, trend only
t <- time(temp_1997_2015)
c <- cycle(temp_1997_2015)
lm_1997_2015 <- lm(temp_1997_2015 ~ t))

summary(lm_1997_2015)
confint(lm_1997_2015)
AIC(lm_1997_2015)

plot(resid(lm_1997_2015))
acf(resid(lm_1997_2015))
pacf(resid(lm_1997_2015))

# glm 
gls_1997_2015 <-gls(temp_1997_2015 ~ t, na.action = na.exclude, cor = corAR1(0.9))
summary(gls_1997_2015)
confint(gls_1997_2015)



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

