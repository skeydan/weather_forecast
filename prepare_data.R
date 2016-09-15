# weather stations overview
# http://weather.rap.ucar.edu/surface/stations.txt

# example URL
# http://www.wunderground.com/history/airport/EDDM/2015/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2015&req_city=NA&req_state=NA&req_statename=NA&format=1 

# columns
# CET,Max TemperatureC,Mean TemperatureC,Min TemperatureC,Dew PointC,
# MeanDew PointC,Min DewpointC,Max Humidity, Mean Humidity, Min Humidity,
# Max Sea Level PressurehPa, Mean Sea Level PressurehPa, Min Sea Level PressurehPa,
# Max VisibilityKm, Mean VisibilityKm, Min VisibilitykM, Max Wind SpeedKm/h,
# Mean Wind SpeedKm/h, Max Gust SpeedKm/h,Precipitationmm, CloudCover, Events,WindDirDegrees
# 2015-1-1,0,-5,-10,-2,-5,-10,100,95,80,1036,1035,1034,6,2,0,11,6,,0.00,5,Fog,232


library(tidyverse)
library(lubridate)
library(scales)
library(nlme)

url_munich_1997 <- 'http://www.wunderground.com/history/airport/EDDM/1997/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=1997&format=1'
url_munich_1998 <- 'http://www.wunderground.com/history/airport/EDDM/1998/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=1998&format=1'
url_munich_1999 <- 'http://www.wunderground.com/history/airport/EDDM/1999/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=1999&format=1'
url_munich_2000 <- 'http://www.wunderground.com/history/airport/EDDM/2000/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2000&format=1'
url_munich_2001 <- 'http://www.wunderground.com/history/airport/EDDM/2001/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2001&format=1'
url_munich_2002 <- 'http://www.wunderground.com/history/airport/EDDM/2002/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2002&format=1'
url_munich_2003 <- 'http://www.wunderground.com/history/airport/EDDM/2003/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2003&format=1'
url_munich_2004 <- 'http://www.wunderground.com/history/airport/EDDM/2004/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2004&format=1'
url_munich_2005 <- 'http://www.wunderground.com/history/airport/EDDM/2005/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2005&format=1'
url_munich_2006 <- 'http://www.wunderground.com/history/airport/EDDM/2006/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2006&format=1'
url_munich_2007 <- 'http://www.wunderground.com/history/airport/EDDM/2007/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2007&format=1'
url_munich_2008 <- 'http://www.wunderground.com/history/airport/EDDM/2008/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2008&format=1'
url_munich_2009 <- 'http://www.wunderground.com/history/airport/EDDM/2009/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2009&format=1'
url_munich_2010 <- 'http://www.wunderground.com/history/airport/EDDM/2010/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2010&format=1'
url_munich_2011 <- 'http://www.wunderground.com/history/airport/EDDM/2011/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2011&format=1'
url_munich_2012 <- 'http://www.wunderground.com/history/airport/EDDM/2012/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2012&format=1'
url_munich_2013 <- 'http://www.wunderground.com/history/airport/EDDM/2013/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2013&format=1'
url_munich_2014 <- 'http://www.wunderground.com/history/airport/EDDM/2014/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2014&format=1'
url_munich_2015 <- 'http://www.wunderground.com/history/airport/EDDM/2015/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2015&format=1'
url_munich_2016 <- 'http://www.wunderground.com/history/airport/EDDM/2016/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2016&format=1'

read_year <- function(url) {
  df <- read_csv(url, col_names = FALSE, skip=2, col_types = cols(X1 = col_date('%Y-%m-%d')))
  df <- rename(df, day = X1, max_temp = X2, mean_temp = X3,
               min_temp = X4, dew = X5, mean_dew = X6, min_dew = X7, max_hum = X8,
               mean_hum = X9, min_hum = X10, max_hpa = X11, mean_hpa = X12,
               min_hpa = X13, max_visib = X14, mean_visib = X15, min_visib = X16,
               max_wind = X17, mean_wind = X18, max_gust = X19, prep = X20,
               cloud = X21, events = X22, winddir = X23)
  #df <- df %>% mutate(day_of_year = as.numeric(strftime(day, format = "%j")))
}

munich_1997 <- read_year(url_munich_1997)
munich_1998 <- read_year(url_munich_1998)
munich_1999 <- read_year(url_munich_1999)
munich_2000 <- read_year(url_munich_2000)
munich_2001 <- read_year(url_munich_2001)
munich_2002 <- read_year(url_munich_2002)
munich_2003 <- read_year(url_munich_2003)
munich_2004 <- read_year(url_munich_2004)
munich_2005 <- read_year(url_munich_2005)
munich_2006 <- read_year(url_munich_2006)
munich_2007 <- read_year(url_munich_2007)
munich_2008 <- read_year(url_munich_2008)
munich_2009 <- read_year(url_munich_2009)
munich_2010 <- read_year(url_munich_2010)
munich_2011 <- read_year(url_munich_2011)
munich_2012 <- read_year(url_munich_2012)
munich_2013 <- read_year(url_munich_2013)
munich_2014 <- read_year(url_munich_2014)
munich_2015 <- read_year(url_munich_2015)
munich_2016 <- read_year(url_munich_2016)

# fill missing dates with NA
# year 2000
munich_2000 <- read_year(url_munich_2000)
for (d in c(('2000-02-23'), ('2000-02-24'), ('2000-02-25'), ('2000-02-26'), ('2000-02-27'), ('2000-02-28'),
            ('2000-03-01'), ('2000-03-02'), ('2000-03-03'), ('2000-03-04'), ('2000-03-05'), ('2000-03-06'),
            ('2000-03-07'), ('2000-03-08'), ('2000-03-09'), ('2000-03-10'), ('2000-03-11'), ('2000-03-12'),
            ('2000-03-13'), ('2000-03-14'), ('2000-03-15'), ('2000-03-16'), ('2000-03-17'), ('2000-03-18'),
            ('2000-03-19'), ('2000-03-20'), ('2000-03-21'), ('2000-03-22'), ('2000-03-23'), ('2000-03-24'),
            ('2000-03-25'), ('2000-03-26'), ('2000-03-27'), ('2000-03-28'), ('2000-03-29'), ('2000-03-30'),
            ('2000-03-31'), 
            ('2000-04-01'), ('2000-04-02'), ('2000-04-03'), ('2000-04-04'), ('2000-04-05'), ('2000-04-06'),
            ('2000-04-07'), ('2000-04-08'), ('2000-04-09'), ('2000-04-10'), ('2000-04-11'), ('2000-04-12'),
            ('2000-04-13'), ('2000-04-14'), ('2000-04-15'), ('2000-04-16'), ('2000-04-17'), ('2000-04-18'),
            ('2000-04-19'), ('2000-04-20'), ('2000-04-21'), ('2000-04-22'), ('2000-04-23'), ('2000-04-24'),
            ('2000-04-25'), ('2000-04-26'), ('2000-04-27'), ('2000-04-28'), ('2000-04-29'), ('2000-04-30'),
            ('2000-05-01'), ('2000-05-02'), ('2000-05-03'),
            ('2000-06-02'), ('2000-06-03'), ('2000-06-04'), ('2000-06-05'), ('2000-06-06'),
            ('2000-06-07'), ('2000-06-08'), ('2000-06-09'),
            ('2000-08-12'), ('2000-08-13'), ('2000-08-14'), ('2000-08-15'), ('2000-08-16'), ('2000-08-17'),
            ('2000-08-18'), ('2000-08-19'), ('2000-08-20'), ('2000-08-21'), ('2000-08-22'), ('2000-08-23'),
            ('2000-08-24'), ('2000-08-25'), ('2000-08-26'), ('2000-08-28'), ('2000-08-29'), ('2000-08-30'),
            ('2000-08-31'))) {
  new <- tibble(day = ymd(d), max_temp = NA, mean_temp = NA,
                min_temp = NA, dew = NA, mean_dew = NA, min_dew = NA, max_hum = NA,
                mean_hum = NA, min_hum = NA, max_hpa = NA, mean_hpa = NA,
                min_hpa = NA, max_visib = NA, mean_visib = NA, min_visib = NA,
                max_wind = NA, mean_wind = NA, max_gust = NA, prep = NA,
                cloud = NA, events = NA, winddir = NA)
  munich_2000 <- rbind(munich_2000, new)
}

munich_2000 <- munich_2000 %>% arrange(day)
nrow(munich_2000)
distinct(munich_2000, day)

# year 2004
munich_2004 <- read_year(url_munich_2004)
for (d in c(('2004-01-02'), ('2004-01-03'), ('2004-01-04'), ('2004-01-05'), ('2004-01-06'),
            ('2004-01-07'), ('2004-01-08'), ('2004-01-09'), ('2004-01-10'), ('2004-01-11'), ('2004-01-12'),
            ('2004-01-13'), ('2004-01-14'), ('2004-01-15'), ('2004-01-16'), ('2004-01-17'), ('2004-01-18'),
            ('2004-01-19'), ('2004-01-20'), ('2004-01-21'), ('2004-01-22'), ('2004-01-23'), ('2004-01-24'),
            ('2004-01-25'), ('2004-01-26'), ('2004-01-27'), ('2004-01-28'), ('2004-01-29'), ('2004-01-30'))) {
  new <- tibble(day = ymd(d), max_temp = NA, mean_temp = NA,
                min_temp = NA, dew = NA, mean_dew = NA, min_dew = NA, max_hum = NA,
                mean_hum = NA, min_hum = NA, max_hpa = NA, mean_hpa = NA,
                min_hpa = NA, max_visib = NA, mean_visib = NA, min_visib = NA,
                max_wind = NA, mean_wind = NA, max_gust = NA, prep = NA,
                cloud = NA, events = NA, winddir = NA)
  munich_2004 <- rbind(munich_2004, new)
}

munich_2004 <- munich_2004 %>% arrange(day)
nrow(munich_2004)
distinct(munich_2004, day)

# remove 266th day for leap years
munich_2016 <- munich_2016 %>% filter(day != date('2016-02-29'))
munich_2012 <- munich_2012 %>% filter(day != date('2012-02-29'))
munich_2008 <- munich_2008 %>% filter(day != date('2008-02-29'))
munich_2004 <- munich_2004 %>% filter(day != date('2004-02-29'))
munich_2000 <- munich_2000 %>% filter(day != date('2000-02-29'))

# 
munich_1997_2015 <- rbind(munich_1997, munich_1998, munich_1999, munich_2000, munich_2001, munich_2002,
                          munich_2003, munich_2004, munich_2005, munich_2006, munich_2007, munich_2008,
                          munich_2009, munich_2010, munich_2011, munich_2012, munich_2013, munich_2014,
                          munich_2015)
munich_1997_2016 <-  rbind(munich_1997_2015, munich_2016)

nrow(munich_1997_2016)
distinct(munich_1997_2016, day)

write_csv(munich_1997_2015, 'munich_1997_2015.csv')
write_csv(munich_2016, 'munich_2016.csv')
write_csv(munich_1997_2016, 'munich_1997_2016.csv')

