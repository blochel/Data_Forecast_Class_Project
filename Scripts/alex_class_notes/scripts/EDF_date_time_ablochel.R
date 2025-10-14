# Ecology Dynamics and Forecasting 1 --------------------------------------
# 

library('tidyverse')
library('ggplot2')
library('lubridate')

setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting')

#where it is
#'C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/dates_times'
#data files needed 
#'NEON_Harvardforest_date_2001_2006.csv'
#'NEON_Harvardforest_datetime.csv'

#many versions of writing dates 



# daily -------------------------------------------------------------------


daily <- read.csv('dates_times/NEON_Harvardforest_date_2001_2006.csv',
         stringsAsFactors = FALSE, header = TRUE)

#what type of data class()
class(daily$date)


us_date <- c('01-16-2025', '12-16-2014', '07-16-2015', '01-16-2023')
sort(us_date) #is as character

iso_dates <- c('2015-01-16','2014-12-16','2015-07-16','2023-01-16')
sort(iso_dates) #is a character 



#order data
daily <- daily[order(daily$date),]

#plot data
daily %>% 
  ggplot( aes(x= date, y=airt))+
  geom_point()

#checking missing data
daily %>% 
  ggplot(aes(year(date))) +
  geom_histogram(stat = 'count')

#make Date colum 
daily$asdat.date <- as.Date(daily$date)

#plot new Date column 
daily %>% 
  ggplot( aes(x= asdat.date, y=airt))+
  geom_point()






# hourly ------------------------------------------------------------------

datetime <- read.csv('dates_times/NEON_Harvardforest_datetime.csv',
                   stringsAsFactors = FALSE, header = TRUE)

str(datetime)

#messing with dates, lubridate
testdate <- '08-23-2025'
testdate2 <- '24 July 2015'

output1 <- mdy(testdate)
output2 <- dmy(testdate2)

#change date column to Date 
datetime <- datetime %>% 
  mutate(formatdate = ymd_hm(datetime), 
         test_date = as.Date(formatdate), 
         test_hour = hour(formatdate), 
         test_min = minute(formatdate), 
         test_month = day(formatdate))

datetime %>% head()


