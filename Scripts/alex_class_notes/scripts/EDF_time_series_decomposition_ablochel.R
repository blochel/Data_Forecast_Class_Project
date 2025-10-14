


# tsibble ----------------------------------------------------------------



library("fpp3")
library('slider')




#sorry i was late to class


setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/')


data = read.csv('time_series_decomposition/portal_timeseries.csv', stringsAsFactors = FALSE)
str(data)


data = dplyr::mutate(data, 
                     month=tsibble::yearmonth(date))
class(data$month)
str(data)


data_ts = tsibble::as_tsibble(data, index=month)

autoplot(data_ts, NDVI)

data_ts <- data_ts %>% 
  mutate(ma_13 = slide_dbl(NDVI, 
                            mean, 
                            .before = 6,
                            .after = 6,
                            .complete = TRUE #only calculate if you have 6 before and 6 after
                            )) 

autoplot(data_ts, NDVI) + 
  autolayer(data_ts, ma_13, color = 'blue', size = 2)



add_decomp <- data_ts %>%  model(classical_decomposition(NDVI, 
                                                      type ='additive')) %>% 
  components()

autoplot(add_decomp) 


stl_output <- data_ts %>% 
  model(STL(NDVI ~ trend(window = 25) + 
              season(window = 13), 
            robust = TRUE)) %>% 
  components()
#window = #The span (in lags) of the loess window, which should be odd. 
#          If NULL, the default, 
#          extodd(ceiling((1.5*period) / (1-(1.5/s.window)))), is taken.

#other things STL can do:
#degree	The degree of locally-fitted polynomial. Should be zero or one.

#jump	Integers at least one to increase speed of the respective smoother. 
#          Linear interpolation happens between every jump-th value.

autoplot(stl_output) 


stl_output2 <- data_ts %>% 
  model(STL(NDVI ~ trend(window = 25) + 
              season(window = 7), 
            robust = TRUE)) %>% 
  components()

autoplot(stl_output2) 

