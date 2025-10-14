install.packages(c('fable', #time series models 
                   'feasts', 
                   'tsibble', 
                   'dplyr', 
                   'ggplot2'))
library('fable')
library('feasts')
library('tsibble')
library('dplyr')
library('tidyverse')
library("fpp3") #fore some reason PDQ did not work so i loaded this too

setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/')

#these are included in fpp3


pp_data <- read_csv('time_series_decomposition/pp_abundance_by_month.csv')

pp_data<- pp_data %>% 
  mutate(month = yearmonth(month)) %>% 
  as_tsibble(index = month)

gg_tsdisplay(pp_data)  


#y(t) = B1*y(t-1)+e(t)  e ~ N(0,s)
# pdq
#p = autoregressive AR
#d = trend          I
#q = moving average MA

ar1_model <- model(pp_data, 
                   ARIMA(abundance ~ pdq(1, 0, 0) + 
                           PDQ(0, 0, 0) #SEASONAL SIGNAL 
                   ))

report(ar1_model)


ar1_forecast <- forecast(ar1_model,
                         h = 18  #forecast horizon
                           )

autoplot(ar1_forecast, #forecast
         pp_data) #original data

#converges on the mean, c gets close to 0 when we get closer to the 
#mean of the data if you forecast far enough


ar1_forecast <- forecast(ar1_model,
                         bootstrap = TRUE, #draw random number for e
                         times = 1, #how many times to bootstrap
                         h = 18  #forecast horizon
)

autoplot(ar1_forecast, #forecast
         pp_data) #original data


ar1_forecast <- forecast(ar1_model,
                         bootstrap = TRUE, #draw random number for e
                         times = 1000, #how many times to bootstrap
                         h = 18  #forecast horizon
)

autoplot(ar1_forecast, #forecast
         pp_data) #original data
#point estimate = blue line (the average of all lines from bootstraps)
#prediction interval 80% and 95%


autoplot(ar1_forecast, 
         pp_data,
         level = c(10,80)) #set prediction intervals 

#external drivers 

arimax_model <- model(pp_data, 
                      ARIMA(abundance ~ mintemp))

report(arimax_model)

#y'(t) = c + B1*x(1,t) + B2*y(t-1) + 0,E(t-1) + E(t)
#            min temp    AR          MA

climate_forecasts <- read_csv('time_series_decomposition/pp_future_climate.csv') %>% 
  mutate(month = yearmonth(month)) %>% 
  as_tsibble(index = month)

#need same columns as in the model 



arimax_forecast <- forecast(arimax_model, 
                            new_data = climate_forecasts #other forecast data 
                            )
autoplot(arimax_forecast, pp_data)





arimax_model2 <- model(pp_data, 
                      ARIMA(abundance ~ mintemp + warm_precip +
                              pdq(1, 0, 0) + 
                              PDQ(1, 1, 0),
                            ))

arimax_forecast2 <- forecast(arimax_model, 
                            new_data = climate_forecasts,#other forecast data 
                            bootstrap = TRUE, #draw random number for e
                            times = 1000 #how many times to bootstrap
                             
)
autoplot(arimax_forecast2, pp_data)

