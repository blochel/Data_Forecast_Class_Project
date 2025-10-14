
# time series modeling1 ---- 9/16 ---------------------------------------------------


library("fpp3") #like the tidyverse - key packages for today: tsibble, 
#fable (time series modelling and forecasting), feasts (time series visualization code), dplyr
library('slider')
library('tidyverse')


setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/')


data <- read.csv('time_series_decomposition/portal_timeseries.csv')

data_ts <- data %>% 
  mutate(month = yearmonth(date)) %>% 
  as_tsibble(index = month)


gg_tsdisplay(data_ts, NDVI, plot_type = 'partial')


# assumption data is normally distributed 
# y(t) = c + E(t)   E(t) ~ N(0, s)    
#(c = mean of paramter, E = error term, s = sigmod)



#NVDI
MEAN()

white_noise_model <- model(data_ts, MEAN(NDVI))

report(white_noise_model)

#augment the model object

avg_mdeol_aug <- augment(white_noise_model)

autoplot(avg_mdeol_aug, NDVI) +
  autolayer(avg_mdeol_aug, .fitted, color = 'red') #just showing average

gg_tsresiduals(white_noise_model) #works on raw model, not augmented!

#rain

WN_rain_model <- model(data_ts, MEAN(rain))

#augment the model object

avg_rainM_aug <- augment(WN_rain_model)

autoplot(avg_rainM_aug, rain) +
  autolayer(avg_rainM_aug, .fitted, color = 'blue') 

gg_tsresiduals(WN_rain_model) 



#autoregressive model - AR. regressing the time series against itself. 
#predictor is previous time step 
# AR1 is the simplest -
# y(t) = c + beta1*y(t-1) + E(t)       E(t) ~ N(0,s)
# AR2 -
# y(t) = c + beta1*y(t-1) + beta2*y(t-2) + E(t)       E(t) ~ N(0,s)

#two short term auto correlations leads to modeling t-2 

AR()
#order 2 auto regressive model 
ar_model <- model(data_ts, AR(NDVI ~ order(2)))

report(ar_model)

ar_model_aug <- augment(ar_model)
autoplot(ar_model_aug, NDVI) + 
  autolayer(ar_model_aug, .fitted, color = 'red')

gg_tsresiduals(ar_model)


ar_model_rain <- model(data_ts, AR(rain ~ order(1)))

ar_rain_aug <- augment(ar_model_rain)

autoplot(ar_rain_aug, rain) +
  autolayer(ar_rain_aug, .fitted, color = 'red')


gg_tsresiduals(ar_model_rain)


ar_model_rain2 <- model(data_ts, AR(rain ~ order(20)))
 ar_rain_aug2 <- augment(ar_model_rain2)

 autoplot(ar_rain_aug2, rain) +
   autolayer(ar_rain_aug2, .fitted, color = 'red')
 
 gg_tsresiduals(ar_model_rain2)
 

# time series modeling 2 -- 9/18--------------------------------------------------


 
#library(tsibble)
#library(fable) 
#library(feasts)
#library(dplyr) 

 
#data <- read.csv('time_series_decomposition/portal_timeseries.csv')

#data_ts <- data %>%  
#  mutate(month = yearmonth(date)) %>% 
#  as_tsibble(index = month)
 
#ARIMA MODEL 
#autoregressive (AR)
#moving average (MA)
 
#use past residuals of model
 
#MA MODEL 
#y(t) = c + theta1*e(t-1) + E(t)       E(t) ~ N(0,s)
 
#ARMA MODEL
 #y(t) = c + beta1*y(t-1) + theta1*e(t-1) + E(t)
 #              AR              MA
 
# I = integrated (account for trends in the data)
 #assumes stationary - not a strong long term trend in the data
 #done with differencing - y(t') = y(t) - y(t-1) (first derivative of y(t))

 
#ARIMA MODEL 
 
 #AR = past values
 #I = remove trends 
 #MA = past errors 
 
 
arima_model <- model(data_ts, ARIMA(NDVI))
arima_model 
report(arima_model) 
# p, d, q notation - ARIMA(0,0,3)(1,0,0)
# p = AR order
# d = degree of differencing 
# q = MA order 

#3rd order MA, no differencing, no AR component (0,0,3)
# y(t) = c + theta1*e(t-1) + theta2*e(t-2) + theta3*e(t-3)

#(1,0,0) seasonal signal, 1st order AR, no MA, no differencing 

#season model (SAR1)
#y(t) = c + beta12*y(t-12) + E(t)

#y(t) = c + theta1*e(t-1) + theta2*e(t-2) + theta3*e(t-3)      +      beta12*y(t-12) + E(t)
#                3rd order MA                                               SAR1


arima_model_aug <- augment(arima_model)

autoplot(arima_model_aug, NDVI) +
  autolayer(arima_model_aug, .fitted, color = 'red')

#check seasonal autocorrelation
gg_tsresiduals(arima_model)



arima_rain_model <- model(data_ts, ARIMA(rain))
report(arima_rain_model)


arima_rain_model_aug <- augment(arima_rain_model)
autoplot(arima_rain_model_aug, rain) +
  autolayer(arima_rain_model_aug, .fitted, color = 'blue')

gg_tsresiduals(arima_rain_model)




