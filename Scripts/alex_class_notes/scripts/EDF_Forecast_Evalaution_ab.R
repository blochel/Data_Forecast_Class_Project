

# Forecast Evaluation -----------------------------------------------------


library('fable')
library('feasts')
library('ggtime')
library('tsibble')
library('dplyr')
library('ggplot2')

portal_data <- read.csv('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/time_series_decomposition/portal_timeseries.csv')


portal_data <- portal_data %>%  
  mutate(month = yearmonth(date)) %>%
  as_tsibble(index = month)


portal_models <- model(
  portal_data, 
  arima = ARIMA(NDVI), 
  tslm = TSLM(NDVI ~ rain),
  arimax = ARIMA(NDVI ~ rain)
)


glance(portal_models)
#AIC is not appropriate for forecast models, a lot of auto correlation  


#hind casting 

train <- portal_data %>% 
  filter(month < yearmonth('2011 Dec'))

test <- portal_data %>% 
  filter(month >= yearmonth('2011 Dec'))


ma2_model <- model(train, 
                   ARIMA(NDVI ~ pdq(0, 0, 2) + #second order moving average
                           PDQ(0, 0, 0)))

ma2_forecast <- forecast(ma2_model, 
                         h = 36 #horizon
                         )

autoplot(ma2_forecast, train) + #model forecast
  autolayer(test, NDVI) + #ndvi data from test df
  autolayer(augment(ma2_model), .fitted, color = 'red')


# forecast errors 
#e(t) 
#e(t+h) = y(t+h) - y_pred(t+h) 
#ME = mean(y(t+h) - y_pred(t+h))

# RMSE = sqrt(mean[ (y(t+h) - y_pred(t+h))^2 ]) #root mean square error 

accuracy(ma2_forecast, test)



arimax_test <-  model(train, ARIMA(NDVI ~ rain))

arimax_forecast <- forecast(arimax_test, 
                            h = 36,
                            new_data = test)


autoplot(arimax_forecast, train) 



portal_models <- model(
  train, 
  arima = ARIMA(NDVI), 
  tslm = TSLM(NDVI ~ rain),
  ma2 = ARIMA(NDVI ~ pdq(0, 0, 2) + PDQ(0, 0, 0)),
  arimax = ARIMA(NDVI ~ rain)
)

all_models <- forecast(portal_models, 
         h = 36,
         new_data = test)



autoplot(all_models, train) 
accuracy(all_models, test)



# evaluating coverage ----------------------------------------------------


ma2_intervals <- hilo(ma2_forecast, level = 80) %>%  #general function that covers intervals of x% of data 
  unpack_hilo(`80%`)

lower_ma2 <- ma2_intervals$`80%_lower`
upper_ma2 <- ma2_intervals$`80%_upper`
observation_ma2 <- test$NDVI


in_interval_ma2 <- observation_ma2 > lower_ma2 & observation_ma2 < upper_ma2
length(in_interval_ma2[in_interval_ma2 == TRUE]) / length(in_interval_ma2)

# how good the model predicts, this can be compared with other models. 

arimax_intervals <- hilo(arimax_forecast, level = 80) %>% 
  unpack_hilo(`80%`)

lower_max <- arimax_intervals$`80%_lower`
upper_max <- arimax_intervals$`80%_upper`
observation_max <- test$NDVI


in_interval_max <- observation_max > lower_max & observation_max < upper_max
length(in_interval_max[in_interval_max == TRUE]) / length(in_interval_max)

#what the right amount of points within the set interval, 
# this case it should be 80% - (the closer to 80% the better, in this case)
#really identical models == equivalent 




# winkler score -----------------------------------------------------------

# W = (upper - lower) + 2/a(lower - y(t))         if y(t) < lower
# W = (upper - lower)                             if lower < y(t) < upper 
# W = (upper - lower) + 2/a(y(t) - upper)         if y(t) > upper
# a = 1 - prediction_interval                     if prediction_interval = %
# in the case of using intervals of 80% a = 1-0.8 = 0.2


accuracy(
  ma2_forecast, 
  test,
  list(winkler = winkler_score),
  level = 80
)


# continuous rank probability score = CRPS
# contentious score of the winkler score 

accuracy(
  all_models, 
  test,
  list(winkler = winkler_score, crps = CRPS, rmse = RMSE),
  level = 80 )
#good to look at several scores, they are showing slightly different things 

#evaluate the score speratley for one or more columns 
accuracies <- accuracy(
  all_models, 
  test,
  list(winkler = winkler_score, crps = CRPS, rmse = RMSE),
  level = 80, 
  by = c('.model', 'month'))

ggplot(data = accuracies, mapping =  aes(month, y = winkler))+
  geom_line()
#remember - want them low
