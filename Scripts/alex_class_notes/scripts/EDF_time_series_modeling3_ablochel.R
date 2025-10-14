library(tsibble) # convert time-series data in a tsibble
library(fable) # main package for modeling and forecasting with time-series data
library(feasts) # time-series data visualization
library(dplyr) # data manipulation

setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/')


data <- read.csv("time_series_decomposition/portal_timeseries.csv") %>% 
  mutate(month = yearmonth(date)) %>% 
  as_tsibble(index = month)

pp_data <- read.csv("time_series_decomposition/pp_abundance_timeseries.csv") %>% 
  as_tsibble(index = newmoonnumber)

gg_tsdisplay(pp_data, abundance) #high acf 


#y(t) = c + B1*X(t) + e(t)  e ~ N(0,s)

#time series linear model TSLM()

#m1
tslm_model <- model(pp_data, 
                    TSLM(abundance ~ mintemp)
                    )

report(tslm_model)
tslm_model_aug <- augment(tslm_model)

autoplot(tslm_model_aug, abundance) +
  autolayer(tslm_model_aug, .fitted, col = 'magenta')

gg_tsresiduals(tslm_model)

#m2
tslm_model2 <- model(pp_data, 
                    TSLM(abundance ~ mintemp + cool_precip)
)

report(tslm_model2)
tslm_model_aug2 <- augment(tslm_model2)

autoplot(tslm_model_aug2, abundance) +
  autolayer(tslm_model_aug2, .fitted, col = 'magenta')

gg_tsresiduals(tslm_model2)

#m3
#with time trend 
#y(t) = c + B1*X1(t) + B2*X2(t) + B3*t        e(t)  e ~ N(0,s)

tslm_model3 <- model(pp_data, 
                     TSLM(abundance ~ mintemp + 
                            cool_precip + 
                            trend())
)

report(tslm_model3)
tslm_model_aug3 <- augment(tslm_model3)

autoplot(tslm_model_aug3, abundance) +
  autolayer(tslm_model_aug3, .fitted, col = 'magenta')

gg_tsresiduals(tslm_model3)



#m4

tslm_model4 <- model(pp_data, 
                     TSLM(abundance ~ mintemp +
                            cool_precip + 
                            warm_precip +
                            trend()))

report(tslm_model4)
tslm_model_aug4 <- augment(tslm_model4)

autoplot(tslm_model_aug4, abundance) + 
  autolayer(tslm_model_aug4, .fitted, col = 'magenta')

gg_tsresiduals(tslm_model4)

#violated assumption that there's not auto correlation high acf 



#Dynamic Regression Model 
#combine ARIMA and exogenous drivers, standard linear models 

arimax_model <- model(pp_data, ARIMA(
  abundance ~ mintemp
))

report(arimax_model)

#y(t) = c + B1*X1(t) + B2*y(t-1) + theta1*E(t-1) + E(t)
#y(t)' = y(t) - y(t-1)

#y(t)' = B1*X1'(t) + B2*y'(t-1) + theta1*E(t-1) + E(t)

#y(t)' = B1*X1'(t) + n'(t)                  n = ata
#n'(t) =  B2*n'(t-1) + theta1*E(t-1) + E(t)

arimax_model_aug <- augment(arimax_model)
autoplot(arimax_model_aug, abundance) +
  autolayer(arimax_model_aug, .fitted, color = 'magenta')

gg_tsresiduals(arimax_model)


#E(t) can be set so we're not predicting negative abundance 



arimax_model2 <- model(pp_data, ARIMA(
  abundance ~ mintemp +
    cool_precip
))

report(arimax_model2)

arimax_model_aug2 <- augment(arimax_model2)
autoplot(arimax_model_aug2, abundance) +
  autolayer(arimax_model_aug2, .fitted, color = 'magenta')

gg_tsresiduals(arimax_model2)
