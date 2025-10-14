library('fpp3')


# time series autocorrelation ---------------------------------------------

set.seed(1) #random draw 
whitenoise = tsibble(time_step = 1:273,
                     wn = rnorm(273, mean = 0.18, sd = 0.056), #273 points
                     index = time_step) 


autoplot(whitenoise, wn) + #zero autocorrelation 
  geom_hline(yintercept = 0.18)


setwd('C:/Users/alexanderblochel/OneDrive - University of Florida/Desktop/classes/WIS6934 - Topics in Wildlife Ecology and Conservation Ecology Dynamics and Forecasting/')
data = read.csv('time_series_decomposition/portal_timeseries.csv', stringsAsFactors = FALSE)



data_ts <- data %>%  
  mutate(date, month = yearmonth(date)) %>% 
  as_tsibble(index = month) #this is what we're looking at 'months'


autoplot(data_ts, NDVI) +
  geom_hline(yintercept = 0.18)

# lags in correlations depends on the organism that you're looking at

gg_lag(data_ts, NDVI, geom = 'point') +
  labs(x = 'lagged NDVI')

ACF_results = ACF(data_ts, NDVI, lag_max = 12) 
ACF_results     #correlation coefficients 

autoplot(ACF_results) #show signals of autocorrelation 


gg_lag(data_ts, rodents, geom = 'point') +
  labs(x = 'lagged rodents')

ACF_results = ACF(data_ts, rodents, lag_max = 12) 
ACF_results     #correlation coefficients 

autoplot(ACF_results) #show signals of autocorrelation 

gg_tsdisplay(data_ts, rodents, plot_type = 'partial')
#takes into account the first month auto correlation 
#once we take into account that there's a strong correlation from 
#one month to the next, but there's not a direct relationship 
#between month 1 vs month 4 


gg_tsdisplay(data_ts, NDVI, plot_type = 'partial')

#auto regressive model (rodents) = really strong correlation from one month to the next 


set.seed(1)

x = w = rnorm(1000)
for(t in 2:1000) x[t] = x[t-1] + w[t]
randomwalk <- tsibble(sample = 1:1000, rwalk = x, 
                      index = sample)

gg_tsdisplay(randomwalk)

#cross correlation function
ccf.plantrain <- ccf(data_ts$rain, data_ts$NDVI) #rain first, then green
plot(ccf.plantrain)
