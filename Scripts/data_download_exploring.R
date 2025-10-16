
# data exploration --------------------------------------------------------

library(tidyverse)


NDVI_df <- read.csv('Data/NDVI.csv')
Rain_Temp_df <- read.csv('Data/ppt_T.csv')
Rodent_df <- read.csv('Data/rodent_energy_abundance.csv')



range(NDVI_df$date)
range(Rain_Temp_df$year)
range(Rodent_df$censusdate)


remotes::install_github("weecology/portalr")


