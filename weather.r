THIS IS WORTHLESS, BECAUSE OF API LIMITS
DOWNLOAD BULK DATA AND UPDATE WITH NEW STUFF

# Weather data

install.packages('rnoaa')
library(rnoaa)
library(tidyverse)

# token
# WyzMcZFIhFBYiFwzjvqUSEhybOoYozyb

bwi <- ncdc_stations(datasetid='GHCND', stationid='GHCND:USW00093721', token='WyzMcZFIhFBYiFwzjvqUSEhybOoYozyb')

bwi_data <- ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00093721', startdate = '1939-07-01', enddate = '1940-06-30', token='WyzMcZFIhFBYiFwzjvqUSEhybOoYozyb')

bwi_data$data

bwi_data_2 <- bwi_data$data

ncdc_datasets(token='WyzMcZFIhFBYiFwzjvqUSEhybOoYozyb')
