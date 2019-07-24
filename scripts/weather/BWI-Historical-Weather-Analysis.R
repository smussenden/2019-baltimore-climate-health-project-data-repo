#### BWI Weather Analysis ###

#######################
#### Load Packages ####
#######################

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(janitor)
library(weathermetrics)

### Tlaked to Dan Li at Boston
The way to do this is as follows:
Take the 20 years of data at BWI and take the 20 years of data at the inner harbor.  Align them to the closest hour in each data set. Take the average for each hour-day combo, so we only have one value per hour.  Join together on datetime, and do an inner join so we only have records where we have hourly daily value.  Calculate the difference in temperature.  Then calculate the average difference by hour by season (spring, summer, winter, fall).  Total of 24 x 4 separate values to adjust for.  THen use that to work backwards and adjust 75 years of data. 

# Load Baltimore Inner Harbor Temperature Data
temp_data <- read_csv("data/input-data/baltimore-bwi-temperature/BWI.csv")
https://www.researchgate.net/publication/235663083_Synergistic_Interactions_between_Urban_Heat_Islands_and_Heat_Waves_The_Impact_in_Cities_Is_Larger_than_the_Sum_of_Its_Parts
The trends in the bottom panel of Fig. 5 illustrate that,
during the heat wave period, the daytime and nighttime
UHI indices are both enhanced signiﬁcantly compared
to the period before the heat wave, implying synergistic
interactions between the urban heat island and the heat
wave. The urban heat island effect at night is invariably
larger than its daytime counterpart, which is consistent
with previous observational (Oke 1982) and modeling
studies (Oleson et al. 2011; Fischer et al. 2012). This is
due to larger daytime heat storage in urban areas than
in rural areas (the WRF simulations discussed next

                Synergistic Interactions between Urban Heat Islands and Heat Waves: The Impact in Cities Is Larger than the Sum of Its Parts*                
                
                http://dx.doi.org/10.1175/JAMC-D-13-02.s1.
                Corresponding author address: Dan Li, Dept. of Civil and En-
                  vironmental Engineering, Princeton University, Princeton, NJ
                08540.
                E-mail: danl.princeton.2009@gmail.com
                SEPTEMBER 2013 L I A N D B O U - Z E I D 2051
                DOI: 10.1175/JAMC-D-13-02.1
                Ó2013 American Meteorological Society
                https://journals.ametsoc.org/doi/suppl/10.1175/JAMC-D-13-02.1/suppl_file/10.1175_JAMC-D-13-02.s1.txt       
##########################
# Clean Temperature Data #
##########################

temp_data <- temp_data %>%
  filter(tmpf != 'M', dwpf != 'M') %>%
  mutate(tmpf = as.numeric(tmpf),
         dwpf = as.numeric(dwpf)) %>%
  mutate(date = as.Date(valid, format="%Y-%m-%d")) %>%
  mutate(year=year(valid)) %>%
  mutate(month=month(valid)) %>%
  mutate(hour=hour(valid)) %>%
  mutate(day=day(valid)) %>%
  mutate(hour=hour(valid)) %>%
  distinct(valid, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=tmpf, dp=dwpf, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = dwpf, t = tmpf, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, tmpf, dwpf, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature = mean(tmpf),
            avg_hourly_dewpoint = mean(dwpf),
            avg_hourly_relative_humidity = mean(relative_humidity),
            avg_hourly_heat_index = mean(heat_index)
            ) %>%
  filter(!is.na(avg_hourly_relative_humidity)) %>%
  distinct()


###### CHECKS ######
###

hourly_temp_data <- temp_data %>%
  group_by(year, month, day, hour) %>%
  summarise(count=n())

daily_temp_data <- temp_data %>%
  group_by(year, month, day) %>%
  summarise(count=n())

##################
#### 90 plus days#
##################

daily_summary_stats <- temp_data %>%
  group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)
  ) 

days_index_90_plus <- daily_summary_stats %>%
  filter(max_daily_heat_index >= 90)


year_days_index_90_plus <- days_index_90_plus %>%
  group_by(year(date)) %>%
  summarise(count=n())

barplot(year_days_index_90_plus$count)


daily_summary_stats <- temp_data %>%
  group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)
  ) 



test <- daily_summary_stats %>%
  filter(is.na(avg_daily_relative_humidity))

monthly_summary_stats <- temp_data %>%
  group_by(month) %>%
  summarise(avg_monthly_temperature = mean(avg_hourly_temperature),
            avg_monthly_dewpoint = mean(avg_hourly_dewpoint),
            avg_monthly_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_monthly_heat_index = mean(avg_hourly_heat_index),
            max_monthly_temperature = max(avg_hourly_temperature),
            max_monthly_dewpoint = max(avg_hourly_dewpoint),
            max_monthly_relative_humidity = max(avg_hourly_relative_humidity),
            max_monthly_heat_index = max(avg_hourly_heat_index)
  ) 

year_month_summary_stats <- temp_data %>%
  group_by(year, month) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)
  ) 

temp_data <- temp_data %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  arrange(year, month)



##########################
# Clean Temperature Data #
##########################

# Build a datetime object from DATE and TIME
temp_data  <- temp_data  %>%
  mutate(temp_time = as.POSIXct(DATETIME, format="%Y-%m-%d %H:%M"))

# Remove seconds from datettime object
temp_data$temp_time <- format(as.POSIXct(temp_data$temp_time), "%m-%d-%Y %H:%M")

# Filter out missing values for temp and humidity and convert to numeric
temp_data <- temp_data %>%
  filter(DEW_POINT != 'M', TEMPERATURE != 'M') %>%
  mutate(DEW_POINT = as.numeric(DEW_POINT),
         TEMPERATURE = as.numeric(TEMPERATURE))