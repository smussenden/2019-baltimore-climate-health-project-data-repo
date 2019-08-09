#######################
#### Load Packages ####
#######################

# For debugging rm(list=ls())

rm(list=ls())

install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")
install.packages("readxl")
install.packages("janitor")
install.packages("weathermetrics")
install.packages("ggplot2")
install.packages("reshape")

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(janitor)
library(weathermetrics)
library(ggplot2)
library(reshape)

##############################
##### LOAD DATA ##############
##############################


# Load Baltimore Inner Harbor Temperature Data
bwi_data <- read_csv("baltimore-bwi-temperature/BWI.csv")

# Load Baltimore Inner Harbor Temperature Data
inner_harbor_data <- read_excel("baltimore-inner-harbor-temperature/DMH.xlsx")

##############################
##### CLEAN DATA #############
##############################

# Clean BWI data 
bwi_data <- bwi_data %>%
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
  summarise(avg_hourly_temperature_bwi = mean(tmpf),
            avg_hourly_dewpoint_bwi = mean(dwpf),
            avg_hourly_relative_humidity_bwi = mean(relative_humidity),
            avg_hourly_heat_index_bwi = mean(heat_index)
  ) %>%
  filter(!is.na(avg_hourly_relative_humidity_bwi)) %>%
  distinct()

# Clean Inner Harbor data 

inner_harbor_data <- inner_harbor_data %>%
  filter(TEMPERATURE != 'M', DEW_POINT != 'M') %>%
  mutate(TEMPERATURE = as.numeric(TEMPERATURE),
         DEW_POINT = as.numeric(DEW_POINT)) %>%
  mutate(date = as.Date(DATETIME, format="%Y-%m-%d")) %>%
  mutate(year=year(DATETIME)) %>%
  mutate(month=month(DATETIME)) %>%
  mutate(hour=hour(DATETIME)) %>%
  mutate(day=day(DATETIME)) %>%
  mutate(hour=hour(DATETIME)) %>%
  distinct(DATETIME, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=TEMPERATURE, dp=DEW_POINT, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = DEW_POINT, t = TEMPERATURE, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, TEMPERATURE, DEW_POINT, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature_dmh = mean(TEMPERATURE),
            avg_hourly_dewpoint_dmh = mean(DEW_POINT),
            avg_hourly_relative_humidity_dmh = mean(relative_humidity),
            avg_hourly_heat_index_dmh = mean(heat_index)
  ) %>%
  filter(!is.na(avg_hourly_relative_humidity_dmh)) %>%
  distinct()

#Save cleaned files for easy loading
clean_bwi_data <- write.csv(bwi_data, "/Users/tdiff/Desktop/td/clean_bwi_data.csv")
clean_inner_harbor_data <- write.csv(inner_harbor_data, "/Users/tdiff/Desktop/td/clean_inner_harbor_data.csv")

glimpse(inner_harbor_data)

test <- inner_harbor_data %>%
  filter(year > 2018)

###########################################
###### Adjust Inner Harbor Temperatures####
###########################################

# We have 75 years of dew point and temperature data for BWI, which allows us to calculate heat index
# We have 20 years of dew point and temperature data for DMH (Inner Harbor), which allows us to calculate heat index. 
# I talked with Dan Li, a climate researcher at Boston University who has studied variation in urban heat island temperatures between suburbs in different seasons and different hours of the day, who said a pretty good way to calculate the difference would be this:
# 1. Join the 20 years of DMH data to the 20 years of BWI data.  Join on date and hour, so we end up with a data frame of values for both DMH and BWI for each date and hour in our data set.  Do an inner join, so that if for some reason DMH is missing an hour from its data set (or vice versa), it doesn't mess up our averages. Calculate new columns with difference in temperature, dew point, relative humidity and heat index between BWI and DMH.  Since in almost all cases BWI will be colder than DMH, make it so the difference is positive if DMH is greater than BWI.
dmhbwi20_w <- 
  inner_join(clean_bwi_data, clean_inner_harbor_data, by = c("date", "hour")) %>%
  rename(year = year.x, month = month.x, day = day.x) %>%
  select(-year.y, -month.y, -day.y) %>%
  mutate(tempDiff = avg_hourly_temperature_dmh - avg_hourly_temperature_bwi,
         dewDiff = avg_hourly_dewpoint_dmh - avg_hourly_dewpoint_bwi,
         rhDiff = avg_hourly_relative_humidity_dmh - avg_hourly_relative_humidity_bwi,
         hiDiff = avg_hourly_heat_index_dmh - avg_hourly_heat_index_bwi) 

# 4. Group by month and hour and calculate the AVERAGE difference in temperature, dew point, relative humidity and heat index. 
summarized_join <- dmhbwi20_w %>%
  group_by(month, hour) %>%
  summarize(avgTempDiff = mean(tempDiff),
         avgDewDiff = mean(dewDiff),
         avgRhDiff = mean(rhDiff),
         avgHiDiff = mean(hiDiff))
  
# 5. Go back to the original 75 years of BWI data (after it's been cleaned, not on raw import).  Write case when function adjusting the temperature and dew point values based on table created in step 4. Create two new columns to recalculate heat index and relative humidity (see cleaning function above, weathermetrics package has a nice way of doing this). Call them adjusted_heat_index, adjusted_relative_humidity 
join_bwi_data_w <-
 inner_join(summarized_join, clean_bwi_data, by = c("month", "hour")) %>%
  mutate(adjusted_temp = avg_hourly_temperature_bwi + avgTempDiff,
         adjusted_dew = avg_hourly_dewpoint_bwi + avgDewDiff,
         adjusted_heat_index = heat.index(t=adjusted_temp, dp=adjusted_dew, temperature.metric = "fahrenheit", round=0),
         adjusted_relative_humidity = dewpoint.to.humidity(dp = adjusted_dew, t = adjusted_temp, temperature.metric = "fahrenheit"))
join_bwi_data_w

#Take new adjusted columns and put into a new table as estimates for inner harbor data
estimated_inner_harbor_data_w <- join_bwi_data_w %>%
  select(-avgTempDiff, -avgDewDiff, -avgRhDiff, -avgHiDiff, -avg_hourly_temperature_bwi, -avg_hourly_dewpoint_bwi, -avg_hourly_relative_humidity_bwi, -avg_hourly_heat_index_bwi)
estimated_inner_harbor_data_w

#Save newly calculated/estimated inner harbor data going back 70 years
estimated_inner_harbor_data <- write.csv(estimated_inner_harbor_data_w, "/Users/tdiff/Desktop/td/estimated_inner_harbor_data.csv")

#Check estimated data against cleaned inner harbor data
compare_inner_harbor_data <- estimated_inner_harbor_data %>%
  right_join(clean_inner_harbor_data, by = c("date", "hour")) %>%
  mutate(difference_temp = adjusted_temp - avg_hourly_temperature_dmh,
         difference_dew = adjusted_dew - avg_hourly_dewpoint_dmh,
         difference_rh = adjusted_relative_humidity - avg_hourly_relative_humidity_dmh,
         difference_hi = adjusted_heat_index - avg_hourly_heat_index_dmh)

summarized_compare <- compare_inner_harbor_data %>%
  group_by(year.x, month.x) %>%
  filter(adjusted_heat_index != "NA") %>%
  summarize(avg_difference_temp = mean(difference_temp),
            avg_difference_dew = mean(difference_dew),
            avg_difference_rh = mean(difference_rh),
            avg_difference_ri = mean(difference_hi))

###########################################
###### START HERE ##########################
###########################################
##Load cleaned files
clean_bwi_data <- read.csv("/Users/tdiff/Desktop/td/clean_bwi_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))
clean_inner_harbor_data <- read.csv("/Users/tdiff/Desktop/td/clean_inner_harbor_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))
estimated_inner_harbor_data <- read.csv("/Users/tdiff/Desktop/td/estimated_inner_harbor_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

###########################################
###### QUESTIONS ##########################
###########################################

# Our goal is to find out how frequent extremely high temperatures have been in Baltimore over the last century. 

#FOR INNER HARBOR: What have the patterns been with 90+, 103+ heat index days -- key values for heat index? What years have had the most of these days? What have the longest stretches been? Are there more recently that have been intense?
hi_over_ninety_dmh <- estimated_inner_harbor_data %>%
  select(-X.1, -X) %>%
  filter(adjusted_heat_index >= 90) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year, month) %>%
  summarise(count=n())
  
ggplot(data = hi_over_ninety_dmh, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Days When Heat Index > 90 in Inner Harbor",
       x = "Year",
       y = "#Days")
ggsave(filename = "dmh_days_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

###similar to above but for hours instead of days
hi_over_ninety_dmh <- estimated_inner_harbor_data %>%
  select(-X.1, -X) %>%
  filter(adjusted_heat_index >= 90) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_ninety_dmh, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Hours When Heat Index > 90 in Inner Harbor",
       x = "Year",
       y = "#Hours")
ggsave(filename = "dmh_hours_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#Inner harbor heat index >= 103
hi_over_103_dmh <- estimated_inner_harbor_data %>%
  select(-X.1, -X) %>%
  filter(adjusted_heat_index >= 103) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_103_dmh, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Days When Heat Index > 103 in Inner Harbor",
       x = "Year",
       y = "Days")
ggsave(filename = "dmh_days_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#same as above but for hours, not days
hi_over_103_dmh <- estimated_inner_harbor_data %>%
  select(-X.1, -X) %>%
  filter(adjusted_heat_index >= 103) %>%
  group_by(year, month) %>%
  summarise(count=n())

ggplot(data = hi_over_103_dmh, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Hours When Heat Index > 103 in Inner Harbor",
       x = "Year",
       y = "#Hours")
ggsave(filename = "dmh_hours_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#Looks like 1980s and 1990s had spikes in the amount of days with heat indexes greater than 90 or 103, with current years trending downward. The only months with hi > 103 are June-September. Months with hi>90 range from May-September, with a couple Octobers every down and then, but no more in recent years than in the past

####BWI DATA####
#BWI over 90 data
hi_over_ninety_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 90) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_ninety_bwi, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Days When Heat Index > 90 in BWI",
       x = "Year",
       y = "Days")
ggsave(filename = "bwi_days_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#same as above but #hours instead of days
hi_over_ninety_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 90) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_ninety_bwi, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Hours When Heat Index > 90 in BWI",
       x = "Year",
       y = "#Hours")
ggsave(filename = "bwi_hours_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#BWI over 103 data
hi_over_103_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 103) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year, month) %>%
  summarise(count=n())

ggplot(data = hi_over_103_bwi, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Days When Heat Index > 103 in BWI",
       x = "Year",
       y = "Days")
ggsave(filename = "bwi_days_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

#same as above but #hours instead of days
hi_over_103_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 103) %>%
  group_by(year, month) %>%
  summarise(count=n())

ggplot(data = hi_over_103_bwi, aes(x=year, y=count)) + geom_col() + geom_smooth() +
  labs(title = "#Hours When Heat Index > 103 in BWI",
       x = "Year",
       y = "#Hours")
ggsave(filename = "bwi_hours_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

# Have the hottest months of the year (July, August) gotten more hot on average?
#Inner harbor data, done by building a multi-line graph by making months into separate columns, joining, and then plotting; geom_smooth lines are easier to read than geom_line
july_dmh <- estimated_inner_harbor_data %>%
  filter(month == 7,
         adjusted_heat_index != "NA") %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_july = mean(adjusted_heat_index))

august_dmh <- estimated_inner_harbor_data %>%
  filter(month ==8,
         adjusted_heat_index != "NA") %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_august = mean(adjusted_heat_index))

july_august_dmh <-
  inner_join(july_dmh, august_dmh, by="year")

ggplot(data = july_august_dmh, aes(x=year)) + 
  geom_smooth(aes(y=monthly_average_heat_index_july, color = "July")) +
  geom_smooth(aes(y=monthly_average_heat_index_august, color = "August")) +
  labs(title = "Inner Harbor July and August Average Heat Indexes",
       x = "Year",
       y = "Average Heat Index")

ggsave(filename = "dmh_july_august_heat_indexes_smoothed.png",
       device = "png", path = "/Users/tdiff/Desktop/td",
       width = 20, height = 15, units = "in")

##BWI July/August Average Heat Index Data
july_bwi <- clean_bwi_data %>%
  filter(month == 7) %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_july = mean(avg_hourly_heat_index_bwi))

august_bwi <- clean_bwi_data %>%
  filter(month ==8) %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_august = mean(avg_hourly_heat_index_bwi))

july_august_bwi <-
  inner_join(july_bwi, august_bwi, by="year")

ggplot(data = july_august_bwi, aes(x=year)) + 
  geom_smooth(aes(y=monthly_average_heat_index_july, color = "July")) +
  geom_smooth(aes(y=monthly_average_heat_index_august, color = "August")) +
  labs(title = "BWI July and August Average Heat Indexes",
       x = "Year",
       y = "Average Heat Index")

ggsave(filename = "bwi_july_august_heat_indexes_smoothed.png",
       device = "png", path = "/Users/tdiff/Desktop/td",
       width = 20, height = 15, units = "in")

#It does seem that the average heat index in July and August are rising, but they're rising back to a level reached about 70 years ago

# How many days have not had a heat index BELOW 80 degrees, even at night? Are there more frequent recent stretches
#Use the minimum heat index value for each day using summarise

bwi_days_above_80 <- clean_bwi_data %>%
  group_by(year, date) %>%
  summarise(min_heat_index = min(avg_hourly_heat_index_bwi)) %>%
  filter(min_heat_index >= 80) %>%
  summarise(bwi_days=n())
#count is 52

ggplot(data=bwi_days_above_80, aes(x=year, y=bwi_days)) + 
  geom_col() +
  labs(title = "BWI Number of Days Where Heat Index Stayed Above 80 Degres",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "bwi_days_above_80_col.png",
       device = "png", path = "/Users/tdiff/Desktop/td/Graphs",
       width = 20, height = 15, units = "in")

dmh_days_above_80 <- estimated_inner_harbor_data %>%
  group_by(year, date) %>%
  summarise(min_heat_index = min(adjusted_heat_index)) %>%
  filter(min_heat_index >= 80) %>%
  summarise(dmh_days=n())
#count is 322

ggplot(data=dmh_days_above_80, aes(x=year, y=dmh_days)) + 
  geom_col() +
  labs(title = "Inner Harbor Number of Days Where Heat Index Stayed Above 80 Degres",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "dmh_days_above_80_col.png",
       device = "png", path = "/Users/tdiff/Desktop/td",
       width = 20, height = 15, units = "in")

dmh_days_above_80_other <- clean_inner_harbor_data %>%
  group_by(year, date) %>%
  summarise(min_heat_index = min(avg_hourly_heat_index_dmh)) %>%
  filter(min_heat_index >= 80) %>%
  summarise(count=n())
 #count is 259

days_above_80 <-
  full_join(dmh_days_above_80, bwi_days_above_80, by=c("year")) %>%
  days_above_80[is.na(days_above_80)] <- 0

#tried using geom_col, but data wouldn't change when position set to dodge. Tried to melt but didn't work:

#melt_days_above_80 <- melt(days_above_80, id = c("year"), measure=c("dmh_days", "bwi_days"))

ggplot(data=days_above_80, aes(x=year)) + 
  geom_smooth(aes(y=dmh_days, fill = "Inner Harbor")) +
  geom_smooth(aes(y=bwi_days, fill = "BWI")) +
  labs(title = "Number of Days Where Heat Index Stayed Above 80 Degres",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "days_above_80_smoothed.png",
       device = "png", path = "/Users/tdiff/Desktop/td",
       width = 20, height = 15, units = "in")

##################
#### RANDOM CODE THAT MIGHT BE HELPFUL#
##################

##Daily summary stats for bwi(original data) and dmh (using calculated/estimated data)
daily_summary_stats_bwi <- clean_bwi_data %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature_bwi),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint_bwi),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity_bwi),
            avg_daily_heat_index = mean(avg_hourly_heat_index_bwi),
            max_daily_temperature = max(avg_hourly_temperature_bwi),
            max_daily_dewpoint = max(avg_hourly_dewpoint_bwi),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity_bwi),
            max_daily_heat_index = max(avg_hourly_heat_index_bwi)) 

daily_summary_stats_dmh <- estimated_inner_harbor_data
group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)) 

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
            max_daily_heat_index = max(avg_hourly_heat_index)) 


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

