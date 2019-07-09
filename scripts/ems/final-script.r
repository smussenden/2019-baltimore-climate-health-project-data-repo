############################################
# Baltimore EMS - Temperature Analysis #####
############################################
rm(list=ls())
options(scipen = 999)
#######################
#### Load Packages ####
#######################

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(janitor)
library(weathermetrics)

#######################
#### Load EMS Data ####
#######################

# Load EMS data, which is contained in five separate Excel files.
# Note that we have complete years for 2014, 2015, 2016, 2017 and partial (1.1 - 11.30) for 2018.

EMS_2014 <- read.csv("data/input-data/baltimore-ems/raw-data/ems-data/csv/2014.csv")
EMS_2015 <- read.csv("data/input-data/baltimore-ems/raw-data/ems-data/csv/2015.csv")
EMS_2016 <- read.csv("data/input-data/baltimore-ems/raw-data/ems-data/csv/2016.csv")
EMS_2017 <- read.csv("data/input-data/baltimore-ems/raw-data/ems-data/csv/2017.csv")
EMS_2018_partial <- read.csv("data/input-data/baltimore-ems/raw-data/ems-data/csv/2018-1.1-11.30.csv")

# Bind together into one dataframe

EMS_all <- bind_rows(EMS_2014, EMS_2015, EMS_2016, EMS_2017, EMS_2018_partial)

# Remove all dataframes but EMS_all

rm(list=setdiff(ls(), "EMS_all"))

#######################
### Clean EMS Data ####
#######################

# Rename columns to get rid of funky codes

EMS_all <- EMS_all %>%
  rename(incident_date = Incident.Date,
         incident_number = Incident.Number,
         primary_impression =  Primary.Impression,
         arrived_on_scene_time = Times...Arrived.on.Scene.Time,
         zipcode = Incident.Postal.Code..E8.15.,
         destination_patient_disposition = Destination.Patient.Disposition..E20.10.,
         destination_code = Destination.Code..E20.2.
  )

# Filter EMS all to remove the dispositions we don't want.

EMS_all <- EMS_all %>%
  filter(destination_patient_disposition != "" &
           destination_patient_disposition != "No Patient Found" &
           destination_patient_disposition != "Cancelled en Route/On Arrival" &
           destination_patient_disposition != "Cancelled Prior to Response" &
           destination_patient_disposition != "Provided ALS personnel to scene, no transport" &
           destination_patient_disposition != "Standby Only - No Patient Contacts" &
           destination_patient_disposition != "Cancelled On Arrival" &
           destination_patient_disposition != "Operational Support Provided Only")

# Remove N/A and blank values
EMS_all <- EMS_all[-which(EMS_all$arrived_on_scene_time=="" | is.na( EMS_all$incident_date) | EMS_all$incident_number=="" | EMS_all$primary_impression=="" | EMS_all$zipcode=="" | EMS_all$destination_patient_disposition==""), ]

# remove all characters from ZIP code field
v1 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
EMS_all$zipcode <- gsub(paste0("[^", paste(v1, collapse=""), "]+"), "", EMS_all$zipcode)

# Filter for only Baltimore ZIP codes used in our analysis
balt_zips <- c("21201","21202","21205","21206","21207","21208","21209","21210","21211","21212","21213","21214", "21215","21216","21217","21218","21222","21223","21224","21225","21226","21227","21228","21229", "21230","21231","21234","21236","21237","21239","21251")

EMS_all <- subset(EMS_all, zipcode %in% balt_zips )

# create full date/time object of arrival time
EMS_all <- EMS_all %>%
  mutate(arrive_time = as.POSIXct(paste(incident_date, arrived_on_scene_time), format="%m/%d/%y %H:%M:%S")) %>%
  select(incident_date, arrived_on_scene_time, arrive_time, everything())


# add temperature time object that will merge nicely with our temperature data (which is all captured at the 54th minute of every hour) by changing minute to 54, but keeping everything else constant. 
EMS_all <- EMS_all  %>%
  mutate(temp_time = update(arrive_time, minutes = 54, seconds = 0)) %>%
  select(incident_date, arrived_on_scene_time, arrive_time, temp_time, everything())

# remove seconds from temperature time
# EMS_all$temp_time <- format(as.POSIXct(EMS_all$temp_time), "%d-%m-%Y %H:%M")
EMS_all$temp_time <- format(as.POSIXct(EMS_all$temp_time), "%m-%d-%Y %H:%M")

# Create categories for conditions we care about
EMS_all <- EMS_all %>%
  mutate(primary_impression_group = if_else(primary_impression %in% c("Pain", "Other", "Abdominal Pain/Problems", "Weakness", "Other Illness/Injury", "Nausea/Vomiting (Unknown Etiology)", "General Malaise/Sick", "Back Pain (Non-Traumatic)", "Headache", "OB/Gyn - Vaginal Hemorrhage", "Fever", "Unknown Problem", "Poisoning", "Drowning/Near Drowning", "Electrocution", "Inhalation Injury (Toxic Gas)", "Lightening Strike", "Depressurization/Scuba", "SIDS (Sudden Infant Death Syndrome)", "Traumatic Injury", "OB/Gyn - Other", "OB/Gyn - OB/Delivery", "Sepsis",  "Other CNS Problem", "Migraine", "Allergic Reaction", "Abuse / Neglect", "Stings/Venomous Bites", "Airway Obstruction", "Anaphylaxis", "Hypovolemia/Shock", "Overpressurization", "Hazmat Exposure - Chem, Bio, Rad", "Inhalation - Smoke", "Sexual Assault/Rape", "Other Endocrine/Metabolic Problem", "Bowel Obstruction", "Apparent Life-Threatening Event", "Diarrhea", "Burn", "Pregnancy/OB Delivery", "G.I. Bleed", "Other GU Problems", "Patient Assist Only", "Not Applicable", "Other Abdominal/GI Problem", "No Apparent Illness/Injury"), "Other", primary_impression),
         primary_impression_category = case_when(
           primary_impression_group %in% c("ETOH Abuse", "Withdrawal/Overdose ETOH", "Withdrawal/Overdose Drugs", "Altered Level of Consciousness", "Substance/Drug Abuse") ~ "Substance abuse",
           primary_impression_group %in% c("Cardiac Arrest", "Cardiac Rhythm Disturbance", "Hypotension", "Hypertension", "CHF (Congestive Heart Failure)", "Chest Pain/Discomfort", "Chest Pain - STEMI", "Other Cardiovascular Problem", "Abdominal Aortic Aneurysm" ) ~ "Heart/circulatory",
           primary_impression_group %in% c("Heat Exhaustion/Heat Stroke", "Hyperthermia" ) ~ "Acute heat conditions",
           primary_impression_group %in% c("Diabetic Hypoglycemia", "Diabetic Hyperglycemia") ~ "Diabetes complication",
           primary_impression_group %in% c("COPD (Emphysema/Chronic Bronchitis)", "Asthma", "Respiratory Distress", "Respiratory Arrest", "Croup") ~ "Respiratory",
           primary_impression_group %in% c("Stroke/CVA", "TIA (Transient Ischemic Attack)") ~ "Stroke",
           TRUE ~ primary_impression_group
         ))

# Save as an RDS file and as a csv for later loading
saveRDS(EMS_all, file = "data/output-data/ems/ems_clean.rds")

#########################
# Load Temperature Data #
#########################

# Load Baltimore Inner Harbor Temperature Data FROM IOWA ASOS REPO
temp_data <- read_excel("data/input-data/baltimore-inner-harbor-temperature/DMH.xlsx")

##########################
# Clean Temperature Data #
##########################

# Build a datetime object from DATE and TIME
temp_data  <- temp_data  %>%
  mutate(temp_time = as.POSIXct(DATETIME, format="%Y-%m-%d %H:%M"))

# Remove seconds from datettime object
# temp_data$temp_time <- format(as.POSIXct(temp_data$temp_time), "%d-%m-%Y %H:%M")
temp_data$temp_time <- format(as.POSIXct(temp_data$temp_time), "%m-%d-%Y %H:%M")

# Filter out missing values for temp and humidity and convert to numeric
temp_data <- temp_data %>%
  filter(DEW_POINT != 'M', TEMPERATURE != 'M') %>%
  mutate(DEW_POINT = as.numeric(DEW_POINT),
         TEMPERATURE = as.numeric(TEMPERATURE))

# DMH Calculate Heat Index Temperature -- TY, TY, TY weathermetrics package! 
temp_data <- temp_data %>%
  mutate(HEAT_INDEX = heat.index(t=TEMPERATURE, dp=DEW_POINT, temperature.metric = "fahrenheit", output.metric = "fahrenheit"))

# Sanity check to make sure we have right number of hourly temp counts
# Note some missing values because of inconsistencies in temp data
# temp_data_check <- temp_data %>%
#  group_by(year(DATETIME)) %>%
#  summarise(count = n())

# Round temperature
temp_data <- temp_data %>%
   mutate(TEMPERATURE = round(as.numeric(TEMPERATURE),0))

# Take out inconsistent values that don't match our 54 method. 
temp_data <- temp_data %>%
  filter(str_detect(temp_time, ":54")) %>%
  filter(TEMPERATURE != 1) %>%
  distinct()

# Filter dates to match our EMS call data 

temp_data <- temp_data %>%
  filter(DATETIME >= date("2014-01-1") & DATETIME <= ("2018-11-30"))

############################################
# Merge Temperature/Heat Index & EMS Data ##
############################################

# Merge data
# Note, of the 581,542 observations in EMS ALL, ~10K failed to join in the merge because we are missing temperature values for those times.  In some cases entire days are missing, in some cases it's a few hours.  Rather than introduce new errors by imputing temperature values, we're going to leave these out.  They are mostly wintertime temps.
full_data <- inner_join(EMS_all, temp_data, by=c("temp_time"))

##########################################################
# Adjust for Urban Heat Island and Calculate Heat Index ##
##########################################################

# Load urban heat island data with morning, aft and night median temperature.
urban_heat_zcta <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/zcta_clipped_lidartree_temp.csv")

# Convert geoid to zcta and select needed columns
urban_heat_zcta <- urban_heat_zcta %>%
  select(geoid10, temp_median_am, temp_median_aft, temp_median_pm) %>%
  mutate(zcta = geoid10)

# Find location of inner harbor temperature station from census block OR zcta temperature analysis and pull morning, aft and night temperature. KDMH is located right next to Science Center
# Block Number: 245102201003013
# Temp Median AM: 81.0
# Temp Median Aft: 95.1
# Temp Median PM: 90.8
# Zip Code: 21230
# Temp Median AM: 79.6
# Temp Median Aft: 95.3
# Temp Median PM: 89.8

# Values between block and zip are pretty close, let's use ZIP to be consistent.
# Note: a positive value in difference columns means warmer than inner harbor KDMH weather station at Science Center. A negative value means cooler.
urban_heat_zcta <- urban_heat_zcta %>%
  mutate(temp_median_am_kdmh_zip = 79.6,
         temp_median_aft_kdmh_zip = 95.3,
         temp_median_pm_kdmh_zip = 89.8,
         am_difference = temp_median_am - temp_median_am_kdmh_zip,
         aft_difference = temp_median_aft - temp_median_aft_kdmh_zip,
         pm_difference = temp_median_pm - temp_median_pm_kdmh_zip,
         zipcode = as.character(zcta)
         ) %>%
  select(zipcode, matches("difference"))

# Join urban_heat_zcta to EMS_all
full_data <- full_data %>%
  clean_names() %>%
  inner_join(urban_heat_zcta, by=c("zipcode"))

# Create columns with adjusted temperature
full_data <- full_data %>%
  mutate(hour = hour(datetime)) %>%
  mutate(adjusted_temperature =
           case_when(hour >= 20 ~ as.character(temperature + pm_difference),
                     hour >= 12 ~ as.character(temperature + aft_difference),
                     hour >= 4 ~ as.character(temperature + am_difference),
                     hour >= 0 ~ as.character(temperature + pm_difference)
                     ),
         adjusted_temperature = round(as.numeric(adjusted_temperature), 0)
      )

# Now that we've adjusted temperature, calculate the adjusted heat index
full_data <- full_data %>%
  mutate(adjusted_heat_index = heat.index(t=adjusted_temperature, dp=dew_point, temperature.metric = "fahrenheit", output.metric = "fahrenheit"))

##################################################
### Create Temperature and Heat Index Buckets ####
##################################################

#### Temperature Buckets #######

# Create a temperature bucket in main data set, one bucket per 10 degree block and a column so we can sort by temp_bucket nicely.
full_data <- full_data %>%
  mutate(adjusted_temp_bucket = case_when(
    adjusted_temperature >= 0 & adjusted_temperature <10 ~ "0s",
    adjusted_temperature >= 10 & adjusted_temperature <20 ~ "10s",
    adjusted_temperature >= 20 & adjusted_temperature <30 ~ "20s",
    adjusted_temperature >= 30 & adjusted_temperature <40 ~ "30s",
    adjusted_temperature >= 40 & adjusted_temperature <50 ~ "40s",
    adjusted_temperature >= 50 & adjusted_temperature <60 ~ "50s",
    adjusted_temperature >= 60 & adjusted_temperature <70 ~ "60s",
    adjusted_temperature >= 70 & adjusted_temperature <80 ~ "70s",
    adjusted_temperature >= 80 & adjusted_temperature <90 ~ "80s",
    adjusted_temperature >= 90 & adjusted_temperature <100 ~ "90s",
    adjusted_temperature >= 100 & adjusted_temperature <110 ~ "100s",
    adjusted_temperature >= 110 & adjusted_temperature <120 ~ "110s"
  ), adjusted_temp_bucket_order = case_when(
    adjusted_temp_bucket == "0s" ~ "A",
    adjusted_temp_bucket == "10s" ~ "B",
    adjusted_temp_bucket == "20s" ~ "C",
    adjusted_temp_bucket == "30s" ~ "D",
    adjusted_temp_bucket == "40s" ~ "E",
    adjusted_temp_bucket == "50s" ~ "F",
    adjusted_temp_bucket == "60s" ~ "G",
    adjusted_temp_bucket == "70s" ~ "H",
    adjusted_temp_bucket == "80s" ~ "I",
    adjusted_temp_bucket == "90s" ~ "J",
    adjusted_temp_bucket == "100s" ~ "K",
    adjusted_temp_bucket == "110s" ~ "L"
  )
)

# Create a second temperature bucket in main data set with a five-part NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_temp_nws_five_scale_bucket = case_when(
    adjusted_temperature <= 79 ~ "not_unsafe_under_79",
    adjusted_temperature >= 80 & adjusted_temperature <= 90 ~ "caution_80_90",
    adjusted_temperature >= 91 & adjusted_temperature <= 102 ~ "extreme_caution_91_102",
    adjusted_temperature >= 103 & adjusted_temperature <= 124 ~ "danger_103_124",
    adjusted_temperature >= 125 ~ "extreme_danger_125_plus"
  ), adjusted_temp_nws_five_scale_bucket_order = case_when(
    adjusted_temp_nws_five_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_temp_nws_five_scale_bucket == "caution_80_90" ~ "B",
    adjusted_temp_nws_five_scale_bucket == "extreme_caution_91_102" ~ "C",
    adjusted_temp_nws_five_scale_bucket == "danger_103_124" ~ "D",
    adjusted_temp_nws_five_scale_bucket == "extreme_danger_125_plus" ~ "E"
  )
  )

# Create a third temperature bucket in main data set with three part NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_temp_nws_three_scale_bucket = case_when(
    adjusted_temperature <= 79 ~ "not_unsafe_under_79",
    adjusted_temperature >= 80 & adjusted_temperature <= 102 ~ "all_caution_80_102",
    adjusted_temperature >= 103 ~ "all_danger_103_plus"
  ), adjusted_temp_nws_three_scale_bucket_order = case_when(
    adjusted_temp_nws_three_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_temp_nws_three_scale_bucket == "all_caution_80_102" ~ "B",
    adjusted_temp_nws_three_scale_bucket == "all_danger_103_plus" ~ "C"
  )
  )

# Create a fourth temperature bucket in main data set with a binary NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_temp_nws_binary_scale_bucket = case_when(
    adjusted_temperature <= 79 ~ "not_unsafe_under_79",
    adjusted_temperature >= 80 ~ "unsafe_80_plus"
  ), adjusted_temp_nws_scale_binary_bucket_order = case_when(
    adjusted_temp_nws_binary_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_temp_nws_binary_scale_bucket == "unsafe_80_plus" ~ "B"
  )
  )

#### Heat Index Buckets #######

# Create a heat index bucket in main data set, one bucket per 10 degree block and a column so we can sort by heat_index_bucket nicely.
full_data <- full_data %>%
  mutate(adjusted_heat_index_bucket = case_when(
    adjusted_heat_index >= 0 & adjusted_heat_index <10 ~ "0s",
    adjusted_heat_index >= 10 & adjusted_heat_index <20 ~ "10s",
    adjusted_heat_index >= 20 & adjusted_heat_index <30 ~ "20s",
    adjusted_heat_index >= 30 & adjusted_heat_index <40 ~ "30s",
    adjusted_heat_index >= 40 & adjusted_heat_index <50 ~ "40s",
    adjusted_heat_index >= 50 & adjusted_heat_index <60 ~ "50s",
    adjusted_heat_index >= 60 & adjusted_heat_index <70 ~ "60s",
    adjusted_heat_index >= 70 & adjusted_heat_index <80 ~ "70s",
    adjusted_heat_index >= 80 & adjusted_heat_index <90 ~ "80s",
    adjusted_heat_index >= 90 & adjusted_heat_index <100 ~ "90s",
    adjusted_heat_index >= 100 & adjusted_heat_index <110 ~ "100s",
    adjusted_heat_index >= 110 & adjusted_heat_index <120 ~ "110s",
    adjusted_heat_index >= 120 & adjusted_heat_index <130 ~ "120s"
  ), adjusted_heat_index_bucket_order = case_when(
    adjusted_heat_index_bucket == "0s" ~ "A",
    adjusted_heat_index_bucket == "10s" ~ "B",
    adjusted_heat_index_bucket == "20s" ~ "C",
    adjusted_heat_index_bucket == "30s" ~ "D",
    adjusted_heat_index_bucket == "40s" ~ "E",
    adjusted_heat_index_bucket == "50s" ~ "F",
    adjusted_heat_index_bucket == "60s" ~ "G",
    adjusted_heat_index_bucket == "70s" ~ "H",
    adjusted_heat_index_bucket == "80s" ~ "I",
    adjusted_heat_index_bucket == "90s" ~ "J",
    adjusted_heat_index_bucket == "100s" ~ "K",
    adjusted_heat_index_bucket == "110s" ~ "L",
    adjusted_heat_index_bucket == "120s" ~ "M"
  )
  )

# Create a second temperature bucket in main data set with a five-part NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_heat_index_nws_five_scale_bucket = case_when(
    adjusted_heat_index <= 79 ~ "not_unsafe_under_79",
    adjusted_heat_index >= 80 & adjusted_heat_index <= 90 ~ "caution_80_90",
    adjusted_heat_index >= 91 & adjusted_heat_index <= 102 ~ "extreme_caution_91_102",
    adjusted_heat_index >= 103 & adjusted_heat_index <= 124 ~ "danger_103_124",
    adjusted_heat_index >= 125 ~ "extreme_danger_125_plus"
  ), adjusted_heat_index_nws_five_scale_bucket_order = case_when(
    adjusted_heat_index_nws_five_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_heat_index_nws_five_scale_bucket == "caution_80_90" ~ "B",
    adjusted_heat_index_nws_five_scale_bucket == "extreme_caution_91_102" ~ "C",
    adjusted_heat_index_nws_five_scale_bucket == "danger_103_124" ~ "D",
    adjusted_heat_index_nws_five_scale_bucket == "extreme_danger_125_plus" ~ "E"
  )
  )

# Create a third temperature bucket in main data set with three part NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_heat_index_nws_three_scale_bucket = case_when(
    adjusted_heat_index <= 79 ~ "not_unsafe_under_79",
    adjusted_heat_index >= 80 & adjusted_heat_index <= 102 ~ "all_caution_80_102",
    adjusted_heat_index >= 103 ~ "all_danger_103_plus"
  ), adjusted_heat_index_nws_three_scale_bucket_order = case_when(
    adjusted_heat_index_nws_three_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_heat_index_nws_three_scale_bucket == "all_caution_80_102" ~ "B",
    adjusted_heat_index_nws_three_scale_bucket == "all_danger_103_plus" ~ "C"
  )
  )

# Create a fourth temperature bucket in main data set with a binary NWS temperature danger scale, and a column so we can sort nicely.
full_data <- full_data %>%
  mutate(adjusted_heat_index_nws_binary_scale_bucket = case_when(
    adjusted_heat_index <= 79 ~ "not_unsafe_under_79",
    adjusted_heat_index >= 80 ~ "unsafe_80_plus"
  ), adjusted_heat_index_nws_scale_binary_bucket_order = case_when(
    adjusted_heat_index_nws_binary_scale_bucket == "not_unsafe_under_79" ~ "A",
    adjusted_heat_index_nws_binary_scale_bucket == "unsafe_80_plus" ~ "B"
  )
  )

##############################################################################
########### RUN THESE TO JUST FILTER FOR SUMMER 2018 #########################
##############################################################################
# IF RUN THESE, SKIP DOWN TO SECOND SET OF CREATE MATRIXES

full_data <- full_data %>%
  mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  filter(datetime >= date("2018-06-21") & datetime <= ("2018-09-21"))

temp_data <- temp_data %>%
  mutate(year = year(DATETIME)) %>%
  mutate(month = month(DATETIME)) %>%
  filter(DATETIME >= date("2018-06-21") & DATETIME <= ("2018-09-20") )

##############################################################################
########### RUN THESE JUST TO JUST FILTER FOR TARGET ZIPS ####################
##############################################################################
# RUN AFTER FILTERING FOR SUMMER 2018 IF DESIRED
# IF RUN THESE, SKIP DOWN TO THIRD AND FOURTH SET OF MATRIX CODE
target_zip <- '21213' OR target_zip <- '21205'

full_data <- full_data %>%
  filter(zipcode == target_zip)



####################################################
# Create dataframe of hourly reading temp counts  ##
####################################################

# Create a dataframe with a count of hourly temperature readings for each degree
temp_count_per_degree <- temp_data %>%
  select(TEMPERATURE) %>%
  group_by(TEMPERATURE) %>%
  summarise(temp_count_per_degree=n()) %>%
  arrange(TEMPERATURE)

# Create a dataframe with a count of hourly heat index readings per degree
heat_index_count_per_degree <- temp_data %>%
  select(HEAT_INDEX) %>%
  group_by(HEAT_INDEX) %>%
  summarise(heat_index_count_per_degree=n()) %>%
  arrange(HEAT_INDEX)

#######################################################################
# Create temperature and heat index buckets in our temperature data ###
#######################################################################
#### Temperature Buckets #######

# Create a temperature bucket in temp data set, one bucket per 10 degree block and a column so we can sort by temp_bucket nicely.
temp_data <- temp_data %>%
  mutate(temp_bucket = case_when(
    TEMPERATURE >= 0 & TEMPERATURE <10 ~ "0s",
    TEMPERATURE >= 10 & TEMPERATURE <20 ~ "10s",
    TEMPERATURE >= 20 & TEMPERATURE <30 ~ "20s",
    TEMPERATURE >= 30 & TEMPERATURE <40 ~ "30s",
    TEMPERATURE >= 40 & TEMPERATURE <50 ~ "40s",
    TEMPERATURE >= 50 & TEMPERATURE <60 ~ "50s",
    TEMPERATURE >= 60 & TEMPERATURE <70 ~ "60s",
    TEMPERATURE >= 70 & TEMPERATURE <80 ~ "70s",
    TEMPERATURE >= 80 & TEMPERATURE <90 ~ "80s",
    TEMPERATURE >= 90 & TEMPERATURE <100 ~ "90s",
    TEMPERATURE >= 100 & TEMPERATURE <110 ~ "100s",
    TEMPERATURE >= 110 & TEMPERATURE <120 ~ "110s"
  ), temp_bucket_order = case_when(
    temp_bucket == "0s" ~ "A",
    temp_bucket == "10s" ~ "B",
    temp_bucket == "20s" ~ "C",
    temp_bucket == "30s" ~ "D",
    temp_bucket == "40s" ~ "E",
    temp_bucket == "50s" ~ "F",
    temp_bucket == "60s" ~ "G",
    temp_bucket == "70s" ~ "H",
    temp_bucket == "80s" ~ "I",
    temp_bucket == "90s" ~ "J",
    temp_bucket == "100s" ~ "K",
    temp_bucket == "110s" ~ "L"
  )
  )

# Create a second temperature bucket in temp data set with a five-part NWS temperature danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(temp_nws_five_scale_bucket = case_when(
    TEMPERATURE <= 79 ~ "not_unsafe_under_79",
    TEMPERATURE >= 80 & TEMPERATURE <= 90 ~ "caution_80_90",
    TEMPERATURE >= 91 & TEMPERATURE <= 102 ~ "extreme_caution_91_102",
    TEMPERATURE >= 103 & TEMPERATURE <= 124 ~ "danger_103_124",
    TEMPERATURE >= 125 ~ "extreme_danger_125_plus"
  ), temp_nws_five_scale_bucket_order = case_when(
    temp_nws_five_scale_bucket == "not_unsafe_under_79" ~ "A",
    temp_nws_five_scale_bucket == "caution_80_90" ~ "B",
    temp_nws_five_scale_bucket == "extreme_caution_91_102" ~ "C",
    temp_nws_five_scale_bucket == "danger_103_124" ~ "D",
    temp_nws_five_scale_bucket == "extreme_danger_125_plus" ~ "E"
  )
  )

# Create a third temperature bucket in temp data set with three part NWS temperature danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(temp_nws_three_scale_bucket = case_when(
    TEMPERATURE <= 79 ~ "not_unsafe_under_79",
    TEMPERATURE >= 80 & TEMPERATURE <= 102 ~ "all_caution_80_102",
    TEMPERATURE >= 103 ~ "all_danger_103_plus"
  ), temp_nws_three_scale_bucket_order = case_when(
    temp_nws_three_scale_bucket == "not_unsafe_under_79" ~ "A",
    temp_nws_three_scale_bucket == "all_caution_80_102" ~ "B",
    temp_nws_three_scale_bucket == "all_danger_103_plus" ~ "C"
  )
  )

# Create a fourth temperature bucket in temp data set with a binary NWS temperature danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(temp_nws_binary_scale_bucket = case_when(
    TEMPERATURE <= 79 ~ "not_unsafe_under_79",
    TEMPERATURE >= 80 ~ "unsafe_80_plus"
  ), temp_nws_scale_binary_bucket_order = case_when(
    temp_nws_binary_scale_bucket == "not_unsafe_under_79" ~ "A",
    temp_nws_binary_scale_bucket == "unsafe_80_plus" ~ "B"
  )
  )

#### Heat Index Buckets #######

# Create a heat index bucket in temp data set, one bucket per 10 degree block and a column so we can sort by temp_bucket nicely.
temp_data <- temp_data %>%
  mutate(heat_index_bucket = case_when(
    HEAT_INDEX >= 0 & HEAT_INDEX <10 ~ "0s",
    HEAT_INDEX >= 10 & HEAT_INDEX <20 ~ "10s",
    HEAT_INDEX >= 20 & HEAT_INDEX <30 ~ "20s",
    HEAT_INDEX >= 30 & HEAT_INDEX <40 ~ "30s",
    HEAT_INDEX >= 40 & HEAT_INDEX <50 ~ "40s",
    HEAT_INDEX >= 50 & HEAT_INDEX <60 ~ "50s",
    HEAT_INDEX >= 60 & HEAT_INDEX <70 ~ "60s",
    HEAT_INDEX >= 70 & HEAT_INDEX <80 ~ "70s",
    HEAT_INDEX >= 80 & HEAT_INDEX <90 ~ "80s",
    HEAT_INDEX >= 90 & HEAT_INDEX <100 ~ "90s",
    HEAT_INDEX >= 100 & HEAT_INDEX <110 ~ "100s",
    HEAT_INDEX >= 110 & HEAT_INDEX <120 ~ "110s",
    HEAT_INDEX >= 120 & HEAT_INDEX <130 ~ "120s"
  ), heat_index_bucket_order = case_when(
    heat_index_bucket == "0s" ~ "A",
    heat_index_bucket == "10s" ~ "B",
    heat_index_bucket == "20s" ~ "C",
    heat_index_bucket == "30s" ~ "D",
    heat_index_bucket == "40s" ~ "E",
    heat_index_bucket == "50s" ~ "F",
    heat_index_bucket == "60s" ~ "G",
    heat_index_bucket == "70s" ~ "H",
    heat_index_bucket == "80s" ~ "I",
    heat_index_bucket == "90s" ~ "J",
    heat_index_bucket == "100s" ~ "K",
    heat_index_bucket == "110s" ~ "L",
    heat_index_bucket == "120s" ~ "M"
  )
  )

# Create a second HEAT_INDEX bucket in temp data set with a five-part NWS HEAT_INDEX danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(heat_index_nws_five_scale_bucket = case_when(
    HEAT_INDEX <= 79 ~ "not_unsafe_under_79",
    HEAT_INDEX >= 80 & HEAT_INDEX <= 90 ~ "caution_80_90",
    HEAT_INDEX >= 91 & HEAT_INDEX <= 102 ~ "extreme_caution_91_102",
    HEAT_INDEX >= 103 & HEAT_INDEX <= 124 ~ "danger_103_124",
    HEAT_INDEX >= 125 ~ "extreme_danger_125_plus"
  ), heat_index_nws_five_scale_bucket_order = case_when(
    heat_index_nws_five_scale_bucket == "not_unsafe_under_79" ~ "A",
    heat_index_nws_five_scale_bucket == "caution_80_90" ~ "B",
    heat_index_nws_five_scale_bucket == "extreme_caution_91_102" ~ "C",
    heat_index_nws_five_scale_bucket == "danger_103_124" ~ "D",
    heat_index_nws_five_scale_bucket == "extreme_danger_125_plus" ~ "E"
  )
  )

# Create a third HEAT_INDEX bucket in temp data set with three part NWS HEAT_INDEX danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(heat_index_nws_three_scale_bucket = case_when(
    HEAT_INDEX <= 79 ~ "not_unsafe_under_79",
    HEAT_INDEX >= 80 & HEAT_INDEX <= 102 ~ "all_caution_80_102",
    HEAT_INDEX >= 103 ~ "all_danger_103_plus"
  ), heat_index_nws_three_scale_bucket_order = case_when(
    heat_index_nws_three_scale_bucket == "not_unsafe_under_79" ~ "A",
    heat_index_nws_three_scale_bucket == "all_caution_80_102" ~ "B",
    heat_index_nws_three_scale_bucket == "all_danger_103_plus" ~ "C"
  )
  )

# Create a fourth HEAT_INDEX bucket in temp data set with a binary NWS HEAT_INDEX danger scale, and a column so we can sort nicely.
temp_data <- temp_data %>%
  mutate(heat_index_nws_binary_scale_bucket = case_when(
    HEAT_INDEX <= 79 ~ "not_unsafe_under_79",
    HEAT_INDEX >= 80 ~ "unsafe_80_plus"
  ), heat_index_nws_scale_binary_bucket_order = case_when(
    heat_index_nws_binary_scale_bucket == "not_unsafe_under_79" ~ "A",
    heat_index_nws_binary_scale_bucket == "unsafe_80_plus" ~ "B"
  )
  )


##################################################################################
# Create dataframes with a count of hourly temperature readings for each bucket ##
##################################################################################

# 10 degree block buckets
temp_count_per_bucket <- temp_data %>%
  select(temp_bucket) %>%
  group_by(temp_bucket) %>%
  summarise(temp_count_per_bucket=n()) %>%
  arrange(temp_bucket)

# Five-part NWS temperature danger scale
temp_count_per_nws_five_scale_bucket <- temp_data %>%
  select(temp_nws_five_scale_bucket) %>%
  group_by(temp_nws_five_scale_bucket) %>%
  summarise(temp_count_per_nws_five_scale_bucket=n()) %>%
  arrange(temp_nws_five_scale_bucket)

# Three-part NWS temperature danger scale
temp_count_per_nws_three_scale_bucket <- temp_data %>%
  select(temp_nws_three_scale_bucket) %>%
  group_by(temp_nws_three_scale_bucket) %>%
  summarise(temp_count_per_nws_three_scale_bucket=n()) %>%
  arrange(temp_nws_three_scale_bucket)

# Binary NWS temperature danger scale
temp_count_per_nws_binary_scale_bucket <- temp_data %>%
  select(temp_nws_binary_scale_bucket) %>%
  group_by(temp_nws_binary_scale_bucket) %>%
  summarise(temp_count_per_nws_binary_scale_bucket=n()) %>%
  arrange(temp_nws_binary_scale_bucket)

##################################################################################
# Create dataframes with a count of hourly heat_index readings for each bucket ###
##################################################################################

# 10 degree block buckets
heat_index_count_per_bucket <- temp_data %>%
  select(heat_index_bucket) %>%
  group_by(heat_index_bucket) %>%
  summarise(heat_index_count_per_bucket=n()) %>%
  arrange(heat_index_bucket)

# Five-part NWS heat index danger scale
heat_index_count_per_nws_five_scale_bucket <- temp_data %>%
  select(heat_index_nws_five_scale_bucket) %>%
  group_by(heat_index_nws_five_scale_bucket) %>%
  summarise(heat_index_count_per_nws_five_scale_bucket=n()) %>%
  arrange(heat_index_nws_five_scale_bucket)

# Three-part NWS heat index danger scale
heat_index_count_per_nws_three_scale_bucket <- temp_data %>%
  select(heat_index_nws_three_scale_bucket) %>%
  group_by(heat_index_nws_three_scale_bucket) %>%
  summarise(heat_index_count_per_nws_three_scale_bucket=n()) %>%
  arrange(heat_index_nws_three_scale_bucket)

# Binary NWS heat index danger scale
heat_index_count_per_nws_binary_scale_bucket <- temp_data %>%
  select(heat_index_nws_binary_scale_bucket) %>%
  group_by(heat_index_nws_binary_scale_bucket) %>%
  summarise(heat_index_count_per_nws_binary_scale_bucket=n()) %>%
  arrange(heat_index_nws_binary_scale_bucket)

##################################################################################
# Create a dataframe with a count of calls for each degree temperature ###########
##################################################################################
 
all_calls_count_per_degree_temperature <- full_data %>%
  select(adjusted_temperature) %>%
  group_by(adjusted_temperature) %>%
  summarise(all_calls_count_per_degree=n()) %>%
  arrange(adjusted_temperature)

##################################################################################
# Create a dataframe with a count of calls for each degree heat index ############
##################################################################################

all_calls_count_per_degree_heat_index <- full_data %>%
  select(adjusted_heat_index) %>%
  group_by(adjusted_heat_index) %>%
  summarise(all_calls_count_per_degree=n()) %>%
  arrange(adjusted_heat_index)

##################################################################################
# Create dataframes with a count of calls for each temperature bucket ############
##################################################################################

# 10 degree block buckets
all_calls_count_per_bucket_temp <- full_data %>%
  select(adjusted_temp_bucket) %>%
  group_by(adjusted_temp_bucket) %>%
  summarise(all_calls_count_per_bucket=n()) %>%
  arrange(adjusted_temp_bucket)

# Five-part NWS temperature danger scale
all_calls_count_per_nws_five_scale_bucket_temp <- full_data %>%
  select(adjusted_temp_nws_five_scale_bucket) %>%
  group_by(adjusted_temp_nws_five_scale_bucket) %>%
  summarise(all_calls_count_per_nws_five_scale_bucket=n()) %>%
  arrange(adjusted_temp_nws_five_scale_bucket)

# Three-part NWS temperature danger scale
all_calls_count_per_nws_three_scale_bucket_temp <- full_data %>%
  select(adjusted_temp_nws_three_scale_bucket) %>%
  group_by(adjusted_temp_nws_three_scale_bucket) %>%
  summarise(all_calls_count_per_nws_three_scale_bucket=n()) %>%
  arrange(adjusted_temp_nws_three_scale_bucket)

# Binary NWS temperature danger scale
all_calls_count_per_nws_binary_scale_bucket_temp <- full_data %>%
  select(adjusted_temp_nws_binary_scale_bucket) %>%
  group_by(adjusted_temp_nws_binary_scale_bucket) %>%
  summarise(all_calls_count_per_nws_binary_scale_bucket=n()) %>%
  arrange(adjusted_temp_nws_binary_scale_bucket)


##################################################################################
# Create dataframes with a count of calls for each heat index bucket #############
##################################################################################

# 10 degree block buckets
all_calls_count_per_bucket_heat_index <- full_data %>%
  select(adjusted_heat_index_bucket) %>%
  group_by(adjusted_heat_index_bucket) %>%
  summarise(all_calls_count_per_bucket=n()) %>%
  arrange(adjusted_heat_index_bucket)

# Five-part NWS heat index danger scale
all_calls_count_per_nws_five_scale_bucket_heat_index <- full_data %>%
  select(adjusted_heat_index_nws_five_scale_bucket) %>%
  group_by(adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(all_calls_count_per_nws_five_scale_bucket=n()) %>%
  arrange(adjusted_heat_index_nws_five_scale_bucket)

# Three-part NWS heat index danger scale
all_calls_count_per_nws_three_scale_bucket_heat_index <- full_data %>%
  select(adjusted_heat_index_nws_three_scale_bucket) %>%
  group_by(adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(all_calls_count_per_nws_three_scale_bucket=n()) %>%
  arrange(adjusted_heat_index_nws_three_scale_bucket)

# Binary NWS heat index danger scale
all_calls_count_per_nws_binary_scale_bucket_heat_index <- full_data %>%
  select(adjusted_heat_index_nws_binary_scale_bucket) %>%
  group_by(adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(all_calls_count_per_nws_binary_scale_bucket=n()) %>%
  arrange(adjusted_heat_index_nws_binary_scale_bucket)

#################################################################
### Create Matrixes for entire data, NOT JUST SUMMER 2018 #######
#################################################################
# If you have used the UNLOCK section above to filter just for summer months, DO NOT USE this, skip down to next set of create matrixes.


########### 10 Degree Temperature Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | 10 Degree Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

all_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  # inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | 10 Degree Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

all_sd_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`, `110s`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | 10 Degree Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

all_percent_calls_condition_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `0s`, `10s`, `20s`, `30s`,`40s`,`50s`,`60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

########### Binary NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | Binary NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

all_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | Binary NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

all_sd_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate_at(vars(contains("safe")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | Binary NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

all_percent_calls_condition_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_binary_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

########### Three NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | three NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

all_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | three NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

all_sd_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | three NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

all_percent_calls_condition_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_three_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

########### Five NWS Buckets #################
#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | five NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

all_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | five NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

all_sd_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | five NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

all_percent_calls_condition_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_five_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)

############################################################
### Create Matrixes JUST FOR SUMMER 2018 ########################
############################################################
# If you have used the UNLOCK section above to filter just for summer months, USE THIS

########### 10 Degree Temperature Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | 10 Degree Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

su_2018_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | 10 Degree Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

su_2018_sd_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | 10 Degree Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

su_2018_percent_calls_condition_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

########### Binary NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | Binary NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

su_2018_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | Binary NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

su_2018_sd_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate_at(vars(contains("safe")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | Binary NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

su_2018_percent_calls_condition_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_binary_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

########### Three NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | three NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

su_2018_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | three NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

su_2018_sd_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | three NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

su_2018_percent_calls_condition_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_three_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

########### Five NWS Buckets #################
#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | five NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

su_2018_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | five NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

su_2018_sd_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | five NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

su_2018_percent_calls_condition_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_five_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)



#####################################################################################
### Create Matrixes JUST FOR ONE TARGET ZIP CODE SUMMER 2018 ########################
#####################################################################################
# If you have used the UNLOCK section above to filter just for summer months, USE THIS

########### 10 Degree Temperature Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | 10 Degree Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

tz_su_2018_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | 10 Degree Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

tz_su_2018_sd_call_heat_index_ratio_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`60s`,`70s`, `80s`, `90s`, `100s`, `110s`))) %>%
  mutate_at(vars(contains("0s")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("0s")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | 10 Degree Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

tz_su_2018_percent_calls_condition_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_bucket_heat_index, by = "adjusted_heat_index_bucket") %>%
  inner_join(heat_index_count_per_bucket, by = c("adjusted_heat_index_bucket" = "heat_index_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `60s`,`70s`, `80s`, `90s`, `100s`, `110s`)

########### Binary NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | Binary NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

tz_su_2018_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | Binary NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

tz_su_2018_sd_call_heat_index_ratio_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_binary_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`,`unsafe_80_plus`))) %>%
  mutate_at(vars(contains("safe")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | Binary NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

tz_su_2018_percent_calls_condition_binary_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_binary_scale_bucket_heat_index, by = "adjusted_heat_index_nws_binary_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_binary_scale_bucket, by = c("adjusted_heat_index_nws_binary_scale_bucket" = "heat_index_nws_binary_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_binary_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_binary_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`,`unsafe_80_plus`)

########### Three NWS Buckets #################

#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | three NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

tz_su_2018_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | three NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

tz_su_2018_sd_call_heat_index_ratio_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_three_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | three NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

tz_su_2018_percent_calls_condition_three_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_three_scale_bucket_heat_index, by = "adjusted_heat_index_nws_three_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_three_scale_bucket, by = c("adjusted_heat_index_nws_three_scale_bucket" = "heat_index_nws_three_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_three_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_three_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `all_caution_80_102`, `all_danger_103_plus`)

########### Five NWS Buckets #################
#### Table 1 | Primary Impression Group | Ratio Condition Calls v Heat Index | five NWS Buckets #####
# Ratio of number of calls for each condition type in each heat index bucket to total number of hours in each given temperature bucket. A lower number indicates a higher number of calls for each condition adjusted for the fact that some temperatures are simply more common than other others. It's 70 degrees for many more hours in a year than it is 110.  

tz_su_2018_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)

#### Table 2 | Primary Impression Group | Ratio Condition Calls v Heat Index converted to SD | five NWS Buckets #####
# This takes the table above, and converts the values into standard deviation above and below mean. This makes it a) easier to compare across conditions and b) easier to intepret by humans (hopefully).  A value of 1 is one standard deviation above the mean.  A value of 2 is two standard deviations above the mean.  A value of -1 is one standard deviation below the mean.  So, when the value for heart attacks is 1 when it's in the heat index danger zone, and -1 when it's 70, that means heart attacks are more common, relatively speaking, when it's 110.  

tz_su_2018_sd_call_heat_index_ratio_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(calls_to_heat_index_ratio = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, calls_to_heat_index_ratio) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`) %>%
  rowwise() %>%
  mutate(call_to_heat_index_ratio_mean = mean(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate(call_to_heat_index_ratio_sd = sd(na.rm=TRUE, c(`not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`))) %>%
  mutate_at(vars(contains("safe"), contains("caution"), contains("danger")), ~((.-call_to_heat_index_ratio_mean)/call_to_heat_index_ratio_sd)*-1) %>%
  rename_at(vars(contains("safe"), contains("caution"), contains("danger")), funs(paste0(.,"_-sd"))) %>%
  mutate(`sd_%_of_mean` = (call_to_heat_index_ratio_sd/call_to_heat_index_ratio_mean)*100)

#### Table 3 | Primary Impression Group | Percentage of Calls | five NWS Buckets #####
# Percentage expressed as number of calls for a given condition in a given bucket dividied by total number of calls for that bucket.
# As in: When the temps are above 80, 4 percent of all calls at that temperature are for Asthma.

tz_su_2018_percent_calls_condition_five_primary_impression_group <- full_data %>%
  filter(primary_impression_group != "Other") %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(all_calls_count_per_nws_five_scale_bucket_heat_index, by = "adjusted_heat_index_nws_five_scale_bucket") %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(percent_calls_w_condition = (condition_calls_count_per_bucket/all_calls_count_per_nws_five_scale_bucket)*100) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, percent_calls_w_condition) %>%
  select(primary_impression_group, `not_unsafe_under_79`, `caution_80_90`, `extreme_caution_91_102`, `danger_103_124`)

### Write to Output Folder
write_csv(tz_su_2018_call_heat_index_ratio_five_primary_impression_group, "data/output-data/ems/tz_su_2018_call_heat_index_ratio_five_primary_impression_group.csv")
write_csv(tz_su_2018_sd_call_heat_index_ratio_five_primary_impression_group, "data/output-data/ems/tz_su_2018_sd_call_heat_index_ratio_five_primary_impression_group.csv")
write_csv(tz_su_2018_percent_calls_condition_five_primary_impression_group, "data/output-data/ems/tz_su_2018_percent_calls_condition_five_primary_impression_group.csv")

write_csv(tz_su_2018_call_heat_index_ratio_three_primary_impression_group, "data/output-data/ems/tz_su_2018_call_heat_index_ratio_three_primary_impression_group.csv")
write_csv(tz_su_2018_sd_call_heat_index_ratio_three_primary_impression_group, "data/output-data/ems/tz_su_2018_sd_call_heat_index_ratio_three_primary_impression_group.csv")
write_csv(tz_su_2018_percent_calls_condition_three_primary_impression_group, "data/output-data/ems/tz_su_2018_percent_calls_condition_three_primary_impression_group.csv")

write_csv(tz_su_2018_call_heat_index_ratio_binary_primary_impression_group, "data/output-data/ems/tz_su_2018_call_heat_index_ratio_binary_primary_impression_group.csv")
write_csv(tz_su_2018_sd_call_heat_index_ratio_binary_primary_impression_group, "data/output-data/ems/tz_su_2018_sd_call_heat_index_ratio_binary_primary_impression_group.csv")
write_csv(tz_su_2018_percent_calls_condition_binary_primary_impression_group, "data/output-data/ems/tz_su_2018_percent_calls_condition_binary_primary_impression_group.csv")

write_csv(tz_su_2018_call_heat_index_ratio_primary_impression_group, "data/output-data/ems/tz_su_2018_call_heat_index_ratio_primary_impression_group.csv")
write_csv(tz_su_2018_sd_call_heat_index_ratio_primary_impression_group, "data/output-data/ems/tz_su_2018_sd_call_heat_index_ratio_primary_impression_group.csv")
write_csv(tz_su_2018_percent_calls_condition_primary_impression_group, "data/output-data/ems/tz_su_2018_percent_calls_condition_primary_impression_group.csv")



### Write to Output Folder
write_csv(su_2018_call_heat_index_ratio_five_primary_impression_group, "data/output-data/ems/su_2018_call_heat_index_ratio_five_primary_impression_group.csv")
write_csv(su_2018_sd_call_heat_index_ratio_five_primary_impression_group, "data/output-data/ems/su_2018_sd_call_heat_index_ratio_five_primary_impression_group.csv")
write_csv(su_2018_percent_calls_condition_five_primary_impression_group, "data/output-data/ems/su_2018_percent_calls_condition_five_primary_impression_group.csv")

write_csv(su_2018_call_heat_index_ratio_three_primary_impression_group, "data/output-data/ems/su_2018_call_heat_index_ratio_three_primary_impression_group.csv")
write_csv(su_2018_sd_call_heat_index_ratio_three_primary_impression_group, "data/output-data/ems/su_2018_sd_call_heat_index_ratio_three_primary_impression_group.csv")
write_csv(su_2018_percent_calls_condition_three_primary_impression_group, "data/output-data/ems/su_2018_percent_calls_condition_three_primary_impression_group.csv")

write_csv(su_2018_call_heat_index_ratio_binary_primary_impression_group, "data/output-data/ems/su_2018_call_heat_index_ratio_binary_primary_impression_group.csv")
write_csv(su_2018_sd_call_heat_index_ratio_binary_primary_impression_group, "data/output-data/ems/su_2018_sd_call_heat_index_ratio_binary_primary_impression_group.csv")
write_csv(su_2018_percent_calls_condition_binary_primary_impression_group, "data/output-data/ems/su_2018_percent_calls_condition_binary_primary_impression_group.csv")

write_csv(su_2018_call_heat_index_ratio_primary_impression_group, "data/output-data/ems/su_2018_call_heat_index_ratio_primary_impression_group.csv")
write_csv(su_2018_sd_call_heat_index_ratio_primary_impression_group, "data/output-data/ems/su_2018_sd_call_heat_index_ratio_primary_impression_group.csv")
write_csv(su_2018_percent_calls_condition_primary_impression_group, "data/output-data/ems/su_2018_percent_calls_condition_primary_impression_group.csv")
