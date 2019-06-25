#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("janitor")

library(tidyverse)
library(readxl)
library(janitor)
library(magrittr)

# Turn off scientific notation
options(scipen = 999)

# For cleanup
rm(list=ls())

#################################################################
######## Load and Clean LIDAR Tree Data #########################
#################################################################

#### Block ####
# 2007
tree_block_lidar_2007 <- read_csv("treecover/by_block/btree_statistics_by_block_2007_lidar.csv") %>%
  rename_all(tolower)

# 2015
tree_block_lidar_2015 <- read_csv("treecover/by_block/btree_statistics_by_block_2015_lidar.csv") %>%
  rename_all(tolower)

# Join 2009 to 2015
tree_block_lidar_2007_2015 <- tree_block_lidar_2007 %>%
  left_join(tree_block_lidar_2015) %>%
  mutate(
    lid_change_percent = (`15_mean`-`07_mean`)/`07_mean`,
    lid_change_percent_point = (`15_mean`-`07_mean`)
  ) %>%
  rename("07_lid_mean" = "07_mean",
         "15_lid_mean" = "15_mean") %>%
  # Recast non-calculable variables as characters
  mutate_at(vars(matches("geoid10"), matches("statefp10"), 
                 matches("countyfp10"), matches("blockce10")
  ),
  as.character)



# How many times do blocks divide by zero to find the percent change? 793 compared to 12805 (12%)
#tree_block_lidar_2007_2015 %>% group_by(lid_change_percent) %>% filter(is.nan(lid_change_percent)) %>% summarize(n=n())
#tree_block_lidar_2007_2015 %>% filter(!is.nan(lid_change_percent)) %>% summarize(n=n())

#### CSA ####

# 2007
tree_csa_lidar_2007 <- read_csv("treecover/by_community_statistical_area/btree_statistics_by_csa_2007_lidar.csv")

# 2015
tree_csa_lidar_2015 <- read_csv("treecover/by_community_statistical_area/btree_statistics_by_csa_2015_lidar.csv")

# Join 2007 to 2015
tree_csa_lidar_2007_2015 <- tree_csa_lidar_2007 %>%
  left_join(tree_csa_lidar_2015) %>%
  mutate(
    lid_change_percent = (`15_mean`-`07_mean`)/`07_mean`,
    lid_change_percent_point = (`15_mean`-`07_mean`)
  ) %>%
  rename_all(tolower) %>%
  select(-matches("17\\-*")) %>%
  rename("07_lid_mean" = "07_mean",
         "15_lid_mean" = "15_mean") %>%
  # Recast non-calculable variables as characters
  mutate("objectid" = as.character(objectid)) 


#### NSA ####

# 2007
tree_nsa_lidar_2007 <- read_csv("treecover/by_neighborhood_statistical_area/btree_statistics_by_nsa_2007_lidar.csv")

# 2015
tree_nsa_lidar_2015 <- read_csv("treecover/by_neighborhood_statistical_area/btree_statistics_by_nsa_2015_lidar.csv")

# Join 2009 to 2015
tree_nsa_lidar_2007_2015 <- tree_nsa_lidar_2007 %>%
  left_join(tree_nsa_lidar_2015) %>%
  mutate(
    lid_change_percent = (`15_mean`-`07_mean`)/`07_mean`,
    lid_change_percent_point = (`15_mean`-`07_mean`)
  ) %>%
  rename_all(tolower) %>%
  rename("07_lid_mean" = "07_mean",
         "15_lid_mean" = "15_mean") %>%
  select(-matches("nbrdesc"), -matches("color_2")) %>%
  # Recast non-calculable variables as characters
  mutate("objectid" = as.character(objectid)) 


#### ZCTA CLIPPED LIDAR ####

# 2007
tree_zcta_clipped_lidar_2007 <- read_csv("data/input-data/by_zcta_clipped_at_balt_city_border/btree_statistics_by_zcta_2007_lidar.csv")

# 2015
tree_zcta_clipped_lidar_2015 <- read_csv("data/input-data/by_zcta_clipped_at_balt_city_border/btree_statistics_by_zcta_2015_lidar.csv")

# Join 2007 to 2015
tree_zcta_clipped_lidar_2009_2015 <- tree_zcta_clipped_lidar_2007 %>%
  left_join(tree_zcta_clipped_lidar_2015) %>%
  mutate(
    lid_change_percent = (`15_mean`-`07_mean`)/`07_mean`,
    change_percent_point = (`15_mean`-`07_mean`)
  ) %>%
  rename_all(tolower) %>%
  rename("07_lid_mean" = "07_mean",
         "15_lid_mean" = "15_mean") %>%
  select(-matches("nbrdesc"), -matches("color_2")) %>%
  # Recast non-calculable variables as characters
  mutate("zcta5ce10" = as.character(zcta5ce10),
         "geoid10" = as.character(geoid10))

# Remove unneeded files
rm(list=setdiff(ls(), c("tree_block_lidar_2007_2015", "tree_zcta_clipped_lidar_2009_2015", "tree_nsa_lidar_2007_2015", "tree_csa_lidar_2007_2015", "all_temp_block", "all_temp_zcta", "all_temp_nsa", "all_temp_csa")))


###########################################
### Load Clipped ZCTA temperature data ####
###########################################

# Create a function to convert celcius values to fahrenheit
c_to_f_convert <- function(x) (x * (9/5) + 32)

#######################
###### Afternoon ######
#######################

# Afternoon temperatures by clipped ZCTA (ZIP Code Tabulation Area)
temp_aft_clipped_zcta <- read_csv("data/input-data/temperature_values/afternoon/temp-by-zcta-clipped-to-balt-city-border_af.csv") 
temp_aft_clipped_zcta <- temp_aft_clipped_zcta %>%
  rename(temp_mean_aft = t_af_mean,
         temp_median_aft = t_af_median,
         temp_min_aft = t_af_min,
         temp_max_aft = t_af_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

#######################
###### Morning ########
#######################

# Morning temperatures by clipped ZCTA (ZIP Code Tabulation Area)
temp_am_clipped_zcta <- read_csv("data/input-data/temperature_values/morning/temp-by-zcta-clipped-to-balt-city-border_am.csv") 
temp_am_clipped_zcta <- temp_am_clipped_zcta %>%
  rename(temp_mean_am = t_am_mean,
         temp_median_am = t_am_median,
         temp_min_am = t_am_min,
         temp_max_am = t_am_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

#######################
###### Evening ########
#######################

# Evening temperatures by clipped ZCTA (ZIP Code Tabulation Area)
temp_pm_clipped_zcta <- read_csv("data/input-data/temperature_values/evening/temp-by-zcta-clipped-to-balt-city-border_pm.csv") 
temp_pm_clipped_zcta <- temp_pm_clipped_zcta %>%
  rename(temp_mean_pm = t_pm_mean,
         temp_median_pm = t_pm_media,
         temp_min_pm = t_pm_min,
         temp_max_pm = t_pm_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)


######################################
###### JOIN AM, AFTERNOON, PM ########
######################################


all_temp_clipped_zcta <- temp_am_clipped_zcta %>%
  left_join(temp_aft_clipped_zcta) %>%
  left_join(temp_pm_clipped_zcta) %>%
  mutate_at(vars(matches("ZCTA5CE10"), matches("GEOID10")), as.character) %>% # Recast non-calculable variables as characters
  rename_all(tolower)

# Remove unneeded files
rm(list=setdiff(ls(), c("all_temp_block", "all_temp_clipped_zcta", "all_temp_nsa", "all_temp_csa", "tree_block_lidar_2007_2015", "tree_zcta_clipped_lidar_2009_2015", "tree_nsa_lidar_2007_2015", "tree_csa_lidar_2007_2015", "all_temp_block", "all_temp_zcta", "all_temp_nsa", "all_temp_csa")))

###############################################################################################
### Load precleaned NAIP, Landsat, AirbusTree Tree Data and demographic + temperature data ####
###############################################################################################

## Block
pre_cleaned_block_data <- read_csv("output/data/cleaned/without-lidar/blocks_tree_temp_demographics.csv") %>%
  # Recast non-calculable variables as characters
  mutate_at(vars(matches("geoid10"), matches("statefp10"), 
                 matches("countyfp10"), matches("blockce10")
  ),
  as.character)

## CSA
pre_cleaned_csa_data<- read_csv("output/data/cleaned/without-lidar/csa_tree_temp_demographics.csv") %>%
  # Recast non-calculable variables as characters
  mutate("objectid" = as.character(objectid)) 

## NSA
pre_cleaned_nsa_data <- read_csv("output/data/cleaned/without-lidar/nsa_tree_temp.csv") %>%
  # Recast non-calculable variables as characters
  mutate("objectid" = as.character(objectid))

# ZCTA unclipped NAIP
pre_cleaned_unclipped_zcta_data <- read_csv("output/data/cleaned/without-lidar/zcta_tree_temp_demographics.csv")

################################################################
### Join LIDAR Tree Data with NAIP/Landsat/Airbus Tree Data ####
### and Demographic + Temperature data #########################
################################################################

all_cleaned_block_data <- pre_cleaned_block_data %>%
  left_join(tree_block_lidar_2007_2015)

all_cleaned_csa_data <- pre_cleaned_csa_data %>%
  left_join(tree_csa_lidar_2007_2015)

all_cleaned_nsa_data <- pre_cleaned_nsa_data %>%
  left_join(tree_nsa_lidar_2007_2015)

all_cleaned_zcta_clipped_data <- all_temp_clipped_zcta %>%
  left_join(tree_zcta_clipped_lidar_2009_2015)

# all_cleaned_zcta_data <- pre_cleaned_zcta_data %>%
#   left_join(tree_zcta_lidar_2007_2015)

# Remove unneeded files
rm(list=setdiff(ls(), c("all_cleaned_block_data", "all_cleaned_csa_data", "all_cleaned_nsa_data", "all_cleaned_zcta_data")))


#################################################################
######## Write merged files out for later use ###################
#################################################################

write_csv(all_cleaned_block_data,"data/output-data/cleaned/with-lidar/blocks_lidartree_temp_demographics.csv")
write_csv(all_cleaned_csa_data,"data/output-data/cleaned/with-lidar/csa_lidartree_temp_demographics.csv")
write_csv(all_cleaned_nsa_data,"data/output-data/cleaned/with-lidar/nsa_lidartree_temp.csv")
write_csv(all_cleaned_zcta_clipped_data,"data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/zcta_clipped_lidartree_temp_demographics.csv")
# write_csv(zcta_tree_temp_demographics,"output/data/cleaned/zcta_lidartree_temp_demographics.csv")
