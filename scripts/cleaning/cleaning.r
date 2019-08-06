##### Baltimore Climate and Health Project #####
##### By Sean Mussenden, Roxanne Ready, Jake Gluck and Jane Gerard #####

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

# FIXED, June 12 -Rox: ZCTA TEMPERATURE AND TREE FILES HAVE PROBLEMS.  We have duplicate zip codes 21227:4 ; 21226:3 ; 21222: 2 ; 21225 : 2.  The Demographics ZCTA has the correct list of ZIPS. Need to get with Roxanne to debug. 
# zcta_tree_temp_demographics %>% group_by(ZIPCODE2) %>% filter(n()>1) %>% summarize(n=n())

#################################################################
######## Load and Clean Temperature Data ########################
#################################################################

## CNS analysis of Urban Heat Island study, which captured micro-level temperature variations for all of Baltimore City on August 29, 2018. Data: https://osf.io/e63x9/. Full methodology paper: https://osf.io/ur7my/.
## Temperature data was compiled in three batches for morning, afternoon and evening, in a raster image with one temperature (Celcius) per pixels.
## In QGIS, for each time period, CNS calculated zonal statistics (mean temp, median temp, min temp and max temp) for several geographic units. 
## The geographic units are: U.S. Census blocks, ZCTAs (ZIP Code tabulation areas, a U.S. Census proxy for ZIP Code units), Neighborhood Statistical Areas (the best representation of true Baltimore neighborhoods, comprised of custom groupings of U.S. Census blocks, by Baltimore City) and Community Statistical Areas (groupings of neighborhoods, via BNIA. In some cases, it's one neighborhood per CSA. In some cases multiple).
## The files were exported from QGIS as CSVs and will be loaded in here. 

# Create a function to convert celcius values to fahrenheit
c_to_f_convert <- function(x) (x * (9/5) + 32)

#######################
###### Afternoon ######
#######################

# Afternoon temperatures by Block
temp_aft_block <- read_csv("urban_heat_island_temperature/temperature_values/afternoon/temp-by-block_af.csv") %>%
  filter(COUNTYFP10 == "510") %>%
  rename(temp_mean_aft = temp_mean,
         temp_median_aft = temp_median,
         temp_min_aft = temp_min,
         temp_max_aft = temp_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Afternoon temperatures by ZCTA (ZIP Code Tabulation Area)
temp_aft_zcta <- read_csv("urban_heat_island_temperature/temperature_values/afternoon/temp-by-zcta_af.csv") %>%
  rename(temp_mean_aft = t_af_mean,
         temp_median_aft = t_af_median,
         temp_min_aft = t_af_min,
         temp_max_aft = t_af_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Afternoon temperatures by Neighborhood Statistical Area (best representation of true city neighborhoods)
temp_aft_nsa <- read_csv("urban_heat_island_temperature/temperature_values/afternoon/temp-by-nsa_af.csv") %>%
  rename(temp_mean_aft = temp_mean,
         temp_median_aft = temp_median,
         temp_min_aft = temp_min,
         temp_max_aft = temp_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Afternoon temperatures by Community Statistical Area (small groups of city neighborhoods)
temp_aft_csa <- read_csv("urban_heat_island_temperature/temperature_values/afternoon/temp-by-csa_af.csv") %>%
  mutate(temp_median = temp_media) %>%
  select(OBJECTID, CSA2010, temp_mean, temp_median, temp_min, temp_max) %>%
  rename(temp_mean_aft = temp_mean,
         temp_median_aft = temp_median,
         temp_min_aft = temp_min,
         temp_max_aft = temp_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

#######################
###### Morning ########
#######################

# Morning temperatures by Block
temp_am_block <- read_csv("urban_heat_island_temperature/temperature_values/morning/temp-by-block_am.csv") %>%
  filter(COUNTYFP10 == "510") %>%
  rename(temp_mean_am = temp_mea_3,
         temp_median_am = temp_med_1,
         temp_min_am = temp_min_1,
         temp_max_am = temp_max_1) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Morning temperatures by ZCTA (ZIP Code Tabulation Area)
temp_am_zcta <- read_csv("urban_heat_island_temperature/temperature_values/morning/temp-by-zcta_am.csv") %>%
  rename(temp_mean_am = t_am_mean,
         temp_median_am = t_am_median,
         temp_min_am = t_am_min,
         temp_max_am = t_am_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Morning temperatures by Neighborhood Statistical Area (best representation of true city neighborhoods)
temp_am_nsa <- read_csv("urban_heat_island_temperature/temperature_values/morning/temp-by-nsa_am.csv") %>%
  rename(temp_mean_am = temp_mean,
         temp_median_am = temp_median,
         temp_min_am = temp_min,
         temp_max_am = temp_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Morning temperatures by Community Statistical Area (small groups of city neighborhoods)
temp_am_csa <- read_csv("urban_heat_island_temperature/temperature_values/morning/temp-by-csa_am.csv") %>%
  mutate(temp_mean_am = temp_mea_2,
         temp_median_am = temp_med_2,
         temp_min_am = temp_min_2,
         temp_max_am = temp_max_2) %>%
  select(OBJECTID, CSA2010, temp_mean_am, temp_median_am, temp_min_am, temp_max_am) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

#######################
###### Evening ########
#######################

# Evening temperatures by Block
temp_pm_block <- read_csv("urban_heat_island_temperature/temperature_values/evening/temp-by-block_pm.csv") %>%
  filter(COUNTYFP10 == "510") %>%
  rename(temp_mean_pm = temp_mea_4,
         temp_median_pm = temp_med_2,
         temp_min_pm = temp_min_2,
         temp_max_pm = temp_max_2) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Evening temperatures by ZCTA (ZIP Code Tabulation Area)
temp_pm_zcta <- read_csv("urban_heat_island_temperature/temperature_values/evening/temp-by-zcta_pm.csv")  %>%
  rename(temp_mean_pm = t_pm_mean,
         temp_median_pm = t_pm_median,
         temp_min_pm = t_pm_min,
         temp_max_pm = t_pm_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Evening temperatures by Neighborhood Statistical Area (best representation of true city neighborhoods)
temp_pm_nsa <- read_csv("urban_heat_island_temperature/temperature_values/evening/temp-by-nsa_pm.csv") %>%
  rename(temp_mean_pm = temp_mean,
         temp_median_pm = temp_median,
         temp_min_pm = temp_min,
         temp_max_pm = temp_max) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)

# Evening temperatures by Community Statistical Area (small groups of city neighborhoods)
temp_pm_csa <- read_csv("urban_heat_island_temperature/temperature_values/evening/temp-by-csa_pm.csv") %>%
  mutate(temp_mean_pm = temp_mea_1,
         temp_median_pm = temp_med_1,
         temp_min_pm = temp_min_1,
         temp_max_pm = temp_max_1) %>%
  select(OBJECTID, CSA2010, temp_mean_pm, temp_median_pm, temp_min_pm, temp_max_pm) %>%
  mutate_at(vars(contains("temp")), c_to_f_convert)


######################################
###### JOIN AM, AFTERNOON, PM ########
######################################

all_temp_block <- temp_am_block %>%
  left_join(temp_aft_block) %>%
  left_join(temp_pm_block) %>%
  mutate_at(vars(matches("STATEFP10"), matches("GEOID10")), as.character) %>% # Recast non-calculable variables as characters
  rename_all(tolower)

all_temp_csa <- temp_am_csa %>%
  left_join(temp_aft_csa) %>%
  left_join(temp_pm_csa) %>%
  mutate_at(vars(matches("OBJECTID")), as.character) %>% # Recast non-calculable variables as characters
  rename_all(tolower)

all_temp_nsa <- temp_am_nsa %>%
  left_join(temp_aft_nsa) %>%
  left_join(temp_pm_nsa) %>%
  rename_all(tolower)

all_temp_zcta <- temp_am_zcta %>%
  left_join(temp_aft_zcta) %>%
  left_join(temp_pm_zcta) %>%
  mutate_at(vars(matches("ZCTA5CE10"), matches("GEOID10")), as.character) %>% # Recast non-calculable variables as characters
  rename_all(tolower)

# Remove unneeded files
rm(list=setdiff(ls(), c("all_temp_block", "all_temp_zcta", "all_temp_nsa", "all_temp_csa")))

#################################################################
######## Load and Clean Tree Data ###############################
#################################################################

## CNS analysis of Baltimore City tree canopy cover. Raster files were provided by Descartes Labs, one for 2009 and one for 2017-18.  The 2009 data is from the USDA NAIP imagery, from June 30, 2019.  The 2017-2018 data is from a mix of USGS Landsat and airbus and NAIP data.  They will be updating the 2017-2018 data for us with just NAIP data for apples to apples comparison, should be June 11, 2017 data.
## These images were developed via a trained CNN ML model, and deliver a pixel-by-pixel probability of tree canopy cover or not tree canopy cover, on a scale from 0 (definitely NOT a tree, according to the model) to 255 (definitely a tree, according to the model).
## We pulled the raster files into QGIS for analysis and converted each pixel to a 0 value (not a tree) or a 1 value (a tree) by setting a probability threshold. There is no "correct" answer for the appropriate probability threshold to select. We experimented with several different probability thresholds, after talking with data scientists at Descartes and performing a random sampling accuracy check of results, we settled on two thresholds -- 25 percent probability of tree cover (63.75 on 0 to 255 scale) and 50 percent probability of tree cover (127.5 on 0 to 255 scale). In terms of accuracy, there's danger in terms of going too high or too low.  Setting it too low overestimates the extent of tree cover.  Setting it too high underestimates it.  In general, the differences are most evident around the edges of obvious trees.  Setting the probability lower seems to count border pixels as tree cover.  Setting it too high shrinks the canopy bounds a bit.  
## When then used the same shapefiles as described above -- Census Blocks, Census ZCTAs, NSAs and BNIA CSAs -- and calculated zonal statistics describing tree canopy cover in each geographic unit. Each geographic unit had some pixels with a 0 value (not a tree) and a 1 value (a tree).  We added those values together and divided by the total number of pixels -- essentially calculating a mean for each geographic unit.  The mean is therefore the same thing as the percentage of that geographic unit that is covered with tree canopy.  For example, a value of .31 for a given neighborhood means that 31 percent of total area of that neighborhood is canopied. 
## We did this process for both 2009 and 2017-18. Then, in a separate R file, we calculated both the percentage change for each geographic unit between 2009 and 2017-18 and the percentage point change.   
## That information was exported to CSVs and uploaded here. 


#### Block ####
# 2009
tree_block_2009 <- read_csv("treecover/by_block/btree_statistics_by_block_2009.csv") %>%
  rename("09-63_mean" = "tc_mean63_2009",
         "09-127_mean" = "tc_mean127_2009",
         "09-63_count" = "tc_count63_2009",
         "09-127_count" = "tc_count127_2009",
         "09-63_stdev" = "tc_stdv63_2009",
         "09-127_stdev" = "tc_stdv127_2009"
  ) %>%
  rename_all(tolower)

# 2017
tree_block_2017 <- read_csv("treecover/by_block/btree_statistics_by_block_2017.csv") %>%
  rename_all(tolower)

# Join 2009 to 2017
tree_block_2009_2017 <- tree_block_2009 %>%
  left_join(tree_block_2017) %>%
  mutate(
    change_percent_127 = (`17-127_mean`-`09-127_mean`)/`09-127_mean`,
    change_percent_point_127 = (`17-127_mean`-`09-127_mean`),
    change_percent_63 = (`17-63_mean`-`09-63_mean`)/`09-63_mean`,
    change_percent_point_63 = (`17-63_mean`-`09-63_mean`)
  ) %>%
  select(-matches("count"), -matches("stdev")) %>%
  # Recast non-calculable variables as characters
  mutate_at(vars(matches("geoid10"), matches("statefp10"), 
                 matches("countyfp10"), matches("blockce10")
  ),
  as.character)

# How many times do blocks divide by zero to find the percent change? 1614 compared to 11,984 (12%)
#tree_block_2009_2017 %>% group_by(change_percent_127) %>% filter(is.nan(change_percent_127)) %>% summarize(n=n())
#tree_block_2009_2017 %>% filter(!is.nan(change_percent_127)) %>% summarize(n=n())

#### CSA ####

# 2009
tree_csa_2009 <- read_csv("treecover/by_community_statistical_area/btree_statistics_by_csa_2009.csv")

# 2017-2018
tree_csa_2017 <- read_csv("treecover/by_community_statistical_area/btree_statistics_by_csa_2017.csv")

# Join 2009 to 2017-2018
tree_csa_2009_2017 <- tree_csa_2009 %>%
  left_join(tree_csa_2017) %>%
  mutate(
    change_percent_127 = (`17-127_mean`-`09-127_mean`)/`09-127_mean`,
    change_percent_point_127 = (`17-127_mean`-`09-127_mean`),
    change_percent_63 = (`17-63_mean`-`09-63_mean`)/`09-63_mean`,
    change_percent_point_63 = (`17-63_mean`-`09-63_mean`)
  ) %>%
  select(-matches("count"), -matches("stdev")) %>%
  rename_all(tolower) %>%
  mutate("objectid" = as.character(objectid)) # Recast non-calculable variables as characters

#### NSA ####

# 2009
tree_nsa_2009 <- read_csv("treecover/by_neighborhood_statistical_area/btree_statistics_by_nsa_2009.csv")

# 2017-2018
tree_nsa_2017 <- read_csv("treecover/by_neighborhood_statistical_area/btree_statistics_by_nsa_2017.csv")

# Join 2009 to 2017-2018
tree_nsa_2009_2017 <- tree_nsa_2009 %>%
  left_join(tree_nsa_2017) %>%
  mutate(
    change_percent_127 = (`17-127_mean`-`09-127_mean`)/`09-127_mean`,
    change_percent_point_127 = (`17-127_mean`-`09-127_mean`),
    change_percent_63 = (`17-63_mean`-`09-63_mean`)/`09-63_mean`,
    change_percent_point_63 = (`17-63_mean`-`09-63_mean`)
  ) %>%
  rename_all(tolower) %>%
  select(-matches("count"), -matches("stdev"), -("nbrdesc")) %>%
  mutate("objectid" = as.character(objectid)) # Recast non-calculable variables as characters

#### ZCTA ####

# 2009
tree_zcta_2009 <- read_csv("treecover/by_ZCTA/btree_statistics_by_zcta_2009.csv")

# 2017-18
tree_zcta_2017 <- read_csv("treecover/by_ZCTA/btree_statistics_by_zcta_2017.csv")

# Join 2009 to 2017-2018
tree_zcta_2009_2017 <- tree_zcta_2009 %>%
  left_join(tree_zcta_2017) %>%
  mutate(
    change_percent_127 = (`17-127_mean`-`09-127_mean`)/`09-127_mean`,
    change_percent_point_127 = (`17-127_mean`-`09-127_mean`),
    change_percent_63 = (`17-63_mean`-`09-63_mean`)/`09-63_mean`,
    change_percent_point_63 = (`17-63_mean`-`09-63_mean`)
  ) %>%
  select(-matches("count"), -matches("stdev")) %>%
  rename_all(tolower) %>%
  mutate("zcta5ce10" = as.character(zcta5ce10),
         "geoid10" = as.character(geoid10))

# Remove unneeded files
rm(list=setdiff(ls(), c("tree_block_2009_2017", "tree_zcta_2009_2017", "tree_nsa_2009_2017", "tree_csa_2009_2017", "all_temp_block", "all_temp_zcta", "all_temp_nsa", "all_temp_csa")))

#################################################################
######## Load and Clean Demographics Data #######################
#################################################################

### Blocks
# Read in 2010 population data, SF1 U.S. Census data by block. Used only to filter out no population blocks before doing analysis. Otherwise, 2010 data is just a bit too old to use, but in a pinch we can reconsider.
demographics_block <- read_csv("demographics/by_block/2010_SF1_Baltimore_City_Population_by_Block.csv") %>%
  rename_all(tolower) %>%
  mutate("geoid10" = as.character(geoid10))

### CSA
# Data from BNIA, reading in 2017 values. https://bniajfi.org/vital_signs/data_downloads/.  
# Clean column names and remove citywide values for Baltimore City so it joins correctly. 

demographics_csa <- read_csv("demographics/by_community_statistical_area/VS_Indicators_2017_Only_BNIA.csv") %>%
  clean_names() %>%
  rename_all(tolower) %>%
  filter(csa2010 != "Baltimore City")

### NSA
# No available demographic data, because these are shapefiles constructed by the city from census blocks, not an actual census product.  Block groups and tracts stretch across the boundaries.  Therefore, the only available valid data is from 2010 census. In a pinch, we can go back and use the 2010 stats, but that's pretty freaking old.

### ZCTA
# From American Community Survey, 5 year averages 2017.
# 
demographics_zcta <- read_csv("demographics/by_zcta/acs_2017_baltimore_zctas.csv") %>%
  rename_all(tolower) %>%
  filter(zcta != "CITYWIDE")

#################################################################
######## Merge Tree, Heat, Demographics #########################
#################################################################

#### Blocks (Note: this merged correctly.  All three files have 13598 records, as does the merged file)
# Demographics only for removing low population blocks
blocks_tree_temp_demographics <- all_temp_block %>%
  rename_all(tolower) %>% 
  left_join(tree_block_2009_2017) %>%
  left_join(demographics_block) %>%
  rename_all(tolower) %>%
  select(-"countyfp10", -"statefp10", -"tractce10", -"blockce10", -"name10", -"mtfcc10", -"uace10", -"uatype", -"funcstat10", -"name", -"ur10") %>%
  select("geoid10", everything()) %>%
  mutate(change_percent_63 = ifelse(is.infinite(change_percent_63) | is.nan(change_percent_63), NA, change_percent_63)) %>%
  mutate(change_percent_127 = ifelse(is.infinite(change_percent_127) | is.nan(change_percent_127), NA, change_percent_127))


### CSA 
# Note: all three files have 55 records, as does joined file
csa_tree_temp_demographics <- all_temp_csa %>%
  left_join(tree_csa_2009_2017) %>%
  left_join(demographics_csa) %>%
  select(-"id") %>%
  rename_all(tolower)

#### NSA
# Note: no demographics for this
# Note: the difference in the number of records here (279 for temp and 278 for trees) is the fact that temp has an extra record for Unnamed areas, because of slight differences between extent of temp raster and nsa shapefile.  Here, we drop the unnamed column on join.
nsa_tree_temp <- all_temp_nsa %>%
  left_join(tree_nsa_2009_2017, by=c('nsa_name' = 'label')) %>%
  filter(nsa_name != "Unnamed") %>%
  select(-matches("color_2")) %>%
  select("objectid", everything()) %>%
  rename_all(tolower)

#### ZCTA
zcta_tree_temp_demographics <- all_temp_zcta %>%
  left_join(tree_zcta_2009_2017) %>%
  right_join(demographics_zcta, by=c('zcta5ce10' = 'zcta')) %>%
  rename(zcta = 'zcta5ce10') %>%
  select(-"geoid10", -"classfp10", -"mtfcc10", -"funcstat10")

# Confirm missing ZCTAs are the ones we deliberately excluded; YES
zcta_tree_temp_demographics %>% 
  filter(is.na(aland10))

# Filter out those ZCTAs
zcta_tree_temp_demographics <- zcta_tree_temp_demographics %>%
  filter(!is.na(aland10)) %>%
  rename_all(tolower)


rm(list=setdiff(ls(), c("blocks_tree_temp_demographics", "zcta_tree_temp_demographics", "nsa_tree_temp", "csa_tree_temp_demographics")))


#################################################################
######## Write merged files out for later use ###################
#################################################################

write_csv(blocks_tree_temp_demographics,"output/data/cleaned/blocks_tree_temp_demographics.csv")
write_csv(csa_tree_temp_demographics,"output/data/cleaned/csa_tree_temp_demographics.csv")
write_csv(nsa_tree_temp,"output/data/cleaned/nsa_tree_temp.csv")
write_csv(zcta_tree_temp_demographics,"output/data/cleaned/zcta_tree_temp_demographics.csv")



