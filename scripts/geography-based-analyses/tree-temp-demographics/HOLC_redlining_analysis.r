#### Analysis of HOLC redlinined areas analysis

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("Hmisc")
library(tidyverse)
library(corrr)
library(Hmisc)
library(magrittr)

### For debugging
rm(list=ls())

#################################################################
######## Load Data Produced in Cleaning.r Script File ###########
#################################################################

redlining_tree <- read_csv("../data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/HOLC_redlining_2015_tree_canopy.csv")

#################################################################
########## Analysis #############################################
#################################################################

redlining_tree_summary_table <- redlining_tree %>%
  group_by(holc_grade) %>%
  summarise(total_area_pixels = sum(`15_count`),
            total_canopy_pixels = sum(`15_sum`)) %>%
  mutate(average_canopy_cover = total_canopy_pixels/total_area_pixels)

#################################################################
########## Write out as CSV #####################################
#################################################################

write_csv(redlining_tree_summary_table, "data/output-data/HOLC_redlining_canopy_analysis/HOLC_redlining_canopy_analysis.csv")

### NOTE: Need to add heat to this as well