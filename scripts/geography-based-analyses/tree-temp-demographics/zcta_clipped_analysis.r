#### Analysis of ZIP Code Tabulation Areas
#### ZIPS clipped at Balt City Border
#### Cannot later be joined with hospital data
#### Can be joined with demographic data

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("Hmisc")
library(tidyverse)
library(corrr)

#################################################################
######## Load Data Produced in Cleaning.r Script File ###########
#################################################################

zcta_clipped_tree_temp <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/zcta_clipped_lidartree_temp.csv")  
zcta_clipped_tree_temp <- zcta_clipped_tree_temp %>% 
  mutate(zcta = as.character(zcta5ce10))  %>%
  mutate(lid_change_percent_point = change_percent_point) %>%
  select(-matches("t_a"), -matches("t_pm"))
glimpse(zcta_clipped_tree_temp)

#################################################################
########## Define Functions #####################################
#################################################################

# Run functions.R
source("scripts/geography-based-analyses/tree-temp-demographics/functions.R")

# Select computable values within *this particular* df
select_x <- function(df){
  return(df %>%
           select_if(is.numeric) )
}

# Cleanup
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("make_correlation_matrix_graphic", 
                    "write_matrix_csv", 
                    "zcta_clipped_tree_temp", 
                    "select_x", 
                    "cleanup")
                  ),
     pos = 1
     )
}

#################################################################
########## Build Correlation Matrices, Graphics #################
#################################################################

##################################
### Heat vs demographics #########
##################################
# Doesn't make sense to do, because we don't have demographic information for clipped ZCTAs


##################################
### Trees vs demographics ########
##################################
# Doesn't make sense to do, because we don't have demographic information for clipped ZCTAs


##################################
### Trees vs heat ################
##################################
        
# Build correlation matrix
tree_vs_heat_clipped_zcta_correlation_matrix <- zcta_clipped_tree_temp %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("lid")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "^temp_")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(tree_vs_heat_clipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_heat_clipped_zcta_correlation_matrix_long <- tree_vs_heat_clipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:5) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_heat_clipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()

##################################
### Tree cover vs tree change ####
##################################

# Build correlation matrix
treecover_vs_coverchange_clipped_zcta_correlation_matrix <- zcta_clipped_tree_temp %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("lid_change")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "lid_mean")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(treecover_vs_coverchange_clipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
treecover_vs_coverchange_clipped_zcta_correlation_matrix_long <- treecover_vs_coverchange_clipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:3) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(treecover_vs_coverchange_clipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()
