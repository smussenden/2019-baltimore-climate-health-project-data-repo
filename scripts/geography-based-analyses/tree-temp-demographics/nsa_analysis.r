#### Analysis of Community Statistical Areas
#### 
options(scipen=999)
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

nsa_tree_temp_demographics <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/nsa_lidartree_temp.csv")

#################################################################
########## Define Functions #####################################
#################################################################

# Run functions.R
source("scripts/geography-based-analyses/tree-temp-demographics/functions.R")

# Select computable values within *this particular* df
select_x <- function(df){
  return(df %>%
    select_if(is.numeric) %>%
    select(-objectid, -acres, -label))
}

# Cleanup
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("make_correlation_matrix_graphic", 
                    "write_matrix_csv", 
                    "nsa_tree_temp_demographics", 
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
# Not useful unless we get demographics for NSA


##################################
### Trees vs demographics ########
##################################
# Not useful unless we get demographics for NSA, which we will not


##################################
### Trees vs heat ################
##################################
        
# Build correlation matrix
tree_vs_heat_nsa_correlation_matrix <- nsa_tree_temp_demographics %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("63"), matches("127"), matches ("lid")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "^temp_")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(tree_vs_heat_nsa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_heat_nsa_correlation_matrix_long <- tree_vs_heat_nsa_correlation_matrix %>%
  gather("variable_2", "value", 2:13) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_heat_nsa_correlation_matrix_long)

# Remove all but master file and functions
cleanup()

##################################
### Tree cover vs tree change ####
##################################

# Build correlation matrix
treecover_vs_coverchange_nsa_correlation_matrix <- nsa_tree_temp_demographics %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(-matches("change"), -matches("temp"), -matches("acres")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "change")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(treecover_vs_coverchange_nsa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
treecover_vs_coverchange_nsa_correlation_matrix_long <- treecover_vs_coverchange_nsa_correlation_matrix %>%
  gather("variable_2", "value", 2:7) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(treecover_vs_coverchange_nsa_correlation_matrix_long)

# Remove all but master file and functions
cleanup()
