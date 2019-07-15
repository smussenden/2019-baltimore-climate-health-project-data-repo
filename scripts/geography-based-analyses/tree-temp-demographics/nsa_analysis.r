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


###############################
######## Load Data  ###########
###############################

### Load Data Produced in Cleaning.r
nsa_tree_temp_demographics <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-lidar-use/nsa_lidartree_temp.csv") %>%
  mutate_at(vars(starts_with("temp")), as.double)

### Load data created by "street-tree-data-cleaning.R"
# Load the full dataset with categories added in cleaning file
street_trees_categorized <- read_csv("data/input-data/street-trees/csv/by_nsa/street_trees_nsa_categorized.csv") %>%
  # Make "condition" a factor so it can be ordered
  mutate_at(vars(matches("condition")), as.factor) %>%
  mutate(condition = fct_relevel(condition, 
                                 c("absent",
                                   "stump",
                                   "dead",
                                   "poor",
                                   "fair",
                                   "good",
                                   "unknown")
  )) %>%
  mutate(difficulty_level = as.integer(difficulty_level)) %>%
  mutate(difficulty_level_char = if_else(is.na(difficulty_level), "Live", as.character(difficulty_level))) %>%
  mutate_at(vars(matches("difficulty_level_char")), as.factor) %>%
  mutate(difficulty_level_char = recode(difficulty_level_char,
                                        "Live" = "Live Tree",
                                        "1" = "Easiest to Plant",
                                        "2" = "Removal Required", 
                                        "3" = "Must Break Concrete",
                                        "4" = "Unsuitable")) %>%
  mutate(difficulty_level_char = fct_relevel(difficulty_level_char,
                                             c("Live Tree",
                                               "Easiest to Plant",
                                               "Removal Required", 
                                               "Must Break Concrete",
                                               "Unsuitable")))

# Load the master summaries table created in cleaning file
master_street_tree_summaries <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv") %>%
  # Add variable to track whether a target NSA
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

#################################################################
########## Define Functions #####################################
#################################################################

# Run functions.R
source("scripts/geography-based-analyses/tree-temp-demographics/functions.R")

# Select computable values within *this particular* df
select_x <- function(df){
  return(df %>%
    select_if(is.numeric) %>%
    select(-matches("objectid"), -matches("acres"), -starts_with('rank')))
  #-contains("combined")
  # -var(starts_with('rank'))
}

# Cleanup
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("make_correlation_matrix_graphic", 
                    "write_matrix_csv", 
                    "nsa_tree_temp_demographics", 
                    "street_trees_categorized",
                    "master_street_tree_summaries",
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

##############################
##### SM ANALYSIS ############
##############################

# Add "target neighborhood columns"
nsa_tree_temp_demographics <- nsa_tree_temp_demographics %>%
  mutate(target_neighborhood = case_when(
    nsa_name %in% c("Berea", "Broadway East", "Oliver", "Middle East", "Biddle Street","Milton-Montford", "Madison-Eastend", "CARE", "McElderry Park", "Ellwood Park/Monument", "Patterson Place", "Patterson Park Neighborhood", "Baltimore Highlands", "Highlandtown", "Upper Fells Point") ~ "target",
    nsa_name %in% c("Butcher's Hill", "Canton", "Washington Hill") ~ "counterpoint",
    TRUE                      ~ "other"
  )) %>%
  filter(target_neighborhood != "other")

# Basic scatter plot
ggplot(nsa_tree_temp_demographics, aes(x=`07_lid_mean`, y=lid_change_percent_point, colour=target_neighborhood)) + geom_point() + geom_text(aes(label=nsa_name),hjust=0, vjust=0)
# Change the point size, and shape

plot(nsa_tree_temp_demographics$temp_mean_aft, nsa_tree_temp_demographics$lid_change_percent_point)


##############################
##### ROX ANALYSIS ###########
##############################

### Build correlation matrix df
tree_var_to_condition_corr_df <- nsa_tree_temp_demographics %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  left_join(master_street_tree_summaries, by = c("nsa_name" = "nbrdesc")) %>%
  mutate_at(vars(starts_with("temp")), as.double) %>%
  select_x()

### Temp to mean at condition
# Build correlation matrix
temp_to_condition_corr_matrix <- tree_var_to_condition_corr_df %>%
  as.matrix() %>%
  correlate() %>%
  focus(ends_with("_perc_of_live")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "^temp_")) %>%
  select(variable, everything(), -rowname) 

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
temp_to_condition_corr_matrix_long <- temp_to_condition_corr_matrix %>%
  gather("variable_2", "value", 2:4) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(temp_to_condition_corr_matrix_long, "NSA")


### Tmep to height/diameter
# Build correlation matrix
temp_to_size_corr_matrix <- tree_var_to_condition_corr_df %>%
  select(matches("temp_mean_aft"), matches("temp_median_aft"), avg_ht, avg_ht_controled, avg_diam, avg_diam_controled) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("avg_")) %>%
  mutate(variable=rowname) %>%
  select(variable, everything(), -rowname) 

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
temp_to_size_corr_matrix_long <- temp_to_size_corr_matrix %>%
  gather("variable_2", "value", 2:5) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(temp_to_size_corr_matrix_long, "NSA")



