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

# Set working directory
# setwd("/Users/rready/Desktop/2019-baltimore-climate-health-project-data-repo")

#########################
### Define functions ###############################################
########################
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("cleanup", 
                    "tree_by_tree",
                    "tree_condition_by_nbr",
                    "tree_height_diam_by_nbr", 
                    "summary_tbl_1",
                    "tree_spaces_count_by_nbr_all")
                  ),
     pos = 1
     )
}

#####################
##### Load data #################################################
#####################

tree_by_tree <- read_csv("data/input-data/street-trees/csv/by_nsa/street_trees_nsa_join_table.csv") %>%
  select(-COLOR_2, -LABEL) %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  mutate(condition = ifelse(condition == "stump w", "stump", condition)) %>%
  # Group "sprout" with "stump"
  mutate(condition = ifelse(condition == "sprouts", "stump", condition)) %>%
  filter(condition != "n/a") %>%
  # Make "condition" a factor so it can be ordered
  mutate_at(vars(matches("condition")), 
            as.factor) %>%
  mutate(condition = fct_relevel(condition, 
                                 c("absent",
                                   "stump",
                                   "dead",
                                   "poor",
                                   "fair",
                                   "good")
  )
  ) %>%
  mutate_at(vars(matches("tree_ht"), matches("dbh")), as.numeric)
#mutate_at(vars(matches("nbrdesc")), tolower)


###########################
### Counts of variables ###############################################
###########################

## Examine cagegories of information

tree_by_tree %>% 
  group_by(condition) %>%
  dplyr::summarize(num = n()) %>%
  arrange(-desc(condition))

tree_by_tree %>% 
  group_by(space_type) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))

tree_by_tree %>% 
  group_by(utilities) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))


######################
### Begin analysis ###############################################
###################### 

#########################
### Actual vs Absent ###
########################

tree_spaces_count_by_nbr <- tree_by_tree %>%
  # How many tree-able spaces are there, both with and without trees?
  group_by(nbrdesc) %>%
  dplyr::summarize(total_possible = n())

tree_count_by_nbr <- tree_by_tree %>% 
  # Filter out tree-able spaces without trees, because we want to know the percent *of existing trees* at each condition
  filter(condition != "absent") %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(total_trees = n())

tree_empty_spaces_count_by_nbr <- tree_by_tree %>% 
  # How many tree-able spaces don't have trees?
  filter(condition == "absent") %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(total_absent = n())

tree_potential_sites <- tree_by_tree %>% 
  # How many tree-able are considered "potential" pits?
  filter(str_detect(space_type, "potential")) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(potential_wellpit = n())

# Join to see how many actual trees vs "absent" trees vs "potential" aka labor intensive for city
tree_spaces_count_by_nbr_all <- tree_spaces_count_by_nbr %>%
  left_join(tree_count_by_nbr) %>%
  left_join(tree_empty_spaces_count_by_nbr) %>%
  left_join(tree_potential_sites) 
  

# Write to CSV for later use
write_csv(tree_spaces_count_by_nbr_all, "data/output-data/street-tree-analyses/trees_actual_v_tree_spaces.csv")

################################
### Look at potential sites ####
################################

tree_potential_sites <- tree_by_tree %>% 
  # How many tree-able are considered "potential" pits?
  filter(str_detect(space_type, "potential")) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(potential_wellpit = n()) %>%
  left_join(tree_spaces_count_by_nbr) %>%
  left_join(tree_empty_spaces_count_by_nbr)

# Clean up workspace
cleanup()

######################################
### Tree condition by neighborhood ###
######################################

# Tree condition by neighborhood
tree_condition_by_nbr <- tree_by_tree %>% 
  # Filter out tree-able spaces without trees, because we want to know the percent *of existing trees* at each condition
  filter(condition != "absent") %>%
  group_by(nbrdesc, condition) %>%
  dplyr::summarize(num = n()) %>%
  # Join the tree count by neighborhood for percentage calculations
  left_join(tree_spaces_count_by_nbr_all) %>%
  select(-total_absent, -total_possible) %>%
  mutate(perc = round(100*(num / total_trees), 2)) %>%
  # Drop num to spread
  select(-num) %>%
  spread(condition, perc) %>%
  # Debug: Check that percentages add up (drop in next step)
  mutate(perc_check = rowSums(.[3:7], na.rm = T)) %>%
  select(-total_trees, everything(), -perc_check)

# Write to csv
write_csv(tree_condition_by_nbr_all, "data/output-data/street-tree-analyses/tree_condition_by_nbr.csv")


############################################
### Tree height/diameter by neighborhood ###
############################################

# Tree height/diameter
tree_height_diam_by_nbr <- tree_by_tree %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(combined_ht = sum(tree_ht),
            avg_ht = round(mean(tree_ht), 2),
            combined_diam = sum(dbh),
            avg_diam = round(mean(dbh), 2)
  )


# Wite it to csv
write_csv(height_diam, "data/output-data/street-tree-analyses/tree_height_diameter_by_nbr.csv")


####################
### Master table ###
####################

# Join all summaries into master table...
summary_tbl_1 <- tree_height_diam_by_nbr %>%
  left_join(tree_condition_by_nbr, by = "nbrdesc") %>%
  select(-total_trees) %>%
  left_join(tree_spaces_count_by_nbr_all, by = "nbrdesc")
  
# ...and write to csv
write_csv(summary_tbl_1, "data/output-data/street-tree-analyses/tree_height_diameter_condition_by_nbr.csv")

# Clean up workspace
cleanup()

######################
### Generate plots ###
######################

ggplot(tree_condition_by_nbr) +
  geom_bar(aes(x = ))

