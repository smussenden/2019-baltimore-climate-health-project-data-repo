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
                    "tree_by_tree_categorized",
                    "spaces_count_by_nsa_all",
                    "count_at_condition",
                    "tree_percent_by_nsa",
                    "empty_spaces_by_diff_by_nsa",
                    "tree_condition_by_nsa",
                    "tree_height_diam_by_nsa",
                    "perc_stump_dead_in_moderate")
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
  # Eliminate the "stump w" typo, group "sprout" with "stump"
  mutate(condition = ifelse((condition == "stump w")|(condition == "sprouts"), "stump", condition)) %>%
  mutate(condition = ifelse(condition == "n/a", "unknown", condition)) %>%
  #filter(condition != "n/a") %>%
  # Make "condition" a factor so it can be ordered
  mutate_at(vars(matches("condition")), 
            as.factor) %>%
  mutate(condition = fct_relevel(condition, 
                                 c("absent",
                                   "stump",
                                   "dead",
                                   "poor",
                                   "fair",
                                   "good",
                                   "unknown")
  )
  ) %>%
  mutate_at(vars(matches("tree_ht"), matches("dbh")), as.numeric)

###################################
### Add meaningful categories ###############################################
###################################

#### Levels of difficulty to plant ####
# Live tree exists = NA
# Absent and not potential = 1
# Stump or dead = 2
# Absent and potential = 3
# Absent and "not suitable" = 4
# All others = 0

tree_by_tree_categorized <- tree_by_tree %>%
  # Add col for top-level cat
  mutate(has_live_tree = ifelse(
    (condition == "poor")|
      (condition == "fair")|
      (condition == "good"), 
    T, F)) %>%
  # Add col for second-level cat
  mutate(difficulty_level = case_when(
    # Difficulty of planting NA if tree already there.
    has_live_tree == T ~ NA_real_,
    # Difficulty level 1 if...
    condition == "absent" & # No tree,
      !str_detect(space_type, "potential") & # Not marked "potential" aka doesn't require breaking concrete,
      (!str_detect(spp, "not suit") | !str_detect(common, "not suit") # Not marked unsuitable.
      ) ~ 1,
    # Difficulty level 2 if removal of dead tree required.
    condition == "stump" | condition == "dead" ~ 2,
    # Difficulty level 3 if... 
    condition == "absent" & # Marked absent (AND by elimination also marked potential),
      (!str_detect(spp, "not suit") | !str_detect(common, "not suit") # Not marked unsuitable.
      ) ~ 3, 
    # Difficulty level 4 if marked unsuitable
    str_detect(spp, "not suit") ~ 4,
    # Catches all others at difficulty level 0 (9 erratta: unknowns)
    TRUE ~ 0
  ) )

# Write to CSV for later use
write_csv(tree_by_tree_categorized, "data/output-data/street-tree-analyses/street_trees_nsa_categorized.csv")

###########################
### Counts of variables ###############################################
###########################

tree_by_tree_categorized %>% 
  group_by(condition) %>%
  dplyr::summarize(num = n()) %>%
  arrange(-desc(condition))

tree_by_tree_categorized %>% 
  group_by(space_type) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))

tree_by_tree_categorized %>% 
  group_by(utilities) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))
  
tree_by_tree_categorized %>% 
  group_by(difficulty_level) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))

tree_by_tree_categorized %>% 
  group_by(has_live_tree) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))

######################################################
### Basic count tables, required for later queries ###
######################################################

# Count total spaces that are tracked, both with and without trees
spaces_count_by_nsa <- tree_by_tree_categorized %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(tracked_spaces = n())

# Count total spaces with live trees (later we want to know the percent *of existing trees* at each condition)
live_count_by_nsa <- tree_by_tree_categorized %>% 
  filter(has_live_tree == T) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(live_trees = n())

# Count total spaces without live trees (includes dead trees and stumps)
empty_spaces_count_by_nsa <- tree_by_tree_categorized %>% 
  filter(has_live_tree == F) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(empty_spaces = n())

# Combine all counts into single table of COUNT of LIVE/EMPTY/ALL
spaces_count_by_nsa_all <- spaces_count_by_nsa %>%
    left_join(live_count_by_nsa) %>%
    left_join(empty_spaces_count_by_nsa)

# Remove building blocks from workspace
cleanup()

# Find PERCENT of trees to total spaces in each neighborhood
tree_percent_of_all_by_nsa <- spaces_count_by_nsa_all %>%
  # What percent of trees are there compared to tree spaces?
  mutate(perc_trees_to_spaces = round(100*(live_trees / tracked_spaces), 2))

# Write to csv
write_csv(tree_percent_of_all_by_nsa, "data/output-data/street-tree-analyses/tree_percent_of_all_by_nsa.csv")


count_at_condition <- tree_by_tree_categorized %>% 
  group_by(nbrdesc, condition) %>%
  dplyr::summarize(num = n()) %>%
  spread(condition, num)

###################################
### Difficulty of planting ########
###################################

# COUNT spaces AT EACH DIFFICULTY in each nsa
empty_spaces_by_diff_by_nsa_count <- tree_by_tree_categorized %>% 
  group_by(nbrdesc, difficulty_level) %>%
  dplyr::summarize(num = n()) %>%
  spread(difficulty_level, num) %>%
  rename(
    num_unknown = "0",
    num_easy = "1",
    num_moderate = "2",
    num_hard = "3",
    num_not_suitable = "4",
    num_has_tree = "<NA>"
  ) %>%
  select(-num_unknown)

##################################################
### Perc of Stump or dead in cat 2 / moderate ###
##################################################

perc_stump_dead_in_moderate <- tree_by_tree_categorized %>%
  # Only dead trees or empty spaces at diff 2
  filter(has_live_tree == FALSE,
         difficulty_level == 2) %>%
  select(nbrdesc, condition, has_live_tree, difficulty_level) %>%
  group_by(nbrdesc, condition) %>%
  # Count how many in each condition
  dplyr::summarize(num_in_condition = n()) %>%
  # Join a temp table to find total of difficulty==2 for perc calc
  left_join(
    (tree_by_tree_categorized %>%
       # Only dead trees or empty spaces at diff 2
      filter(has_live_tree == FALSE,
             difficulty_level == 2) %>%
      group_by(nbrdesc) %>%
      # Count how many at moderate difficulty
      dplyr::summarize(total = n()))
  ) %>%
  mutate(perc_in_condition = round(100*(num_in_condition/total), 2))

count_spread <- perc_stump_dead_in_moderate %>%
  select(nbrdesc, condition, num_in_condition) %>%
  spread(condition, num_in_condition) %>%
  rename(stump_count = stump,
         dead_count = dead)

perc_spread <- perc_stump_dead_in_moderate %>%
  select(nbrdesc, condition, perc_in_condition) %>%
  spread(condition, perc_in_condition) %>%
  rename(stump_perc_of_moderate_diff = stump,
         dead_perc_of_moderate_diff = dead)

perc_stump_dead_in_moderate <- count_spread %>%
  left_join(
    (tree_by_tree_categorized %>%
       # Only dead trees or empty spaces at diff 2
       filter(has_live_tree == FALSE,
              difficulty_level == 2) %>%
       group_by(nbrdesc) %>%
       # Count how many at moderate difficulty
       dplyr::summarize(total_at_moderate_diff = n()))
  ) %>%
  left_join(perc_spread)
  
cleanup()

# Write to csv
write_csv(perc_stump_dead_in_moderate, "data/output-data/street-tree-analyses/perc_stump_dead_in_moderate.csv")

# PERCENT of total empty spots AT EACH DIFFICULTY in each nsa
empty_spaces_by_diff_by_nsa_perc <- tree_by_tree_categorized %>% 
  group_by(nbrdesc, difficulty_level) %>%
  dplyr::summarize(num = n()) %>%
  # Join totals for perc calc
  left_join(spaces_count_by_nsa_all %>% select(nbrdesc , empty_spaces)) %>%
  mutate(perc_difflvl_to_empty_spaces = round(100*(num/empty_spaces), 2)) %>%
  select(-num) %>%
  spread(difficulty_level, perc_difflvl_to_empty_spaces) %>%
  select(-"<NA>") %>%
  rename(
    perc_unknown = "0",
    perc_easy = "1",
    perc_moderate = "2",
    perc_hard = "3",
    perc_not_suitable = "4",
    num_empty_spaces = "empty_spaces"
  ) %>%
  select(-perc_unknown)

# Join into master DIFFICULTY table
empty_spaces_by_diff_by_nsa <- empty_spaces_by_diff_by_nsa_count %>%
  left_join(empty_spaces_by_diff_by_nsa_perc)
# Remove building blocks from workspace
cleanup()

# Write to csv
write_csv(empty_spaces_by_diff_by_nsa, "data/output-data/street-tree-analyses/empty_spaces_by_diff_by_nsa.csv")

######################################
### Tree condition by nsa ############
######################################

# Tree condition by neighborhood starting table
tree_condition_by_nsa_long <- tree_by_tree_categorized %>% 
  # Filter for tree-able spaces with live trees, because we want to know the percent *of existing trees* at each condition
  filter(has_live_tree == T) %>%
  group_by(nbrdesc, condition) %>%
  dplyr::summarize(num_trees = n()) %>%
  # Find percent of live trees are in each condition?
  # Join table to get total number of live trees
  left_join(spaces_count_by_nsa_all) %>%
  mutate(perc_condition_to_live_trees = round(100*(num_trees/live_trees), 2))

# Tree condition percent by nsa
tree_condition_perc_by_nsa_wide <- tree_condition_by_nsa_long %>%
  # Select cols to spread
  select(nbrdesc, condition, perc_condition_to_live_trees) %>%
  spread(condition, perc_condition_to_live_trees) %>%
  rename(
    poor_perc = poor,
    fair_perc = fair,
    good_perc = good
  ) 

# Tree condition count by nsa
tree_condition_count_by_nsa_wide <- tree_condition_by_nsa_long %>%
  # Select cols to spread
  select(nbrdesc, condition, num_trees) %>%
  spread(condition, num_trees) %>%
  rename(
    poor_count = poor,
    fair_count = fair,
    good_count = good
  )

# Join condition info tables into final
tree_condition_by_nsa <- tree_condition_perc_by_nsa_wide %>%
  left_join(tree_condition_count_by_nsa_wide) %>%
  left_join(spaces_count_by_nsa_all %>% select(nbrdesc, live_trees))

# Remove building blocks from workspace
cleanup()

# Write to csv
write_csv(tree_condition_by_nsa, "data/output-data/street-tree-analyses/tree_condition_by_nsa.csv")

############################################
### Tree height/diameter by neighborhood ###
############################################

# Tree height/diameter
tree_height_diam_by_nsa <- tree_by_tree_categorized %>%
  filter(has_live_tree == T) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(combined_ht = sum(tree_ht),
                   avg_ht = round(mean(tree_ht), 2),
                   combined_diam = sum(dbh),
                   avg_diam = round(mean(dbh), 2)
  )


# Wite it to csv
write_csv(tree_height_diam_by_nsa, "data/output-data/street-tree-analyses/tree_height_diameter_by_nsa.csv")

### Clean up workspace
cleanup()


#################################################
### Put all in master table ###
#################################################

master_by_nsa <- empty_spaces_by_diff_by_nsa %>%
  left_join(perc_stump_dead_in_moderate) %>%
  left_join(spaces_count_by_nsa_all) %>%
  left_join(tree_condition_by_nsa) %>%
  left_join(tree_height_diam_by_nsa) %>%
  left_join(tree_percent_by_nsa) %>%
  left_join(count_at_condition %>% select(absent_count = absent, unknown_count = unknown))

# Write to csv
write_csv(master_by_nsa, "data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")

