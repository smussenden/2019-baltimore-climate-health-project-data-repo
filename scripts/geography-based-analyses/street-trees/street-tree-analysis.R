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
                    "tree_as_percent_of_spaces_by_nsa",
                    "empty_spaces_by_diff_by_nsa",
                    "tree_condition_by_nsa")
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

############################################################
### Counts of variables (printed to console not stored) ###############################################
############################################################

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
  dplyr::summarize(all_tracked_spaces = n())

# Count total spaces with live trees (later we want to know the percent *of existing trees* at each condition)
live_count_by_nsa <- tree_by_tree_categorized %>% 
  filter(has_live_tree == T) %>%
  group_by(nbrdesc) %>%
  dplyr::summarize(spaces_with_live_trees = n())

# Count total spaces without live trees (includes dead trees and stumps)
empty_spaces_count_by_nsa <- tree_by_tree_categorized %>% 
  # Only spaces without a live tree
  filter(has_live_tree == F) %>%
  # Use difficulty level to categorize by suitable/unsuitable
  mutate(is_suitable = case_when(
    difficulty_level == 4 ~ F,
    TRUE ~ T
  )) %>%
  group_by(nbrdesc, is_suitable) %>%
  dplyr::summarize(count_spaces = n()) %>%
  spread(is_suitable, count_spaces) %>%
  rename(
    suitable_without_tree = "TRUE",
    unsuitable_without_tree = "FALSE"
  ) %>%
  # Include the total (KI: Should just be a rowsum but can't get that to work.)
  left_join(tree_by_tree_categorized %>% 
              filter(has_live_tree == F) %>%
              group_by(nbrdesc) %>%
              dplyr::summarize(spaces_without_live_trees = n()))

# Combine all counts into single table of COUNT of LIVE/EMPTY/(SUITABLE/UNSUITABLE)/ALL
spaces_count_by_nsa_all <- spaces_count_by_nsa %>%
    left_join(live_count_by_nsa) %>%
    left_join(empty_spaces_count_by_nsa) %>%
  # Rearange cols
  select(
    nbrdesc, all_tracked_spaces, 
    spaces_with_live_trees, spaces_without_live_trees,
    suitable_without_tree, unsuitable_without_tree
  )

# Find PERCENTs of trees/spaces/suitable/nonsuitable:
tree_as_percent_of_spaces_by_nsa <- spaces_count_by_nsa_all %>%
  # 1. percent of trees compared to total spaces
  mutate(perc_spaces_treed = round(100*(spaces_with_live_trees / all_tracked_spaces), 2)) %>%
  # 2. percent of non-tree'ed spaces compared to total spaces
  mutate(perc_spaces_nontreed = round(100*(spaces_without_live_trees / all_tracked_spaces), 2)) %>%
  # 3. percent of non-tree'ed spaces are suitable non-tree'ed spaces
  mutate(perc_of_nontreed_are_suitable = round(100*(suitable_without_tree / spaces_without_live_trees), 2))

# Remove building blocks from workspace
cleanup()

# Write to csv
write_csv(tree_as_percent_of_spaces_by_nsa, "data/output-data/street-tree-analyses/tree_as_percent_of_spaces_by_nsa.csv")


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
    num_spaces_with_live_trees = "<NA>"
  ) %>%
  select(-num_unknown)

# PERCENT of total empty spots AT EACH DIFFICULTY in each nsa
empty_spaces_by_diff_by_nsa_perc <- tree_by_tree_categorized %>% 
  group_by(nbrdesc, difficulty_level) %>%
  dplyr::summarize(num = n()) %>%
  # Join totals for perc calc
  left_join(spaces_count_by_nsa_all %>% select(nbrdesc , spaces_without_live_trees)) %>%
  mutate(perc_difflvl_to_empty_spaces = round(100*(num/spaces_without_live_trees), 2)) %>%
  select(-num) %>%
  spread(difficulty_level, perc_difflvl_to_empty_spaces) %>%
  select(-"<NA>") %>%
  rename(
    perc_of_nontreed_unknown = "0",
    perc_of_nontreed_easy = "1",
    perc_of_nontreed_moderate = "2",
    perc_of_nontreed_hard = "3",
    perc_of_nontreed_unsuitable = "4",
    num_spaces_without_live_trees = "spaces_without_live_trees"
  ) %>%
  select(-perc_of_nontreed_unknown)

# Join into master DIFFICULTY table
empty_spaces_by_diff_by_nsa <- empty_spaces_by_diff_by_nsa_count %>%
  left_join(empty_spaces_by_diff_by_nsa_perc) %>%
  # Rearange for readability
  select(1, 6:7, 2:5, 8:11)
# Remove building blocks from workspace
cleanup()

# Write to csv
write_csv(empty_spaces_by_diff_by_nsa, "data/output-data/street-tree-analyses/nontreed_spaces_by_diff_by_nsa.csv")

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
  mutate(perc_condition_to_live_trees = round(100*(num_trees/spaces_with_live_trees), 2))

# Tree condition percent by nsa
tree_condition_perc_by_nsa_wide <- tree_condition_by_nsa_long %>%
  # Select cols to spread
  select(nbrdesc, condition, perc_condition_to_live_trees) %>%
  spread(condition, perc_condition_to_live_trees) %>%
  rename(
    poor_perc_of_live = poor,
    fair_perc_of_live = fair,
    good_perc_of_live = good
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
  left_join(spaces_count_by_nsa_all %>% select(nbrdesc, spaces_with_live_trees)) %>%
  # Rearange for readability
  select(
    1, 8, 5:7, 2:4
  )

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

# Add to tree_condition_by_nsa
tree_condition_by_nsa <- tree_condition_by_nsa %>%
  left_join(tree_height_diam_by_nsa) %>%
  # Drop combined totals cols
  select(-contains("combined"))

# Wite it to csv
write_csv(tree_condition_by_nsa, "data/output-data/street-tree-analyses/tree_condition_by_nsa.csv")

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
  left_join(tree_as_percent_of_spaces_by_nsa) %>%
  left_join(count_at_condition %>% select(absent_count = absent, unknown_count = unknown))

# Write to csv
write_csv(master_by_nsa, "data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")

