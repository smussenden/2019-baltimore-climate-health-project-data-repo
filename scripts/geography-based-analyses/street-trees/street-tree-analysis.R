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
library(plyr)

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
                    "tree_condition_by_nsa",
                    "master_by_nsa",
                    "master_by_nsa_filtered",
                    "cbPalette")
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

# Not suitable : "a space exists to plant but it may be too small or near a hazard or obstruction," per Nathan Randolph, GIS Analyst at Department of Recreation and Parks


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
    has_live_tree == T ~ c,
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
write_csv(tree_by_tree_categorized, "data/input-data/street-trees/csv/by_nsa/street_trees_nsa_categorized.csv")

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
  dplyr::rename(
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
  dplyr::rename(
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
  left_join(tree_as_percent_of_spaces_by_nsa %>% select(nbrdesc , spaces_without_live_trees)) %>%
  mutate(perc_difflvl_to_empty_spaces = round(100*(num/spaces_without_live_trees), 2)) %>%
  select(-num) %>%
  spread(difficulty_level, perc_difflvl_to_empty_spaces) %>%
  select(-"<NA>") %>%
  dplyr::rename(
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

##########################################################
### Group trees into easy-moderate and hard-unsuitable ###
##########################################################

empty_spaces_by_broad_diff_by_nsa <- tree_by_tree_categorized %>% 
  mutate(diff_grouping = case_when(
    difficulty_level == 1 | difficulty_level == 2 ~ "easy_or_moderate",
    difficulty_level == 3 | difficulty_level == 4 ~ "hard_or_unsuitable",
    TRUE ~ "NA"
  )) %>%
  select(-difficulty_level) %>%
  filter(diff_grouping != "NA") %>%
  group_by(nbrdesc, diff_grouping) %>%
  dplyr::summarize(num = n()) %>%
  spread(diff_grouping, num) %>%
  dplyr::rename(
    num_easy_or_moderate = easy_or_moderate,
    num_hard_or_unsuitable = hard_or_unsuitable
  ) %>%
  # Join totals for perc calc
  left_join(tree_as_percent_of_spaces_by_nsa %>% select(nbrdesc , spaces_without_live_trees)) %>%
  mutate(perc_easy_or_moderate_of_nontreed = round(100*(num_easy_or_moderate/spaces_without_live_trees), 2))

## Add to master diff table
empty_spaces_by_diff_by_nsa <- empty_spaces_by_diff_by_nsa %>%
  left_join(empty_spaces_by_broad_diff_by_nsa) %>%
  select(-spaces_without_live_trees)

# (Re)write to csv
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
  left_join(tree_as_percent_of_spaces_by_nsa) %>%
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
  left_join(tree_as_percent_of_spaces_by_nsa %>% select(nbrdesc, spaces_with_live_trees)) %>%
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


###############################
### Put all in master table ###
###############################

master_by_nsa <- tree_as_percent_of_spaces_by_nsa %>%
  left_join(empty_spaces_by_diff_by_nsa) %>%
  left_join(tree_condition_by_nsa)

# Write to csv
write_csv(master_by_nsa, "data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")


######################################
### Analyses based on above tables ##################################################################
######################################

# List of target NSAs
target_nsas <- c("Berea", "Broadway East", "Oliver", "Middle East", "Biddle Street","Milton-Montford", "Madison-Eastend", "CARE", "McElderry Park", "Ellwood Park/Monument", "Patterson Place", "Patterson Park Neighborhood", "Baltimore Highlands", "Highlandtown", "Upper Fells Point") %>%
  lapply(tolower)

# List of nearby NSAs that are richer to use as counterpoints
counterpoint_nsas <- c("Butcher's Hill", "Canton", "Washington Hill") %>%
  lapply(tolower)

# Filtered master list of all above data for target and counterpoint NSAs only
master_by_nsa_filtered <- master_by_nsa %>%
  subset(nbrdesc %in% target_nsas) %>%
  mutate(is_target_nsa = T) %>%
  full_join(master_by_nsa %>%
               subset(nbrdesc %in% counterpoint_nsas) %>%
               mutate(is_target_nsa = F)) %>%
  select(1, 31, 2:30)

# Write to csv
write_csv(master_by_nsa_filtered, "data/output-data/street-tree-analyses/master_street_tree_by_nsa_targetonly.csv")

cleanup()
#######################
### Visualize ###
#######################

# Colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)


# TO DO
# Plot height / nsa [DONE]
# Plot diam / nsa [DONE]
# Redo height/diam filtered for young trees (diam > x)
# 

# Plot HEIGHT by nsa
ggplot(master_by_nsa_filtered, 
       aes(x = reorder(nbrdesc, avg_ht), 
           y = avg_ht, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
           )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Height by NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")
ggsave(filename = "avg_tree_height_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot DIAMETER by nsa
ggplot(master_by_nsa_filtered, 
       aes(x = reorder(nbrdesc, avg_diam), 
           y = avg_diam, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Diameter by NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")
ggsave(filename = "avg_tree_diameter_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot EMPTY SPACES by nsa
ggplot(master_by_nsa_filtered) +
  geom_col(aes(x = reorder(nbrdesc, perc_spaces_nontreed), 
               y = perc_spaces_nontreed, 
               fill = factor(is_target_nsa, 
                             # Rename fill levels in legend
                             labels=c("Counterpoint NSA"," Target NSA"))
               )
           ) +
  coord_flip() +
  labs(title = "Number of Spaces Without Trees",
       x = "Neighborhood Statistical Area",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette)


# Plot PERCENT NONVIABLE EMPTY SPACES by nsa
ggplot(master_by_nsa_filtered) +
  geom_col(aes(x = reorder(nbrdesc, -perc_of_nontreed_are_suitable), 
               y = (100 - perc_of_nontreed_are_suitable ), 
               fill = factor(is_target_nsa, 
                             # Rename fill levels in legend
                             labels=c("Counterpoint NSA"," Target NSA"))
               )
           ) +
  coord_flip() +
  labs(title = "Percent of Untreed Areas That Are Unsuitable for Planting",
       x = "Neighborhood Statistical Area",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "perc_nonviable_empty.png", device = "png", path = "data/output-data/street-tree-analyses/plots")

# Plot PERCENT of UNTREED areas are EASY TO PLANT by nsa
ggplot(master_by_nsa_filtered) +
  geom_col(aes(x = reorder(nbrdesc, perc_of_nontreed_easy), 
               y = perc_of_nontreed_easy, 
               fill = factor(is_target_nsa, 
                             # Rename fill levels in legend
                             labels=c("Counterpoint NSA"," Target NSA"))
  )
  ) +
  coord_flip() +
  labs(title = "Percent of Untreed Areas That Are Easy to Plant",
       x = "Neighborhood Statistical Area",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "perc_easy_to_tree.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot PERCENT of UNTREED areas are EASY OR MODERATE TO PLANT by nsa
ggplot(master_by_nsa_filtered) +
  geom_col(aes(x = reorder(nbrdesc, perc_easy_or_moderate_of_nontreed), 
               y = perc_easy_or_moderate_of_nontreed, 
               fill = factor(is_target_nsa, 
                             # Rename fill levels in legend
                             labels=c("Counterpoint NSA"," Target NSA"))
  )
  ) +
  coord_flip() +
  labs(title = "Percent of Untreed Areas That Are Easy-Moderate to Plant",
       x = "Neighborhood Statistical Area",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "perc_easymoderate_to_tree.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot PROPORTIONAL CONDITION of TREE SPACES by nsa
ggplot(filter(tree_by_tree_categorized, 
              (nbrdesc %in% target_nsas) | (nbrdesc %in% counterpoint_nsas))
       ) +
  geom_bar(aes(x = reorder(nbrdesc, condition=="good"), 
               fill = condition),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Condition of Tree Spaces by Neighborhood",
       x = "",
       y = "",
       fill = "Condition") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "prop_condition_of_tree_spaces_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot PROPORTIONAL CONDITION of LIVING TREES by nsa
ggplot(filter(tree_by_tree_categorized, 
              ((nbrdesc %in% target_nsas) | (nbrdesc %in% counterpoint_nsas)) &
                ((condition != "absent") & (condition != "stump") & (condition != "dead")))
) +
  geom_bar(aes(x = reorder(nbrdesc, condition=="good"), 
               fill = condition),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Condition of Living Trees by Neighborhood",
       x = "",
       y = "",
       fill = "Condition") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "prop_condition_of_live_trees_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot PROPORTIONAL of PLANTABLE SPACES vs LIVE TREES by nsa
ggplot(filter(tree_by_tree_categorized, 
              (nbrdesc %in% target_nsas) | (nbrdesc %in% counterpoint_nsas))
) +
  geom_bar(aes(x = reorder(nbrdesc, has_live_tree==T), 
               fill = factor(has_live_tree, labels = c("Dead or No Tree", "Live Tree"))),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Trees to Empty Spaces by Neighborhood",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette)
# Save to file
ggsave(filename = "prop_trees_to_spaces_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


