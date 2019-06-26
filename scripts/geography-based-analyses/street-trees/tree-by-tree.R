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
# setwd("/Users/rready/Desktop/baltimore-climate-project")

#####################
##### Load data #####
#####################

tree_by_tree <- read_csv("data/treecover/street_trees_nsa_join_table.csv") %>%
  select(-COLOR_2, -LABEL) %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  mutate(condition = ifelse(condition == "stump w", "stump", condition)) %>%
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
                                   "sprouts",
                                   "n/a")
  )
  ) %>%
  mutate_at(vars(matches("tree_ht"), matches("dbh")), as.numeric)
#mutate_at(vars(matches("nbrdesc")), tolower)


######################
### Begin analysis ###
######################

## Examine cagegories of information

tree_by_tree %>% 
  group_by(condition) %>%
  dplyr::summarize(num = n()) %>%
  arrange(-desc(condition))

tree_by_tree %>% 
  group_by(space_type) %>%
  dplyr::summarize(num = n()) %>%
  arrange(desc(num))

## Summary analyses 

# Condition by neighborhood including "potential" tree sites
tree_condition_by_nbr_all <- tree_by_tree %>% 
  group_by(nbrdesc, condition) %>%
  summarize(num = n()) %>%
  mutate(perc = round(100*(num / sum(num)), 2)) %>%
  select(-num) %>%
  spread(condition, perc)

# Condition by neighborhood not including "potential" tree sites
tree_condition_by_nbr_filled <- tree_by_tree %>% 
  #filter(!space_type %in% "potential") %>%
  filter(!str_detect(space_type, "potential")) %>%
  group_by(nbrdesc, condition) %>%
  summarize(num = n()) %>%
  mutate(perc = round(100*(num / sum(num)), 2)) %>%
  select(-num) %>%
  spread(condition, perc)

summary_tbl_1 <- tree_by_tree %>%
  group_by(nbrdesc) %>%
  summarize(combined_ht = sum(tree_ht),
            avg_ht = round(mean(tree_ht), 2),
            combined_diam = sum(dbh),
            avg_diam = round(mean(dbh), 2)
  ) %>%
  left_join(tree_condition_by_nbr)

write_csv(summary_tbl_1, "output/data/tree_by_tree_neighborhood_summary.csv")


