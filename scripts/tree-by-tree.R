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
### Begin Analysis
#####################

condition_levels <- factor(c("dead",
                             "poor",
                             "fair",
                             "good",
                             "absent",
                             "sprouts",
                             "stump",
                             "stump w",
                             "n/a"),
                           ordered = TRUE
                           )


tree_by_tree <- read_csv("../baltimore-climate-project/data/treecover/street_trees_nsa_join_table.csv") %>%
  select(-COLOR_2, -LABEL) %>%
  rename_all(tolower) %>%
  mutate_all(tolower) %>%
  mutate_at(vars(matches("street"), matches("onstr"), 
                 matches("side"), matches("spp"),
                 matches("condition"), matches("mt"),
                 matches("loc_type"), matches("space_type"),
                 matches("utilities"), matches("hard_scape"),
                 matches("cultivar"), matches("common"),
                 matches("genus"), matches("nbrdesc")
                 ), 
            as.factor) %>%
  mutate_at(vars(matches("tree_ht"), matches("dbh")), as.numeric)
  #mutate_at(vars(matches("nbrdesc")), tolower)


tree_condition_by_nbr <- tree_by_tree %>% 
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
