##### Baltimore Climate and Health Project #####
##### By Sean Mussenden, Roxanne Ready, Jake Gluck and Jane Gerard #####

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
library(tidyverse)

# Turn off scientific notation
options(scipen = 999)

# For cleanup
# rm(list=ls())

#################################################################
######## Load Data ##########
#################################################################
street_trees_nsa <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")

street_trees_ungrouped <- read_csv("data/output-data/street-tree-analyses/street_trees_nsa_categorized.csv")


###################################
######### Hist ####################
###################################
mcelderry_street_trees_ungrouped <- street_trees_ungrouped %>%
  filter(nbrdesc == "mcelderry park")

roland_street_trees_ungrouped <- street_trees_ungrouped %>%
  filter(nbrdesc == "roland park")

hist(street_trees_nsa$avg_ht)
hist(mcelderry_street_trees_ungrouped$tree_ht)
hist(roland_street_trees_ungrouped$tree_ht)

