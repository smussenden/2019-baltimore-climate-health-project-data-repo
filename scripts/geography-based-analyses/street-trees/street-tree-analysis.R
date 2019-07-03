##### Baltimore Climate and Health Project #####
##### By Sean Mussenden, Roxanne Ready, Jake Gluck and Jane Gerard #####

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("janitor")

library(tidyverse)
#library(readxl)
#library(janitor)
#library(magrittr)
#library(plyr) # Note: plyr conflicts with dplyr in some cases. Use dplyr::fun() for dplyr functions when plyr is loaded.

# Turn off scientific notation
options(scipen = 999)

# For cleanup
rm(list=ls())

# Set working directory
# setwd("/Users/rready/Desktop/2019-baltimore-climate-health-project-data-repo")

#########################################
### Define functions, store variables ################################################################
#########################################
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("cleanup",
                    "street_trees_categorized",
                    "master_street_tree_summaries",
                    "master_street_tree_summaries_filtered",
                    "cbPalette",
                    "target_nsas",
                    "counterpoint_nsas")
  ),
  pos = 1
  )
}

# Colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

# List of target NSAs
target_nsas <- c("Berea", "Broadway East", "Oliver", "Middle East", "Biddle Street","Milton-Montford", "Madison-Eastend", "CARE", "McElderry Park", "Ellwood Park/Monument", "Patterson Place", "Patterson Park Neighborhood", "Baltimore Highlands", "Highlandtown", "Upper Fells Point") %>%
  lapply(tolower)

# List of nearby NSAs that are richer to use as counterpoints
counterpoint_nsas <- c("Butcher's Hill", "Canton", "Washington Hill") %>%
  lapply(tolower)

#####################
##### Load data #################################################################################################
#####################

### Load data created by "street-tree-data-cleaning.R"

# Load the full dataset with categories added in cleaning file
street_trees_categorized <- read_csv("data/input-data/street-trees/csv/by_nsa/street_trees_nsa_categorized.csv")

# Load the master summaries table created in cleaning file
master_street_tree_summaries <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv") %>%
  # Make "condition" a factor so it can be ordered
  mutate_at(vars(matches("condition")), 
            as.factor) %>%
  # Add variable to track whether a target NSA
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

# Load the filtered master summaries table created in cleaning file
master_street_tree_summaries_filtered <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa_targetonly.csv") %>%
  # Make "condition" a factor so it can be ordered
  mutate_at(vars(matches("condition")), 
            as.factor) %>%
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

###################################################################
### Add more rankings on top of what was added in cleaning file ###
###################################################################

a <- master_street_tree_summaries %>%
  # Add ranking for perc good
  arrange(nbrdesc, x) %>%
  mutate(rank_x = rank(desc(x), na.last = "keep", ties.method = "first"))

# Update the save file
write_csv(a, "data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")
  

#######################
### Visualize #####################################################################################################
#######################


############################################
### Tree HEIGHT by neighborhood ############
############################################

# Plot HEIGHT by nsa for TARGET nsas using controled averages
ggplot(master_street_tree_summaries_filtered, 
       aes(x = reorder(nbrdesc, avg_ht_controled), 
           y = avg_ht_controled, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
           )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Height of Target NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")

# Save to file
ggsave(filename = "avg_tree_height_target_nsas.png", device = "png", path = "data/output-data/street-tree-analyses/plots")

# Plot HEIGHT by nsa for BOTTOM 100 nsas using controled averages
ggplot(master_street_tree_summaries %>% 
         top_n(100, -avg_ht_controled) %>%
         filter(!is.na(avg_ht_controled)), 
       aes(x = reorder(nbrdesc, avg_ht_controled), 
           y = avg_ht, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Not Target NSA", "Target NSA"))
           )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Height of Bottom 100 NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")

# Save to file
ggsave(filename = "avg_tree_height_bottom100_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots",
       width = 6, height = 9.5, units = "in")

# Plot HEIGHT by nsa for TOP 100 nsas using controled averages
ggplot(master_street_tree_summaries %>% 
         top_n(100, avg_ht_controled) %>%
         filter(!is.na(avg_ht_controled)), 
       aes(x = reorder(nbrdesc, avg_ht_controled), 
           y = avg_ht_controled, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Not Target NSA"))
           )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Height of Top 100 NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")

# Save to file
ggsave(filename = "avg_tree_height_top100_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots",
       width = 6, height = 9.5, units = "in")

# Plot HEIGHT by nsa for ALL nsas using controled averages
ggplot(filter(master_street_tree_summaries, !is.na(avg_ht_controled)), 
       aes(x = reorder(nbrdesc, avg_ht_controled), 
           y = avg_ht_controled, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Not Target NSA", "Target NSA"))
           )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Height of NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "avg_tree_height_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots",
       width = 6, height = 19, units = "in")


############################################
### Tree DIAMETER by neighborhood ##########
############################################

# Plot DIAMETER of TARGET nsas using controled averages
ggplot(master_street_tree_summaries_filtered, 
       aes(x = reorder(nbrdesc, avg_diam_controled), 
           y = avg_diam_controled, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Diameter of Target NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")

# Save to file
ggsave(filename = "avg_tree_diameter_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots")


# Plot DIAMETER of ALL nsas using controled averages
ggplot(filter(master_street_tree_summaries, !is.na(avg_diam_controled)), 
       aes(x = reorder(nbrdesc, avg_diam_controled), 
           y = avg_diam_controled, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Tree Diameter of All NSAs",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "avg_tree_diameter_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots",
       width = 6, height = 19, units = "in")



############################################
### Count/Perc of trees in each NSA ########
############################################

# Plot COUNT OF TREES for TARGET nsas
ggplot(master_street_tree_summaries_filtered, 
       aes(x = reorder(nbrdesc, spaces_with_live_trees), 
           y = spaces_with_live_trees, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of Trees in Each NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "bottom")

# Plot COUNT OF TREES for ALL nsas
ggplot(master_street_tree_summaries, 
       aes(x = reorder(nbrdesc, spaces_with_live_trees), 
           y = spaces_with_live_trees, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Counterpoint NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of Trees in Each NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "tree_count_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/tree-count",
       width = 6, height = 19, units = "in")

### Some quick calculations comparing the targets to citywide
# Compare DIAMETER and MEAN of ALL nsas (minus those with fewer than 50 trees)...
master_street_tree_summaries %>%
  filter(spaces_with_live_trees >= 50) %>%
  summarise(diam_median = median(avg_diam),
            diam_mean = mean(avg_diam))

# ...to DIAMETER and MEAN of TARGET nsas
master_street_tree_summaries %>%
  filter(is_target_nsa == T) %>%
  summarise(diam_median = median(avg_diam),
            diam_mean = mean(avg_diam))

# Distribution histograms showing HEIGHT of TARGET nsas
ggplot(filter(street_trees_categorized, 
                (is_target_nsa == T) & 
                (has_live_tree == T)
              )) +
  geom_histogram(aes(x = dbh, fill = "nbrdesc"), binwidth = 1, show.legend = FALSE) +
  facet_wrap(~nbrdesc, scales = "free", nrow = 3) +
  labs(title = "Distribution of Tree Diameter Among Target Neighborhoods",
       x = "Trunk Diameter in Inches",
       y = "") +
  xlim(NA, 30)

# Save to file
ggsave(filename = "tree_count_distro_target_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")

# Distribution histograms showing HEIGHT of TOP 15 nsas
street_trees_categorized %>%
  filter((has_live_tree == T)) %>%
  select(nbrdesc, dbh) %>%
  # Filter by X NSAs the largest average diameter trees and at least Y trees
  right_join(street_trees_categorized %>%
               filter(has_live_tree == T) %>%
               select(nbrdesc, dbh, has_live_tree) %>%
               group_by(nbrdesc) %>%
               summarize(med_dbh = median(dbh),
                         num_trees = sum(has_live_tree)) %>%
               arrange(desc(med_dbh)) %>%
               # At least Y trees
               filter(num_trees >= 50) %>%
               # Top X by average diameter
               top_n(15)
  ) %>%
  ggplot() +
  geom_histogram(aes(x = dbh, fill = "nbrdesc"), binwidth = 1, show.legend = FALSE) +
  facet_wrap(~nbrdesc, scales = "free", nrow = 3) +
  labs(title = "Distribution of Tree Diameter Among Neighborhoods With Largest Trees",
       x = "Trunk Diameter in Inches",
       y = "") +
  xlim(NA, 30)

# Save to file
ggsave(filename = "tree_count_distro_top15_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")


