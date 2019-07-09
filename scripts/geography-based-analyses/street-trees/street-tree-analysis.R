##### Baltimore Climate and Health Project #####
##### By Sean Mussenden, Roxanne Ready, Jake Gluck and Jane Gerard #####

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("janitor")
## install.packages("DescTools")

library(tidyverse)
require(scales) # For percent labeling on distribution tables
library("DescTools") # For %like% operator
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
  ))

# Load the master summaries table created in cleaning file
master_street_tree_summaries <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv") %>%
  # Add variable to track whether a target NSA
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

# Load the filtered master summaries table created in cleaning file
master_street_tree_summaries_filtered <- read_csv("data/output-data/street-tree-analyses/master_street_tree_by_nsa_targetonly.csv") %>%
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

# Quick count
master_street_tree_summaries %>%
  filter(num_spaces_with_live_trees >= 50) %>%
  select(nbrdesc) %>%
  dplyr::summarise(n())

street_trees_categorized %>%
  filter(notes %like% "%lightning%") %>%
  select(nbrdesc) %>%
  dplyr::summarise(n())

street_trees_categorized %>%
  filter(nbrdesc %in% "madison-eastend", has_live_tree == T, loc_type %in% "street") %>%
  dplyr::summarise(n())

# Quick count at a value
street_trees_categorized %>%
  right_join(master_street_tree_summaries %>% filter(num_spaces_with_live_trees >= 50)) %>%
  filter(dbh >= 6, is_target_nsa == T, condition == "good") %>%
  select(nbrdesc) %>%
  #group_by(nbrdesc) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))

# Create a temp working table to consistently overwrite without messing up loaded table
wk <- street_trees_categorized

###################################################################
### Add more rankings on top of what was added in cleaning file ###
###################################################################

# a <- master_street_tree_summaries %>%
#   # Add ranking for perc good
#   arrange(nbrdesc, x) %>%
#   mutate(rank_x = rank(desc(x), na.last = "keep", ties.method = "first"))
# 
# # Update the save file
# write_csv(a, "data/output-data/street-tree-analyses/master_street_tree_by_nsa.csv")


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
ggsave(filename = "avg_tree_height_target_nsas.png", device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")

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
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter",
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
ggsave(filename = "avg_tree_diameter_by_nsa.png", device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")


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
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter",
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

#########################
### Height / Diameter ###
#########################

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

# DISTRIBUTION histograms showing DIAMETER of TARGET nsas
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
ggsave(filename = "diameter_distro_target_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")

# DISTRIBUTION histograms showing DIAMETER of TOP 15 nsas
street_trees_categorized %>%
  filter((has_live_tree == T)) %>%
  select(nbrdesc, dbh) %>%
  # Filter by X NSAs with the largest average diameter trees and at least Y trees
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
ggsave(filename = "diameter_distro_top15_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")




# DISTRIBUTION histograms showing HEIGHT of TARGET nsas
ggplot(filter(street_trees_categorized, 
              (is_target_nsa == T) & 
                (has_live_tree == T)
)) +
  geom_histogram(aes(x = tree_ht, fill = "nbrdesc"), binwidth = 3, show.legend = FALSE) +
  facet_wrap(~nbrdesc, scales = "free", nrow = 3) +
  labs(title = "Distribution of Tree Height Among Target Neighborhoods",
       x = "Tree Height in Inches",
       y = "") +
  xlim(NA, 100)

# Save to file
ggsave(filename = "height_distro_target_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")

# DISTRIBUTION histograms showing HEIGHT of TOP 15 nsas
street_trees_categorized %>%
  filter((has_live_tree == T)) %>%
  select(nbrdesc, tree_ht) %>%
  # Filter by X NSAs with the largest average height trees and at least Y trees
  right_join(street_trees_categorized %>%
               filter(has_live_tree == T) %>%
               select(nbrdesc, tree_ht, has_live_tree) %>%
               group_by(nbrdesc) %>%
               summarize(med_tree_ht = median(tree_ht),
                         num_trees = sum(has_live_tree)) %>%
               arrange(desc(med_tree_ht)) %>%
               # At least Y trees
               filter(num_trees >= 50) %>%
               # Top X by average diameter
               top_n(15)
  ) %>%
  ggplot() +
  geom_histogram(aes(x = tree_ht, fill = "nbrdesc"), binwidth = 3, show.legend = FALSE) +
  facet_wrap(~nbrdesc, scales = "free", nrow = 3) +
  labs(title = "Distribution of Tree Height Among Neighborhoods With Largest Trees",
       x = "Tree Height in Inches",
       y = "") +
  xlim(NA, 100)

 # Save to file
ggsave(filename = "height_distro_top15_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")


# DISTRIBUTION lines showing DIAMETER compared to TOP 15 nsas
wk <- street_trees_categorized %>%
  filter((has_live_tree == T) & (loc_type %in% "street")) %>%
  select(nbrdesc, dbh) %>%
  # Filter by X NSAs with the largest average diameter trees and at least Y trees
  right_join(street_trees_categorized %>%
               filter((has_live_tree == T) & (loc_type %in% "street")) %>%
               select(nbrdesc, dbh, has_live_tree) %>%
               group_by(nbrdesc) %>%
               summarize(med_dbh = median(dbh),
                         num_trees = sum(has_live_tree)) %>%
               arrange(desc(med_dbh)) %>%
               # At least Y trees
               filter(num_trees >= 50) %>%
               # Top X by average diameter
               top_n(15)
  )

label <- tibble(
  x = Inf,
  y = Inf,
  label = "Park trees were excluded \nfrom these calculations."
)

ggplot() +
  # Top 15 NSAs
  geom_density(data = wk, 
               aes(x = dbh),
               color = "#0072B2")+
  # Target NSAs
  geom_density(data = filter(street_trees_categorized, (is_target_nsa == T) & (has_live_tree == T) & (loc_type %in% "street")), 
               aes(x = dbh),
               color = "#E69F00") +
  labs(title = "Distribution of Tree Diameter, Target NSAs (yellow) vs. Top 15 (blue)",
       x = "Tree Diameter in Inches",
       y = "") +
  xlim(NA, 50) +
  geom_text(aes(x = x, y = y, label = label), data = label, vjust = "top", hjust = "right") +
  scale_y_continuous(labels = percent)
  
# Save to file
ggsave(filename = "diameter_distro_comparative_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")


# DISTRIBUTION lines showing HEIGHT compared to TOP 15 nsas
wk <- street_trees_categorized %>%
  filter((has_live_tree == T) & (loc_type %in% "street")) %>%
  select(nbrdesc, tree_ht) %>%
  # Filter by X NSAs with the largest average height trees and at least Y trees
  right_join(street_trees_categorized %>%
               filter((has_live_tree == T) & (loc_type %in% "street")) %>%
               select(nbrdesc, tree_ht, has_live_tree) %>%
               group_by(nbrdesc) %>%
               summarize(med_tree_ht = median(tree_ht),
                         num_trees = sum(has_live_tree)) %>%
               arrange(desc(med_tree_ht)) %>%
               # At least Y trees
               filter(num_trees >= 50) %>%
               # Top X by average diameter
               top_n(15)
  )

ggplot() +
  # Top 15 NSAs
  geom_density(data = wk, 
               aes(x = tree_ht),
               color = "#0072B2") +
  # Target NSAs
  geom_density(data = filter(street_trees_categorized, (is_target_nsa == T) & (has_live_tree == T) & (loc_type %in% "street")), 
               aes(x = tree_ht),
               color = "#E69F00") +
  labs(title = "Distribution of Tree Height, Target NSAs (yellow) vs. Top 15 (blue)",
       x = "Tree Diameter in Inches",
       y = "") +
  xlim(NA, 90) +
  geom_text(aes(x = x, y = y, label = label), data = label, vjust = "top", hjust = "right") +
  scale_y_continuous(labels = percent)

# Save to file
ggsave(filename = "height_distro_comparative_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter")

###################################
### Distribution of young trees ###
###################################

# What percent of live trees are young in target nsas?
wk <- street_trees_categorized %>%
  # 1. Count live trees
  filter(has_live_tree == T) %>%
  select(nbrdesc) %>%
  group_by(nbrdesc) %>%
  dplyr::summarise(n_live = n()) %>%
  # 2. Filter out trees under a low diameter (4 inches)
  right_join(street_trees_categorized %>%
               filter(has_live_tree == T) %>%
               select(nbrdesc, dbh) %>%
               mutate(is_young = ifelse(dbh < 4, T, F)) %>%
               group_by(nbrdesc, is_young) %>%
               dplyr::summarise(n_young = n()) %>%
               filter(is_young == T) %>%
               select(-is_young)
  ) %>%
  # 3. Divide young by live for percent
  mutate(perc_young = round(100*(n_young/n_live), 2)) %>%
  # 4. Rejoin full table, controled for 
  right_join(master_street_tree_summaries) %>%
  select(-n_live) %>%
  # Add ranking for perc young
  arrange(nbrdesc, perc_young) %>%
  mutate(rank_perc_young = rank(desc(perc_young), na.last = "keep", ties.method = "first"))

# Plot PERCENT DISTRIBUTION of live trees are YOUNG in ALL nsas
wk %>% # Must run from line 320(ish)
  # Control for at least 50 trees
  filter(spaces_with_live_trees >= 50) %>%
  ggplot() +
  geom_col(aes(x = reorder(nbrdesc, perc_young), 
               y = perc_young, 
               fill = factor(is_target_nsa, labels=c("Counterpoint NSA"," Target NSA"))
               )) +
  coord_flip() +
  labs(title = "Percent Young Live Trees \n(Trunk Diameter <4 inches) ",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "perc_young_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/height-diameter",
       width = 6, height = 19, units = "in")


############################################
### Tree CONDITION by neighborhood #########
############################################


# Condidering only older trees (more than 6 inches), what is the proportional condition of living trees by nsa?

# DISTRIBUTION histogram showing DIAMETER of ALL nsas to find young trees
# street_trees_categorized %>%
#   filter((has_live_tree == T)) %>%
#   select(nbrdesc, dbh) %>%
#   ggplot() +
#   geom_histogram(aes(x = dbh, fill = "nbrdesc"), binwidth = 1, show.legend = FALSE) +
#   labs(title = "Distribution of Tree Diameter Across All Neighborhoods",
#        x = "Trunk Diameter in Inches",
#        y = "") +
#   # Adjust X tics
#   scale_x_continuous(breaks = seq(0, 60, by = 2), limits = c(NA, 60))

## WORKING TABLE for CONDITION PLOTS
wk <- street_trees_categorized %>%
  filter((has_live_tree == T), dbh > 6) %>%
  select(nbrdesc, condition, is_target_nsa) %>%
  #group_by(nbrdesc, condition) %>%
  #summarize(num_condition = n()) %>%
  # Filter out nsas with fewer than 50 trees
  right_join(street_trees_categorized %>%
               filter(has_live_tree == T, dbh > 6) %>%
               select(nbrdesc, dbh, has_live_tree) %>%
               group_by(nbrdesc) %>%
               summarize(num_trees = sum(has_live_tree)) %>%
               # At least 50 trees
               filter(num_trees >= 50)) %>%
  select(nbrdesc, condition, is_target_nsa)

# PROPORTIONAL CONDITION of LIVING, OLDER trees across ALL nsas
wk %>%
  ggplot() +
  geom_bar(aes(x = reorder(nbrdesc, condition=="good"), 
               fill = condition),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Condition of Live, \nOlder Trees (>6 in) by NSA",
       x = "",
       y = "",
       fill = "Condition") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "prop_condition_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/condition",
       width = 6, height = 19, units = "in")

# PROPORTIONAL CONDITION of LIVING, OLDER trees across TARGET nsas
wk %>%
  filter(is_target_nsa == T) %>%
  ggplot() +
  geom_bar(aes(x = reorder(nbrdesc, condition=="good"), 
               fill = condition),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Condition of Live, \nOlder Trees (>6 in) in Target NSAs",
       x = "",
       y = "",
       fill = "Condition") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "prop_condition_target_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/condition")

# PROPORTIONAL CONDITION of LIVING, OLDER trees across TOP 15 CONDITION == GOOD nsas
wk %>%
  right_join(wk %>% 
               select(nbrdesc, condition) %>%
               filter(condition == "good") %>%
               select(nbrdesc) %>%
               group_by(nbrdesc) %>%
               summarize(num_condition = n()) %>%
               arrange(desc(num_condition)) %>%
               top_n(15)
               ) %>%
  ggplot() +
  geom_bar(aes(x = reorder(nbrdesc, condition=="good"), 
               fill = condition),
           position = "fill") +
  coord_flip() +
  labs(title = "Proportional Condition of Live, \nOlder Trees (>6 in) in NSAs with the \nHighest Percent of Good Condition Trees",
       x = "",
       y = "",
       fill = "Condition") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "prop_condition_top15_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/condition")

# Find rankings of PERC GOOD
# wk (ln 530) is already filtered for only live trees and nsas with fewer than 50 trees
good_ranking <- wk %>% 
  # 1. Find count at each condition
  select(nbrdesc, condition) %>%
  #filter(condition == "good") %>%
  select(nbrdesc, condition) %>%
  group_by(nbrdesc, condition) %>%
  summarize(num_condition = n()) %>%
  # 2. Find total live trees 
  left_join(street_trees_categorized %>% 
              filter((has_live_tree == T), dbh > 6) %>%
              select(nbrdesc) %>%
              group_by(nbrdesc) %>%
              dplyr::summarize(num_live = n()) %>%
              select(nbrdesc, num_live)) %>%
  # 3. Find percent of condition to live
  mutate(perc_condition = round(100*(num_condition/num_live), 2)) %>%
  # 4. Drop cconditions that aren't "good"
  filter(condition == "good") %>%
  dplyr::rename(perc_good = perc_condition,
                num_good = num_condition) %>%
  # 5. Arrange and rank
  select(nbrdesc, num_good, perc_good) %>%
  arrange(desc(perc_good)) %>%
  ungroup %>%
  dplyr::mutate(perc_good_rank = rank(desc(perc_good), na.last = "keep", ties.method = "first")) %>%
  # 6. Add whether target nsa
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

# PERCENT of GOOD condition trees
ggplot(good_ranking, 
       aes(x = reorder(nbrdesc, perc_good), 
           y = perc_good, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Not Target NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Percent of Live, Older Trees (>6 in) \nin Good Condition by NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "condition_good_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/condition",
       width = 6, height = 19, units = "in")



# Find rankings of PERC POOR
# wk (ln 530) is already filtered for only live trees and nsas with fewer than 50 trees
poor_ranking <- wk %>% 
  # 1. Find count at each condition
  select(nbrdesc, condition) %>%
  group_by(nbrdesc, condition) %>%
  summarize(num_condition = n()) %>%
  # 2. Find total live trees 
  left_join(street_trees_categorized %>% 
              filter((has_live_tree == T), dbh > 6) %>%
              select(nbrdesc) %>%
              group_by(nbrdesc) %>%
              dplyr::summarize(num_live = n()) %>%
              select(nbrdesc, num_live)) %>%
  # 3. Find percent of condition to live
  mutate(perc_condition = round(100*(num_condition/num_live), 2)) %>%
  # 4. Drop cconditions that aren't "good"
  filter(condition == "poor") %>%
  dplyr::rename(perc_poor = perc_condition,
                num_poor = num_condition) %>%
  # 5. Arrange and rank
  select(nbrdesc, num_poor, perc_poor) %>%
  arrange(desc(perc_poor)) %>%
  ungroup %>%
  dplyr::mutate(perc_poor_rank = rank(desc(perc_poor), na.last = "keep", ties.method = "first")) %>%
  # 6. Add whether target nsa
  mutate(is_target_nsa = case_when(
    nbrdesc %in% target_nsas ~ T,
    TRUE ~ F 
  ))

# PERCENT of GOOD condition trees
ggplot(poor_ranking, 
       aes(x = reorder(nbrdesc, perc_poor), 
           y = perc_poor, 
           fill = factor(is_target_nsa, 
                         # Rename fill levels in legend
                         labels=c("Not Target NSA"," Target NSA"))
       )) +
  geom_col() +
  coord_flip() +
  labs(title = "Percent of Live, Older Trees (>6 in) \nin Poor Condition by NSA",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values=cbPalette) +
  theme(legend.position = "top")

# Save to file
ggsave(filename = "condition_poor_all_nsas.png", 
       device = "png", path = "data/output-data/street-tree-analyses/plots/condition",
       width = 6, height = 19, units = "in")




####################################################
### Density of "tree removal" (field: mt) by NSA ###
####################################################



###############
### Above calcs but with parks removed
###############
# By street? By loc_type == park?
