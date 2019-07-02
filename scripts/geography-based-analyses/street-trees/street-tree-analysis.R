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

# Need to add target NSA info

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















# Plot EMPTY SPACES by nsa
ggplot(master_street_tree_summaries_filtered) +
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
ggplot(master_street_tree_summaries_filtered) +
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


###################################
### Difficulty of planting ########
###################################

# Plot PERCENT of UNTREED areas are EASY TO PLANT by nsa
ggplot(master_street_tree_summaries_filtered) +
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
ggplot(master_street_tree_summaries_filtered) +
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


######################################
### Tree condition by nsa ############
######################################

# Plot PROPORTIONAL CONDITION of TREE SPACES by nsa
ggplot(filter(street_trees_categorized, 
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
ggplot(filter(street_trees_categorized, 
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
ggplot(filter(street_trees_categorized, 
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



