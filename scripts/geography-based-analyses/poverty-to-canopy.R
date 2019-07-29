#### Analysis of Community Statistical Areas
#### CSA poverty to tree canopy 

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################

# install.packages('RColorBrewer', dependencies = T)
library(tidyverse)
library(here) # A helpful package for file management
library(viridis) # A pretty, colorblind-friendly set of palettes
library(colorspace)

# Turn off scientific notation
options(scipen = 999)

###############################
####### LOAD DATA ########
###############################

csa_tree_temp_demographics <- read_csv(here(
  # PATH
  "data", "output-data", "cleaned", "tree-temp-demographic-w-naip-lidar-use",
  # FILE NAME
  "csa_lidartree_temp_demographics.csv")) %>%
  select(matches("csa2010"), 
         matches("09-63_mean"),
         matches("temp_mean_aft"),
         matches("median_household_income"), 
         matches("percent_of_households_earning_less_than_25_000"), 
         matches("percent_of_households_earning_25_000_to_40_000"), 
         matches("percent_of_households_earning_40_000_to_60_000"), 
         matches("percent_of_households_earning_60_000_to_75_000"), 
         matches("percent_of_households_earning_more_than_75_000"), 
         matches("percent_of_family_households_living_below_the_poverty_line")) %>%
  mutate_at(vars(matches("percent")), round, 2)

###############################
####### PLOTTING STUFF ########
###############################

## First graph
# csa_tree_temp_demographics %>%
#   ggplot() +
#   geom_point(aes(x = median_household_income/1000,
#                  y = `09-63_mean`,
#                  color = `09-63_mean`),
#              size=4) +
#   coord_flip() +
#   scale_color_viridis(direction = -1) +
#   labs(title = "Median Income to Percent Tree Canopy",
#        subtitle = "In thousands of dollars",
#        x = "",
#        y = "") +
#   scale_x_continuous(label = scales::dollar_format(),
#                      breaks = seq(0, 150, 10)) + 
#   scale_y_continuous(label = scales::percent_format(accuracy = 1.0),
#                      breaks = seq(0, 1, .1)) + 
#   theme_bw() +
#   theme(legend.position = "none",
#         plot.title = element_text(size = 20),
#         plot.subtitle = element_text(size = 16))
#   
# # Save to file
# ggsave(filename = "median-income-to-mean-tree-cover.png",
#        device = "png", path = here("data", "output-data", "income-to-treecover"),
#        width = 6, height = 6, units = "in")


# CSAs to call out
callout_df <- c("Canton", "Greater Roland Park/Poplar Hill", "Greenmount East", "Patterson Park North & East")

## Revised graph
csa_tree_temp_demographics %>%
  ggplot() +
  # This section for the basic plot
  geom_point(aes(x = percent_of_family_households_living_below_the_poverty_line/100,
                 y = `09-63_mean`,
                 color = `09-63_mean`),
             size=4) +
  # This section for circling all sample neighborhood points
  geom_point(data = csa_tree_temp_demographics %>%
               filter(csa2010 %in% c("Canton", "Greater Roland Park/Poplar Hill", "Greenmount East", "Patterson Park North & East")),
             aes(x = percent_of_family_households_living_below_the_poverty_line/100,
                 y = `09-63_mean`,
                 color = `09-63_mean`),
             size=6, shape = 1) +
  # This section for labeling Canton, etc.
  ggrepel::geom_label_repel(data = subset(csa_tree_temp_demographics, 
                        csa2010 %in% c("Canton", "Greater Roland Park/Poplar Hill", "Greenmount East")),
            aes(x = percent_of_family_households_living_below_the_poverty_line/100, 
                y = `09-63_mean`, 
                label = csa2010),
            min.segment.length = .1,
            segment.alpha = .5,
            nudge_x = .05,
            nudge_y = .05) +
  # This section for labeling Patterson Park (so it can be nudged)
  ggrepel::geom_label_repel(data = subset(csa_tree_temp_demographics, 
                        csa2010 %in% "Patterson Park North & East"),
            aes(x = percent_of_family_households_living_below_the_poverty_line/100, 
                y = `09-63_mean`, 
                label = csa2010),
            min.segment.length = .1,
            segment.alpha = .5,
            nudge_x = -.05) +
  # Colors and label formatting follow
  coord_flip() +
  scale_colour_gradient(low = "#E0FEA9", high = "#144A11") +
  labs(title = "Poverty to Tree Canopy",
       subtitle = "Percent of households living below the poverty line \ncompared to the percent of tree cover in that neighborhood",
       x = "",
       y = "Tree Cover") +
  scale_x_continuous(label = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, .1)) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, .1)) + 
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12))

# Save to file
ggsave(filename = "poverty-to-mean-tree-cover.png",
       device = "png", path = here("data", "output-data", "income-to-treecover"),
       width = 6, height = 6, units = "in")

