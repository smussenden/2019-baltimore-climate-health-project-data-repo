#### Analysis of Community Statistical Areas
#### CSA poverty to tree canopy 

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################

# install.packages('colorspace', dependencies = T)
library(tidyverse)
library(here) # A helpful package for file management
library(colorspace)
library(ggplot2)

# Turn off scientific notation
options(scipen = 999)

###############################
####### LOAD DATA ########
###############################

csa_tree_temp_demographics <- read_csv(here::here(
  # PATH
  "data", "output-data", "cleaned", "tree-temp-demographic-w-naip-lidar-use",
  # FILE NAME
  "csa_lidartree_temp_demographics.csv")) %>%
  select(matches("csa2010"), 
         matches("07_lid_mean"),
         matches("temp_mean_aft"),
         matches("median_household_income"), 
         matches("percent_of_households_earning_less_than_25_000"), 
         matches("percent_of_households_earning_25_000_to_40_000"), 
         matches("percent_of_households_earning_40_000_to_60_000"), 
         matches("percent_of_households_earning_60_000_to_75_000"), 
         matches("percent_of_households_earning_more_than_75_000"), 
         matches("percent_of_family_households_living_below_the_poverty_line")) %>%
  mutate_at(vars(matches("percent")), round, 2)

# CSAs to call out
callout_ls <- c("Canton", "Clifton-Berea", "Greater Roland Park/Poplar Hill", "Greenmount East")

###############################
####### PLOTTING STUFF ########
###############################

## INCOME TO TREE COVER
# csa_tree_temp_demographics %>%
#   ggplot() +
#   geom_point(aes(x = median_household_income/1000,
#                  y = `07_lid_mean`,
#                  color = `07_lid_mean`),
#              size=4) +
#   # This section shows the trend line
#   geom_smooth(aes(x = median_household_income/1000,
#                   y = `07_lid_mean`), method = glm, se = FALSE) +
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

## POVERTY TO TREE COVER
csa_tree_temp_demographics %>%
  # Start ggplot and set x and y for entire plot
  ggplot(aes(
    x = percent_of_family_households_living_below_the_poverty_line/100, 
    y = `07_lid_mean`
    )) +
  # This section for the basic scatterplot
  geom_point(aes(color = `07_lid_mean`),
             size=4) +
  # This section for circling all sample neighborhood points
  geom_point(data = csa_tree_temp_demographics %>%
               filter((csa2010 %in% callout_ls) 
                      # Patterson Park must be included seperately because of its unique label positioning
                      | (csa2010 == "Patterson Park North & East") 
                      ),
             aes(color = `07_lid_mean`),
             size=6, shape = 1) +
  # This section shows the trend line
  geom_smooth(se = FALSE, # Removes gray banding
              method = glm, 
              color = "black") +
  # This section for labeling Canton, etc.
  ggrepel::geom_label_repel(data = csa_tree_temp_demographics %>%
                              filter(csa2010 %in% callout_ls) %>%
                              mutate(csa2010 = case_when(
                                csa2010 == "Greenmount East" ~ "Greenmount East \n(includes part of Broadway East)", 
                                csa2010 == "Clifton-Berea" ~ "Clifton-Berea \n(includes part of Broadway East)",
                                T ~ csa2010)),
            aes(label = csa2010),
            min.segment.length = .1,
            segment.alpha = .5,
            alpha = .85,
            nudge_x = .05,
            nudge_y = .06) +
  # This section for labeling Patterson Park (so it can be nudged)
  ggrepel::geom_label_repel(data = csa_tree_temp_demographics %>%
                              filter(csa2010 == "Patterson Park North & East") %>%
                              mutate(csa2010 = case_when(
                                csa2010 == "Patterson Park North & East" ~ "Patterson Park North & East \n(includes most of McElderry Park)",
                                T ~ csa2010)),
                            aes(label = csa2010),
                            min.segment.length = .1,
                            segment.alpha = .5,
                            alpha = .85,
                            nudge_x = -.06,
                            nudge_y = .03) +
  # Colors and label formatting follow
  #coord_flip() +
  scale_colour_gradient(low = "#E0FEA9", high = "#144A11") +
  labs(title = "Poverty to Tree Canopy",
       subtitle = "Percent of households living below the poverty line \ncompared to the percent of tree cover in the area",
       x = "Percent of households living below the poverty line",
       y = "Percent of land covered by trees") +
  scale_x_continuous(label = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, .1)) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, .1)) + 
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12))

# Save to file
ggsave(filename = "poverty-to-mean-tree-cover2.png",
       device = "png", path = here("data", "output-data", "income-to-treecover"),
       width = 8, height = 8, units = "in")


# MISC
nsa_tree_lidar <- read_csv(here("data", "btree_statistics_by_nsa_2007_lidar.csv")) %>%
  left_join(read_csv(here("data", "btree_statistics_by_nsa_2015_lidar.csv"))) %>%
  select(LABEL, `07_mean`, `15_mean` ) %>%
  mutate(canopy_change = round((`15_mean` - `07_mean`), 4))
write_csv(nsa_tree_lidar, here("data", "btree_statistics_by_nsa_2007_2015_lidar.csv"))

nsa_tree_lidar_both <- read_csv(here("data", "btree_statistics_by_nsa_2007_2015_lidar.csv"))