#### Analysis of Community Statistical Areas
#### CSA poverty to tree canopy 

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################

#install.packages('here', dependencies = T)
library(tidyverse)
library(here) # A helpful package for file management
library(viridis) # A pretty, colorblind-friendly set of palettes

# Turn off scientific notation
options(scipen = 999)

#######################################
####### NOTES ON BEST PRACTICE ########
#######################################

# 1. Use Projects rather than including rm(list=ls()): 
# * Why? 
#   * rm() doesn't actually ensure a truly clean workspace due to hidden items.
#   * Using a Project eliminates the need for local file directory paths, which will break on different computers.
#   * Allows the use of the here() package and function which ensures better cross-system compatability.
# * How?
#   * When starting a new project, use File > New Project the first time you create a new file.
#   * To ensure a clean workspace on startup, use RStudio > Preferences > General
#     * Uncheck "Restore .RData..."
#     * Set "Save Workspace to .RData on exit" to "never"
#     * Uncheck "Always save history..."
#   * Access Projects by doing any of the following:
#     * opening them directly from the Finder
#     * using File > Open Project
#     * using the Project drop-down in the upper-right of RStudio.
#
# 2. Be intentional in making graphs colorblind-accessible:
# * Why?
#   * The default ggplot palette is not strongly differentiated in value
#   * Non-colorblind-friendly palettes are not only inaccessible, but unclear when printed in black and white
# * How?
#   * Use premade palettes created with colorblindness in mind:
#     * A particluarly good one is Viridis, found and explained here: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#     * Another option is to use the following:
# Colorblind-friendly palette
# cbPalette <- c("#999999", # Dark Gray
#                "#E69F00", # Mustard Yellow
#                "#56B4E9", # Sky Blue
#                "#009E73", # Strong Green
#                "#F0E442", # Lemon Yellow
#                "#0072B2", # Denim Blue
#                "#D55E00", # Rust Orange
#                "#CC79A7") # Lavender
# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

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

csa_tree_temp_demographics %>%
  ggplot() +
  geom_point(aes(x = median_household_income/1000,
                 y = `09-63_mean`,
                 color = `09-63_mean`),
             size=4) +
  coord_flip() +
  scale_color_viridis(direction = -1) +
  labs(title = "Median Income to Percent Tree Canopy",
       subtitle = "In thousands of dollars",
       x = "",
       y = "") +
  scale_x_continuous(label = scales::dollar_format(),
                     breaks = seq(0, 150, 10)) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1.0),
                     breaks = seq(0, 1, .1)) + 
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))
  
# Save to file
ggsave(filename = "median-income-to-mean-tree-cover.png",
       device = "png", path = here("data", "output-data", "income-to-treecover"),
       width = 6, height = 6, units = "in")
