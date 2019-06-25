#### Analysis of ZIP Code Tabulation Areas
#### 

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("Hmisc")
library(tidyverse)
library(corrr)
library(magrittr)

#################################################################
######## Load Data Produced in Cleaning.r Script File ###########
#################################################################

zcta_tree_temp_demographics <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-only-use-with-caution/zcta_not_clipped_balt_city_border_tree_temp_demographics.csv")  
zcta_tree_temp_demographics <- zcta_tree_temp_demographics %<>% 
  mutate(zcta = as.character(zcta)) %>%
  select(-matches("t_a"), -matches("t_pm"))
  

#################################################################
########## Define Functions #####################################
#################################################################

# Run functions.R
source("scripts/geography-based-analyses/tree-temp-demographics/functions.R")

# Select computable values within *this particular* df
select_x <- function(df){
  return(df %>%
           select_if(is.numeric)
         )
}

# Cleanup
cleanup <- function() {
  rm(list=setdiff(ls(pos = 1), 
                  c("make_correlation_matrix_graphic", 
                    "write_matrix_csv", 
                    "zcta_tree_temp_demographics", 
                    "select_x", 
                    "cleanup")
                  ),
     pos = 1
     )
}

#################################################################
########## Build Correlation Matrices, Graphics #################
#################################################################

##################################
### Heat vs demographics #########
##################################
# Build correlation matrix
heat_vs_demographics_unclipped_zcta_correlation_matrix <- zcta_tree_temp_demographics %>%
  select_x() %>%
  select(-matches("17-"), -matches("09-"), -matches("change")) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("temp_")) %>%
  mutate(variable=rowname) %>%
  select(variable, everything(), -rowname) %>%
  filter(variable != "percent_of_area_covered_by_trees")

# Write it out to a csv for later use
write_matrix_csv(heat_vs_demographics_unclipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
heat_vs_demographics_unclipped_zcta_correlation_matrix_long <- heat_vs_demographics_unclipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:13) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(heat_vs_demographics_unclipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()

# # Analyzing data. 
# 
# working <- zcta_tree_temp_demographics %>%
#   select(zcta2010, temp_median_aft, matches("vacant"))
# 
# ### Confirming
# 
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# res2<-rcorr(as.matrix(zcta_tree_temp_demographics[,3:136]))
# test <- flattenCorrMatrix(res2$r, res2$P)
# test <- test %>%
#   filter(p < .05)
# 
# cor.test(zcta_tree_temp_demographics$temp_mean_aft, zcta_tree_temp_demographics$percent_of_residents_black_african_american_non_hispanic)
# cor.test(zcta_tree_temp_demographics$temp_mean_aft, zcta_tree_temp_demographics$walk_score)
# 
# library(Hmisc) # You need to download it first.
# test <- as.matrix(zcta_tree_temp_demographics)
# test <- complete.cases(test)
# new <- rcorr(zcta_tree_temp_demographics$temp_mean_am, zcta_tree_temp_demographics$temp_median_am, type="pearson")
# rcorr(zcta_tree_temp_demographics$temp_mean_aft, zcta_tree_temp_demographics$walk_score)
# baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)


##################################
### Trees vs demographics ########
##################################
# Build correlation matrix
tree_vs_demographics_unclipped_zcta_correlation_matrix <- zcta_tree_temp_demographics %>%
  select_x() %>%
  select(-matches("temp_")) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("17-"), matches("09-"), matches("change")) %>%
  mutate(variable=rowname) %>%
  select(variable, everything(), -rowname)

# Write it out to a csv for later use
write_matrix_csv(tree_vs_demographics_unclipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_demographics_unclipped_zcta_correlation_matrix_long <- tree_vs_demographics_unclipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:9) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_demographics_unclipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()


##################################
### Trees vs heat ################
##################################
        
# Build correlation matrix
tree_vs_heat_unclipped_zcta_correlation_matrix <- zcta_tree_temp_demographics %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("17"), matches("09"), matches("change")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "^temp_")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(tree_vs_heat_unclipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_heat_unclipped_zcta_correlation_matrix_long <- tree_vs_heat_unclipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:10) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_heat_unclipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()

##################################
### Tree cover vs tree change ####
##################################

# Build correlation matrix
treecover_vs_coverchange_unclipped_zcta_correlation_matrix <- zcta_tree_temp_demographics %>%
  select_x() %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("17"), matches ("09")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "change")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(treecover_vs_coverchange_unclipped_zcta_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
treecover_vs_coverchange_unclipped_zcta_correlation_matrix_long <- treecover_vs_coverchange_unclipped_zcta_correlation_matrix %>%
  gather("variable_2", "value", 2:6) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(treecover_vs_coverchange_unclipped_zcta_correlation_matrix_long)

# Remove all but master file and functions
cleanup()
 