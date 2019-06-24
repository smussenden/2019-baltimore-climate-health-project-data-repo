#### Analysis of Community Statistical Areas
#### Rename this. CSA Analysis

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
## install.packages("Hmisc")
library(tidyverse)
library(corrr)
library(Hmisc)

### For debugging
rm(list=ls())

#################################################################
######## Load Data Produced in Cleaning.r Script File ###########
#################################################################

csa_tree_temp_demographics <- read_csv("output/data/cleaned/csa_tree_temp_demographics.csv")

#################################################################
########## Define Functions #####################################
#################################################################

# Function to save each matrix as CSV
write_matrix_csv <- function(dataframe) {
   # Store dataframe name for later use
   dataframe_name <- deparse(substitute(dataframe))
   
   # Create filename for csv
    filename <- paste0("output/data/correlation_matrices/", dataframe_name,".csv")
   
   # Write out csv  
    write_csv(dataframe, path = filename)
  
} 

# Function to make a nice little correlation matrix heatmap for each graphic

make_correlation_matrix_graphic <- function(dataframe) {
  
  # Store name of dataframe for use in title
  dataframe_name <- deparse(substitute(dataframe))
  
  # Build chart title
  chart_title <- paste0("Correlations by CSA (Neighborhood Group) in Baltimore City | ", dataframe_name )
  
  # Create graph
  ggplot(data = dataframe, aes(x = variable_2, y = variable)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint=0) +
    geom_text(aes(label = round(value, 2)*100), size = 2) +
    ggtitle(chart_title) +
    theme(axis.text.x = element_text(angle=50,hjust=1),
          plot.title = element_text(),
          axis.title.x = element_text(),
          axis.title.y = element_text()
    )
  # Create filename and filepath to save image. 
  filename <- paste0(dataframe_name,".png")
  ggsave(filename, plot = last_plot(), device = "png", path = "output/plots/correlation_matrix_images", scale = 1, width = 20, height = 20, units = "in", dpi = 300)
  
}  

# Function to flatten correlation matrix that shows p values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#################################################################
########## Build Correlation Matrices, Graphics #################
#################################################################

#########################################################################
### Build a table to check for statistical significance (p <.05) ########
#########################################################################

# Create a list that shows all correlations, including r and p
csa_tree_temp_demographics_p_r_all <- rcorr(as.matrix(csa_tree_temp_demographics[,3:136]))

test <- as.matrix(csa_tree_temp_demographics[,3:136])

# Flatten it, using function
csa_tree_temp_demographics_p_r_all_flat <- flattenCorrMatrix(csa_tree_temp_demographics_p_r_all$r, csa_tree_temp_demographics_p_r_all$P)

# Calculate R2 and rename varialbes
csa_tree_temp_demographics_p_r_all_flat <- csa_tree_temp_demographics_p_r_all_flat %>%
  mutate(r2=cor*cor,
         r=cor,
         variable_1=row,
         variable_2=column) %>%
  select(variable_1, variable_2, p, r, r2)

# Filter out insignificant results
csa_tree_temp_demographics_p_r_all_flat <- csa_tree_temp_demographics_p_r_all_flat %>%
  filter(p < .05) %>%
  filter(variable_1 == "temp_median_aft")
http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r


############################
### Heat vs demographics ###
############################

# Build correlation matrix
heat_vs_demographics_csa_correlation_matrix <- csa_tree_temp_demographics %>%
  select(-OBJECTID, -CSA2010, -id, -matches("09"), -matches("1718"), -matches("change_percent")) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("temp_")) %>%
  mutate(variable=rowname) %>%
  select(variable, everything(), -rowname) %>%
  filter(variable != "percent_of_area_covered_by_trees")

# Write it out to a csv for later use
write_matrix_csv(heat_vs_demographics_csa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
heat_vs_demographics_csa_correlation_matrix_long <- heat_vs_demographics_csa_correlation_matrix %>%
  gather("variable_2", "value", 2:13) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(heat_vs_demographics_csa_correlation_matrix_long)

# Remove all but master file and functions
rm(list=setdiff(ls(), c("make_correlation_matrix_graphic", "write_matrix_csv", "csa_tree_temp_demographics")))

# Analyzing data. 

working <- csa_tree_temp_demographics %>%
  select(CSA2010, temp_median_aft, matches("vacant"))

### Confirming

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<-rcorr(as.matrix(csa_tree_temp_demographics[,3:136]))
test <- flattenCorrMatrix(res2$r, res2$P)
test <- test %>%
  filter(p < .05)

cor.test(csa_tree_temp_demographics$temp_mean_aft, csa_tree_temp_demographics$percent_of_residents_black_african_american_non_hispanic)
cor.test(csa_tree_temp_demographics$temp_mean_aft, csa_tree_temp_demographics$walk_score)

library(Hmisc) # You need to download it first.
test <- as.matrix(csa_tree_temp_demographics)
test <- complete.cases(test)
new <- rcorr(csa_tree_temp_demographics$temp_mean_am, csa_tree_temp_demographics$temp_median_am, type="pearson")
rcorr(csa_tree_temp_demographics$temp_mean_aft, csa_tree_temp_demographics$walk_score)
baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)

############################
### Trees vs demographics ##
############################

# Build correlation matrix
tree_vs_demographics_csa_correlation_matrix <- csa_tree_temp_demographics %>%
  select(-OBJECTID, -CSA2010, -id, -matches("temp_")) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("1718"), matches("09"), matches("change_percent")) %>%
  mutate(variable=rowname) %>%
  select(variable, everything(), -rowname)

# Write it out to a csv for later use
write_matrix_csv(tree_vs_demographics_csa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_demographics_csa_correlation_matrix_long <- tree_vs_demographics_csa_correlation_matrix %>%
  gather("variable_2", "value", 2:9) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_demographics_csa_correlation_matrix_long)

# Remove all but master file and functions
rm(list=setdiff(ls(), c("make_correlation_matrix_graphic", "write_matrix_csv", "csa_tree_temp_demographics")))

############################
### Trees vs heat ##########
############################
        
# Build correlation matrix
tree_vs_heat_csa_correlation_matrix <- csa_tree_temp_demographics %>%
  select(-OBJECTID, -CSA2010, -id) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("1718"), matches("09"), matches("change_percent")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "^temp_")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(tree_vs_heat_csa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
tree_vs_heat_csa_correlation_matrix_long <- tree_vs_heat_csa_correlation_matrix %>%
  gather("variable_2", "value", 2:9) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(tree_vs_heat_csa_correlation_matrix_long)

# Remove all but master file and functions
rm(list=setdiff(ls(), c("make_correlation_matrix_graphic", "write_matrix_csv", "csa_tree_temp_demographics")))

########################################
### Tree cover vs tree change ##########
########################################

# Build correlation matrix
treecover_vs_coverchange_csa_correlation_matrix <- csa_tree_temp_demographics %>%
  select(-OBJECTID, -CSA2010, -id) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("1718"), matches ("09")) %>%
  mutate(variable=rowname) %>%
  filter(str_detect(variable, "change_percent")) %>%
  select(variable, everything(), -rowname) 

# Write it out to a csv for later use
write_matrix_csv(treecover_vs_coverchange_csa_correlation_matrix)

# Make correlation long instead of wide so it can be passed to ggplot correctly. 
treecover_vs_coverchange_csa_correlation_matrix_long <- treecover_vs_coverchange_csa_correlation_matrix %>%
  gather("variable_2", "value", 2:5) %>%
  arrange(desc(value))

# Build graphic
make_correlation_matrix_graphic(treecover_vs_coverchange_csa_correlation_matrix_long)

# Remove all but master file and functions
rm(list=setdiff(ls(), c("make_correlation_matrix_graphic", "write_matrix_csv", "csa_tree_temp_demographics")))

