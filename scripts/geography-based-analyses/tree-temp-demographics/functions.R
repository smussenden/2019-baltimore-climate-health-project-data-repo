## Store functions

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")
library(tidyverse)
library(corrr)


####################################
######## Define Functions ##########
####################################

# Function to save each matrix as CSV
write_matrix_csv <- function(dataframe) {
  # Store dataframe name for later use
  dataframe_name <- deparse(substitute(dataframe))
  
  # Create filename for csv
  filename <- paste0("data/output-data/correlation_matrices/", dataframe_name,".csv")
  
  # Write out csv  
  write_csv(dataframe, path = filename)
  
} 

# Function to make a nice little correlation matrix heatmap for each graphic

make_correlation_matrix_graphic <- function(dataframe, grouping = "GROUPING") {
  
  # Store name of dataframe for use in title
  dataframe_name <- deparse(substitute(dataframe))
  
  # Build chart title
  chart_title <- paste0("Correlations by ", grouping, " in Baltimore City | ", dataframe_name )
  
  # Create graph
  ggplot(data = dataframe, aes(x = variable_2, y = variable)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint=0) +
    geom_text(aes(label = round(value, 2)*100), size = 10) +
    ggtitle(chart_title) +
    theme(axis.text=element_text(size=14),
          axis.text.x = element_text(size=14,angle=50,hjust=1),
          plot.title = element_text(size=14)
    )
  # Create filename and filepath to save image. 
  filename <- paste0(dataframe_name,".png")
  ggsave(filename, plot = last_plot(), device = "png", path = "data/output-data/plots/correlation-matrix-images", scale = 1, width = 20, height = 20, units = "in", dpi = 300)
  
}  
