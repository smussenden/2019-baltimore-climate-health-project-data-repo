##################################################
#### Hospital Analysis ###########################
##################################################

########################
#### Load Packages #####
########################

library(tidyverse)
library(corrr)

# for debug
rm(list=ls())


#################################
##### Load Hospital Data ########
#################################

ip_full_zip <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/ip/ip_full_zip_demographics_heat.csv")
ip_full_zip_medicaid <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/ip/ip_full_medicaid_demographics_heat.csv")
ip_full_zip_qtr <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/ip/ip_full_qtr_demographics_heat.csv")
ip_full_zip_race <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/ip/ip_full_race_demographics_heat.csv")
ip_full_zip_age <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/ip/ip_full_age_demographics_heat.csv")

op_er_full_zip <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/op_er/op_er_full_zip_demographics_heat.csv")
op_er_full_zip_medicaid <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/op_er/op_er_full_medicaid_demographics_heat.csv")
op_er_full_zip_qtr <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/op_er/op_er_full_qtr_demographics_heat.csv")
op_er_full_zip_race <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/op_er/op_er_full_race_demographics_heat.csv")
op_er_full_zip_age <- read_csv("data/output-data/hscrc-hospital-data/data_by_zip/op_er/op_er_full_age_demographics_heat.csv")


ER VISITS

ASTHMA
COPD
HEART DISEASE
DIABETES
KIDNEY DISEASE

Last summer in Baltimore, when the heat index hit 103 degrees, EMS calls increased citywide for asthma, COPD and cardiovascular conditions.



For the last five years in Baltimore, low-income people in the hottest parts of the city have visited the hospital at higher rates for those same conditions, when compared to low-income people in the cooler parts of town.


###################################################################
######## Load in unclipped tree heat temp demographic info ########
###################################################################

zcta_not_clipped_balt_city_border_tree_temp_demographics <- read_csv("data/output-data/cleaned/tree-temp-demographic-w-naip-only-use-with-caution/zcta_not_clipped_balt_city_border_tree_temp_demographics.csv")

zcta_not_clipped_balt_city_border_tree_temp_demographics <- zcta_not_clipped_balt_city_border_tree_temp_demographics %>%
  select(-matches('10'), -starts_with('t_')) %>%
  mutate(zcta = as.character(zcta)) %>%
  filter(zcta != 'CITYWIDE')



#################################################################
########## Define the function to build correlation matrix(s) ###
#################################################################

make_correlation_matrix <- function(dataframe, dataframe_name, visit_type) {
  
  dataframe <- dataframe %>%
    select(matches('zcta'), 
           matches('all_effects_heat'),         
           matches('kidney_disease'),             
           matches('copd'),                     
           matches('asthma'),                    
           matches('respiratory'),              
           matches('hypertension'),              
           matches('heart_attack'),             
           matches('congestive_artery_disease'),
           matches('angina_pectoris'),           
           matches('heart_disease'),            
           matches('circulatory'),                                 
           matches('diabetes'),
           -matches('_percap')
    )
  

  #Merge dataframe with demographic/heat information
  merge_demographics_heat_tree <- zcta_not_clipped_balt_city_border_tree_temp_demographics %>%
    left_join(dataframe, by = 'zcta')

  # Build CSV filepath before writing out to CSV  
  filename <- paste0("data/output-data/hscrc-hospital-data/output_data_by_zip/", visit_type, "/", dataframe_name,"_demographics_heatx.csv")
  
  # Write to a csv for later use
  write_csv(merge_demographics_heat_tree, path=filename)
  
  #Create the correlation matrix. Remove conditions with na values.
  correlation_matrix <- merge_demographics_heat_tree %>%
    select(-zcta) %>%
    as.matrix %>%
    correlate() %>%
    focus(-matches("_prev")) %>%
    select(matches("rowname|_%$|_f$|_d$|_a$|^temp_|_mean$")) %>%
    filter(complete.cases(.))
  print(correlation_matrix)
    
  # Build CSV filepath before writing out to CSV  
  filename <- paste0("data/output-data/hscrc-hospital-data/output_correlation_matrix/", visit_type, "/", dataframe_name,"_correlationsx.csv")
  
  # Write to a csv for later use
  write_csv(correlation_matrix, path=filename)

  # Convert the matrix table into a "long" file so that we can plot it.
  correlation_matrix_long <- correlation_matrix %>%
    stretch() %>%
    select(y,x,r) %>%
    arrange(y)
  
  # Create a heatmap table showing color-coded correlation strength, after first creating chart title.
  
  # Figure out how to build this itteratively
  chart_title <- paste0("Correlations between Health Conditions and (Heat, Demographics, Tree Canopy) \n by Zip Code in Baltimore City" , visit_type, " | ", dataframe_name)
  
  ggplot(data = correlation_matrix_long, aes(x = x, y = y)) +
    geom_tile(aes(fill = r)) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint=0) +
    geom_text(aes(label = round(r, 2)*100), size = 2) +
    ggtitle(chart_title) +
    xlab("neighborhood factors") +
    ylab("conditions") +
    theme(axis.text.x = element_text(angle=50,hjust=1),
          plot.title = element_text(),
          axis.title.x = element_text(),
          axis.title.y = element_text()
    )
  # Create filename and filepath to save image. 
  filename <- paste0(dataframe_name, "_correlationsx.png")
  filepath <- paste0("data/output-data/hscrc-hospital-data/output_correlation_matrix_images/", visit_type)
  
  # Save image.
  ggsave(filename, plot = last_plot(), device = "png", path = filepath, scale = 1, width = 8, height = 11, units = "in", dpi = 300)
  
  # Remove temporary dataframes
  # rm(merge_demographics_heat, envir = .GlobalEnv)
}

make_correlation_matrix(ip_full_zip, "ip_full_zip", "ip")

#################################################################
########## Run IP Correlation Matrix ############################
#################################################################
make_correlation_matrix(ip_full_zip, "ip_full_zip", "ip")
make_correlation_matrix(ip_full_zip_medicaid, "ip_full_zip_medicaid", "ip")
make_correlation_matrix(ip_full_zip_qtr, "ip_full_zip_qtr", "ip")
make_correlation_matrix(ip_full_zip_race, "ip_full_zip_race", "ip")
make_correlation_matrix(ip_full_zip_age, "ip_full_zip_age", "ip")


#################################################################
########## Run OP_ER Correlation Matrix ############################
#################################################################

make_correlation_matrix(op_er_full_zip, "op_er_full_zip", "op_er")
make_correlation_matrix(op_er_full_zip_medicaid, "op_er_full_zip_medicaid", "op_er")
make_correlation_matrix(op_er_full_zip_qtr, "op_er_full_zip_qtr", "op_er")
make_correlation_matrix(op_er_full_zip_race, "op_er_full_zip_race", "op_er")
make_correlation_matrix(op_er_full_zip_age, "op_er_full_zip_age", "op_er")









################################################
##### Clean Hospital Data ######################
################################################

ip_full_zip_medicaid <- ip_full_zip_medicaid %>%
  select(matches('zcta'), 
         matches('all_effects_heat'),         
         matches('kidney_disease'),             
         matches('copd'),                     
         matches('asthma'),                    
         matches('respiratory'),              
         matches('hypertension'),              
         matches('heart_attack'),             
         matches('congestive_artery_disease'),
         matches('angina_pectoris'),           
         matches('heart_disease'),            
         matches('circulatory'),                                 
         matches('diabetes'),
         -matches('_percap')
  )


#####################################
#### OTHER #####
#####################################

# Poverty explains these thigs better than anything else
merge_demographics_heat_tree <- zcta_not_clipped_balt_city_border_tree_temp_demographics %>%
  left_join(ip_full_zip, by = 'zcta')

glimpse(merge_demographics_heat_tree)

# Multiple Linear Regression Example 
merge_demographics_heat_tree <- merge_demographics_heat_tree %>%
  select(-zcta, -matches("127|63|multirace|count|percap|population"))
fit <- lm(medicaid_asthma_prev ~ poverty, data=merge_demographics_heat_tree)
summary(fit) # show results

fit <- lm(`poverty_%.x` ~ asthma_prev, data=merge_demographics_heat_tree)
summary(fit) # show results
https://www.datacamp.com/community/tutorials/linear-regression-R