#################################################################
##### Analysis of Health Conditions in Baltimore ZIP Codes ######
#################################################################

#### NEED TO FUNCTIONIZE THIS

# This script file exists to look for relationships between features of Baltimore ZIP Codes and the prevelance of health conditions that medical research has shown to be exacerbated by heat and cold.

# From the census, we obtained the features of zip codes include race, age, income, poverty, house prices, single family homes. This was done in a separate file, and it needs to be merged with this repo.

# From an Urban Heat Islands data study, we calculated the median, min, max and mean temperature by Baltimore Zip Code. This was done in a separate repo, and needs to be merged into this repo.

# Using HSCRC hosptial visit data, we calculated the prevalance of more than a dozen conditions exacerbated by heat and/or cold by Zip Code. This was done on an air-gapped computer on which the hospital data was stored, so we can't merge that with this repo, only the output files with prevalance by Zip Code.  Also, anything under 10 in counts by condition by zip has been scrubbed.

# For debugging rm(list=ls())
# gc()

#################################################################
######## Install necessary packages and load libraries ##########
#################################################################
## install.packages('tidyverse')
## install.packages("corrr")

library(tidyverse)
library(corrr)

#################################################################
########## Load in Disease Prevalance by Zip Code Data ##########
#################################################################

### This is the HSCRC Data.
### Put fuller description here of what it is.
### In these data sets, one record per zip code.

### ip_full_zip. Master Data Set, not subdivided by anything other than zip code
ip_full_zip <- read_csv("data/input_data/ip/ip_full_zip.csv")
### ip_full_year. Same as ip_full_zip, but with crosstabs by year ###
ip_full_year <- read_csv("data/input_data/ip/ip_full_year.csv")
### ip_full_qtr. Same as ip_full_zip, but with crosstabs by quarter, which is a proxy for season  ###
ip_full_qtr <- read_csv("data/input_data/ip/ip_full_qtr.csv")
### ip_full_race. Same as ip_full_zip, but with crosstabs for black and white (from health data)  ###
ip_full_race <- read_csv("data/input_data/ip/ip_full_race.csv")
### ip_full_medicaid. Same as ip_full_zip, but with crosstabs for medicaid and not medicaid ### NEED TO GO BACK AND DO THIS.
ip_full_medicaid <- read_csv("data/input_data/ip/ip_full_medicaid.csv")
### ip_full_age. Same as ip_full_zip, but with crosstabs for age group###
ip_full_age <- read_csv("data/input_data/ip/ip_full_age.csv")
### op_er_full_zip. Master Data Set, not subdivided by anything other than zop_er code
op_er_full_zip <- read_csv("data/input_data/op_er/op_er_full_zip.csv")
### op_er_full_year. Same as op_er_full_zip, but with crosstabs by year ###
op_er_full_year <- read_csv("data/input_data/op_er/op_er_full_year.csv")
### op_er_full_qtr. Same as op_er_full_zip, but with crosstabs by quarter, which is a proxy for season  ###
op_er_full_qtr <- read_csv("data/input_data/op_er/op_er_full_qtr.csv")
### op_er_full_race. Same as op_er_full_zip, but with crosstabs for black and white (from health data)  ###
op_er_full_race <- read_csv("data/input_data/op_er/op_er_full_race.csv")
### op_er_full_medicaid. Same as op_er_full_zip, but with crosstabs for medicaid and not medicaid ### NEED TO GO BACK AND DO THIS.
op_er_full_medicaid <- read_csv("data/input_data/op_er/op_er_full_medicaid.csv")
### op_er_full_age. Same as op_er_full_zip, but with crosstabs for age group###
op_er_full_age <- read_csv("data/input_data/op_er/op_er_full_age.csv")


#################################################################
### Load in Temperature and Demographics by Zip Code Data #######
#################################################################

### Read in the data
demographic_heat_data <- read_csv("data/input_data/temp/baltimore-data-all.csv")

### Create a function to convert celcius values to fahrenheit
c_to_f_convert <- function(x) (x * (9/5) + 32)

### Create percentages calculations for neighborhoods, and adjust temperature to fahrenheit

demographic_heat_data <- demographic_heat_data %>%
  mutate_at(vars(contains("heat")), c_to_f_convert) %>%
  mutate("white_%" = (`population of white alone`/`total population`)*100) %>%
  mutate("black_%" = (`population of black or african american alone`/`total population`)*100) %>%
  mutate("otherrace_or_multirace_%" = (100 - `white_%` - `black_%`)) %>%
  ## mutate("racecheck_should_total_100" = (`white_%` + `black_%` + `otherrace_or_multirace_%` )) %>%
  mutate("poverty_%" = ((`poverty population`/`poverty population determined`)*100 )) %>%
  mutate("single_family_detached_houses_%" = ((`number of units - 1, detached`/`total structures`)*100 )) %>%
  mutate("65+_%" = ((`population 65+`/`total population`)*100 )) %>%
  mutate("0-17_%" = ((`population 0-17`/`total population`)*100 )) %>%
  mutate("disabled_%" = ((`disabled population`/`total population`)*100 )) %>%
  mutate("renter_%" = ((`renter occupied household`/(`renter occupied household` + `owner occupied household`)*100 ))) %>%
  mutate("vacancy_%" = ((`total vacancies`/`total structures`)*100 )) %>%
  mutate("home_median_price_d" = `home median price`) %>%
  mutate("median_household_income_d" = `median household income`) %>%
  mutate("median_age_a" = `median age`) %>%
  mutate("heat_mean_f" = `heat mean`) %>%
  mutate("heat_median_f" = `heat median`) %>%
  mutate("heat_min_f" = `heat min`) %>%
  mutate("heat_max_f" = `heat max`) %>%
  select(zcta,`white_%`, `black_%`, `otherrace_or_multirace_%`, `poverty_%`,`single_family_detached_houses_%`, `65+_%`,`0-17_%`,`disabled_%`, `renter_%`, `vacancy_%`, `home_median_price_d`,`median_household_income_d`, `median_age_a`, `heat_mean_f`, `heat_median_f`, `heat_min_f`, `heat_max_f`)

#################################################################
########## Define the function to build correlation matrix(s) ###
#################################################################

make_correlation_matrix <- function(visit_type, dataframe) {
  
  #Merge dataframe with demographic/heat information
  merge_demographics_heat <- demographic_heat_data %>%
    left_join(dataframe, by = c("zcta" = "ZIPCODE"))
  
  # Store the name of the dataframe for later use. 
  dataframe_name <- deparse(substitute(dataframe))

  # Build CSV filepath before writing out to CSV  
  filename <- paste0("data/output_data/data_by_zip/", visit_type, "/", dataframe_name,"_demographics_heat.csv")
  
  # Write to a csv for later use
  write_csv(merge_demographics_heat, path=filename)
  
  # Load the merged_demographics_file into the environment 
  assign(paste0(dataframe_name, "_demographics_heat"), merge_demographics_heat, envir = parent.frame())

  #Create the correlation matrix. Remove conditions with na values.
  correlation_matrix <- merge_demographics_heat %>%
    filter(zcta != 21251) %>%
    select(-zcta) %>%
    as.matrix %>%
    correlate() %>%
    focus(-matches("_prev")) %>%
    select(matches("rowname|_%$|_f$|_d$|_a$")) %>%
    filter(complete.cases(.))
  
  # Build CSV filepath before writing out to CSV  
  filename <- paste0("data/output_data/correlation_matrix/", visit_type, "/", dataframe_name,"_correlations.csv")
  
  # Write to a csv for later use
  write_csv(correlation_matrix, path=filename)
  
  # Load the merged_demographics_file into the environment 
  assign(paste0(dataframe_name, "_correlation_matrix"), correlation_matrix, envir = parent.frame())
  
  # Convert the matrix table into a "long" file so that we can plot it.
  correlation_matrix_long <- correlation_matrix %>%
    stretch() %>%
    select(y,x,r) %>%
    arrange(y)
  
  # Create a heatmap table showing color-coded correlation strength, after first creating chart title.
  
  chart_title <- paste0("Correlations between Health Conditions and Demographics \n by Zip Code in Baltimore City ", visit_type," | ", dataframe_name)
  
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
  filename <- paste0(dataframe_name,"_correlations.png")
  filepath <- paste0("images/correlation_matrix_images/", visit_type)
  
  # Save image.
  ggsave(filename, plot = last_plot(), device = "png", path = filepath, scale = 1, width = 8, height = 11, units = "in", dpi = 300)
  
  # Remove temporary dataframes
  # rm(merge_demographics_heat, envir = .GlobalEnv)
  

}

#################################################################
########## Run IP Correlation Matrix ############################
#################################################################

make_correlation_matrix("ip", ip_full_zip)
make_correlation_matrix("ip", ip_full_age)
make_correlation_matrix("ip", ip_full_medicaid)
make_correlation_matrix("ip", ip_full_qtr)
make_correlation_matrix("ip", ip_full_race)
make_correlation_matrix("ip", ip_full_year)

#################################################################
########## Run OP_ER Correlation Matrix ############################
#################################################################

make_correlation_matrix("op_er", op_er_full_zip)
make_correlation_matrix("op_er", op_er_full_age)
make_correlation_matrix("op_er", op_er_full_medicaid)
make_correlation_matrix("op_er", op_er_full_qtr)
make_correlation_matrix("op_er", op_er_full_race)
make_correlation_matrix("op_er", op_er_full_year)

# Remove unneeded files
rm(ip_full_zip, ip_full_age, ip_full_medicaid, ip_full_qtr,ip_full_race,ip_full_year, op_er_full_zip, op_er_full_age, op_er_full_medicaid, op_er_full_qtr,op_er_full_race,op_er_full_year, demographic_heat_data)


#################################################################
########## AGE EXAMINATION ######################################
#################################################################

View(head(ip_full_age_demographics_heat))

ipfadh_asthma <- ip_full_age_demographics_heat %>%
  select(zcta, `white_%`, `black_%`, `poverty_%`,`65+_%`,`0-17_%`, contains("asthma_prev"))
View(ipfadh_asthma)

operfadh_asthma <- op_er_full_age_demographics_heat %>%
  select(zcta, `white_%`, `black_%`, `poverty_%`,`65+_%`,`0-17_%`, contains("asthma_prev"))
View(operfadh_asthma)