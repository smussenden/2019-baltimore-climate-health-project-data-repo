#### Maryland Hospital Data Cleaning Script ####
#### None of the data is stored publicly #####
#### Kept on air-gapped password protected computer #####
#### Limits on sharing data in numbers greater than 10 publicly ####
#### Original in SAS format ####

# Load Libraries #####
library('dplyr')
library('tidyr')
library('readr')
library('purrr')
library('stringr')
library('haven')
library('tibble')

#### Create List of Baltimore ZIP Codes #####
#### http://gis-baltimore.opendata.arcgis.com/datasets/zip-codes/data?orderBy=ZIPCODE1&page=4 Pulled out 21287, Hopkins zip ####

baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)

as.character(baltzips) -> baltzip

#### Read in IP and OP_ER master files #####

ip <- readRDS("modified_data/ip/ip.rds")
op_er <- readRDS("modified_data/op_er/op_er.rds")

#### Filter IP and OP_ER master files just for Baltimore ZIP code data ###

balt_ip <- subset(ip, ip$ZIPCODE %in% baltzips)
balt_op_er <- subset(op_er, op_er$ZIPCODE %in% baltzips)

#### Remove unneded files 
rm(list=ls()[! ls() %in% c("balt_op_er", "balt_ip")])
gc()

#### Write out 
saveRDS(balt_ip, file="modified_data/ip/balt_ip.rds")
saveRDS(balt_op_er, file="modified_data/op_er/balt_op_er.rds")
rm(list=ls()[! ls() %in% c("balt_op_er", "balt_ip")])
gc()

#### If starting again, read in IP and OP_ER master files #####
# balt_ip <- readRDS("modified_data/ip/balt_ip.rds")
# balt_op_er <- readRDS("modified_data/op_er/balt_op_er.rds")

# Define diagnosis code fields for function
diag_codes_ip <- c("row_unique_id","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","DIAG29")

diag_codes_op_er <- c("row_unique_id","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28")

# Define balt_op_er column names
balt_op_er_column_names <- intersect(colnames(balt_op_er),diag_codes_ip)

# Define balt_ip column names
balt_ip_column_names <- intersect(colnames(balt_ip),diag_codes_op_er)

##### Define global variables for saving files #####

# Define a function to create separate data frames for each condition.  Feed it the name of a dataframe (balt_op_er or balt_ip), type ("ip" or "op_er"), the names of the diagnosis columns from said data frame (balt_ip_column_names or balt_op_er_column_names) name of the condition (defined as a variable), icd9 codes for said condition, and icd10 codes for said condition

conditions_function <- function(df, type, column_names, condition, codes_9, codes_10) {
  
  # Create dataframe for ICD 9 codes
  temp1 <- df %>%
    filter(ICDFLAG == "9") %>%
    select(column_names) %>%
    filter_all(codes_9)
  
  # Create dataframe for ICD 10 codes
  temp2 <- df %>%
    filter(ICDFLAG == "0") %>%
    select(column_names) %>%
    filter_all(codes_10)
  
  # Bind together icd9 and icd10 dataframes to create full list of records with selected conditions
  temp3 <- bind_rows(temp1, temp2)
  
  # Join the dataframe of conditions back with full table.  This gives us all columns for all cases that have selected condition.
  temp4 <- df %>%
    semi_join(temp3, df,  by = 'row_unique_id')
  
  # Create file paths to store RDS files for each condition type, ip and op 
  filename_ip <- str_c("modified_data/ip/balt_ip_conditions/",condition,"_balt_ip.rds")  
  filename_op_er <- str_c("modified_data/op_er/balt_op_er_conditions/",condition,"_balt_op_er.rds")  
  # If else function to see which folder to store our output files in. 
  if (type == "ip") (
    filename <- filename_ip
  ) else (
    filename <- filename_op_er
  )
  
  # Save the file 
  saveRDS(temp4, file=filename)
  
  # Print
  print(paste0(condition," ", type, " total | icd9: ", nrow(temp1), " | icd10: ", nrow(temp2), " | both: ", nrow(temp3)))
  
  # Remove all but master files before looping through.  
  rm(list=ls()[! ls() %in% c("balt_op_er", "balt_ip", "balt_ip_column_names", "balt_op_er_column_names", "filename_ip", "filename_op_er", "conditions_function", "conditions","codes_9", "codes_10")])
  gc()
}


#############################################
###### CREATE FILES FOR EACH CONDITION ######
#############################################
# Note: eventually, we need to functionize this so we can loop through a dataframe with conditions.  But I was having a very hard time getting values from dataframe to pass correctly to the function, so fuck it, we're doing it the hard way. 

###### DIABETES ######
# CONFIRMED FOR 9 and 10
# Define condition name
condition <- "diabetes"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"249") | startsWith(.,"250")) 
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"E08") | startsWith(.,"E09") | startsWith(.,"E10") | startsWith(.,"E11") |  startsWith(.,"E12") | startsWith(.,"E13"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### THYROID ######
# CONFIRMED
# Define condition name
condition <- "thyroid"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"240") | startsWith(.,"241") | startsWith(.,"242") | startsWith(.,"243") | startsWith(.,"244") | startsWith(.,"245") | startsWith(.,"246"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"E00") | startsWith(.,"E01") | startsWith(.,"E02") | startsWith(.,"E03") | startsWith(.,"E04") | startsWith(.,"E05") | startsWith(.,"E06") | startsWith(.,"E07"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 


###### CIRCULATORY CONDITIONS ######
##### CIRCULATORY FOR ICD9 is MISSING 41, ICD10 is good
# Define condition name
condition <- "circulatory"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"39") | startsWith(.,"40") | startsWith(.,"42") | startsWith(.,"43") | startsWith(.,"44") | startsWith(.,"45"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### HEART DISEASE ######
### FOR ICD-9 HEART DISEASE IS NOT RIGHT, 4397 should be 397. 
### This is poorly defined.  Let's not use in final output


# Define condition name
condition <- "heart_disease"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"393") | startsWith(.,"394") | startsWith(.,"395") | startsWith(.,"396") | startsWith(.,"4397")| startsWith(.,"398") | startsWith(.,"399") | startsWith(.,"40") | startsWith(.,"41") | startsWith(.,"42"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I05") | startsWith(.,"I06") | startsWith(.,"I07") | startsWith(.,"I08") | startsWith(.,"I09") | startsWith(.,"I1") | startsWith(.,"I2") | startsWith(.,"I3") | startsWith(.,"I4") | startsWith(.,"I50") | startsWith(.,"I51") | startsWith(.,"I52"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### ANGINA PECTORIS ######
# CONFIRMED
# Define condition name
condition <- "angina_pectoris"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"413"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I20"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### CONGESTIVE ARTERY DISEASE ######
# DON'T USE THIS, NOT RIGHT. 428 is not congestive artery disease.  DO NOT USE IN FINAL OUTPUT

# Define condition name
condition <- "congestive_artery_disease"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"428"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I509"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### CEREBROVASCULAR DISEASE ######
# CONFIRMED
# Define condition name
condition <- "cerebrovascular_disease"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"43"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I6"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### HEART_ATTACK ######
# CONFIRMED
# Define condition name
condition <- "heart_attack"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"410"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I21"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### HYPERTENSION ######
# CONFIRMED
# Define condition name
condition <- "hypertension"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"401") | startsWith(.,"402") | startsWith(.,"403") | startsWith(.,"404") | startsWith(.,"405"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"I10") | startsWith(.,"I11") | startsWith(.,"I12") | startsWith(.,"I13") | startsWith(.,"I14") | startsWith(.,"I15") | startsWith(.,"I16"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### RESPIRATORY ######
# CONFIRMED
# Define condition name
condition <- "respiratory"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"46") | startsWith(.,"47") | startsWith(.,"48") | startsWith(.,"49") | startsWith(.,"50") | startsWith(.,"51"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### ASTHMA ######
# Confirmed
# Define condition name
condition <- "asthma"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"493"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J45"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 


###### COPD ######
# CONFIRMED - TOP LEVEL CATEGORY for CHRONIC LOWER RESP DISESEAS
# Define condition name
condition <- "copd"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"490") | startsWith(.,"491") | startsWith(.,"492") | startsWith(.,"493") | startsWith(.,"494") | startsWith(.,"495") | startsWith(.,"496"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J40") | startsWith(.,"J41") | startsWith(.,"J42") | startsWith(.,"J43") | startsWith(.,"J44") | startsWith(.,"J45") | startsWith(.,"J46") | startsWith(.,"J47"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 


###### BRONCHITIS ######
# THIS IS right, but don't use in final graphic, a little messy
# Define condition name
condition <- "bronchitis"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"466") | startsWith(.,"490") | startsWith(.,"491"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J20") | startsWith(.,"J21") | startsWith(.,"J40") | startsWith(.,"J41") | startsWith(.,"J42"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### ACUTEBRONCHITIS ######
# Confirmed, but don't use in final graphic.
# Define condition name
condition <- "acute_bronchitis"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"466"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J20"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### INFLUENZA ######
# Confirmed
# Define condition name
condition <- "influenza"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"487") | startsWith(.,"488"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"J09") | startsWith(.,"J10") | startsWith(.,"J11"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 


###### MENTAL ILLNESS ######
# Confirmed, but too broad, don't use in final graphics
# Define condition name
condition <- "mental_illness"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"29") | startsWith(.,"30") | startsWith(.,"31"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"F"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### KIDNEY DISEASE ######
# Crosswalk here is problematic. Don't use in final reporting.
# Define condition name
condition <- "kidney_disease"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"584") | startsWith(.,"585") | startsWith(.,"586"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"N17") | startsWith(.,"N18") | startsWith(.,"N19"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### OSTEOARTHRITIS ######
# Good. 
# Define condition name
condition <- "osteoarthritis"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"715"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"M15") | startsWith(.,"M16") | startsWith(.,"M17") | startsWith(.,"M18") | startsWith(.,"M19"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### HYPOTHERMIA ###### 
# Confirmed
# Define condition name
condition <- "hypothermia"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"9916"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T68"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10) 

###### ALL EFFECTS HEAT ###### 
# Good
# Define condition name
condition <- "all_effects_heat"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"992"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T67"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)

###### HEATSTROKE ###### 
# Good, but don't use in final graphic
# Define condition name
condition <- "heatstroke"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"9920"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T670"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)

###### CARBON MONOXIDE POISONING ###### 
# Good.
# Define condition name
condition <- "carbon_monoxide"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"986"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T58"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)

###### HEAT CRAMPS ###### 
# Good but don't use
# Define condition name
condition <- "heat_cramps"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"9922"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T672"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)


###### HEAT SYNCOPE ###### 
# Good but don't use
# Define condition name
condition <- "heat_syncope"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"9921"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T671"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)

###### HEAT EXHAUSTION ###### 
# Good but don't use
# Define condition name
condition <- "heat_exhaustion"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"9923") | startsWith(.,"9924") | startsWith(.,"9925"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"T673") | startsWith(.,"T674") | startsWith(.,"T675"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)

###### URTICARIA ###### 
# Good but don't use
# Define condition name
condition <- "urticaria"
# Define ICD 9 codes for condition
codes_9 <- any_vars(startsWith(.,"708"))
# Define ICD10 codes for condition
codes_10 <- any_vars(startsWith(.,"L50"))
# Run function to output RDS file of IP cases with condition
conditions_function(balt_ip, "ip", balt_ip_column_names, condition, codes_9, codes_10) 
# Run function to output RDS file of OP_ER cases with condition
conditions_function(balt_op_er, "op_er", balt_op_er_column_names, condition, codes_9, codes_10)


#### From here, go to create_large_files.R