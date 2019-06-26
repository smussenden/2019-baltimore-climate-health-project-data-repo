#### Maryland Hospital Data Cleaning Script ####
#### None of the data is stored publicly #####
#### Kept on air-gapped password protected computer #####
#### Limits on sharing data in numbers greater than 10 publicly ####
#### Original in SAS format ####
#### Previous file is subset.r

library('dplyr')
library('haven')
library('tidyr')
library('readr')
library('purrr')
library('stringr')
library('haven')

#### If starting again, read in IP and OP_ER master files #####
balt_ip <- readRDS("modified_data/ip/balt_ip.rds")
balt_op_er <- readRDS("modified_data/op_er/balt_op_er.rds")

### Load OP_ER Condition Data ####

urticaria_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/urticaria_balt_op_er.rds")
heat_exhaustion_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heat_exhaustion_balt_op_er.rds")
heat_syncope_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heat_syncope_balt_op_er.rds")
heat_cramps_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heat_cramps_balt_op_er.rds")
carbon_monoxide_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/carbon_monoxide_balt_op_er.rds")
heatstroke_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heatstroke_balt_op_er.rds")
all_effects_heat_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/all_effects_heat_balt_op_er.rds")
hypothermia_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/hypothermia_balt_op_er.rds")
osteoarthritis_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/osteoarthritis_balt_op_er.rds")
kidney_disease_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/kidney_disease_balt_op_er.rds")
mental_illness_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/mental_illness_balt_op_er.rds")
influenza_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/influenza_balt_op_er.rds")
acute_bronchitis_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/acute_bronchitis_balt_op_er.rds")
bronchitis_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/bronchitis_balt_op_er.rds")
copd_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/copd_balt_op_er.rds")
asthma_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/asthma_balt_op_er.rds")
respiratory_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/respiratory_balt_op_er.rds")
hypertension_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/hypertension_balt_op_er.rds")
heart_attack_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heart_attack_balt_op_er.rds")
cerebrovascular_disease_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/cerebrovascular_disease_balt_op_er.rds")
congestive_artery_disease_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/congestive_artery_disease_balt_op_er.rds")
angina_pectoris_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/angina_pectoris_balt_op_er.rds")
heart_disease_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/heart_disease_balt_op_er.rds")
circulatory_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/circulatory_balt_op_er.rds")
thyroid_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/thyroid_balt_op_er.rds")
diabetes_balt_op_er <- readRDS("modified_data/op_er/balt_op_er_conditions/diabetes_balt_op_er.rds")

#### Load IP Condition Data #####
urticaria_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/urticaria_balt_ip.rds")
heat_exhaustion_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heat_exhaustion_balt_ip.rds")
heat_syncope_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heat_syncope_balt_ip.rds")
heat_cramps_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heat_cramps_balt_ip.rds")
carbon_monoxide_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/carbon_monoxide_balt_ip.rds")
heatstroke_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heatstroke_balt_ip.rds")
all_effects_heat_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/all_effects_heat_balt_ip.rds")
hypothermia_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/hypothermia_balt_ip.rds")
osteoarthritis_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/osteoarthritis_balt_ip.rds")
kidney_disease_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/kidney_disease_balt_ip.rds")
mental_illness_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/mental_illness_balt_ip.rds")
influenza_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/influenza_balt_ip.rds")
acute_bronchitis_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/acute_bronchitis_balt_ip.rds")
bronchitis_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/bronchitis_balt_ip.rds")
copd_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/copd_balt_ip.rds")
asthma_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/asthma_balt_ip.rds")
respiratory_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/respiratory_balt_ip.rds")
hypertension_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/hypertension_balt_ip.rds")
heart_attack_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heart_attack_balt_ip.rds")
cerebrovascular_disease_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/cerebrovascular_disease_balt_ip.rds")
congestive_artery_disease_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/congestive_artery_disease_balt_ip.rds")
angina_pectoris_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/angina_pectoris_balt_ip.rds")
heart_disease_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/heart_disease_balt_ip.rds")
circulatory_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/circulatory_balt_ip.rds")
thyroid_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/thyroid_balt_ip.rds")
diabetes_balt_ip <- readRDS("modified_data/ip/balt_ip_conditions/diabetes_balt_ip.rds")

#### JAKE START HERE #####
#### LOAD EVERYTHING ELSE #####

## First pull out only the columns that we have deemed as necessary ##

small_cols_ip <- as.vector(c("row_unique_id","FY", "HOSPID","NATADM","SOURCADM_IP","SEX","ETHNICIT","MARISTAT","ZIPCODE","PRIN_HMO","SECN_HMO","PAT_DISP_IP","PAYER1","PAYER2","MAJSERVI","DAILYSER","NONPSYCD","PSYCHDAY","MSGDAYS","CCUDAYS","BURNDAYS","NEO_DAYS","PIC_DAYS","TRM_DAYS","OTHRDAYS","BIRTHWGT","ICDFLAG","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","DIAG29","E_CODE","R_FLAG","PRINPROC","PROC1","PROC2","PROC3","PROC4","PROC5","PROC6","PROC7","PROC8","PROC9","PROC10","PROC11","PROC12","PROC13","PROC14","RHB_AC","RHB_IG","PDIAGPOA","TOT_CHG","ATPHYNPI","OPPHYNPI","SEVERITY","RACE","RESIDENT_STATUS","LOS","SAS_COUNTY","QTR","YEAR","DATATYPE","AGE_GROUP","READM30","DIED","FYEAR","UNIQUEID","WEIGHT","PQI","PAU_READMIT","PAU_READMIT_CHG","PAU_PQI","PAU_PQI_CHG","PQI_ELIG"))

small_cols_op_er <- as.vector(c("row_unique_id","HOSPID","SEX","ETHNICIT","MARISTAT","ZIPCODE","PRIN_HMO","SECN_HMO","PAYER1","PAYER2","ICDFLAG","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","E_CODE","TOT_CHG","OPPHYNPI","RACE","RESIDENT_STATUS","SAS_COUNTY","QTR","YEAR", "FY","Age_Group","uniqueid"))

balt_ip_small <- balt_ip[,small_cols_ip]
balt_op_er_small <- balt_op_er[,small_cols_op_er]

### Rename two columns in op_er to make upper
balt_op_er_small <- balt_op_er_small %>%
  rename(AGE_GROUP = Age_Group) %>%
  rename(UNIQUEID = uniqueid)


## now add in to the data a binary column, specifing if each row contains a diagnoses that we have deemed as interesting ####

balt_ip_small$urticaria <- ifelse(balt_ip_small$row_unique_id %in% urticaria_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heat_exhaustion <- ifelse(balt_ip_small$row_unique_id %in% heat_exhaustion_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heat_syncope <- ifelse(balt_ip_small$row_unique_id %in% heat_syncope_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heat_cramps <- ifelse(balt_ip_small$row_unique_id %in% heat_cramps_balt_ip$row_unique_id, 1, 0)
balt_ip_small$carbon_monoxide <- ifelse(balt_ip_small$row_unique_id %in% carbon_monoxide_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heatstroke <- ifelse(balt_ip_small$row_unique_id %in% heatstroke_balt_ip$row_unique_id, 1, 0)
balt_ip_small$all_effects_heat <- ifelse(balt_ip_small$row_unique_id %in% all_effects_heat_balt_ip$row_unique_id, 1, 0)
balt_ip_small$hypothermia <- ifelse(balt_ip_small$row_unique_id %in% hypothermia_balt_ip$row_unique_id, 1, 0)
balt_ip_small$osteoarthritis <- ifelse(balt_ip_small$row_unique_id %in% osteoarthritis_balt_ip$row_unique_id, 1, 0)
balt_ip_small$kidney_disease <- ifelse(balt_ip_small$row_unique_id %in% kidney_disease_balt_ip$row_unique_id, 1, 0)
balt_ip_small$mental_illness <- ifelse(balt_ip_small$row_unique_id %in% mental_illness_balt_ip$row_unique_id, 1, 0)
balt_ip_small$influenza <- ifelse(balt_ip_small$row_unique_id %in% influenza_balt_ip$row_unique_id, 1, 0)
balt_ip_small$acute_bronchitis <- ifelse(balt_ip_small$row_unique_id %in% acute_bronchitis_balt_ip$row_unique_id, 1, 0)
balt_ip_small$bronchitis <- ifelse(balt_ip_small$row_unique_id %in% bronchitis_balt_ip$row_unique_id, 1, 0)
balt_ip_small$copd <- ifelse(balt_ip_small$row_unique_id %in% copd_balt_ip$row_unique_id, 1, 0)
balt_ip_small$asthma <- ifelse(balt_ip_small$row_unique_id %in% asthma_balt_ip$row_unique_id, 1, 0)
balt_ip_small$respiratory <- ifelse(balt_ip_small$row_unique_id %in% respiratory_balt_ip$row_unique_id, 1, 0)
balt_ip_small$hypertension <- ifelse(balt_ip_small$row_unique_id %in% hypertension_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heart_attack <- ifelse(balt_ip_small$row_unique_id %in% heart_attack_balt_ip$row_unique_id, 1, 0)
balt_ip_small$cerebrovascular_disease <- ifelse(balt_ip_small$row_unique_id %in% cerebrovascular_disease_balt_ip$row_unique_id, 1, 0)
balt_ip_small$congestive_artery_disease <- ifelse(balt_ip_small$row_unique_id %in% congestive_artery_disease_balt_ip$row_unique_id, 1, 0)
balt_ip_small$angina_pectoris <- ifelse(balt_ip_small$row_unique_id %in% angina_pectoris_balt_ip$row_unique_id, 1, 0)
balt_ip_small$heart_disease <- ifelse(balt_ip_small$row_unique_id %in% heart_disease_balt_ip$row_unique_id, 1, 0)
balt_ip_small$circulatory <- ifelse(balt_ip_small$row_unique_id %in% circulatory_balt_ip$row_unique_id, 1, 0)
balt_ip_small$thyroid <- ifelse(balt_ip_small$row_unique_id %in% thyroid_balt_ip$row_unique_id, 1, 0)
balt_ip_small$diabetes <- ifelse(balt_ip_small$row_unique_id %in% diabetes_balt_ip$row_unique_id, 1, 0)

# now do this for op er
balt_op_er_small$urticaria <- ifelse(balt_op_er_small$row_unique_id %in% urticaria_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heat_exhaustion <- ifelse(balt_op_er_small$row_unique_id %in% heat_exhaustion_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heat_syncope <- ifelse(balt_op_er_small$row_unique_id %in% heat_syncope_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heat_cramps <- ifelse(balt_op_er_small$row_unique_id %in% heat_cramps_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$carbon_monoxide <- ifelse(balt_op_er_small$row_unique_id %in% carbon_monoxide_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heatstroke <- ifelse(balt_op_er_small$row_unique_id %in% heatstroke_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$all_effects_heat <- ifelse(balt_op_er_small$row_unique_id %in% all_effects_heat_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$hypothermia <- ifelse(balt_op_er_small$row_unique_id %in% hypothermia_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$osteoarthritis <- ifelse(balt_op_er_small$row_unique_id %in% osteoarthritis_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$kidney_disease <- ifelse(balt_op_er_small$row_unique_id %in% kidney_disease_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$mental_illness <- ifelse(balt_op_er_small$row_unique_id %in% mental_illness_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$influenza <- ifelse(balt_op_er_small$row_unique_id %in% influenza_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$acute_bronchitis <- ifelse(balt_op_er_small$row_unique_id %in% acute_bronchitis_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$bronchitis <- ifelse(balt_op_er_small$row_unique_id %in% bronchitis_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$copd <- ifelse(balt_op_er_small$row_unique_id %in% copd_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$asthma <- ifelse(balt_op_er_small$row_unique_id %in% asthma_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$respiratory <- ifelse(balt_op_er_small$row_unique_id %in% respiratory_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$hypertension <- ifelse(balt_op_er_small$row_unique_id %in% hypertension_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heart_attack <- ifelse(balt_op_er_small$row_unique_id %in% heart_attack_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$cerebrovascular_disease <- ifelse(balt_op_er_small$row_unique_id %in% cerebrovascular_disease_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$congestive_artery_disease <- ifelse(balt_op_er_small$row_unique_id %in% congestive_artery_disease_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$angina_pectoris <- ifelse(balt_op_er_small$row_unique_id %in% angina_pectoris_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$heart_disease <- ifelse(balt_op_er_small$row_unique_id %in% heart_disease_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$circulatory <- ifelse(balt_op_er_small$row_unique_id %in% circulatory_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$thyroid <- ifelse(balt_op_er_small$row_unique_id %in% thyroid_balt_op_er$row_unique_id, 1, 0)
balt_op_er_small$diabetes <- ifelse(balt_op_er_small$row_unique_id %in% diabetes_balt_op_er$row_unique_id, 1, 0)

# Add in the population for each zipcode to each table.  
# Create a list of zips
baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(baltzips) -> baltzips
# Create a list of population
# Pop and Race ACS DEMOGRAPHIC AND HOUSING ESTIMATES  more information 2013-2017 American Community Survey 5-Year Estimates https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_DP05&prodType=table
#  S1701 POVERTY STATUS IN THE PAST 12 MONTHS  more information 2013-2017 American Community Survey 5-Year Estimates https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1701&prodType=table
# Units in structure Universe: Housing units  more information 2013-2017 American Community Survey 5-Year Estimates  https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B25024&prodType=table https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B25024&prodType=table
# Median home price Median Value https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B25077&prodType=table
# Median household income: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S2503&prodType=table
# Median Age: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B01002&prodType=table
# Age groups: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S0101&prodType=table
# Year Structure built https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B25035&prodType=table
# Vacancy: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_B25004&prodType=table
# Occupancy type: https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_DP04&prodType=table

baltpops <- c(16595,21788,14766,51287,48243,36797,27462,15258,16436,34010,31733,20790,59404,30037,36563,48815,54771,23811,50957,33001,7770,33926,47806,48225,34657,16729,69649,39333,30085,32065,881)
# Put them zips and pop into a data frame
balt <- do.call(rbind, Map(data.frame, ZIPCODE=baltzips,POPULATION=baltpops))
# Join it back to our two master data frames
balt_ip_small <- inner_join(x = balt_ip_small, y = balt, by = 'ZIPCODE')
balt_op_er_small <- inner_join(x = balt_op_er_small, y = balt, by = 'ZIPCODE')

# Define a list of conditions
conditions <- list("urticaria",	"heat_exhaustion",	"heat_syncope",	"heat_cramps",	"carbon_monoxide",	"heatstroke",	"all_effects_heat",	"hypothermia",	"osteoarthritis",	"kidney_disease",	"mental_illness",	"influenza",	"acute_bronchitis",	"bronchitis",	"copd",	"asthma",	"respiratory",	"hypertension",	"heart_attack",	"cerebrovascular_disease",	"congestive_artery_disease",	"angina_pectoris",	"heart_disease",	"circulatory",	"thyroid",	"diabetes")

#### Jake

###### By Zip ######

## by zip ip
rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions")])
gc()
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  percap_name <- paste0(condition, "_percap")
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!percap_name := sum(!!as.name(condition) == 1)/1022650
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  percap_name <- paste0(condition, "_percap")
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!percap_name := sum(!!as.name(condition) == 1)/1022650
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_zip <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip")])
gc()

## OP_ER ZIP

city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  percap_name <- paste0(condition, "_percap")
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!percap_name := sum(!!as.name(condition) == 1)/1022650
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  percap_name <- paste0(condition, "_percap")
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!percap_name := sum(!!as.name(condition) == 1)/1022650
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_zip <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip")])
gc()

#### Race ####

## race ip
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  black_count_name <- paste0("black_",condition,"_count")
  white_count_name <- paste0("white_",condition,"_count")
  black_prev_name <- paste0("black_",condition,"_prev")
  white_prev_name <- paste0("white_",condition,"_prev")
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!black_count_name := sum(RACE == 2 & !!as.name(condition) == 1),
              !!white_count_name := sum(RACE == 1 & !!as.name(condition) == 1),
              !!black_prev_name := sum(RACE == 2 & !!as.name(condition) == 1)/sum(RACE == 2),
              !!white_prev_name := sum(RACE == 1 & !!as.name(condition) == 1)/sum(RACE == 1)
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  black_count_name <- paste0("black_",condition,"_count")
  white_count_name <- paste0("white_",condition,"_count")
  black_prev_name <- paste0("black_",condition,"_prev")
  white_prev_name <- paste0("white_",condition,"_prev")
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!black_count_name := sum(RACE == 2 & !!as.name(condition) == 1),
              !!white_count_name := sum(RACE == 1 & !!as.name(condition) == 1),
              !!black_prev_name := sum(RACE == 2 & !!as.name(condition) == 1)/sum(RACE == 2),
              !!white_prev_name := sum(RACE == 1 & !!as.name(condition) == 1)/sum(RACE == 1)
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_race <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race")])
gc()

## race op er
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  black_count_name <- paste0("black_",condition,"_count")
  white_count_name <- paste0("white_",condition,"_count")
  black_prev_name <- paste0("black_",condition,"_prev")
  white_prev_name <- paste0("white_",condition,"_prev")
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!black_count_name := sum(RACE == 2 & !!as.name(condition) == 1),
              !!white_count_name := sum(RACE == 1 & !!as.name(condition) == 1),
              !!black_prev_name := sum(RACE == 2 & !!as.name(condition) == 1)/sum(RACE == 2),
              !!white_prev_name := sum(RACE == 1 & !!as.name(condition) == 1)/sum(RACE == 1)
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  black_count_name <- paste0("black_",condition,"_count")
  white_count_name <- paste0("white_",condition,"_count")
  black_prev_name <- paste0("black_",condition,"_prev")
  white_prev_name <- paste0("white_",condition,"_prev")
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              !!black_count_name := sum(RACE == 2 & !!as.name(condition) == 1),
              !!white_count_name := sum(RACE == 1 & !!as.name(condition) == 1),
              !!black_prev_name := sum(RACE == 2 & !!as.name(condition) == 1)/sum(RACE == 2),
              !!white_prev_name := sum(RACE == 1 & !!as.name(condition) == 1)/sum(RACE == 1)
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_race <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race")])
gc()

#### AGE ####

# age ip
youth_ages = c("00-01","02-04","05-09","10-14","15-19")
middle_ages = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
old_ages = c("65-69","70-74","75-79","80-84","85+")

city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  zero_to_nineteen_count_name <- paste0("zero_to_nineteen_", condition,"_count")
  twenty_to_sixtyfour_count_name <- paste0("twenty_to_sixtyfour_", condition,"_count")
  over_sixtyfive_count_name <- paste0("over_sixtyfive_", condition,"_count")
  
  zero_to_nineteen_prev_name <- paste0("zero_to_nineteen_", condition,"_prev")
  twenty_to_sixtyfour_prev_name <- paste0("twenty_to_sixtyfour_", condition,"_prev")
  over_sixtyfive_prev_name <- paste0("over_sixtyfive_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!zero_to_nineteen_count_name := sum(AGE_GROUP %in% youth_ages & !!as.name(condition) == TRUE),
              !!twenty_to_sixtyfour_count_name := sum(AGE_GROUP %in% middle_ages & !!as.name(condition) == TRUE),
              !!over_sixtyfive_count_name := sum(AGE_GROUP %in% old_ages & !!as.name(condition) == TRUE),
              
              !!zero_to_nineteen_prev_name := sum(!!as.name(zero_to_nineteen_count_name))/sum(AGE_GROUP %in% youth_ages),
              !!twenty_to_sixtyfour_prev_name := sum(!!as.name(twenty_to_sixtyfour_count_name))/sum(AGE_GROUP %in% middle_ages),
              !!over_sixtyfive_prev_name := sum(!!as.name(over_sixtyfive_count_name))/sum(AGE_GROUP %in% old_ages)

    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  zero_to_nineteen_count_name <- paste0("zero_to_nineteen_", condition,"_count")
  twenty_to_sixtyfour_count_name <- paste0("twenty_to_sixtyfour_", condition,"_count")
  over_sixtyfive_count_name <- paste0("over_sixtyfive_", condition,"_count")
  
  zero_to_nineteen_prev_name <- paste0("zero_to_nineteen_", condition,"_prev")
  twenty_to_sixtyfour_prev_name <- paste0("twenty_to_sixtyfour_", condition,"_prev")
  over_sixtyfive_prev_name <- paste0("over_sixtyfive_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!zero_to_nineteen_count_name := sum(AGE_GROUP %in% youth_ages & !!as.name(condition) == TRUE),
              !!twenty_to_sixtyfour_count_name := sum(AGE_GROUP %in% middle_ages & !!as.name(condition) == TRUE),
              !!over_sixtyfive_count_name := sum(AGE_GROUP %in% old_ages & !!as.name(condition) == TRUE),
              
              !!zero_to_nineteen_prev_name := sum(!!as.name(zero_to_nineteen_count_name))/sum(AGE_GROUP %in% youth_ages),
              !!twenty_to_sixtyfour_prev_name := sum(!!as.name(twenty_to_sixtyfour_count_name))/sum(AGE_GROUP %in% middle_ages),
              !!over_sixtyfive_prev_name := sum(!!as.name(over_sixtyfive_count_name))/sum(AGE_GROUP %in% old_ages)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_age <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age")])
gc()

# age op er
youth_ages = c("00-01","02-04","05-09","10-14","15-19")
middle_ages = c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64")
old_ages = c("65-69","70-74","75-79","80-84","85+")

city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  zero_to_nineteen_count_name <- paste0("zero_to_nineteen_", condition,"_count")
  twenty_to_sixtyfour_count_name <- paste0("twenty_to_sixtyfour_", condition,"_count")
  over_sixtyfive_count_name <- paste0("over_sixtyfive_", condition,"_count")
  
  zero_to_nineteen_prev_name <- paste0("zero_to_nineteen_", condition,"_prev")
  twenty_to_sixtyfour_prev_name <- paste0("twenty_to_sixtyfour_", condition,"_prev")
  over_sixtyfive_prev_name <- paste0("over_sixtyfive_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!zero_to_nineteen_count_name := sum(AGE_GROUP %in% youth_ages & !!as.name(condition) == TRUE),
              !!twenty_to_sixtyfour_count_name := sum(AGE_GROUP %in% middle_ages & !!as.name(condition) == TRUE),
              !!over_sixtyfive_count_name := sum(AGE_GROUP %in% old_ages & !!as.name(condition) == TRUE),
              
              !!zero_to_nineteen_prev_name := sum(!!as.name(zero_to_nineteen_count_name))/sum(AGE_GROUP %in% youth_ages),
              !!twenty_to_sixtyfour_prev_name := sum(!!as.name(twenty_to_sixtyfour_count_name))/sum(AGE_GROUP %in% middle_ages),
              !!over_sixtyfive_prev_name := sum(!!as.name(over_sixtyfive_count_name))/sum(AGE_GROUP %in% old_ages)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  zero_to_nineteen_count_name <- paste0("zero_to_nineteen_", condition,"_count")
  twenty_to_sixtyfour_count_name <- paste0("twenty_to_sixtyfour_", condition,"_count")
  over_sixtyfive_count_name <- paste0("over_sixtyfive_", condition,"_count")
  
  zero_to_nineteen_prev_name <- paste0("zero_to_nineteen_", condition,"_prev")
  twenty_to_sixtyfour_prev_name <- paste0("twenty_to_sixtyfour_", condition,"_prev")
  over_sixtyfive_prev_name <- paste0("over_sixtyfive_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!zero_to_nineteen_count_name := sum(AGE_GROUP %in% youth_ages & !!as.name(condition) == TRUE),
              !!twenty_to_sixtyfour_count_name := sum(AGE_GROUP %in% middle_ages & !!as.name(condition) == TRUE),
              !!over_sixtyfive_count_name := sum(AGE_GROUP %in% old_ages & !!as.name(condition) == TRUE),
              
              !!zero_to_nineteen_prev_name := sum(!!as.name(zero_to_nineteen_count_name))/sum(AGE_GROUP %in% youth_ages),
              !!twenty_to_sixtyfour_prev_name := sum(!!as.name(twenty_to_sixtyfour_count_name))/sum(AGE_GROUP %in% middle_ages),
              !!over_sixtyfive_prev_name := sum(!!as.name(over_sixtyfive_count_name))/sum(AGE_GROUP %in% old_ages)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_age <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age")])
gc()

#### QTR ####

# qtr ip
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  q1_count_name <- paste0("q1_", condition,"_count")
  q2_count_name <- paste0("q2_", condition,"_count")
  q3_count_name <- paste0("q3_", condition,"_count")
  q4_count_name <- paste0("q4_", condition,"_count")
  
  q1_prev_name <- paste0("q1_", condition,"_prev")
  q2_prev_name <- paste0("q2_", condition,"_prev")
  q3_prev_name <- paste0("q3_", condition,"_prev")
  q4_prev_name <- paste0("q4_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!q1_count_name := sum(QTR == 1 & !!as.name(condition) == 1),
              !!q2_count_name := sum(QTR == 2 & !!as.name(condition) == 1),
              !!q3_count_name := sum(QTR == 3 & !!as.name(condition) == 1),
              !!q4_count_name := sum(QTR == 4 & !!as.name(condition) == 1),
              

              !!q1_prev_name := sum(!!as.name(q1_count_name))/sum(QTR == 1),
              !!q2_prev_name := sum(!!as.name(q2_count_name))/sum(QTR == 2),
              !!q3_prev_name := sum(!!as.name(q3_count_name))/sum(QTR == 3),
              !!q4_prev_name := sum(!!as.name(q4_count_name))/sum(QTR == 4)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  q1_count_name <- paste0("q1_", condition,"_count")
  q2_count_name <- paste0("q2_", condition,"_count")
  q3_count_name <- paste0("q3_", condition,"_count")
  q4_count_name <- paste0("q4_", condition,"_count")
  
  q1_prev_name <- paste0("q1_", condition,"_prev")
  q2_prev_name <- paste0("q2_", condition,"_prev")
  q3_prev_name <- paste0("q3_", condition,"_prev")
  q4_prev_name <- paste0("q4_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!q1_count_name := sum(QTR == 1 & !!as.name(condition) == 1),
              !!q2_count_name := sum(QTR == 2 & !!as.name(condition) == 1),
              !!q3_count_name := sum(QTR == 3 & !!as.name(condition) == 1),
              !!q4_count_name := sum(QTR == 4 & !!as.name(condition) == 1),
              
              
              !!q1_prev_name := sum(!!as.name(q1_count_name))/sum(QTR == 1),
              !!q2_prev_name := sum(!!as.name(q2_count_name))/sum(QTR == 2),
              !!q3_prev_name := sum(!!as.name(q3_count_name))/sum(QTR == 3),
              !!q4_prev_name := sum(!!as.name(q4_count_name))/sum(QTR == 4)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_qtr <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr")])
gc()

# qtr op_er
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  q1_count_name <- paste0("q1_", condition,"_count")
  q2_count_name <- paste0("q2_", condition,"_count")
  q3_count_name <- paste0("q3_", condition,"_count")
  q4_count_name <- paste0("q4_", condition,"_count")
  
  q1_prev_name <- paste0("q1_", condition,"_prev")
  q2_prev_name <- paste0("q2_", condition,"_prev")
  q3_prev_name <- paste0("q3_", condition,"_prev")
  q4_prev_name <- paste0("q4_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!q1_count_name := sum(QTR == 1 & !!as.name(condition) == 1),
              !!q2_count_name := sum(QTR == 2 & !!as.name(condition) == 1),
              !!q3_count_name := sum(QTR == 3 & !!as.name(condition) == 1),
              !!q4_count_name := sum(QTR == 4 & !!as.name(condition) == 1),
              
              
              !!q1_prev_name := sum(!!as.name(q1_count_name))/sum(QTR == 1),
              !!q2_prev_name := sum(!!as.name(q2_count_name))/sum(QTR == 2),
              !!q3_prev_name := sum(!!as.name(q3_count_name))/sum(QTR == 3),
              !!q4_prev_name := sum(!!as.name(q4_count_name))/sum(QTR == 4)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  q1_count_name <- paste0("q1_", condition,"_count")
  q2_count_name <- paste0("q2_", condition,"_count")
  q3_count_name <- paste0("q3_", condition,"_count")
  q4_count_name <- paste0("q4_", condition,"_count")
  
  q1_prev_name <- paste0("q1_", condition,"_prev")
  q2_prev_name <- paste0("q2_", condition,"_prev")
  q3_prev_name <- paste0("q3_", condition,"_prev")
  q4_prev_name <- paste0("q4_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!q1_count_name := sum(QTR == 1 & !!as.name(condition) == 1),
              !!q2_count_name := sum(QTR == 2 & !!as.name(condition) == 1),
              !!q3_count_name := sum(QTR == 3 & !!as.name(condition) == 1),
              !!q4_count_name := sum(QTR == 4 & !!as.name(condition) == 1),
              
              
              !!q1_prev_name := sum(!!as.name(q1_count_name))/sum(QTR == 1),
              !!q2_prev_name := sum(!!as.name(q2_count_name))/sum(QTR == 2),
              !!q3_prev_name := sum(!!as.name(q3_count_name))/sum(QTR == 3),
              !!q4_prev_name := sum(!!as.name(q4_count_name))/sum(QTR == 4)
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_qtr <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr", "op_er_full_qtr")])
gc()

#### Medicaid ####

# medicaid ip
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  medicaid_count_name <- paste0("medicaid_", condition,"_count")
  not_medicaid_count_name <- paste0("not_medicaid_", condition,"_count")
  commercial_count_name <- paste0("commercial_", condition,"_count")
  medicaid_prev_name <- paste0("medicaid_", condition,"_prev")
  not_medicaid_prev_name <- paste0("not_medicaid_", condition,"_prev")
  commercial_prev_name <- paste0("commercial_", condition,"_prev")
  com_codes <- c(04,05,12,16,17)
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!medicaid_count_name := sum(((PAYER1 == 02 | PAYER1 == 14)) & !!as.name(condition) == 1),
              !!not_medicaid_count_name := sum(((PAYER1 != 02 & PAYER1 != 14)) & !!as.name(condition) == 1),
              !!commercial_count_name := sum((PAYER1 %in% com_codes) & !!as.name(condition) == 1),
              
              !!medicaid_prev_name := sum(!!as.name(medicaid_count_name))/sum(((PAYER1 == 02 | PAYER1 == 14))),
              !!not_medicaid_prev_name := sum(!!as.name(not_medicaid_count_name))/sum(((PAYER1 != 02 & PAYER1 != 14))),
              !!commercial_prev_name := sum(!!as.name(commercial_count_name))/sum((PAYER1 %in% com_codes))

    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}


by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  medicaid_count_name <- paste0("medicaid_", condition,"_count")
  not_medicaid_count_name <- paste0("not_medicaid_", condition,"_count")
  commercial_count_name <- paste0("commercial_", condition,"_count")
  medicaid_prev_name <- paste0("medicaid_", condition,"_prev")
  not_medicaid_prev_name <- paste0("not_medicaid_", condition,"_prev")
  commercial_prev_name <- paste0("commercial_", condition,"_prev")
  com_codes <- c(04,05,12,16,17)
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!medicaid_count_name := sum(((PAYER1 == 02 | PAYER1 == 14)) & !!as.name(condition) == 1),
              !!not_medicaid_count_name := sum(((PAYER1 != 02 & PAYER1 != 14)) & !!as.name(condition) == 1),
              !!commercial_count_name := sum((PAYER1 %in% com_codes) & !!as.name(condition) == 1),
              
              !!medicaid_prev_name := sum(!!as.name(medicaid_count_name))/sum(((PAYER1 == 02 | PAYER1 == 14))),
              !!not_medicaid_prev_name := sum(!!as.name(not_medicaid_count_name))/sum(((PAYER1 != 02 & PAYER1 != 14))),
              !!commercial_prev_name := sum(!!as.name(commercial_count_name))/sum((PAYER1 %in% com_codes))
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_medicaid <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr", "op_er_full_qtr", "ip_full_medicaid")])
gc()

# medicaid op_er
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  medicaid_count_name <- paste0("medicaid_", condition,"_count")
  not_medicaid_count_name <- paste0("not_medicaid_", condition,"_count")
  commercial_count_name <- paste0("commercial_", condition,"_count")
  medicaid_prev_name <- paste0("medicaid_", condition,"_prev")
  not_medicaid_prev_name <- paste0("not_medicaid_", condition,"_prev")
  commercial_prev_name <- paste0("commercial_", condition,"_prev")
  com_codes <- c(04,05,12,16,17)
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!medicaid_count_name := sum(((PAYER1 == 02 | PAYER1 == 14)) & !!as.name(condition) == 1),
              !!not_medicaid_count_name := sum(((PAYER1 != 02 & PAYER1 != 14)) & !!as.name(condition) == 1),
              !!commercial_count_name := sum((PAYER1 %in% com_codes) & !!as.name(condition) == 1),
              
              !!medicaid_prev_name := sum(!!as.name(medicaid_count_name))/sum(((PAYER1 == 02 | PAYER1 == 14))),
              !!not_medicaid_prev_name := sum(!!as.name(not_medicaid_count_name))/sum(((PAYER1 != 02 & PAYER1 != 14))),
              !!commercial_prev_name := sum(!!as.name(commercial_count_name))/sum((PAYER1 %in% com_codes))
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}


by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  medicaid_count_name <- paste0("medicaid_", condition,"_count")
  not_medicaid_count_name <- paste0("not_medicaid_", condition,"_count")
  commercial_count_name <- paste0("commercial_", condition,"_count")
  medicaid_prev_name <- paste0("medicaid_", condition,"_prev")
  not_medicaid_prev_name <- paste0("not_medicaid_", condition,"_prev")
  commercial_prev_name <- paste0("commercial_", condition,"_prev")
  com_codes <- c(04,05,12,16,17)
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!medicaid_count_name := sum(((PAYER1 == 02 | PAYER1 == 14)) & !!as.name(condition) == 1),
              !!not_medicaid_count_name := sum(((PAYER1 != 02 & PAYER1 != 14)) & !!as.name(condition) == 1),
              !!commercial_count_name := sum((PAYER1 %in% com_codes) & !!as.name(condition) == 1),
              
              !!medicaid_prev_name := sum(!!as.name(medicaid_count_name))/sum(((PAYER1 == 02 | PAYER1 == 14))),
              !!not_medicaid_prev_name := sum(!!as.name(not_medicaid_count_name))/sum(((PAYER1 != 02 & PAYER1 != 14))),
              !!commercial_prev_name := sum(!!as.name(commercial_count_name))/sum((PAYER1 %in% com_codes))
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_medicaid <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr", "op_er_full_qtr", "ip_full_medicaid", "op_er_full_medicaid")])
gc()

#### Year ######

# year ip
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  thirteen_count_name <- paste0("thirteen_", condition,"_count")
  fourteen_count_name <- paste0("fourteen_", condition,"_count")
  fifthteen_count_name <- paste0("fifthteen_", condition,"_count")
  sixteen_count_name <- paste0("sixteen_", condition,"_count")
  seventeen_count_name <- paste0("seventeen_", condition,"_count")
  eighteen_count_name <- paste0("eighteen_", condition,"_count")
  
  thirteen_prev_name <- paste0("thirteen_", condition,"_prev")
  fourteen_prev_name <- paste0("fourteen_", condition,"_prev")
  fifthteen_prev_name <- paste0("fifthteen_", condition,"_prev")
  sixteen_prev_name <- paste0("sixteen_", condition,"_prev")
  seventeen_prev_name <- paste0("seventeen_", condition,"_prev")
  eighteen_prev_name <- paste0("eighteen_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!thirteen_count_name := sum(YEAR == "2013" & !!as.name(condition) == 1),
              !!fourteen_count_name := sum(YEAR == "2014" & !!as.name(condition) == 1),
              !!fifthteen_count_name := sum(YEAR == "2015" & !!as.name(condition) == 1),
              !!sixteen_count_name := sum(YEAR == "2016" & !!as.name(condition) == 1),
              !!seventeen_count_name := sum(YEAR == "2017" & !!as.name(condition) == 1),
              !!eighteen_count_name := sum(YEAR == "2018" & !!as.name(condition) == 1),
              

             
              !!thirteen_prev_name := sum(!!as.name(thirteen_count_name))/sum(YEAR == "2013"),
              !!fourteen_prev_name := sum(!!as.name(fourteen_count_name))/sum(YEAR == "2014"),
              !!fifthteen_prev_name := sum(!!as.name(fifthteen_count_name))/sum(YEAR == "2015"),
              !!sixteen_prev_name := sum(!!as.name(sixteen_count_name))/sum(YEAR == "2016"),
              !!seventeen_prev_name := sum(!!as.name(seventeen_count_name))/sum(YEAR == "2017"),
              !!eighteen_prev_name := sum(!!as.name(eighteen_count_name))/sum(YEAR == "2018"),
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  thirteen_count_name <- paste0("thirteen_", condition,"_count")
  fourteen_count_name <- paste0("fourteen_", condition,"_count")
  fifthteen_count_name <- paste0("fifthteen_", condition,"_count")
  sixteen_count_name <- paste0("sixteen_", condition,"_count")
  seventeen_count_name <- paste0("seventeen_", condition,"_count")
  eighteen_count_name <- paste0("eighteen_", condition,"_count")
  
  thirteen_prev_name <- paste0("thirteen_", condition,"_prev")
  fourteen_prev_name <- paste0("fourteen_", condition,"_prev")
  fifthteen_prev_name <- paste0("fifthteen_", condition,"_prev")
  sixteen_prev_name <- paste0("sixteen_", condition,"_prev")
  seventeen_prev_name <- paste0("seventeen_", condition,"_prev")
  eighteen_prev_name <- paste0("eighteen_", condition,"_prev")
  
  summary <- balt_ip_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!thirteen_count_name := sum(YEAR == "2013" & !!as.name(condition) == 1),
              !!fourteen_count_name := sum(YEAR == "2014" & !!as.name(condition) == 1),
              !!fifthteen_count_name := sum(YEAR == "2015" & !!as.name(condition) == 1),
              !!sixteen_count_name := sum(YEAR == "2016" & !!as.name(condition) == 1),
              !!seventeen_count_name := sum(YEAR == "2017" & !!as.name(condition) == 1),
              !!eighteen_count_name := sum(YEAR == "2018" & !!as.name(condition) == 1),
              
              
              
              !!thirteen_prev_name := sum(!!as.name(thirteen_count_name))/sum(YEAR == "2013"),
              !!fourteen_prev_name := sum(!!as.name(fourteen_count_name))/sum(YEAR == "2014"),
              !!fifthteen_prev_name := sum(!!as.name(fifthteen_count_name))/sum(YEAR == "2015"),
              !!sixteen_prev_name := sum(!!as.name(sixteen_count_name))/sum(YEAR == "2016"),
              !!seventeen_prev_name := sum(!!as.name(seventeen_count_name))/sum(YEAR == "2017"),
              !!eighteen_prev_name := sum(!!as.name(eighteen_count_name))/sum(YEAR == "2018"),
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

ip_full_year <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr", "op_er_full_qtr", "ip_full_medicaid", "op_er_full_medicaid", "ip_full_year")])
gc()

# year op er
city_wide <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  thirteen_count_name <- paste0("thirteen_", condition,"_count")
  fourteen_count_name <- paste0("fourteen_", condition,"_count")
  fifthteen_count_name <- paste0("fifthteen_", condition,"_count")
  sixteen_count_name <- paste0("sixteen_", condition,"_count")
  seventeen_count_name <- paste0("seventeen_", condition,"_count")
  eighteen_count_name <- paste0("eighteen_", condition,"_count")
  
  thirteen_prev_name <- paste0("thirteen_", condition,"_prev")
  fourteen_prev_name <- paste0("fourteen_", condition,"_prev")
  fifthteen_prev_name <- paste0("fifthteen_", condition,"_prev")
  sixteen_prev_name <- paste0("sixteen_", condition,"_prev")
  seventeen_prev_name <- paste0("seventeen_", condition,"_prev")
  eighteen_prev_name <- paste0("eighteen_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    summarise(ZIPCODE = "CITYWIDE",
              total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!thirteen_count_name := sum(YEAR == "2013" & !!as.name(condition) == 1),
              !!fourteen_count_name := sum(YEAR == "2014" & !!as.name(condition) == 1),
              !!fifthteen_count_name := sum(YEAR == "2015" & !!as.name(condition) == 1),
              !!sixteen_count_name := sum(YEAR == "2016" & !!as.name(condition) == 1),
              !!seventeen_count_name := sum(YEAR == "2017" & !!as.name(condition) == 1),
              !!eighteen_count_name := sum(YEAR == "2018" & !!as.name(condition) == 1),
              
              
              
              !!thirteen_prev_name := sum(!!as.name(thirteen_count_name))/sum(YEAR == "2013"),
              !!fourteen_prev_name := sum(!!as.name(fourteen_count_name))/sum(YEAR == "2014"),
              !!fifthteen_prev_name := sum(!!as.name(fifthteen_count_name))/sum(YEAR == "2015"),
              !!sixteen_prev_name := sum(!!as.name(sixteen_count_name))/sum(YEAR == "2016"),
              !!seventeen_prev_name := sum(!!as.name(seventeen_count_name))/sum(YEAR == "2017"),
              !!eighteen_prev_name := sum(!!as.name(eighteen_count_name))/sum(YEAR == "2018"),
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(city_wide)){
    city_wide <- as_tibble(summary)
  }else{
    city_wide <- inner_join(x = city_wide, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

by_zip <- NA
for (condition in conditions){
  count_name <- paste0(condition, "_count")
  prev_name <- paste0(condition, "_prev")
  thirteen_count_name <- paste0("thirteen_", condition,"_count")
  fourteen_count_name <- paste0("fourteen_", condition,"_count")
  fifthteen_count_name <- paste0("fifthteen_", condition,"_count")
  sixteen_count_name <- paste0("sixteen_", condition,"_count")
  seventeen_count_name <- paste0("seventeen_", condition,"_count")
  eighteen_count_name <- paste0("eighteen_", condition,"_count")
  
  thirteen_prev_name <- paste0("thirteen_", condition,"_prev")
  fourteen_prev_name <- paste0("fourteen_", condition,"_prev")
  fifthteen_prev_name <- paste0("fifthteen_", condition,"_prev")
  sixteen_prev_name <- paste0("sixteen_", condition,"_prev")
  seventeen_prev_name <- paste0("seventeen_", condition,"_prev")
  eighteen_prev_name <- paste0("eighteen_", condition,"_prev")
  
  summary <- balt_op_er_small %>%
    group_by(ZIPCODE) %>%
    summarise(total_count = n(),
              population = first(POPULATION),
              !!count_name := sum(!!as.name(condition) == 1), 
              !!prev_name := sum(!!as.name(condition) == 1)/n(),
              
              !!thirteen_count_name := sum(YEAR == "2013" & !!as.name(condition) == 1),
              !!fourteen_count_name := sum(YEAR == "2014" & !!as.name(condition) == 1),
              !!fifthteen_count_name := sum(YEAR == "2015" & !!as.name(condition) == 1),
              !!sixteen_count_name := sum(YEAR == "2016" & !!as.name(condition) == 1),
              !!seventeen_count_name := sum(YEAR == "2017" & !!as.name(condition) == 1),
              !!eighteen_count_name := sum(YEAR == "2018" & !!as.name(condition) == 1),
              
              
              
              !!thirteen_prev_name := sum(!!as.name(thirteen_count_name))/sum(YEAR == "2013"),
              !!fourteen_prev_name := sum(!!as.name(fourteen_count_name))/sum(YEAR == "2014"),
              !!fifthteen_prev_name := sum(!!as.name(fifthteen_count_name))/sum(YEAR == "2015"),
              !!sixteen_prev_name := sum(!!as.name(sixteen_count_name))/sum(YEAR == "2016"),
              !!seventeen_prev_name := sum(!!as.name(seventeen_count_name))/sum(YEAR == "2017"),
              !!eighteen_prev_name := sum(!!as.name(eighteen_count_name))/sum(YEAR == "2018"),
              
    )
  assign(condition, as_tibble(summary))
  if (is.na(by_zip)){
    by_zip <- as_tibble(summary)
  }else{
    by_zip <- inner_join(x = by_zip, y = as_tibble(summary), by=c("ZIPCODE","total_count","population"))
  }
}

op_er_full_year <- bind_rows(city_wide, by_zip)

rm(list=ls()[! ls() %in% c("balt_ip_small", "balt_op_er_small", "conditions", "ip_full_zip", "op_er_full_zip", "ip_full_race", "op_er_full_race", "ip_full_age", "op_er_full_age", "ip_full_qtr", "op_er_full_qtr", "ip_full_medicaid", "op_er_full_medicaid", "ip_full_year", "op_er_full_year")])
gc()


# write output to csv files #####
write.csv(ip_full_zip, file= "output_files/ip_full_zip.csv", row.names=FALSE)
write.csv(ip_full_race, file= "output_files/ip_full_race.csv", row.names=FALSE)
write.csv(ip_full_qtr, file= "output_files/ip_full_qtr.csv", row.names=FALSE)
write.csv(ip_full_age, file= "output_files/ip_full_age.csv", row.names=FALSE)
write.csv(ip_full_medicaid, file= "output_files/ip_full_medicaid.csv", row.names=FALSE)
write.csv(ip_full_year, file= "output_files/ip_full_year.csv", row.names=FALSE)

write.csv(op_er_full_zip, file= "output_files/op_er_full_zip.csv", row.names=FALSE)
write.csv(op_er_full_race, file= "output_files/op_er_full_race.csv", row.names=FALSE)
write.csv(op_er_full_qtr, file= "output_files/op_er_full_qtr.csv", row.names=FALSE)
write.csv(op_er_full_age, file= "output_files/op_er_full_age.csv", row.names=FALSE)
write.csv(op_er_full_medicaid, file= "output_files/op_er_full_medicaid.csv", row.names=FALSE)
write.csv(op_er_full_year, file= "output_files/op_er_full_year.csv", row.names=FALSE)
