# This file is not part of the main data flow, but was used to find the best methods to use

# Load Libraries #####
library('dplyr')
library('haven')
library('tidyr')
library('readr')
library('purrr')
library('stringr')
library('haven')

# load data #####
balt_ip = readRDS("modified_data/balt_ip.rds")
asthma_ip = readRDS("modified_data/asthma_ip.rds")
circu_ip = readRDS("modified_data/circu_ip.rds")
respir_ip = readRDS("modified_data/respir_ip.rds")
mental_ip = readRDS("modified_data/mental_ip.rds")

## First pull out only the columns that we have deemed as necessary ####

small_cols <- as.vector(c("HOSPID","NATADM","SOURCADM_IP","SEX","ETHNICIT","MARISTAT","ZIPCODE","PRIN_HMO","SECN_HMO","PAT_DISP_IP","PAYER1","PAYER2","MAJSERVI","DAILYSER","NONPSYCD","PSYCHDAY","MSGDAYS","CCUDAYS","BURNDAYS","NEO_DAYS","PIC_DAYS","TRM_DAYS","OTHRDAYS","BIRTHWGT","PSYCHADM","ICDFLAG","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","DIAG29","E_CODE","R_FLAG","PRINPROC","PROC1","PROC2","PROC3","PROC4","PROC5","PROC6","PROC7","PROC8","PROC9","PROC10","PROC11","PROC12","PROC13","PROC14","PROC15","PROC16","PROC17","PROC18","PROC19","PROC20","PROC21","PROC22","PROC23","PROC24","PROC25","PROC26","PROC27","PROC28","PROC29","RHB_AC","RHB_IG","PDIAGPOA","TOT_CHG","ATPHYNPI","OPPHYNPI","SEVERITY","RACE","COUNTRY","PRELANG","RESIDENT_STATUS","LOS","SAS_COUNTY","QTR","YEAR","DATATYPE","AGE_GROUP","READM30","DIED","FYEAR","UNIQUEID","WEIGHT","PQI","PAU_READMIT","PAU_READMIT_CHG","PAU_PQI","PAU_PQI_CHG","PQI_ELIG"))

balt_ip_small <- balt_ip[,small_cols]

## now add in to the data a binary column ####
#specifing if each row contains a diagnoses that we have deemed as interesting ####

balt_ip_small$asthma <- ifelse(balt_ip_small$UNIQUEID %in% asthma_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$circu <- ifelse(balt_ip_small$UNIQUEID %in% circu_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$mental <- ifelse(balt_ip_small$UNIQUEID %in% mental_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$respir <- ifelse(balt_ip_small$UNIQUEID %in% respir_ip$UNIQUEID, TRUE, FALSE)

rm(list=ls()[! ls() %in% c("obs_op","ip", "asthma_obs_op", "circu_obs_op", "respir_obs_op", "mental_obs_op", "balt_obs_op", "asthma_ip", "circu_ip", "respir_ip", "mental_ip", "balt_ip", "ip_balt_years_and_qtrs", "ip_balt_years", "ip_balt_qtrs","balt_ip_small")])

baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(baltzips) -> baltzips

baltpops <- c(16595,21788,14766,51287,48243,36797,27462,15258,16436,34010,31733,20790,59404,30037,36563,48815,54771,23811,50957,33001,7770,33926,47806,48225,34657,16729,69649,39333,30085,32065,881)

balt <- do.call(rbind, Map(data.frame, ZIPCODE=baltzips,POPULATION=baltpops))

balt_ip_small <- unique(inner_join(x = balt_ip_small, y = balt))

######### Do this by zip code and city wide

###### By Zip ######

city_wide <- balt_ip_small %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(ZIPCODE = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/1022650,
            circu_percap = sum(circu_count)/1022650,
            mental_percap = sum(mental_count)/1022650,
            respir_percap = sum(respir_count)/1022650)

by_zip <- balt_ip_small %>%
  group_by(ZIPCODE) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))


ip_zip <- bind_rows(city_wide, by_zip)

###### By Year ######

city_wide <- balt_ip_small %>%
  group_by(YEAR) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(ZIPCODE = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(YEAR, ZIPCODE) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))


ip_balt_years <- bind_rows(city_wide, by_zip)

###### By Quarter ######

city_wide <- balt_ip_small %>%
  group_by(QTR) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(ZIPCODE = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(QTR, ZIPCODE) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

ip_balt_qtrs <- bind_rows(city_wide, by_zip)

###### By Year and Quarter ######

city_wide <- balt_ip_small %>%
  group_by(YEAR, QTR) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(ZIPCODE = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(YEAR, QTR, ZIPCODE) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(population = first(POPULATION),
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

ip_balt_years_and_qtrs <- bind_rows(city_wide, by_zip)


###### Age ######

city_wide <- balt_ip_small %>%
  group_by() %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(AGE_GROUP = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(AGE_GROUP) %>%
  filter(n() > 1) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n())

ip_balt_age <- bind_rows(city_wide, by_zip)

###### Race ######

city_wide <- balt_ip_small %>%
  group_by() %>%
  filter(n() > 1) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(RACE = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(RACE) %>%
  filter(n() > 1) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n())

ip_balt_race <- bind_rows(city_wide, by_zip)

###### Age and Race ######

city_wide <- balt_ip_small %>%
  group_by() %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(RACE = "CITYWIDE",
            AGE_GROUP = "CITYWIDE",
            asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n(),
            population = first(POPULATION),
            asthma_percap = sum(asthma_count)/first(POPULATION),
            circu_percap = sum(circu_count)/first(POPULATION),
            mental_percap = sum(mental_count)/first(POPULATION),
            respir_percap = sum(respir_count)/first(POPULATION))

by_zip <- balt_ip_small %>%
  group_by(AGE_GROUP,RACE) %>%
  filter(n() > 1) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(asthma_count = sum(asthma_count), 
            circu_count = sum(circu_count), 
            mental_count = sum(mental_count), 
            respir_count = sum(respir_count), 
            total_count = n(),
            asthma_prev = sum(asthma_count)/n(),
            circu_prev = sum(circu_count)/n(),
            mental_prev = sum(mental_count)/n(),
            respir_prev = sum(respir_count)/n())

ip_balt_age_and_race <- bind_rows(city_wide, by_zip)

###### Medicaid ######

city_wide <- balt_ip_small %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(ZIPCODE = "CITYWIDE",
            total_count = n(),
            medicaid_count = sum(PRIN_HMO >= 42 & PRIN_HMO < 52),
            medicaid_prev = sum(medicaid_count)/n())

by_zip <- balt_ip_small %>%
  group_by(ZIPCODE) %>%
  mutate(asthma_count = if_else(asthma == TRUE, 1, 0)) %>%
  mutate(circu_count = if_else(circu == TRUE, 1, 0)) %>%
  mutate(mental_count = if_else(mental == TRUE, 1, 0)) %>%
  mutate(respir_count = if_else(respir == TRUE, 1, 0)) %>%
  summarise(total_count = n(),
            medicaid_count = sum(PRIN_HMO >= 42 & PRIN_HMO < 52),
            medicaid_prev = sum(medicaid_count)/n())

ip_balt_medicaid <- bind_rows(city_wide, by_zip)