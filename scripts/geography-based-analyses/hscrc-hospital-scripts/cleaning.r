#### Maryland Hospital Data Cleaning Script ####
#### None of the data is stored publicly #####
#### Kept on air-gapped password protected computer #####
#### Limits on sharing data in numbers greater than 10 publicly ####
#### Original in SAS format ####

# Load Libraries #####
library('dplyr')
library('haven')
library('tidyr')
library('readr')
library('purrr')
library('stringr')
library('haven')
library('tibble')

#################### LOADING AND CLEANING OP ER FILES ######

# This is the location of original full outpatient files, which were too big to load directly in R.  Just here for transparency sake.  We processed these in SAS first, see below. ####
NOLOAD_op_fy19e <- read_sas("raw_data/unzipped/op/op_fy19e.sas7bdat")
NOLOAD_op_fy18e <- read_sas("raw_data/unzipped/op/op_fy18e.sas7bdat")
NOLOAD_op_fy17e <- read_sas("raw_data/unzipped/op/op_fy17e.sas7bdat")
NOLOAD_op_fy16e <- read_sas("raw_data/unzipped/op/op_fy16e.sas7bdat")
NOLOAD_op_fy15e <- read_sas("raw_data/unzipped/op/op_fy15e.sas7bdat")
NOLOAD_op_fy14e <- read_sas("raw_data/unzipped/op/op_fy14e.sas7bdat")
NOLOAD_op_fy13e <- read_sas("raw_data/unzipped/op/op_fy13e.sas7bdat")

# Read in outpatient emergency files, which are the master op_fyYYe.sas7bdat files, filtered in SAS to just select emergency visits.  This operation was too big to do in R, which is why we did it in SAS. Note we are reading in each file, then immedately saving it as an RDS files. Before moving to the next one, remove the object and run garbage collection.    

#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# op_fy13emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy13_emergency.sas7bdat")
# saveRDS(op_fy13emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy13_emergency.rds")
rm(list = ls())
gc()

op_fy14emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy14_emergency.sas7bdat")
saveRDS(op_fy14emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy14_emergency.rds")
rm(list = ls())
gc()

op_fy15emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy15_emergency.sas7bdat")
saveRDS(op_fy15emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy15_emergency.rds")
rm(list = ls())
gc()

op_fy16emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy16_emergency.sas7bdat")
saveRDS(op_fy16emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy16_emergency.rds")
rm(list = ls())
gc()

op_fy17emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy17_emergency.sas7bdat")
saveRDS(op_fy17emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy17_emergency.rds")
rm(list = ls())
gc()

op_fy18emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy18_emergency.sas7bdat")
saveRDS(op_fy18emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy18_emergency.rds")
rm(list = ls())
gc()

op_fy19emergency <- read_sas("raw_data/unzipped/op/sas_studio_exports/op_fy19_emergency.sas7bdat")
saveRDS(op_fy19emergency, file="raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy19_emergency.rds")
rm(list = ls())
gc()

# Read in emergency visits by FY (processed in SAS, then cleaning.r)
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# op_er13 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy13_emergency.rds")
op_er14 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy14_emergency.rds")
op_er15 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy15_emergency.rds")
op_er16 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy16_emergency.rds")
op_er17 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy17_emergency.rds")
op_er18 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy18_emergency.rds")
op_er19 <- readRDS("raw_data/unzipped/op/sas_studio_exports/rds_conversions/op_fy19_emergency.rds")


# Mutate 14 and 15 to convert year column to YEAR column to match format of other years. 

op_er14 <- op_er14 %>%
  mutate(YEAR = year) %>%
  select(-year)

op_er15 <- op_er15 %>%
  mutate(YEAR = year) %>%
  select(-year)

drop_op_er <- c("datatype", "N_adi01th","N_percentiles","N_adi5th","N_adi10th","COUNTRY","RWHITE","RBLACK","RNAAMER","RASIAN","RHAWAI","ROTHER","RDECLIN","RUNKNOW","PRELANG","PRELANG_CD","HOSPSRCE","HOSPDEST","TAPQ01","TAPQ02","TAPQ03","TAPQ05","TAPQ07","TAPQ08","TAPQ10","TAPQ11","TAPQ12","TAPQ13","TAPQ14","TAPQ15","TAPQ16","TAPQ90","TAPQ91","TAPQ92","TAPQ93","N_ADI01TH","N_PERCENTILES","N_ADI5TH","N_ADI10TH","kp_flag","DATATYPE","PAYER3","TERT_HMO","ATPHYNPI","DIAG29","DIAG30","DIAG31","DIAG32","DIAG33","DIAG34","DIAG35","DIAG36","DIAG37","DIAG38","DIAG39","DIAG40","DIAG41","DIAG42","DIAG43","DIAG44","DIAG45","DIAG46","DIAG47","DIAG48","DIAG49","DIAG50","DIAG51","DIAG52","DIAG53","DIAG54","DIAG55","DIAG56","DIAG57","DIAG58","DIAG59","DIAG60","DIAG61","DIAG62","DIAG63","DIAG64","DIAG65","DIAG66","DIAG67","DIAG68","DIAG69","DIAG70","DIAG71","DIAG72","DIAG73","DIAG74","DIAG75","DIAG76","DIAG77","DIAG78","DIAG79","DIAG80","DIAG81","DIAG82","DIAG83","DIAG84","DIAG85","DIAG86","DIAG87","DIAG88","DIAG89","DIAG90","DIAG91","DIAG92","DIAG93","DIAG94","DIAG95","DIAG96","DIAG97","DIAG98","DIAG99","RCTUNT100","RCTUNT101","RCTUNT102","RCTUNT103","RCTUNT104","RCTUNT105","RCTUNT106","RCTUNT107","RCTUNT108","RCTUNT109","RCTUNT110","RCTUNT111","RCTUNT112","RCTUNT113","RCTUNT114","RCTUNT115","RCTUNT116","RCTUNT117","RCTUNT118","RCTUNT119","RCTCHG100","RCTCHG101","RCTCHG102","RCTCHG103","RCTCHG104","RCTCHG105","RCTCHG106","RCTCHG107","RCTCHG108","RCTCHG109","RCTCHG110","RCTCHG111","RCTCHG112","RCTCHG113","RCTCHG114","RCTCHG115","RCTCHG116","RCTCHG117","RCTCHG118","RCTCHG119","FILLER6A")

# Drop unneded columns from each.

op_erc19 <- op_er19[ , !(names(op_er19) %in% drop_op_er)]
op_erc18 <- op_er18[ , !(names(op_er18) %in% drop_op_er)]
op_erc17 <- op_er17[ , !(names(op_er17) %in% drop_op_er)]
op_erc16 <- op_er16[ , !(names(op_er16) %in% drop_op_er)]
op_erc15 <- op_er15[ , !(names(op_er15) %in% drop_op_er)]
op_erc14 <- op_er14[ , !(names(op_er14) %in% drop_op_er)]
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# op_erc13 <- op_er13[ , !(names(op_er13) %in% drop)]

# Remove original op_er files to save memory, run garbage collection
rm(list=ls()[! ls() %in% c("op_erc19","op_erc18", "op_erc17","op_erc16","op_erc15","op_erc14","op_erc13")])
gc()

# Add field with fiscal year to keep track when combined #####
op_erc19$FY <- '19'
op_erc18$FY <- '18'
op_erc17$FY <- '17'
op_erc16$FY <- '16'
op_erc15$FY <- '15'
op_erc14$FY <- '14'
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# op_erc13$FY <- '13'

# Put them back together.
# For now remove op_erc13
op_er <- bind_rows(op_erc14, op_erc15, op_erc16, op_erc17, op_erc18, op_erc19)

# Remove FY files to save memory, run garbage collection
rm(list=ls()[! ls() %in% c("op_er")])
gc()
### Create a unique identifier for each row called row_unique_id. We'll use this to join back conditions data frame to master data frame later on. Note that it has an asterisk at start and end.  This is so later our looping through conditinos function doesn't get fucked up.

op_er <- op_er %>%
  rowid_to_column("rowid") %>%
  mutate(row_unique_id = paste0("*",rowid,"*")) %>%
  select(row_unique_id, everything(), -rowid)

# Save the file as an rds file for future loading.

saveRDS(op_er, file="modified_data/op_er/op_er.rds")

# remove everything from memory and run garbage collection before moving on
rm(list=ls())
gc()

# READING IN IP FILES #####
ip_fy19e <- read_sas("raw_data/unzipped/ip/all_ip_fy19e.sas7bdat")
ip_fy18e <- read_sas("raw_data/unzipped/ip/all_ip_fy18e.sas7bdat")
ip_fy17e <- read_sas("raw_data/unzipped/ip/all_ip_fy17e.sas7bdat")
ip_fy16e <- read_sas("raw_data/unzipped/ip/all_ip_fy16e.sas7bdat")
ip_fy15e <- read_sas("raw_data/unzipped/ip/all_ip_fy15e.sas7bdat")
ip_fy14e <- read_sas("raw_data/unzipped/ip/all_ip_fy14e.sas7bdat")
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# ip_fy13e <- read_sas("raw_data/unzipped/ip/all_ip_fy13e.sas7bdat")

# Drop all columns that aren't present in every file ####

drop_cols = c("vis_type", "TAPQ09", "PRELANG_CD", "kp_flag", "CHRO_FLAG", "TAPQ13", "ELIG_PQI", "ADMTHR", "RTC", "RET_MPR", "RET_ADX", "RET_SDX", "RET_PR2", "RET_NOR", "RET_NO2", "RET_COM", "RET_PR3", "RET_VCC", "RET_CC", "MAJSDRG", "PREOPTME", "OTHTME1", "OTHTME2", "OTHTME3", "OTHTME4", "OTHTME5", "OTHTME6", "OTHTME7", "OTHTME8", "OTHTME9", "OTHTME10", "C_PRIP", "C_PRIS1", "C_PRIS2", "C_PRIP3", "C_PRIP4", "C_PRIP5", "C_PRIP6", "C_PRIP7", "C_PRIP8", "C_PRIP9", "C_PRIP10", "C_PRIP11", "C_PRIP12", "C_PRIP13", "C_PRIP14", "GRP_RTN", "GRP_RTN2","PROC75","PROC76","PROC77","PROC78","PROC79","PROC80","PROC81","PROC82","PROC83","PROC84","PROC85","PROC86","PROC87","PROC88","PROC89","PROC90","PROC91","PROC92","PROC93","PROC94","PROC95","PROC96","PROC97","PROC98","PROC99","DIAG30","DIAG30POA","DIAG31","DIAG31POA","DIAG32","DIAG32POA","DIAG33","DIAG33POA","DIAG34","DIAG34POA","DIAG35","DIAG35POA","DIAG36","DIAG36POA","DIAG37","DIAG37POA","DIAG38","DIAG38POA","DIAG39","DIAG39POA","DIAG40","DIAG40POA","DIAG41","DIAG41POA","DIAG42","DIAG42POA","DIAG43","DIAG43POA","DIAG44","DIAG44POA","DIAG45","DIAG45POA","DIAG46","DIAG46POA","DIAG47","DIAG47POA","DIAG48","DIAG48POA","DIAG49","DIAG49POA","DIAG50","DIAG50POA","DIAG51","DIAG51POA","DIAG52","DIAG52POA","DIAG53","DIAG53POA","DIAG54","DIAG54POA","DIAG55","DIAG55POA","DIAG56","DIAG56POA","DIAG57","DIAG57POA","DIAG58","DIAG58POA","DIAG59","DIAG59POA","DIAG60","DIAG60POA","DIAG61","DIAG61POA","DIAG62","DIAG62POA","DIAG63","DIAG63POA","DIAG64","DIAG64POA","DIAG65","DIAG65POA","DIAG66","DIAG66POA","DIAG67","DIAG67POA","DIAG68","DIAG68POA","DIAG69","DIAG69POA","DIAG70","DIAG70POA","DIAG71","DIAG71POA","DIAG72","DIAG72POA","DIAG73","DIAG73POA","DIAG74","DIAG74POA","DIAG75","DIAG75POA","DIAG76","DIAG76POA","DIAG77","DIAG77POA","DIAG78","DIAG78POA","DIAG79","DIAG79POA","DIAG80","DIAG80POA","DIAG81","DIAG81POA","DIAG82","DIAG82POA","DIAG83","DIAG83POA","DIAG84","DIAG84POA","DIAG85","DIAG85POA","DIAG86","DIAG86POA","DIAG87","DIAG87POA","DIAG88","DIAG88POA","DIAG89","DIAG89POA","DIAG90","DIAG90POA","DIAG91","DIAG91POA","DIAG92","DIAG92POA","DIAG93","DIAG93POA","DIAG94","DIAG94POA","DIAG95","DIAG95POA","DIAG96","DIAG96POA","DIAG97","DIAG97POA","DIAG98","DIAG98POA","DIAG99","DIAG99POA","RCTUNT100","RCTUNT101","RCTUNT102","RCTUNT103","RCTUNT104","RCTUNT105","RCTUNT106","RCTUNT107","RCTUNT108","RCTUNT109","RCTUNT110","RCTUNT111","RCTUNT112","RCTUNT113","RCTUNT114","RCTUNT115","RCTUNT116","RCTUNT117","RCTUNT118","RCTUNT119","RCTCHG100","RCTCHG101","RCTCHG102","RCTCHG103","RCTCHG104","RCTCHG105","RCTCHG106","RCTCHG107","RCTCHG108","RCTCHG109","RCTCHG110","RCTCHG111","RCTCHG112","RCTCHG113","RCTCHG114","RCTCHG115","RCTCHG116","RCTCHG117","RCTCHG118","RCTCHG119","FILLER1","FILLER2","DIAG10POA","DIAG11POA","DIAG12POA","DIAG13POA","DIAG14POA","DIAG15POA","DIAG16POA","DIAG17POA","DIAG18POA","DIAG19POA","DIAG20POA","DIAG21POA","DIAG22POA","DIAG23POA","DIAG24POA","DIAG25POA","DIAG26POA","DIAG27POA","DIAG28POA","DIAG29POA","PAYER3","TERT_HMO","PROC30","PROC31","PROC32","PROC33","PROC34","PROC35","PROC36","PROC37","PROC38","PROC39","PROC40","PROC41","PROC42","PROC43","PROC44","PROC45","PROC46","PROC47","PROC48","PROC49","PROC50","PROC51","PROC52","PROC53","PROC54","PROC55","PROC56","PROC57","PROC58","PROC59","PROC60","PROC61","PROC62","PROC63","PROC64","PROC65","PROC66","PROC67","PROC68","PROC69","PROC70","PROC71","PROC72","PROC73","PROC74","DAG10POA","DAG11POA","DAG12POA","DAG13POA","DAG14POA","DAG15POA","DAG16POA","DAG17POA","DAG18POA","DAG19POA","DAG20POA","DAG21POA","DAG22POA","DAG23POA","DAG24POA","DAG25POA","DAG26POA","DAG27POA","DAG28POA","DAG29POA","PSYCHADM","PROC15","PROC16","PROC17","PROC18","PROC19","PROC20","PROC21","PROC22","PROC23","PROC24","RWHITE","RBLACK","RNAAMER","RASIAN","RHAWAI","ROTHER","RDECLIN","RUNKNOW","COUNTRY","PRELANG","PROC25","PROC26","PROC27","PROC28","PROC29","WINQI")

ip_fy19e <- ip_fy19e[ , !(names(ip_fy19e) %in% drop_cols)]
ip_fy18e <- ip_fy18e[ , !(names(ip_fy18e) %in% drop_cols)]
ip_fy17e <- ip_fy17e[ , !(names(ip_fy17e) %in% drop_cols)]
ip_fy16e <- ip_fy16e[ , !(names(ip_fy16e) %in% drop_cols)]
ip_fy15e <- ip_fy15e[ , !(names(ip_fy15e) %in% drop_cols)]
ip_fy14e <- ip_fy14e[ , !(names(ip_fy14e) %in% drop_cols)]
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# ip_fy13e <- ip_fy13e[ , !(names(ip_fy13e) %in% drop_cols)]

# ip_fy14e <- ip_fy14e[ , !(names(ip_fy14e) %in% c("TAPQ09", "PRELANG_CD", "kp_flag", "CHRO_FLAG", "TAPQ13", "ELIG_PQI", "ADMTHR", "RTC", "RET_MPR", "RET_ADX", "RET_SDX", "RET_PR2", "RET_NOR", "RET_NO2", "RET_COM", "RET_PR3", "RET_VCC", "RET_CC", "MAJSDRG", "PREOPTME", "OTHTME1", "OTHTME2", "OTHTME3", "OTHTME4", "OTHTME5", "OTHTME6", "OTHTME7", "OTHTME8", "OTHTME9", "OTHTME10", "C_PRIP", "C_PRIS1", "C_PRIS2", "C_PRIP3", "C_PRIP4", "C_PRIP5", "C_PRIP6", "C_PRIP7", "C_PRIP8", "C_PRIP9", "C_PRIP10", "C_PRIP11", "C_PRIP12", "C_PRIP13", "C_PRIP14", "GRP_RTN", "GRP_RTN2"))]

# Add field with fiscal year to keep track when combined #####
ip_fy19e$FY = '19'
ip_fy18e$FY = '18'
ip_fy17e$FY = '17'
ip_fy16e$FY = '16'
ip_fy15e$FY = '15'
ip_fy14e$FY = '14'
#### For now, don't load in 13, too many inconsistencies between fields year to year. Main reason: only 15 diagnosis codes
# ip_fy13e$FY = '13'

# Create data frames of column names again to check work
#cols_ip_fy19e <- as.data.frame(names(ip_fy19e))
#cols_ip_fy18e <- as.data.frame(names(ip_fy18e))
#cols_ip_fy17e <- as.data.frame(names(ip_fy17e))
#cols_ip_fy16e <- as.data.frame(names(ip_fy16e))
#cols_ip_fy15e <- as.data.frame(names(ip_fy15e))
#cols_ip_fy14e <- as.data.frame(names(ip_fy14e))
#cols_ip_fy13e <- as.data.frame(names(ip_fy13e))
#dif <- mapply(setdiff,cols_ip_fy14e,cols_ip_fy13e)

# Bind all data frames together ####
ip <- rbind(ip_fy19e, ip_fy18e, ip_fy17e, ip_fy16e, ip_fy15e, ip_fy14e) 

#clear all files except needed ones from workspace
rm(list=ls()[! ls() %in% c("ip")])
gc()

### Create a unique identifier for each row called row_unique_id. We'll use this to join back conditions data frame to master data frame later on. Note that it has an asterisk at start and end.  This is so later our looping through conditinos function doesn't get fucked up.

ip <- ip %>%
  rowid_to_column("rowid") %>%
  mutate(row_unique_id = paste0("*",rowid,"*")) %>%
  select(row_unique_id, everything(), -rowid)

# write it out to an rds file
saveRDS(ip, file="modified_data/ip/ip.rds")

#clear all files 
rm(list=ls())
gc()
