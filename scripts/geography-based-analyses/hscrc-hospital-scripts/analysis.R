# JAKE GLUCK - CAPITAL NEWS SERVICE 
#### Maryland Hospital Data Cleaning Script ####
#### None of the data is stored publicly #####
#### Kept on air-gapped password protected computer #####
#### Limits on sharing data in numbers greater than 10 publicly ####
#### Original in SAS format ####

#### Questions ####
# We want to do all of the below queries for inpatient and outpatient (emergency room visits only). We also want to do for all years in the database and also by year. 
# For the medical diagnoses we care about, we need to calculate prevalance for each condition by zip code.  We are going to calculate prevalance two different ways.  The first is to do a count of cases with those diagnoses in each zip code, and divide it by the total number of ALL cases in that zip code.  We want to be able to say something like: "in 21012, 12 percent of cases had a diagnosis of X medical condition." The second way is to do a count of cases with that medical condition, and divide it by total populatoin of that zip code (pulled from census).
# We also want to do a time series analysis that connections prevalnce of these conditions -- perhaps on a day by day basis or week by week basis -- to temperature fluctuations. Want to look city wide for all zips, and neighbhorhood by neighborhood.  Are there more heart problem diagnosis when it gets colder or hotter?  What correlations exist.  What time series trends exist?

############### In Patient #################


#### per zip ####

balt_zips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(balt_zips) -> balt_zips

zip_code = NULL
asthma_cases = NULL
circu_cases = NULL
mental_cases = NULL
respir_cases = NULL
all_cases = NULL
perc_asthma = NULL
perc_circu = NULL
perc_mental = NULL
perc_respir = NULL

for (zip in balt_zips){
  
  zip_code <- append(zip_code, zip)
  
  this_zip_asthma <- subset(asthma_ip, asthma_ip$ZIPCODE == zip)
  this_zip_circu <- subset(circu_ip, circu_ip$ZIPCODE == zip)
  this_zip_mental <- subset(mental_ip, mental_ip$ZIPCODE == zip)
  this_zip_respir <- subset(respir_ip, respir_ip$ZIPCODE == zip)
  this_zip_all <- subset(balt_ip, balt_ip$ZIPCODE == zip)
  
  asthma_cases <- append(asthma_cases, nrow(this_zip_asthma))
  circu_cases <- append(circu_cases, nrow(this_zip_circu))
  mental_cases <- append(mental_cases, nrow(this_zip_mental))
  respir_cases <- append(respir_cases, nrow(this_zip_respir))
  all_cases <- append(all_cases, nrow(this_zip_all))

  perc_asthma <- append(perc_asthma, (nrow(this_zip_asthma)/nrow(this_zip_all)))
  perc_circu <- append(perc_circu, (nrow(this_zip_circu)/nrow(this_zip_all)))
  perc_mental <- append(perc_mental, (nrow(this_zip_mental)/nrow(this_zip_all)))
  perc_respir <- append(perc_respir, (nrow(this_zip_respir)/nrow(this_zip_all)))
}

ip_balt_by_zip = data.frame(zip_code,
                asthma_cases,
                circu_cases,
                mental_cases,
                respir_cases,
                all_cases,
                perc_asthma,
                perc_circu,
                perc_mental,
                perc_respir)

##### BY YEAR #######

years <- c(14,15,16,17,18)
as.character(years) -> years

fiscal_year = NULL
asthma_cases = NULL
circu_cases = NULL
mental_cases = NULL
respir_cases = NULL
all_cases = NULL
perc_asthma = NULL
perc_circu = NULL
perc_mental = NULL
perc_respir = NULL

for (year in years){
  print(year)
  fiscal_year <- append(fiscal_year, year)
  
  this_year_asthma <- subset(asthma_ip, asthma_ip$FY == year)
  this_year_circu <- subset(circu_ip, circu_ip$FY == year)
  this_year_mental <- subset(mental_ip, mental_ip$FY == year)
  this_year_respir <- subset(respir_ip, respir_ip$FY == year)
  this_year_all <- subset(balt_ip, balt_ip$FY == year)
  
  asthma_cases <- append(asthma_cases, nrow(this_year_asthma))
  circu_cases <- append(circu_cases, nrow(this_year_circu))
  mental_cases <- append(mental_cases, nrow(this_year_mental))
  respir_cases <- append(respir_cases, nrow(this_year_respir))
  all_cases <- append(all_cases, nrow(this_year_all))
  
  perc_asthma <- append(perc_asthma, (nrow(this_year_asthma)/nrow(this_year_all)))
  perc_circu <- append(perc_circu, (nrow(this_year_circu)/nrow(this_year_all)))
  perc_mental <- append(perc_mental, (nrow(this_year_mental)/nrow(this_year_all)))
  perc_respir <- append(perc_respir, (nrow(this_year_respir)/nrow(this_year_all)))
}

ip_balt_by_year = data.frame(fiscal_year,
                      asthma_cases,
                      circu_cases,
                      mental_cases,
                      respir_cases,
                      all_cases,
                      perc_asthma,
                      perc_circu,
                      perc_mental,
                      perc_respir)

##### BY BOTH YEAR AND QTR AND ZIP #######

balt_zips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(balt_zips) -> balt_zips

years <- c(14,15,16,17,18)
as.character(years) -> years

qtrs <- c(1,2,3,4)
as.character(qtrs) -> qtrs

quarter = NULL
fiscal_year = NULL
zip_code = NULL
asthma_cases = NULL
circu_cases = NULL
mental_cases = NULL
respir_cases = NULL
all_cases = NULL
perc_asthma = NULL
perc_circu = NULL
perc_mental = NULL
perc_respir = NULL

for (qtr in qtrs){
  print(qtr)
  
  this_qtr_asthma <- subset(asthma_ip, asthma_ip$QTR == qtr)
  this_qtr_circu <- subset(circu_ip, circu_ip$QTR == qtr)
  this_qtr_mental <- subset(mental_ip, mental_ip$QTR == qtr)
  this_qtr_respir <- subset(respir_ip, respir_ip$QTR == qtr)
  this_qtr_all <- subset(balt_ip, balt_ip$QTR == qtr)
  
  for (year in years){
    print(year)
    this_year_asthma <- subset(this_qtr_asthma, this_qtr_asthma$FY == year)
    this_year_circu <- subset(this_qtr_circu, this_qtr_circu$FY == year)
    this_year_mental <- subset(this_qtr_mental, this_qtr_mental$FY == year)
    this_year_respir <- subset(this_qtr_respir, this_qtr_respir$FY == year)
    this_year_all <- subset(this_qtr_all, this_qtr_all$FY == year)
    
    for (zip in balt_zips){
      quarter <- append(quarter, qtr)
      fiscal_year <- append(fiscal_year, year)
      zip_code <- append(zip_code, zip)
      
      this_zip_asthma <- subset(asthma_ip, this_year_asthma$ZIPCODE == zip)
      this_zip_circu <- subset(circu_ip, this_year_circu$ZIPCODE == zip)
      this_zip_mental <- subset(mental_ip, this_year_mental$ZIPCODE == zip)
      this_zip_respir <- subset(respir_ip, this_year_respir$ZIPCODE == zip)
      this_zip_all <- subset(balt_ip, this_year_all$ZIPCODE == zip)
      
      asthma_cases <- append(asthma_cases, nrow(this_zip_asthma))
      circu_cases <- append(circu_cases, nrow(this_zip_circu))
      mental_cases <- append(mental_cases, nrow(this_zip_mental))
      respir_cases <- append(respir_cases, nrow(this_zip_respir))
      all_cases <- append(all_cases, nrow(this_zip_all))
      
      perc_asthma <- append(perc_asthma, (nrow(this_zip_asthma)/nrow(this_zip_all)))
      perc_circu <- append(perc_circu, (nrow(this_zip_circu)/nrow(this_zip_all)))
      perc_mental <- append(perc_mental, (nrow(this_zip_mental)/nrow(this_zip_all)))
      perc_respir <- append(perc_respir, (nrow(this_zip_respir)/nrow(this_zip_all)))
    }
  }
}

ip_balt_by_qtr = data.frame(quarter,
                             fiscal_year,
                             zip_code,
                             asthma_cases,
                             circu_cases,
                             mental_cases,
                             respir_cases,
                             all_cases,
                             perc_asthma,
                             perc_circu,
                             perc_mental,
                             perc_respir)

##### BY BOTH YEAR AND ZIP #######

balt_zips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(balt_zips) -> balt_zips

years <- c(14,15,16,17,18)
as.character(years) -> years

fiscal_year = NULL
zip_code = NULL
asthma_cases = NULL
circu_cases = NULL
mental_cases = NULL
respir_cases = NULL
all_cases = NULL
perc_asthma = NULL
perc_circu = NULL
perc_mental = NULL
perc_respir = NULL

for (year in years){
  print(year)
  
  this_year_asthma <- subset(asthma_ip, asthma_ip$FY == year)
  this_year_circu <- subset(circu_ip, circu_ip$FY == year)
  this_year_mental <- subset(mental_ip, mental_ip$FY == year)
  this_year_respir <- subset(respir_ip, respir_ip$FY == year)
  this_year_all <- subset(balt_ip, balt_ip$FY == year)
  
  for (zip in balt_zips){
    fiscal_year <- append(fiscal_year, year)
    zip_code <- append(zip_code, zip)
    
    this_zip_asthma <- subset(asthma_ip, this_year_asthma$ZIPCODE == zip)
    this_zip_circu <- subset(circu_ip, this_year_circu$ZIPCODE == zip)
    this_zip_mental <- subset(mental_ip, this_year_mental$ZIPCODE == zip)
    this_zip_respir <- subset(respir_ip, this_year_respir$ZIPCODE == zip)
    this_zip_all <- subset(balt_ip, this_year_all$ZIPCODE == zip)
    
    asthma_cases <- append(asthma_cases, nrow(this_zip_asthma))
    circu_cases <- append(circu_cases, nrow(this_zip_circu))
    mental_cases <- append(mental_cases, nrow(this_zip_mental))
    respir_cases <- append(respir_cases, nrow(this_zip_respir))
    all_cases <- append(all_cases, nrow(this_zip_all))
    
    perc_asthma <- append(perc_asthma, (nrow(this_zip_asthma)/nrow(this_zip_all)))
    perc_circu <- append(perc_circu, (nrow(this_zip_circu)/nrow(this_zip_all)))
    perc_mental <- append(perc_mental, (nrow(this_zip_mental)/nrow(this_zip_all)))
    perc_respir <- append(perc_respir, (nrow(this_zip_respir)/nrow(this_zip_all)))
  }
 
}

ip_balt_by_both = data.frame(fiscal_year,
                             zip_code,
                             asthma_cases,
                             circu_cases,
                             mental_cases,
                             respir_cases,
                             all_cases,
                             perc_asthma,
                             perc_circu,
                             perc_mental,
                             perc_respir)


cols_balt_ip <- as.data.frame(names(balt_ip))
# cols_circu_obs_op <- names(circu_obs_op)

age_group <- balt_ip %>%
  group_by(AGE_GROUP) %>%
  summarise(count=n())

#Quarters
ip_balt_qtrs <- balt_ip %>%
  group_by(YEAR, QTR) %>%
  summarise(count = n())

#Ages
ip_balt_ages <- balt_ip %>%
  group_by(YEAR, QTR, AGE_GROUP) %>%
  summarise(count = n()) %>%
  spread(AGE_GROUP, count)

#Died
ip_balt_died <- balt_ip %>%
  group_by(YEAR, QTR, ZIPCODE, DIED)  %>%
  filter(DIED==1) %>%
  summarise(count = n()) 

#Sex
ip_balt_sex <- balt_ip %>%
  group_by(YEAR, QTR, ZIPCODE, SEX)  %>%
  summarise(count = n()) 

#all combined problems
ip_balt_all_cases <- balt_ip %>%
  group_by(YEAR, QTR, ZIPCODE)  %>%
  filter(asthma==TRUE)%>%
  filter(circu==TRUE) %>%
  filter(respir==TRUE) %>%
  filter(mental==TRUE) %>%
  summarise(count = n()) 
  
cols_balt_ip <- as.data.frame(names(balt_ip))

small_cols <- as.vector(c("HOSPID","NATADM","SOURCADM_IP","SEX","ETHNICIT","MARISTAT","ZIPCODE","PRIN_HMO","SECN_HMO","PAT_DISP_IP","PRIN_HMO","SECN_HMO","PAT_DISP_IP","PAYER1","PAYER2","MAJSERVI","DAILYSER","NONPSYCD","PSYCHDAY","MSGDAYS","CCUDAYS","BURNDAYS","NEO_DAYS","PIC_DAYS","TRM_DAYS","OTHRDAYS","BIRTHWGT","PSYCHADM","ICDFLAG","PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","DIAG29","E_CODE","R_FLAG","PRINPROC","PROC1","PROC2","PROC3","PROC4","PROC5","PROC6","PROC7","PROC8","PROC9","PROC10","PROC11","PROC12","PROC13","PROC14","PROC15","PROC16","PROC17","PROC18","PROC19","PROC20","PROC21","PROC22","PROC23","PROC24","PROC25","PROC26","PROC27","PROC28","PROC29","RHB_AC","RHB_IG","PDIAGPOA","TOT_CHG","ATPHYNPI","OPPHYNPI","SEVERITY","RACE","COUNTRY","PRELANG","RESIDENT_STATUS","LOS","SAS_COUNTY","QTR","YEAR","DATATYPE","AGE_GROUP","READM30","DIED","FYEAR","UNIQUEID","WEIGHT","PQI","PAU_READMIT","PAU_READMIT_CHG","PAU_PQI","PAU_PQI_CHG","PQI_ELIG"))

very_small_cols <- as.vector(c("QTR","YEAR","DATATYPE","DIED","FYEAR","AGE_GROUP","UNIQUEID","HOSPID_O","WEIGHT","PQI","FY"))

balt_ip_small <- balt_ip[,small_cols]

balt_ip_small$asthma <- ifelse(balt_ip$UNIQUEID %in% asthma_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$circu <- ifelse(balt_ip$UNIQUEID %in% circu_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$mental <- ifelse(balt_ip$UNIQUEID %in% mental_ip$UNIQUEID, TRUE, FALSE)
balt_ip_small$respir <- ifelse(balt_ip$UNIQUEID %in% respir_ip$UNIQUEID, TRUE, FALSE)

rm(list=ls()[! ls() %in% c("obs_op","ip", "asthma_obs_op", "circu_obs_op", "respir_obs_op", "mental_obs_op", "balt_obs_op", "asthma_ip", "circu_ip", "respir_ip", "mental_ip", "balt_ip", "ip_balt_by_zip", "ip_balt_by_year", "ip_balt_by_both", "ip_balt_by_qtr", "ip_balt_qtr","ip_balt_ages", "ip_balt_died", "ip_balt_sex", "balt_ip_small")])
