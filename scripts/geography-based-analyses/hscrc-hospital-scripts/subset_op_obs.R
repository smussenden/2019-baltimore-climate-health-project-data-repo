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

obs_op = readRDS("modified_data/obs_op.rds")

#### op_obs DATA ############################################################################################################

baltzips <- c(21201,21202,21205,21206,21207,21208,21209,21210,21211,21212,21213,21214,21215,21216,21217,21218,21222,21223,21224,21225,21226,21227,21228,21229,21230,21231,21234,21236,21237,21239,21251)
as.character(baltzips) -> baltzip

# Filter op_obs data for Baltimore cases
balt_op_obs <- subset(op_obs, op_obs$ZIPCODE %in% baltzips)
balt_op_obs <- balt_op_obs %>%
  filter(VIS_TYPE == "02")
diag_codes <- c("PRINDIAG","DIAG1","DIAG2","DIAG3","DIAG4","DIAG5","DIAG6","DIAG7","DIAG8","DIAG9","DIAG10","DIAG11","DIAG12","DIAG13","DIAG14","DIAG15","DIAG16","DIAG17","DIAG18","DIAG19","DIAG20","DIAG21","DIAG22","DIAG23","DIAG24","DIAG25","DIAG26","DIAG27","DIAG28","DIAG29")

op_obs_column_names = intersect(colnames(balt_op_obs),diag_codes)

#### Filter Baltimore op_obs cases for diabetes conditions ####
temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  dplyr::filter_all(any_vars(startsWith(.,"249") | startsWith(.,"2250")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"E08") | startsWith(.,"E09") | startsWith(.,"E10") | startsWith(.,"E11") |  startsWith(.,"E12") | startsWith(.,"E13")))

temp3 <- rbind(temp1, temp2)
diabetes_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for thyroid conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"240") | startsWith(.,"241") | startsWith(.,"242") | startsWith(.,"243") | startsWith(.,"244") | startsWith(.,"245") | startsWith(.,"246")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"E00") | startsWith(.,"E01") | startsWith(.,"E02") | startsWith(.,"E03") | startsWith(.,"E04") | startsWith(.,"E05") | startsWith(.,"E06") | startsWith(.,"E07")))

temp3 <- rbind(temp1, temp2)
thyroid_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for circu conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"39") | startsWith(.,"40") | startsWith(.,"42") | startsWith(.,"43") | startsWith(.,"44") | startsWith(.,"45")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I")))

temp3 <- rbind(temp1, temp2)
circu_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heartdisease conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"393") | startsWith(.,"394") | startsWith(.,"395") | startsWith(.,"396") | startsWith(.,"4397")| startsWith(.,"398") | startsWith(.,"399") | startsWith(.,"40") | startsWith(.,"41") | startsWith(.,"42")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I05") | startsWith(.,"I06") | startsWith(.,"I07") | startsWith(.,"I08") | startsWith(.,"I09") | startsWith(.,"I1") | startsWith(.,"I2") | startsWith(.,"I3") | startsWith(.,"I4") | startsWith(.,"I50") | startsWith(.,"I51") | startsWith(.,"I52")))

temp3 <- rbind(temp1, temp2)
heartdisease_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for aginapectoris conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"413")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I20")))

temp3 <- rbind(temp1, temp2)
aginapectoris_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for congestivearterydisease conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"428")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I50.9")))

temp3 <- rbind(temp1, temp2)
congestivearterydisease_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for cerebrovasculardisease conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"43")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I6")))

temp3 <- rbind(temp1, temp2)
cerebrovasculardisease_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heartattack conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"410")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I21")))

temp3 <- rbind(temp1, temp2)
heartattack_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for hypertension conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"401") | startsWith(.,"402") | startsWith(.,"403") | startsWith(.,"404") | startsWith(.,"405")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"I10") | startsWith(.,"I11") | startsWith(.,"I12") | startsWith(.,"I13") | startsWith(.,"I14") | startsWith(.,"I15") | startsWith(.,"I16")))

temp3 <- rbind(temp1, temp2)
hypertension_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for respir conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"46") | startsWith(.,"47") | startsWith(.,"48") | startsWith(.,"49") | startsWith(.,"50") | startsWith(.,"51")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J")))

temp3 <- rbind(temp1, temp2)
respir_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

saveRDS(diabetes_op_obs, file="modified_data/diabetes_op_obs.rds")
saveRDS(thyroid_op_obs, file="modified_data/thyroid_op_obs.rds")
saveRDS(circu_op_obs, file="modified_data/circu_op_obs.rds")
saveRDS(heartdisease_op_obs, file="modified_data/heartdisease_op_obs.rds")
saveRDS(aginapectoris_op_obs, file="modified_data/aginapectoris_op_obs.rds")
saveRDS(congestivearterydisease_op_obs, file="modified_data/congestivearterydisease_op_obs.rds")
saveRDS(cerebrovasculardisease_op_obs, file="modified_data/cerebrovasculardisease_op_obs.rds")
saveRDS(heartattack_op_obs, file="modified_data/heartattack_op_obs.rds")
saveRDS(hypertension_op_obs, file="modified_data/hypertension_op_obs.rds")
saveRDS(respir_op_obs, file="modified_data/respir_op_obs.rds")

#### Filter Baltimore op_obs cases for asthma conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"493")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J45")))

temp3 <- rbind(temp1, temp2)
asthma_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for cop_obsd conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"490") | startsWith(.,"491") | startsWith(.,"492") | startsWith(.,"493") | startsWith(.,"494") | startsWith(.,"495") | startsWith(.,"496")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J40") | startsWith(.,"J41") | startsWith(.,"J42") | startsWith(.,"J43") | startsWith(.,"J44") | startsWith(.,"J45") | startsWith(.,"J46") | startsWith(.,"J47")))

temp3 <- rbind(temp1, temp2)
cop_obsd_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for bronchitis conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"466") | startsWith(.,"490") | startsWith(.,"491")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J20") | startsWith(.,"J21") | startsWith(.,"J40") | startsWith(.,"J41") | startsWith(.,"J42")))

temp3 <- rbind(temp1, temp2)
bronchitis_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for acutebronchitis conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"466")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J20")))

temp3 <- rbind(temp1, temp2)
acutebronchitis_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for influenza conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"487") | startsWith(.,"488")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"J09") | startsWith(.,"J10") | startsWith(.,"J11")))

temp3 <- rbind(temp1, temp2)
influenza_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for mental conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"29") | startsWith(.,"30") | startsWith(.,"31")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"F")))

temp3 <- rbind(temp1, temp2)
mental_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for kidney conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"584") | startsWith(.,"585") | startsWith(.,"586")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"N17") | startsWith(.,"N18") | startsWith(.,"N19")))

temp3 <- rbind(temp1, temp2)
kidney_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for osteoarthritis conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"715")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"M15") | startsWith(.,"M16") | startsWith(.,"M17") | startsWith(.,"M18") | startsWith(.,"M19")))

temp3 <- rbind(temp1, temp2)
osteoarthritis_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

saveRDS(asthma_op_obs, file="modified_data/asthma_op_obs.rds")
saveRDS(cop_obsd_op_obs, file="modified_data/cop_obsd_op_obs.rds")
saveRDS(bronchitis_op_obs, file="modified_data/bronchitis_op_obs.rds")
saveRDS(acutebronchitis_op_obs, file="modified_data/acutebronchitis_op_obs.rds")
saveRDS(influenza_op_obs, file="modified_data/influenza_op_obs.rds")
saveRDS(mental_op_obs, file="modified_data/mental_op_obs.rds")
saveRDS(kidney_op_obs, file="modified_data/kidney_op_obs.rds")
saveRDS(osteoarthritis_op_obs, file="modified_data/osteoarthritis_op_obs.rds")

#### Filter Baltimore op_obs cases for hypothermia conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"991.6")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T68")))

temp3 <- rbind(temp1, temp2)
hypothermia_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heatstroke conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"992.0")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T67.0")))

temp3 <- rbind(temp1, temp2)
heatstroke_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for carbonmonoxide conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"986")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T57")))

temp3 <- rbind(temp1, temp2)
carbonmonoxide_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heatcramps conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"992.2")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T67.2")))

temp3 <- rbind(temp1, temp2)
heatcramps_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heatsyncop_obse conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"992.1")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T67.1")))

temp3 <- rbind(temp1, temp2)
heatsyncop_obse_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for heatexhaustion conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"992.3") | startsWith(.,"992.4") | startsWith(.,"992.5")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"T67.3") | startsWith(.,"T67.4") | startsWith(.,"T67.5")))

temp3 <- rbind(temp1, temp2)
heatexhaustion_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for nbcoldsyndrome conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"778.2")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"P80")))

temp3 <- rbind(temp1, temp2)
nbcoldsyndrome_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))

#### Filter Baltimore op_obs cases for urticaria conditions ####

temp1 <- balt_op_obs %>%
  filter(ICDFLAG == "9") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"708")))

temp2 <- balt_op_obs %>%
  filter(ICDFLAG == "0") %>%
  select(op_obs_column_names) %>%
  filter_all(any_vars(startsWith(.,"L50.2")))

temp3 <- rbind(temp1, temp2)
urticaria_op_obs <- unique(inner_join(x = balt_op_obs, y = temp3))


####### clean and save results #####

# rm(list=ls()[! ls() %in% c("obs_op_obs","ip", "balt_ip", "asthma_obs_op_obs", "circu_obs_op_obs", "respir_obs_op_obs", "mental_obs_op_obs", "balt_obs_op_obs", "diabetes_ip","thyroid_ip","circu_ip","heartdisease_ip","aginapectoris_ip","congestivearterydisease_ip","cerebrovasculardisease_ip","heartattack_ip","hypertension_ip","respir_ip","asthma_ip","cop_obsd_ip","bronchitis_ip","acutebronchitis_ip","influenza_ip","mental_ip","kidney_ip","osteoarthritis_ip","hypothermia_ip","heatstroke_ip","carbonmonoxide_ip","heatcramps_ip","heatsyncop_obse_ip","heatexhaustion_ip","nbcoldsyndrome_ip","urticaria_ip", "balt_ip","ip_fy19e","ip_fy18e","ip_fy17e","ip_fy16e","ip_fy15e","ip_fy14e","ip_fy13e")])



saveRDS(hypothermia_op_obs, file="modified_data/hypothermia_op_obs.rds")
saveRDS(heatstroke_op_obs, file="modified_data/heatstroke_op_obs.rds")
saveRDS(carbonmonoxide_op_obs, file="modified_data/carbonmonoxide_op_obs.rds")
saveRDS(heatcramps_op_obs, file="modified_data/heatcramps_op_obs.rds")
saveRDS(heatsyncop_obse_op_obs, file="modified_data/heatsyncop_obse_op_obs.rds")
saveRDS(heatexhaustion_op_obs, file="modified_data/heatexhaustion_op_obs.rds")
saveRDS(nbcoldsyndrome_op_obs, file="modified_data/nbcoldsyndrome_op_obs.rds")
saveRDS(urticaria_op_obs, file="modified_data/urticaria_op_obs.rds")



# this is to view how many row in fy19 have values in diag 30 to 99

# keep_cols = c("DIAG30","DIAG30POA","DIAG31","DIAG31POA","DIAG32","DIAG32POA","DIAG33","DIAG33POA","DIAG34","DIAG34POA","DIAG35","DIAG35POA","DIAG36","DIAG36POA","DIAG37","DIAG37POA","DIAG38","DIAG38POA","DIAG39","DIAG39POA","DIAG40","DIAG40POA","DIAG41","DIAG41POA","DIAG42","DIAG42POA","DIAG43","DIAG43POA","DIAG44","DIAG44POA","DIAG45","DIAG45POA","DIAG46","DIAG46POA","DIAG47","DIAG47POA","DIAG48","DIAG48POA","DIAG49","DIAG49POA","DIAG50","DIAG50POA","DIAG51","DIAG51POA","DIAG52","DIAG52POA","DIAG53","DIAG53POA","DIAG54","DIAG54POA","DIAG55","DIAG55POA","DIAG56","DIAG56POA","DIAG57","DIAG57POA","DIAG58","DIAG58POA","DIAG59","DIAG59POA","DIAG60","DIAG60POA","DIAG61","DIAG61POA","DIAG62","DIAG62POA","DIAG63","DIAG63POA","DIAG64","DIAG64POA","DIAG65","DIAG65POA","DIAG66","DIAG66POA","DIAG67","DIAG67POA","DIAG68","DIAG68POA","DIAG69","DIAG69POA","DIAG70","DIAG70POA","DIAG71","DIAG71POA","DIAG72","DIAG72POA","DIAG73","DIAG73POA","DIAG74","DIAG74POA","DIAG75","DIAG75POA","DIAG76","DIAG76POA","DIAG77","DIAG77POA","DIAG78","DIAG78POA","DIAG79","DIAG79POA","DIAG80","DIAG80POA","DIAG81","DIAG81POA","DIAG82","DIAG82POA","DIAG83","DIAG83POA","DIAG84","DIAG84POA","DIAG85","DIAG85POA","DIAG86","DIAG86POA","DIAG87","DIAG87POA","DIAG88","DIAG88POA","DIAG89","DIAG89POA","DIAG90","DIAG90POA","DIAG91","DIAG91POA","DIAG92","DIAG92POA","DIAG93","DIAG93POA","DIAG94","DIAG94POA","DIAG95","DIAG95POA","DIAG96","DIAG96POA","DIAG97","DIAG97POA","DIAG98","DIAG99")



# op_obs_fy192e <- op_obs_fy19e[ , (names(op_obs_fy19e) %in% keep_cols)]

