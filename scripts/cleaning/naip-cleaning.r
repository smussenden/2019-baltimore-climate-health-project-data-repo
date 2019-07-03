install.packages('rgdal')
library(rgdal)

#### Load NAIP data #### 
naip_2018_baltimore <- readGDAL("data/input-data/naip/ortho_1-1_hc_s_md510_2018_1/ortho_1-1_hc_s_md510_2018_1.sid")
