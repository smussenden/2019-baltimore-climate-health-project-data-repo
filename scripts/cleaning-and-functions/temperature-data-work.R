#####################
# TEMPERATURE CALCS # --------------------------------------------------------------------------------------
#####################

## HEADER ## --------------------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(forcats)
library(magrittr)

## SHORTCUTS ## --------------------------------------------------------------------------------------------

# cmd-shft-m:     %>%
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \

## WORK ## --------------------------------------------------------------------------------------------

# Load: temperature data (in Celcius)
btemp <- read_csv("data/temperature/temp-by-block.csv", # Load in data
                       col_types = cols(
                         BLOCKCE10 = col_double(),
                         GEOID10 = col_character(),
                         STATEFP10 = col_character(),
                         BLOCKCE10 = col_character(),
                         ALAND10 = col_character(),
                         AWATER10 = col_character(),
                         INTPTLAT10 = col_character(),
                         INTPTLON10 = col_character()
                         ) # Define uncertain data type
) %>%
  filter(COUNTYFP10 == 510) %>% # Filter for just Balt. City
  rename_all(tolower) # Lowercase the col/var names

# Load: neighborhood names (nsa = neighborhood statistical area)
nsa_names <- read_csv("data/NSA-block-crosswalk-2010-frombaltcity.csv", # Load in data
                      col_types = cols(.default = "c") # Import all cols as characters
) %>%
  rename_all(tolower)

# Join to names table
btemp_joined <- btemp %>%
  left_join(nsa_names,
            by = c("geoid10" = "blk2010")) %>%
  mutate(nsa_name=replace(nsa_name, is.na(nsa_name), "Unnamed"))

# Consolidate statistics BY NEIGHBORHOOD
btree_nsa_statistics <- btemp_joined %>%
  group_by(nsa_name) %>% # Group the blocks by neighborhood
  summarize(temp_mean = mean(temp_mean),
            temp_median = median(temp_median),
            temp_min = min(temp_min),
            temp_max = min(temp_max)
            )


## EXPORT STATISTICS -------------------------------------------------------------------------------------------
write_csv(btree_nsa_statistics, "data/temperature/temp_by_nsa.csv")
