-   [Introduction](#introduction)
-   [Links to Data, Cleaning and Analysis Scripts, RMarkdown HTML
    Pages](#links-to-data-cleaning-and-analysis-scripts-rmarkdown-html-pages)
    -   [Load packages](#load-packages)
    -   [Load variables and data](#load-variables-and-data)
-   [Line-by-Line Fact Check \[Rising
    Heat\]](#line-by-line-fact-check-rising-heat)
    -   [Fact: Franklin Square heat
        \[cq\]](#fact-franklin-square-heat-cq)
    -   [Fact: Franklin Square poverty
        \[cq\]](#fact-franklin-square-poverty-cq)
    -   [Fact: Heat and poverty \[cq\]](#fact-heat-and-poverty-cq)
    -   [Fact: Neighborhood heat difference
        \[cq\]](#fact-neighborhood-heat-difference-cq)
    -   [Fact: EMS calls for heat stroke
        \[cq\]](#fact-ems-calls-for-heat-stroke-cq)
    -   [Fact: EMS calls for chronic conditions
        \[cq\]](#fact-ems-calls-for-chronic-conditions-cq)
    -   [Fact: Medicaid and chronic conditions
        \[cq\]](#fact-medicaid-and-chronic-conditions-cq)
    -   [Fact: Franklin Square tree canopy
        \[cq\]](#fact-franklin-square-tree-canopy-cq)
-   [Line-by-Line Fact Check \[Mental
    Health\]](#line-by-line-fact-check-mental-health)
    -   [Fact: Rate of increase of psychiatric calls
        \[cq\]](#fact-rate-of-increase-of-psychiatric-calls-cq)
    -   [Fact: Substance abuse calls rate
        \[cq\]](#fact-substance-abuse-calls-rate-cq)

Introduction
------------

This R markdown document describes a portion of the data analysis for a
reporting project examining the effects of climate-change driven
temperature increases on the health of people who live in cities. The
project was done in partnership with the [University of Maryland Philip
Merrill College of Journalism](https://merrill.umd.edu/), [Capital News
Service](https://cnsmaryland.org/), [the Howard Center for Investigative
Journalism](https://merrill.umd.edu/about-merrill/signature-programs/the-howard-center-for-investigative-journalism/),
[NPR](https://www.npr.org/), [Wide Angle Youth
Media](https://www.wideanglemedia.org/) and
[WMAR](https://www.wmar2news.com/). It also moved on the Associated
Press wire.

For each sentence in the stories “[As Rising Heat Bakes U.S. Cities, The
Poor Often Feel It
Most](https://www.npr.org/templates/story/story.php?storyId=754044732&live=1)”
and “[How High Heat Can Impact Mental
Health](https://www.npr.org/templates/story/story.php?storyId=757034136&live=1)”
based on Howard Center data analysis, this document provides the
original fact, the code and code output that support that fact, and an
explanation where necessary.

Here are links to stories in the series published by participating
organizations:

**CNSMaryland**

-   [Code Red: Baltimore’s Climate
    Divide](https://cnsmaryland.org/interactives/summer-2019/code-red/introduction.html)
-   [Heat & Inequality: In Baltimore, the burden of rising temperatures
    isn’t
    shared](https://cnsmaryland.org/interactives/summer-2019/code-red/neighborhood-heat-inequality.html)
-   [Heat & Health: For people with chronic health conditions, heat and
    humidity are more than a summer
    nuisance](https://cnsmaryland.org/interactives/summer-2019/code-red/heat-health.html)
-   [The Role of Trees: No trees, no shade, no relief as climate heats
    up](https://cnsmaryland.org/interactives/summer-2019/code-red/role-of-trees.html)
-   [Seeking Solutions: Are government leaders and residents ready to
    act?](https://cnsmaryland.org/interactives/summer-2019/code-red/city-climate-future.html)
-   [Behind The Scenes: A look at how and why we reported the
    series](https://cnsmaryland.org/interactives/summer-2019/code-red/behind-scenes.html)

**NPR**

-   [As Rising Heat Bakes U.S. Cities, The Poor Often Feel It
    Most](https://www.npr.org/2019/09/03/754044732/as-rising-heat-bakes-u-s-cities-the-poor-often-feel-it-most)
-   [Trees Are Key To Fighting Urban Heat — But Cities Keep Losing
    Them](https://www.npr.org/2019/09/04/755349748/trees-are-key-to-fighting-urban-heat-but-cities-keep-losing-them)
-   [How High Heat Can Impact Mental
    Health](https://www.npr.org/templates/story/story.php?storyId=757034136&live=1)

**WMAR**

-   [Baltimore neighborhood identified as ‘ground zero’ for local
    effects of climate
    crisis](https://www.wmar2news.com/news/region/baltimore-city/baltimore-neighborhood-identified-as-ground-zero-for-local-effects-of-climate-crisis)

**Associated Press**

-   [Investigation: Urban poor hit hardest as the planet heats
    up](https://www.apnews.com/52ffefa9ecf144d1b9aec367cc52ea74)
-   [As temperatures rise, overdoses and hospital visits
    increase](https://www.apnews.com/4d83616b5ecd46d5896d274a37b39de9)
-   [No trees, no shade, no relief in cities as climate heats
    up](https://www.apnews.com/bebe7895ce374f6f9070fe69ab557c4f)

Links to Data, Cleaning and Analysis Scripts, RMarkdown HTML Pages
------------------------------------------------------------------

-   The entire codebase for this analysis, including R markdown cleaning
    and analysis scripts – and the raw and cleaned data needed to
    reproduce the cleaning and analysis, is available through the
    [Howard Center’s
    GitHub](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide).
    -   [Repo
        Readme](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/README.md)
    -   Data
        -   [Data cleaning
            script](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Data-Cleaning/Data-Cleaning.Rmd)
        -   [Raw data
            folder](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/data/input-data)
        -   [Cleaned data
            folder](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/data/putput-data)
    -   Analysis
        -   [Analysis for
            NPR](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/NPR/NPR-Analysis.Rmd)
        -   [Heat and inequality story
            analysis](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Heat-Inequality/Heat-Inequality-Analysis.Rmd)
        -   [Heat and health story
            analysis](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Heat-Health/Heat-Health-Analysis.Rmd)
        -   [Role of trees story
            analysis](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Role-of-Trees/Role-of-Trees-Analysis.Rmd)
-   A site with HTML versions of the R markdown files, for fact-checking
    the completed the analysis.
    -   [Landing
        page](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/index.html)
    -   [Data cleaning
        script](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/Data-Cleaning.html)
    -   [Analysis for
        NPR](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/NPR-Analysis.html)
    -   [Heat and inequality story
        analysis](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/Heat-Inequality-Analysis.html)
    -   [Heat and health story
        analysis](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/Heat-Health-Analysis.html)
    -   [Role of trees story
        analysis](https://howard-center-investigations.github.io/code-red-baltimore-climate-divide/Role-of-Trees-Analysis.html)
-   NPR has made available a separate GitHub repo for nationwide
    analysis on urban heat islands available
    [here](https://github.com/nprapps/heat-income).

### Load packages

``` r
#######################
#### Load Packages ####
#######################
# For debugging rm(list=ls())

library(tidyverse) # For general data science goodness
library(corrr) # For correlation goodness
library(lubridate) # For working with that datetime
library(spelling) # spellcheck

# Turn off scientific notation in RStudio (prevents coersion to character type)
options(scipen = 999)
```

### Load variables and data

``` r
#########################
#### Store Variables ####
#########################

#### Common path to output data folder ####
path_to_data <- "../../data/output-data/"

###################
#### Load Data ####
###################

### Outdoor temperature data 

# Inner Harbor temperature data
folder <- "baltimore_weather_stations/"
dmh <- read_csv(paste0(path_to_data, folder, "dmh.csv"))

### Urban heat island, tree canopy, demographics data
folder <- "tree_temp_demographics/"

# Neighborhood geography
nsa_tree_temp <- read_csv(paste0(path_to_data, folder, "nsa_tree_temp.csv"))

# Community statistical area geography
csa_tree_temp_demographics <- read_csv(paste0(path_to_data, folder, "csa_tree_temp_demographics.csv"))

### Hospital Data
folder <- "hospital_data/"

# Inpatient admissions data
ip_full_zip_medicaid_correlation_matrix <- read_csv(paste0(path_to_data, folder, "ip/ip_full_zip_medicaid_correlation_matrix.csv"))

# Emergency room admissions data
op_er_full_zip_medicaid_correlation_matrix <- read_csv(paste0(path_to_data, folder, "op_er/op_er_full_zip_medicaid_correlation_matrix.csv"))

### EMS Data
folder <- "ems/"
dmh_ems <- read_csv(paste0(path_to_data, folder, "dmh_ems.csv")) 
EMS_all <- read_csv(paste0(path_to_data, folder, "EMS_all.csv")) 
```

Line-by-Line Fact Check \[Rising Heat\]
---------------------------------------

### Fact: Franklin Square heat \[cq\]

-   **Audio Story:** “Her neighborhood – Franklin Square, no relation to
    her name – is hotter than about two thirds of the neighborhoods in
    Baltimore.”
-   **Web Story:** “Her neighborhood, Franklin Square, is hotter than
    about two thirds of the neighborhoods in Baltimore – about six
    degrees hotter than the city’s coolest neighborhood.”

#### Explanation \[cq\]

Using the urban heat island measurements taken in August 2018 showing
block-by-block temperature variations, we calculated median afternoon
temperatures for each of the city’s 278 neighborhoods. At 95.4 degrees,
Franklin Square was warmer than 67 percent (about two-thirds) of
Baltimore’s neighborhoods. The city’s coolest neighborhood – “Gwynns
Falls/Leakin Park” – was 89.3 degrees F, 6.1 degrees less than Franklin
Square.

#### Supporting code and output \[cq\]

``` r
# Rank, neighborhoods cooler 
nsa_tree_temp %>%
  select(nsa_name, temp_median_aft) %>%
  mutate(rank = dense_rank(temp_median_aft), pct_nhoods_cooler = (rank/max(rank))*100) %>%
  filter(nsa_name == "franklin square")
```

    ## # A tibble: 1 x 4
    ##   nsa_name        temp_median_aft  rank pct_nhoods_cooler
    ##   <chr>                     <dbl> <int>             <dbl>
    ## 1 franklin square            95.4   186              66.9

``` r
# Difference between Franklin Square and coolest
nsa_tree_temp %>%
  select(nsa_name, temp_median_aft) %>%
  filter((nsa_name == "franklin square") | (temp_median_aft == min(temp_median_aft))) %>%
  spread(nsa_name, temp_median_aft) %>%
  mutate(difference = `franklin square` - `gwynns falls/leakin park`)
```

    ## # A tibble: 1 x 3
    ##   `franklin square` `gwynns falls/leakin park` difference
    ##               <dbl>                      <dbl>      <dbl>
    ## 1              95.4                       89.3       6.09

### Fact: Franklin Square poverty \[cq\]

-   **Audio Story:** “It’s also in one of the city’s poorer areas.”
-   **Web Story:** “It’s also in one of the city’s poorest communities,
    with more than one third of residents living in poverty.”

#### Explanation \[cq\]

Franklin Square is entirely contained by a “community statistical area”
known as “Southwest Baltimore”. The poverty rate here is 36 percent, or
more than one-third. There are 55 CSAs in Baltimore. Southwest Baltimore
has the 51st highest poverty rate, making it poorer than more than 90
percent of CSAs.

#### Supporting code and output \[cq\]

``` r
csa_tree_temp_demographics %>%
  select(`csa2010`, percent_of_family_households_living_below_the_poverty_line) %>%
  mutate(rank = dense_rank(percent_of_family_households_living_below_the_poverty_line), pct_nhoods_wealthier = (rank/max(rank))) %>%
  filter(`csa2010` == "southwest baltimore")
```

    ## # A tibble: 1 x 4
    ##   csa2010       percent_of_family_households_livin…  rank pct_nhoods_wealt…
    ##   <chr>                                       <dbl> <int>             <dbl>
    ## 1 southwest ba…                                35.8    51             0.927

### Fact: Heat and poverty \[cq\]

-   **Audio Story:** “And the hottest parts of the city also have higher
    rates of poverty.”
-   **Web Story:** “Across Baltimore, the hottest areas tend to be the
    poorest and that pattern is not unusual.”

#### Explanation \[cq\]

In Baltimore’s “community statistical areas”, we examined the
relationship between heat (median afternoon temperature in our urban
heat island data) and poverty. An r of 1 would indicate a perfect
positive linear relationship, an r of -1 would indicate a perfect
negative linear relationship, and 0 would indicate no relationship.
There were moderate positive correlations between heat poverty (r =.38).
In general, the hotter the neighborhood, the higher the poverty rate,
and vice versa.

#### Supporting code and output \[cq\]

``` r
csa_tree_temp_demographics %>%
  select_if(is.numeric) %>%
  as.matrix() %>%
  correlate() %>%
  focus(matches("temp_")) %>%
  mutate(variable=rowname) %>%
  select(variable, temp_median_aft) %>%
  filter(variable=="percent_of_family_households_living_below_the_poverty_line")
```

    ## # A tibble: 1 x 2
    ##   variable                                                  temp_median_aft
    ##   <chr>                                                               <dbl>
    ## 1 percent_of_family_households_living_below_the_poverty_li…           0.378

### Fact: Neighborhood heat difference \[cq\]

-   **Audio Story:** “Citywide in Baltimore, the hottest neighborhoods
    can differ by as much as 10 degrees from the coolest.”  
-   **Web Story:** “According to a Howard Center analysis of U.S. Census
    data and air temperature data obtained from Portland State
    University and the Science Museum of Virginia, the hottest
    neighborhoods in Baltimore can differ by as much as 10 degrees from
    the coolest.”

#### Explanation \[cq\]

Using median afternoon temperature in a 2018 study by researchers
analyzing the city’s urban heat island, there was a 9.98 degree
difference between the city’s hottest neighborhood (McElderry Park) and
coolest neighborhood (Gwynns Falls/Leakin Park).

#### Supporting code and output \[cq\]

``` r
# Hottest and coolest neighborhoods
nsa_tree_temp %>%
  select(nsa_name, temp_median_aft) %>%
  filter((temp_median_aft == min(temp_median_aft)) | (temp_median_aft == max(temp_median_aft)))
```

    ## # A tibble: 2 x 2
    ##   nsa_name                 temp_median_aft
    ##   <chr>                              <dbl>
    ## 1 gwynns falls/leakin park            89.3
    ## 2 mcelderry park                      99.3

``` r
# Difference between hottest and coolest
nsa_tree_temp %>%
  select(nsa_name, temp_median_aft) %>%
  filter((temp_median_aft == min(temp_median_aft)) | (temp_median_aft == max(temp_median_aft))) %>%
  spread(nsa_name, temp_median_aft) %>%
  mutate(difference = `mcelderry park` - `gwynns falls/leakin park`)
```

    ## # A tibble: 1 x 3
    ##   `gwynns falls/leakin park` `mcelderry park` difference
    ##                        <dbl>            <dbl>      <dbl>
    ## 1                       89.3             99.3       9.98

### Fact: EMS calls for heat stroke \[cq\]

-   **Audio Story:** “When the heat index reached dangerous levels last
    summer, EMS calls increased citywide for heat stroke.”
-   **Web Story:** “In the summer of 2018 in Baltimore, when the heat
    index reached 103 degrees — the threshold deemed dangerous by the
    National Weather Service — EMS calls increased dramatically citywide
    for potentially fatal heat stroke.”

#### Explanation \[cq\]

By merging a dataset of EMS calls with a dataset of hourly Baltimore
heat index values, we were able to determine the heat index at the time
of each EMS call during summer 2018. We looked at select conditions
affected by heat, and compared how call rates changed when it was very
hot – over 103 heat index, a level defined by NWS as "dangerous – and
when the heat index was under 80. The second and third columns below
reflect the number of hours that passed between calls (on average) when
the heat index was below 80 degrees or above 103 degrees.

As expected, calls for heat stroke increased dramatically. When it was
under 80 degrees, there was a call for heat stroke every 480 hours, or
about once every three weeks. When it was 103 degrees or higher, there
was a call for heat stroke every 1.4 hours.

#### Supporting Code \[cq\]

``` r
# Select conditions
conditions <- c("Heat Exhaustion/Heat Stroke")

# Calculate the total number of hours over the course of Summer 2018 that the heat index fell into each heat index level, as defined by the national weather service: not unsafe (under 80), caution (80-89), extreme caution (90-102), danger (103-124).   

heat_index_count_per_nws_five_scale_bucket <- dmh_ems %>%
  select(heat_index_nws_five_scale_bucket) %>%
  group_by(heat_index_nws_five_scale_bucket) %>%
  summarise(heat_index_count_per_nws_five_scale_bucket=n()) %>%
  arrange(heat_index_nws_five_scale_bucket)

# For each target condition, calculate the number of hours between calls at each temperature level.  This metric allows us to account for the fact that simply counting calls in each bucket would be flawed, because it wouldn't adjust for the rarity of very hot temperatures. 

EMS_all %>%
  filter(primary_impression_group %in% conditions) %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(hours_per_call = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  tidyr::spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  select(primary_impression_group, `not_unsafe_under_80`,`danger_103_124`)
```

    ## # A tibble: 1 x 3
    ## # Groups:   primary_impression_group [1]
    ##   primary_impression_group    not_unsafe_under_80 danger_103_124
    ##   <chr>                                     <dbl>          <dbl>
    ## 1 Heat Exhaustion/Heat Stroke                 480           1.39

### Fact: EMS calls for chronic conditions \[cq\]

-   **Audio Story:** “But calls also increased for chronic conditions,
    including several cardiovascular and respiratory conditions.”
-   **Web Story:** “But calls increased for chronic conditions too: EMS
    calls for chronic obstructive pulmonary disorder (COPD) increased by
    nearly 70 percent. Calls for respiratory distress increased by 20
    percent. Calls for cardiac arrest rose by 80 percent and those for
    high blood pressure more than doubled. Other conditions also spiked:
    Psychiatric disorders, substance abuse and dehydration, among
    others.”
-   **Web Story:** “And living day after day in an environment that’s
    literally hotter isn’t just uncomfortable, it can have dire and
    sometimes deadly health consequences – a fact we found reflected in
    Baltimore’s soaring rates of emergency calls when the heat index
    spiked to dangerous levels.”

#### Explanation \[cq\]

By merging a dataset of EMS calls with a dataset of hourly Baltimore
heat index values, we were able to determine the heat index at the time
of each EMS call during summer 2018. We looked at select conditions
affected by heat, and compared how call rates changed when it was very
hot – over 103 heat index, a level defined by NWS as “dangerous” – and
when the heat index was under 80. The third and fourth columns below
reflect the number of hours that passed between calls (on average) when
the heat index was below 80 degrees or above 103 degrees. The table
below reflects the percentage difference (second column) in call rates
between the two temperature groupings.

#### Supporting code and output \[cq\]

``` r
# Select conditions
conditions <- c("Dehydration","Respiratory Distress", "COPD (Emphysema/Chronic Bronchitis)", "Cardiac Arrest", "CHF (Congestive Heart Failure)", "Behavioral/Psychiatric Disorder", "Dehydration", "Chest Pain - STEMI","Hypertension","Substance/Drug Abuse","Withdrawal/Overdose Drugs","Withdrawal/Overdose ETOH")

# Calculate the total number of hours over the course of Summer 2018 that the heat index fell into each heat index level, as defined by the national weather service: not unsafe (under 80), caution (80-89), extreme caution (90-102), danger (103-124).   

heat_index_count_per_nws_five_scale_bucket <- dmh_ems %>%
  select(heat_index_nws_five_scale_bucket) %>%
  group_by(heat_index_nws_five_scale_bucket) %>%
  summarise(heat_index_count_per_nws_five_scale_bucket=n()) %>%
  arrange(heat_index_nws_five_scale_bucket)

# For each target condition, calculate the number of hours between calls at each temperature level.  This metric allows us to account for the fact that simply counting calls in each bucket would be flawed, because it wouldn't adjust for the rarity of very hot temperatures. 

EMS_all %>%
  filter(primary_impression_group %in% conditions) %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(hours_per_call = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  tidyr::spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  select(primary_impression_group, `not_unsafe_under_80`,`danger_103_124`) %>%
  mutate(`calls_per_day_under_80` = 24/`not_unsafe_under_80`) %>%
  mutate(`calls_per_day_over_103` = 24/`danger_103_124`) %>%
  mutate(difference_day_percent = ((`calls_per_day_over_103`-`calls_per_day_under_80`)/`calls_per_day_under_80`)) %>%
  select(primary_impression_group, difference_day_percent, everything())
```

    ## # A tibble: 11 x 6
    ## # Groups:   primary_impression_group [11]
    ##    primary_impress… difference_day_… not_unsafe_unde… danger_103_124
    ##    <chr>                       <dbl>            <dbl>          <dbl>
    ##  1 Behavioral/Psyc…            0.370             1.77           1.29
    ##  2 Cardiac Arrest              0.801             5.30           2.94
    ##  3 Chest Pain - ST…            1.36             41.7           17.7 
    ##  4 CHF (Congestive…            0.698            15              8.83
    ##  5 COPD (Emphysema…            0.695             5.61           3.31
    ##  6 Dehydration                17.9              41.7            2.21
    ##  7 Hypertension                1.59             15.2            5.89
    ##  8 Respiratory Dis…            0.199             3.34           2.79
    ##  9 Substance/Drug …            1.18              2.96           1.36
    ## 10 Withdrawal/Over…            1.11              2.24           1.06
    ## 11 Withdrawal/Over…            1.42              7.11           2.94
    ## # … with 2 more variables: calls_per_day_under_80 <dbl>,
    ## #   calls_per_day_over_103 <dbl>

### Fact: Medicaid and chronic conditions \[cq\]

-   **Audio Story:** “And even when controlling for income, there were
    differences across the city. From 2013 to 2018, Medicaid patients in
    Baltimore’s hottest areas visited the hospital with those conditions
    at higher rates than Medicaid patients in the cooler areas,
    according to the Howard Center analysis.”
-   **Web Story:** “The heat affected residents citywide, but even when
    controlling for income by only looking at the patterns of Medicaid
    patients, there were differences across the city. From 2013 to 2018,
    Medicaid patients in Baltimore’s hottest areas visited the hospital
    at higher rates than Medicaid patients in the city’s coolest areas.
    The low-income patients in the city’s hot spots visited more often
    with several conditions, including asthma, COPD and heart disease,
    according to hospital inpatient and emergency room admissions data
    from the state’s Health Services Cost Review Commission.”

#### Explanation \[cq\]

We examined rates of chronic condition prevalence for low-income people
in different parts of Baltimore by examining in-patient hospital
admissions for people with Medicaid. We discovered that low-income
people in different parts of the city had different prevalence rates for
chronic conditions affected by heat – asthma, COPD, heart disease and
diabetes. And, we found, those differences varied in line with
temperature differences in the area in which they lived.

There were moderate to strong positive relationships (copd, r=.75;
asthma, r=.52; heart\_disease, r=.71; diabetes, r=.5) between a ZIP
code’s prevalence rate for chronic medical conditions as diagnosed in
inpatient hospital visits and a ZIP code’s median afternoon temperature
as measured by urban heat island researchers in August 2018. There were
moderate to strong positive relationships (copd, r=.51; asthma, r=.38;
heart\_disease, r=.44; diabetes, r=.44) between a ZIP code’s prevalence
rate for chronic medical conditions as diagnosed in emergency room
visits and a ZIP code’s median afternoon temperature as measured by
urban heat island researchers in August 2018. That is to say: the higher
the neighborhood temperature, the higher the disease rate among the
poorest inhabitants, and vice versa. This is not a causal relationship
we are describing.

#### Supporting Code \[cq\]

``` r
# Inpatient medicaid admissions
ip_full_zip_medicaid_correlation_matrix %>%
    filter(str_detect(rowname, "asthma|copd|heart_disease|diabetes")) %>%
    select(rowname, temp_median_aft)
```

    ## # A tibble: 4 x 2
    ##   rowname                     temp_median_aft
    ##   <chr>                                 <dbl>
    ## 1 medicaid_copd_prev                    0.753
    ## 2 medicaid_asthma_prev                  0.520
    ## 3 medicaid_heart_disease_prev           0.711
    ## 4 medicaid_diabetes_prev                0.500

``` r
# ER medicaid visits
op_er_full_zip_medicaid_correlation_matrix %>%
    filter(str_detect(rowname, "asthma|copd|heart_disease|diabetes")) %>%
    select(rowname, temp_median_aft)
```

    ## # A tibble: 4 x 2
    ##   rowname                     temp_median_aft
    ##   <chr>                                 <dbl>
    ## 1 medicaid_copd_prev                    0.509
    ## 2 medicaid_asthma_prev                  0.382
    ## 3 medicaid_heart_disease_prev           0.437
    ## 4 medicaid_diabetes_prev                0.440

### Fact: Franklin Square tree canopy \[cq\]

-   **Audio Story:** "The neighborhood where Shakira Franklin lives has
    increased its tree canopy over time. But in recent years, it’s still
    been among the city’s lowest.
-   **Web Story:** “The neighborhood where Shakira Franklin lives has
    increased its tree canopy over time. But by 2015, it still was among
    the city’s lowest.”

#### Explanation \[cq\]

In 2007, 14.2 percent of the neighborhood was covered by tree canopy in
the summer. By 2015, that had increased to 16.1 percent, a 1.9
percentage point increase. In 2015, two-thirds of Baltimore’s 278
neighborhoods had more tree canopy than Franklin Square’s 16 percent
coverage.

#### Supporting Code \[cq\]

``` r
# change
nsa_tree_temp %>%
  select(nsa_name, `avg_canopy_%_07` = `07_lid_mean`, `avg_canopy_15` = `15_lid_mean`, `%_point_change` = lid_change_percent_point) %>%
  filter(nsa_name == "franklin square")
```

    ## # A tibble: 1 x 4
    ##   nsa_name        `avg_canopy_%_07` avg_canopy_15 `%_point_change`
    ##   <chr>                       <dbl>         <dbl>            <dbl>
    ## 1 franklin square             0.142         0.161           0.0189

``` r
# rank
nsa_tree_temp %>%
  select(nsa_name, `avg_canopy_15` = `15_lid_mean`) %>%
  arrange(nsa_name) %>%
  mutate(rank = dense_rank(desc(`avg_canopy_15`)), pct_nhoods_w_more_tree_canopy = (rank/max(rank))) %>%
  filter(nsa_name == "franklin square")
```

    ## # A tibble: 1 x 4
    ##   nsa_name        avg_canopy_15  rank pct_nhoods_w_more_tree_canopy
    ##   <chr>                   <dbl> <int>                         <dbl>
    ## 1 franklin square         0.161   186                         0.669

Line-by-Line Fact Check \[Mental Health\]
-----------------------------------------

### Fact: Rate of increase of psychiatric calls \[cq\]

-   **Audio story**: “The Howard Center analyzed data from emergency
    response calls in Baltimore. They found that in the summer of 2018,
    calls for psychiatric conditions increased by nearly 40 percent when
    the heat index spiked above 103.”
-   **Web story**: An analysis by the University of Maryland’s Howard
    Center for Investigative Journalism found emergency response calls
    relating to psychiatric conditions increased nearly 40% in Baltimore
    in the summer of 2018, when the heat index spiked above 103.

#### Explanation \[cq\]

By merging a dataset of EMS calls with a dataset of hourly Baltimore
heat index values, we were able to determine the heat index at the time
of each EMS call during summer 2018. We looked at select conditions
affected by heat, and compared how call rates changed when it was very
hot – over 103 heat index, a level defined by NWS as “dangerous” – and
when the heat index was under 80. The third and fourth columns below
reflect the number of hours that passed between calls (on average) when
the heat index was below 80 degrees or above 103 degrees. The table
below reflects the percentage difference (second column) in call rates
between the two temperature groupings.

In Summer 2018, when the heat index was under 80 degrees, there was a
medical call for a behavioral and psychiatric disorder every 1.77 hours
(1 hour, 46 minutes). When the heat index hit 103 degrees, the rate of
calls increased dramatically – to one call every 1.29 hours (1 hour, 17
minutes). That’s an increase in the rate of 29 minutes, or an increase
of 37 percent.

#### Supporting code and output \[cq\]

``` r
# Select conditions
conditions <- c("Behavioral/Psychiatric Disorder")

# Calculate the total number of hours over the course of Summer 2018 that the heat index fell into each heat index level, as defined by the national weather service: not unsafe (under 80), caution (80-89), extreme caution (90-102), danger (103-124).   

heat_index_count_per_nws_five_scale_bucket <- dmh_ems %>%
  select(heat_index_nws_five_scale_bucket) %>%
  group_by(heat_index_nws_five_scale_bucket) %>%
  summarise(heat_index_count_per_nws_five_scale_bucket=n()) %>%
  arrange(heat_index_nws_five_scale_bucket)

# For each target condition, calculate the number of hours between calls at each temperature level.  This metric allows us to account for the fact that simply counting calls in each bucket would be flawed, because it wouldn't adjust for the rarity of very hot temperatures. 

EMS_all %>%
  filter(primary_impression_group %in% conditions) %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(hours_per_call = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  select(primary_impression_group, `not_unsafe_under_80`,`danger_103_124`) %>%
  mutate(percent_change = (`not_unsafe_under_80`-`danger_103_124`)/`danger_103_124`) %>%
  mutate(`calls_per_day_under_80` = 24/`not_unsafe_under_80`) %>%
  mutate(`calls_per_day_over_103` = 24/`danger_103_124`) %>%
  mutate(difference_day_percent = ((`calls_per_day_over_103`-`calls_per_day_under_80`)/`calls_per_day_under_80`))
```

    ## # A tibble: 1 x 7
    ## # Groups:   primary_impression_group [1]
    ##   primary_impress… not_unsafe_unde… danger_103_124 percent_change
    ##   <chr>                       <dbl>          <dbl>          <dbl>
    ## 1 Behavioral/Psyc…             1.77           1.29          0.370
    ## # … with 3 more variables: calls_per_day_under_80 <dbl>,
    ## #   calls_per_day_over_103 <dbl>, difference_day_percent <dbl>

### Fact: Substance abuse calls rate \[cq\]

-   **Audio story:** “Calls relating to substance abuse more than
    doubled in extreme heat, according to data analyzed by the Howard
    Center.”
-   **Web story:** “Often, mental health patients are struggling with
    more than a mental illness. Studies have linked heat and increased
    drug overdoses, as well as heightened effects of alcohol poisoning -
    and the Howard Center found calls relating to substance abuse more
    than doubled during dangerous heat in the summer of 2018.”

#### Explanation \[cq\]

By merging a dataset of EMS calls with a dataset of hourly Baltimore
heat index values, we were able to determine the heat index at the time
of each EMS call during summer 2018. We looked at select conditions
affected by heat, and compared how call rates changed when it was very
hot – over 103 heat index, a level defined by NWS as “dangerous” – and
when the heat index was under 80. The third and fourth columns below
reflect the number of hours that passed between calls (on average) when
the heat index was below 80 degrees or above 103 degrees. The table
below reflects the percentage difference in call rates between the two
temperature groupings.

In Summer 2018, when the heat index was under 80 degrees, there was a
medical call for substance abuse every 2.96 hours (nearly 3 hours). When
the heat index hit 103 degrees, the rate of calls increased dramatically
– to one call every 1.36 hours. That’s an increase in the rate of about
an hour and a half, or an increase of 118 percent. A 100 percent
increase is a doubling, so it’s fair to say more than doubled.

#### Supporting code and output \[cq\]

``` r
# Select conditions
conditions <- c("Substance/Drug Abuse")

# Calculate the total number of hours over the course of Summer 2018 that the heat index fell into each heat index level, as defined by the national weather service: not unsafe (under 80), caution (80-89), extreme caution (90-102), danger (103-124).   

heat_index_count_per_nws_five_scale_bucket <- dmh_ems %>%
  select(heat_index_nws_five_scale_bucket) %>%
  group_by(heat_index_nws_five_scale_bucket) %>%
  summarise(heat_index_count_per_nws_five_scale_bucket=n()) %>%
  arrange(heat_index_nws_five_scale_bucket)

# For each target condition, calculate the number of hours between calls at each temperature level.  This metric allows us to account for the fact that simply counting calls in each bucket would be flawed, because it wouldn't adjust for the rarity of very hot temperatures. 

EMS_all %>%
  filter(primary_impression_group %in% conditions) %>%
  group_by(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket) %>%
  summarise(condition_calls_count_per_bucket=n()) %>%
  inner_join(heat_index_count_per_nws_five_scale_bucket, by = c("adjusted_heat_index_nws_five_scale_bucket" = "heat_index_nws_five_scale_bucket")) %>%
  mutate(hours_per_call = heat_index_count_per_nws_five_scale_bucket/condition_calls_count_per_bucket) %>%
  select(primary_impression_group, adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
  select(primary_impression_group, `not_unsafe_under_80`,`danger_103_124`) %>%
  mutate(percent_change = (`not_unsafe_under_80`-`danger_103_124`)/`danger_103_124`) %>%
  mutate(`calls_per_day_under_80` = 24/`not_unsafe_under_80`) %>%
  mutate(`calls_per_day_over_103` = 24/`danger_103_124`) %>%
  mutate(difference_day_percent = ((`calls_per_day_over_103`-`calls_per_day_under_80`)/`calls_per_day_under_80`))
```

    ## # A tibble: 1 x 7
    ## # Groups:   primary_impression_group [1]
    ##   primary_impress… not_unsafe_unde… danger_103_124 percent_change
    ##   <chr>                       <dbl>          <dbl>          <dbl>
    ## 1 Substance/Drug …             2.96           1.36           1.18
    ## # … with 3 more variables: calls_per_day_under_80 <dbl>,
    ## #   calls_per_day_over_103 <dbl>, difference_day_percent <dbl>

-30-
