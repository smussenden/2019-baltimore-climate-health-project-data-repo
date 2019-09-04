-   [Introduction](#introduction)
-   [Setup and Links](#setup-and-links)
    -   [Load packages](#load-packages)
    -   [Load variables and data](#load-variables-and-data)
-   [Line-By-Line Fact-Check](#line-by-line-fact-check)
    -   [Fact: 96 Degrees in Michael Thomas and Alberta Wilkerson’s
        Apartment
        \[cq\]](#fact-96-degrees-in-michael-thomas-and-alberta-wilkersons-apartment-cq)
    -   [Fact: July 2019 Heat Wave \[cq\]](#fact-july-2019-heat-wave-cq)
    -   [Fact: Heat Index in Michael Thomas and Alberta Wilkerson’s
        Apartment
        \[cq\]](#fact-heat-index-in-michael-thomas-and-alberta-wilkersons-apartment-cq)
    -   [Fact: EMS calls for certain conditions spike when it gets very
        hot
        \[cq\]](#fact-ems-calls-for-certain-conditions-spike-when-it-gets-very-hot-cq)
    -   [Fact: Morning temperature on July 19
        \[cq\]](#fact-morning-temperature-on-july-19-cq)
    -   [Fact: Extreme heat in Audrey DeWitt’s House
        \[cq\]](#fact-extreme-heat-in-audrey-dewitts-house-cq)
    -   [Fact: Data for graphic on temperature and health conditions
        \[cq\]](#fact-data-for-graphic-on-temperature-and-health-conditions-cq)
    -   [Fact: Asthma rates in Baltimore
        \[cq\]](#fact-asthma-rates-in-baltimore-cq)
    -   [Fact: Baltimore’s hottest neighborhood
        \[cq\]](#fact-baltimores-hottest-neighborhood-cq)
    -   [Fact: Extreme heat in Stephanie Pingley’s house
        \[cq\]](#fact-extreme-heat-in-stephanie-pingleys-house-cq)
    -   [Fact: EMS calls for psychiatric/drug disorders
        \[cq\]](#fact-ems-calls-for-psychiatricdrug-disorders-cq)

Introduction
------------

This R markdown document describes a portion of the data analysis for a
reporting project examining the effects of climate-change driven
temperature increases on the health of people who live in cities. The
project was done in partnership with the [University of Maryland Philip
Merrill College of Journalism](), [Capital News
Service](https://cnsmaryland.org/), [the Howard Center for Investigative
Journalism](https://merrill.umd.edu/about-merrill/signature-programs/the-howard-center-for-investigative-journalism/),
[NPR](https://www.npr.org/), [Wide Angle Youth
Media](https://www.wideanglemedia.org/) and
[WMAR](https://www.wmar2news.com/). It also moved on the Associated
Press wire.

For each sentence in the story “[Heat & Health: For people with chronic
health conditions, heat and humidity are more than a summer
nuisance](https://cnsmaryland.org/interactives/summer-2019/code-red/heat-health.html)”
that is based on Howard Center data analysis, this document provides the
original fact, the code and code output that support that fact, and an
explanation where necessary.

Here are links to other stories in the series:

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
-   [How High Heat Can Impact Mental
    Health](https://www.npr.org/templates/story/story.php?storyId=757034136&live=1)

**WMAR**

-   [Baltimore neighborhood identified as ‘ground zero’ for local
    effects of climate
    crisis](https://www.wmar2news.com/news/region/baltimore-city/baltimore-neighborhood-identified-as-ground-zero-for-local-effects-of-climate-crisis)

Setup and Links
---------------

-   The entire repo for this analysis, including the data needed to
    reproduce it, is available through the [Howard Center’s
    GitHub](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide).
-   The R markdown file for this specific analysis is available
    [here](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Heat-Health/Heat-Health-Analysis.Rmd).
-   An HTML output of the R markdown file for this analysis is available
    [here](https://howard-center-investigations.github.io/Heat-Health-Analysis.html).
-   The R markdown file used to clean the data used in the analysis is
    available
    [here](https://github.com/Howard-Center-Investigations/Code-Red-Baltimore-Climate-Divide/documentation/Data-Cleaning/Data-Cleaning.Rmd).
    It contains more detail about data sources, cleaning and analysis
    methods, limitations and software tools used.
-   NPR has made available a separate GitHub repo for nationwide
    analysis on urban heat islands available
    [here](https://github.com/nprapps/heat-income).

### Load packages

``` r
#######################
#### Load Packages ####
#######################
# rm(list=ls())
library(tidyverse) # For general data science goodness
library(DescTools) # For %like% operator
library(corrr) # For correlation matrices
library(colorspace) # For improved color palettes
library(ggplot2) # For graphing
library(ggrepel) # For graph labeling
library(lubridate) # for dates
library(spelling)
require(scales) # For percent labeling on distribution tables

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

### Indoor temperature and humidity sensor data

# Michael and Alberta
folder <- "temperature_sensors/michael/"
michael_day_hourly_averages <- read_csv(paste0(path_to_data, folder, "michael_day_hourly_averages.csv"))
michael_day_minute_averages <- read_csv(paste0(path_to_data, folder, "michael_day_minute_averages.csv"))

# Stephanie and family
folder <- "temperature_sensors/stephanie/"
stephanie_day_minute_averages <- read_csv(paste0(path_to_data, folder, "stephanie_day_minute_averages.csv"))
stephanie_day_hourly_averages <- read_csv(paste0(path_to_data, folder, "stephanie_day_hourly_averages.csv"))

# Audrey
folder <- "temperature_sensors/audrey/"
audrey_day_minute_averages <- read_csv(paste0(path_to_data, folder, "audrey_day_minute_averages.csv"))
audrey_day_hourly_averages <- read_csv(paste0(path_to_data, folder, "audrey_day_hourly_averages.csv"))

# Tammy
folder <- "temperature_sensors/tammy/"
tammy_day_minute_averages <- read_csv(paste0(path_to_data, folder, "tammy_day_minute_averages.csv"))
tammy_day_hourly_averages <- read_csv(paste0(path_to_data, folder, "tammy_day_hourly_averages.csv"))

### Outdoor temperature data 

# Inner Harbor temperature data
folder <- "baltimore_weather_stations/"
dmh <- read_csv(paste0(path_to_data, folder, "dmh.csv"))

# Historical temperature data, Baltimore and U.S. 
folder <- "1895-2019-average-temperature/"
us_change_temp <- read_csv(paste0(path_to_data, folder, "us_change_temp.csv"))
baltimore_change_temp <- read_csv(paste0(path_to_data, folder, "baltimore_change_temp.csv"))

# Heat index projections
folder <- "heat_index_projections/"
heat_index_projections <- read_csv(paste0(path_to_data, folder, "heat_index_projections.csv"))

### Urban heat island, tree canopy, demographics data
folder <- "tree_temp_demographics/"

# Neighborhood geography
nsa_tree_temp <- read_csv(paste0(path_to_data, folder, "nsa_tree_temp.csv"))

# Community statistical area geography
csa_tree_temp_demographics <- read_csv(paste0(path_to_data, folder, "csa_tree_temp_demographics.csv"))

# Blocks geography
blocks_tree_temp_demographics <- read_csv(paste0(path_to_data, folder, "blocks_tree_temp_demographics.csv"))

### Hospital Data
folder <- "hospital_data/"

# Inpatient admissions data
ip_full_zip_correlation_matrix <- read_csv(paste0(path_to_data, folder, "ip/ip_full_zip_correlation_matrix.csv"))
ip_full_zip_disease_heat <- read_csv(paste0(path_to_data, folder, "ip/ip_full_zip_disease_heat.csv"))
ip_full_zip_medicaid_correlation_matrix <- read_csv(paste0(path_to_data, folder, "ip/ip_full_zip_medicaid_correlation_matrix.csv"))
ip_full_zip_medicaid_disease_heat <- read_csv(paste0(path_to_data, folder, "ip/ip_full_zip_medicaid_disease_heat.csv"))


# Emergency room admissions data
op_er_full_zip_correlation_matrix <- read_csv(paste0(path_to_data, folder, "op_er/op_er_full_zip_correlation_matrix.csv"))
op_er_full_zip_disease_heat <- read_csv(paste0(path_to_data, folder, "op_er/op_er_full_zip_disease_heat.csv"))
op_er_full_zip_medicaid_matrix <- read_csv(paste0(path_to_data, folder, "op_er/op_er_full_zip_medicaid_correlation_matrix.csv"))
op_er_full_zip_medicaid_disease_heat <- read_csv(paste0(path_to_data, folder, "op_er/op_er_full_zip_medicaid_disease_heat.csv"))

### EMS Data
folder <- "ems/"
dmh_ems <- read_csv(paste0(path_to_data, folder, "dmh_ems.csv")) 
EMS_all <- read_csv(paste0(path_to_data, folder, "EMS_all.csv")) 

####################################
######## Define Functions ##########
####################################

# Function to save each matrix as CSV
write_matrix_csv <- function(dataframe) {
  # Store dataframe name for later use
  dataframe_name <- deparse(substitute(dataframe))
  
  # Create filename for csv
  filename <- paste0("data/output-data/correlation_matrices/", dataframe_name,".csv")
  
  # Write out csv  
  write_csv(dataframe, path = filename)
  
} 

# Function to make a nice little correlation matrix heatmap for each graphic

make_correlation_matrix_graphic <- function(dataframe, grouping = "GROUPING") {
  
  # Store name of dataframe for use in title
  dataframe_name <- deparse(substitute(dataframe))
  
  # Build chart title
  chart_title <- paste0("Correlations by ", grouping, " in Baltimore City | ", dataframe_name )
  
  # Create graph
  ggplot(data = dataframe, aes(x = variable_2, y = variable)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white", midpoint=0) +
    geom_text(aes(label = round(value, 2)*100), size = 10) +
    ggtitle(chart_title) +
    theme(axis.text=element_text(size=14),
          axis.text.x = element_text(size=14,angle=50,hjust=1),
          plot.title = element_text(size=14)
    )
  # Create filename and filepath to save image. 
  filename <- paste0(dataframe_name,".png")
  ggsave(filename, plot = last_plot(), device = "png", path = "data/output-data/plots/correlation-matrix-images", scale = 1, width = 20, height = 20, units = "in", dpi = 300)
  
}  

select_x <- function(df){
  return(df %>%
           select_if(is.numeric) 
           # select(-matches("objectid"), 
           #        -matches("csa2010"), 
           #        -matches("id"), 
           #        -matches("09"), 
           #        -matches("1718"), 
           #        -matches("change_percent")
           #        )
         )
}
```

Line-By-Line Fact-Check
-----------------------

### Fact: 96 Degrees in Michael Thomas and Alberta Wilkerson’s Apartment \[cq\]

“As the temperature in their rowhouse apartment rose to a humid 96
degrees F during a summer heat wave, Michael Thomas and Alberta
Wilkerson sat on their bed, in front of fans, wiping sweat and drinking
water, trying to keep their minds off the heat.”

#### Explanation \[cq\]

A reporter spent time with Michael Thomas and Alberta Wilkerson in the
afternoon of July 20, 2019, and observed this scene. On July 20, 2019,
the average temperature between 4 and 5 p.m. in their home was 96.0
degrees, with a heat index of 109.2.

#### Supporting code and output \[cq\]

``` r
 michael_day_hourly_averages %>%
  mutate(date = date(date_hour)) %>%
  mutate(hour = hour(date_hour)) %>%
  filter(date == "2019-07-20",
         hour == 16) 
```

    ## # A tibble: 1 x 10
    ##   date_hour           mean_indoor_tem… mean_indoor_hea… mean_indoor_rel…
    ##   <dttm>                         <dbl>            <dbl>            <dbl>
    ## 1 2019-07-20 16:00:00               96             109.             51.8
    ## # … with 6 more variables: mean_outdoor_temperature <dbl>,
    ## #   mean_outdoor_heat_index <dbl>, indoor_temperature_difference <dbl>,
    ## #   indoor_heat_index_difference <dbl>, date <date>, hour <int>

### Fact: July 2019 Heat Wave \[cq\]

“The couple decided to stay in their unairconditioned, second-floor home
in Broadway East in East Baltimore during the scorching 11-day stretch
in July. It was a risky decision. The heat wave included two days when
outdoor temperatures hit 100 degrees and sparked a Code Red heat
emergency, which city officials declare when the temperature reaches
dangerous levels.”

#### Explanation \[cq\]

July 12-22 had maximum temperatures at the NWS’s Inner Harbor weather
station of at least 90 degrees F and max heat indexes of at least 92 F,
using hourly snapshot data. The table below shows on only one day was
there an hourly average of 100 degrees, July 21. Using a different
dataset from the NCDC, the official maximum for each day, which is
calculated using minute-by-minute readings, shows it 100 on July 20 and
101 on July 21.

#### Supporting code and output \[cq\]

``` r
 dmh %>%
  filter(month == 7,
         year == 2019,
         day >=12, 
         day <= 22) %>%
  group_by(`date`) %>%
  summarise(min_temp = min(avg_hourly_temperature_dmh),
            max_temp = max(avg_hourly_temperature_dmh),
            mean_temp = mean(avg_hourly_temperature_dmh),
            min_heat_index = min(avg_hourly_heat_index_dmh),
            max_heat_index = max(avg_hourly_heat_index_dmh),
            mean_heat_index = mean(avg_hourly_heat_index_dmh) 
  )
```

    ## # A tibble: 11 x 7
    ##    date       min_temp max_temp mean_temp min_heat_index max_heat_index
    ##    <date>        <dbl>    <dbl>     <dbl>          <dbl>          <dbl>
    ##  1 2019-07-12     72       90        81.7             73             92
    ##  2 2019-07-13     75       91        83.4             75             92
    ##  3 2019-07-14     75       93.9      85.4             76             98
    ##  4 2019-07-15     75       90        82.3             75             93
    ##  5 2019-07-16     77       96.1      85.8             78            104
    ##  6 2019-07-17     75.9     98.1      85.7             77            110
    ##  7 2019-07-18     78.1     93.9      85.4             81            103
    ##  8 2019-07-19     80.1     98.1      90.1             84            108
    ##  9 2019-07-20     82.9     99        91.5             88            108
    ## 10 2019-07-21     81      100        90.9             84            111
    ## 11 2019-07-22     79       96.1      84.6             81            104
    ## # … with 1 more variable: mean_heat_index <dbl>

### Fact: Heat Index in Michael Thomas and Alberta Wilkerson’s Apartment \[cq\]

“At 10:30 p.m. on July 18, the heat index in their living quarters
reached a high of 116 degrees, according to a sensor they allowed
reporters from the University of Maryland’s Howard Center for
Investigative Journalism and Capital News Service to place in their home
for several weeks. That was 22 degrees hotter than the heat index
outdoors.”

#### Explanation \[cq\]

At 10:30 p.m. inside their apartment on July 18, the temperature was
94.5 degrees, but the humidity inside pushed the heat index to 116
degrees. The heat index at the time at the NWS’ Inner Harbor monitoring
station was 94 degrees. It was 22 degrees hotter inside their apartment
than it was at the Inner Harbor, using the heat index as a metric.

#### Supporting code and output \[cq\]

``` r
michael_day_minute_averages %>%
  arrange(desc(mean_indoor_heat_index)) %>%
  mutate(date = date(date_hour_minute)) %>%
  mutate(hour = hour(date_hour_minute)) %>%
  mutate(minute = minute(date_hour_minute)) %>%
  filter(date == "2019-07-18",
         hour == 22,
         minute == 30)
```

    ## # A tibble: 1 x 11
    ##   date_hour_minute    mean_indoor_tem… mean_indoor_hea… mean_indoor_rel…
    ##   <dttm>                         <dbl>            <dbl>            <dbl>
    ## 1 2019-07-18 22:30:00             94.5              116             64.7
    ## # … with 7 more variables: mean_outdoor_temperature <dbl>,
    ## #   mean_outdoor_heat_index <dbl>, indoor_temperature_difference <dbl>,
    ## #   indoor_heat_index_difference <dbl>, date <date>, hour <int>,
    ## #   minute <int>

### Fact: EMS calls for certain conditions spike when it gets very hot \[cq\]

“That helps explain why, during the summer in Baltimore, emergency
medical calls for dehydration, respiratory distress, kidney disease,
diabetes complications, heart attacks and heart failure spiked when the
heat index rose above 103 degrees, according to a Howard Center data
analysis.”

#### Explanation \[cq\]

Using emergency medical call records from Baltimore City, we examined
calls during Summer 2018. They were aligned to heat index data captured
at the Inner Harbor at the time of each call and adjusted for the urban
heat island using the ZIP Code of each call location. The statistics in
the table below represent the number of hours that passed between calls
for select conditions when the temperature was in a given heat index
bucket.

For example, in Summer 2018, when the heat index was under 80 degrees,
there was a medical call for dehydration every 41 hours. When the heat
index hit 103 degrees, the rate of calls increased dramatically – to one
every 2.2 hours. The table below shows the difference in calls per hour
at different temperature groupings.

#### Supporting code and output \[cq\]

``` r
# Select conditions
conditions <- c("Dehydration","Respiratory Distress", "COPD (Emphysema/Chronic Bronchitis)", "End Stage Renal Disease", "Diabetic Hyperglycemia", "Diabetic Hypoglycemia", "Cardiac Arrest", "CHF (Congestive Heart Failure)")


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

    ## # A tibble: 8 x 3
    ## # Groups:   primary_impression_group [8]
    ##   primary_impression_group            not_unsafe_under_80 danger_103_124
    ##   <chr>                                             <dbl>          <dbl>
    ## 1 Cardiac Arrest                                     5.30           2.94
    ## 2 CHF (Congestive Heart Failure)                    15              8.83
    ## 3 COPD (Emphysema/Chronic Bronchitis)                5.61           3.31
    ## 4 Dehydration                                       41.7            2.21
    ## 5 Diabetic Hyperglycemia                             7.33           5.3 
    ## 6 Diabetic Hypoglycemia                              5.96           4.42
    ## 7 End Stage Renal Disease                          160             53   
    ## 8 Respiratory Distress                               3.34           2.79

### Fact: Morning temperature on July 19 \[cq\]

“It was the morning of July 19, and the outdoor temperature would rise
to 98 degrees.”

#### Explanation \[cq\]

July 19 was particularly hot in Baltimore. By 2 p.m., the temperature at
the Inner Harbor hit 98.1 degrees, using hourly averages, the hottest it
would get all day.

#### Supporting code and output \[cq\]

``` r
 dmh %>%
  filter(`date` == date("2019-07-19")) %>%
  group_by(`date`) %>%
  summarise(max_temp = max(avg_hourly_temperature_dmh)) 
```

    ## # A tibble: 1 x 2
    ##   date       max_temp
    ##   <date>        <dbl>
    ## 1 2019-07-19     98.1

### Fact: Extreme heat in Audrey DeWitt’s House \[cq\]

“Despite the units, a sensor placed on the first floor by University of
Maryland journalists recorded a heat index of 92 degrees at 6 a.m. on
July 20. That was two degrees hotter than the heat index outdoors.”

#### Explanation \[cq\]

Air conditioning window units, not uncommon in our reporting, often
weren’t enough to combat the heat. In Audrey DeWitt’s home, the heat
index hit 91.8 degrees at 6 a.m. on July 20. The outdoor heat index was
90 degrees at the time.

#### Supporting code and output \[cq\]

``` r
audrey_day_hourly_averages %>%
  mutate(date=date(date_hour)) %>%
  mutate(hour=hour(date_hour)) %>%
  filter(date == "2019-07-20",
         hour == 6)  %>%
  select(date_hour, mean_indoor_heat_index, mean_outdoor_heat_index)
```

    ## # A tibble: 1 x 3
    ##   date_hour           mean_indoor_heat_index mean_outdoor_heat_index
    ##   <dttm>                               <dbl>                   <dbl>
    ## 1 2019-07-20 06:00:00                   91.8                      90

### Fact: Data for graphic on temperature and health conditions \[cq\]

"In Baltimore, the urban heat island effect means some parts of the city
are hotter than others. And in the hottest parts of the city, it’s an
unfortunate truth that low-income people have higher rates of chronic
health conditions affected by heat, when compared with low-income people
who live in cooler parts of the city, a Howard Center for Investigative
Journalism and Capital News Service analysis found.

The first (left) map shows afternoon temperature variations by ZIP code
in August 2018, as measured by climate researchers. The right (second)
map shows the prevalence of selected health conditions among Medicaid
patients who were admitted to the hospital between 2013 and 2018."

#### Explanation \[cq\]

We examined rates of chronic conditions among low-income people in
different parts of Baltimore by examining in-patient hospital admissions
by people on Medicaid in Baltimore, and discovered that low-income
people in different parts of the city had different diagnosis rates for
chronic conditions affected by heat – asthma, COPD, heart disease,
kidney disease and diabetes. And, we found, those differences varied in
line with temperature differences in the area in which they lived.

There were moderate to strong positive relationships (kidney disease, r
= .6; copd, r=.75; asthma, r=.51; heart\_disease, r=.71; diabetes, r=.5)
between a ZIP code’s prevelence rate for chronic medical conditions as
diagnosed in inpatient hospital visits and a ZIP code’s median afternoon
temperature as measured by urban heat island researchers in August 2018.
That is to say: the higher the neighborhood temperature, the higher the
disease rate among the poorest inhabitants, and vice versa. This is not
a causal relationship we are describing.

We’ve also output the table for the graphic here.

#### Supporting code and output \[cq\]

``` r
ip_full_zip_medicaid_correlation_matrix %>%
    filter(str_detect(rowname, "asthma|copd|kidney|heart_disease|diabetes")) %>%
    select(rowname, temp_median_aft)
```

    ## # A tibble: 5 x 2
    ##   rowname                      temp_median_aft
    ##   <chr>                                  <dbl>
    ## 1 medicaid_kidney_disease_prev           0.596
    ## 2 medicaid_copd_prev                     0.753
    ## 3 medicaid_asthma_prev                   0.520
    ## 4 medicaid_heart_disease_prev            0.711
    ## 5 medicaid_diabetes_prev                 0.500

``` r
ip_full_zip_medicaid_disease_heat  %>%
    select(matches("ZIPCODE|asthma|copd|kidney|heart_disease|diabetes|temp"))%>%
    select(ZIPCODE, temp_median_aft, everything()) %>%
    arrange(ZIPCODE)
```

    ## # A tibble: 26 x 7
    ##    ZIPCODE temp_median_aft medicaid_kidney… medicaid_copd_p…
    ##      <dbl>           <dbl>            <dbl>            <dbl>
    ##  1   21201            95.9           0.123             0.301
    ##  2   21202            97.1           0.138             0.274
    ##  3   21205            95.7           0.116             0.307
    ##  4   21206            94.1           0.0820            0.218
    ##  5   21207            93.5           0.0995            0.195
    ##  6   21209            93.1           0.0636            0.120
    ##  7   21210            93.2           0.0890            0.194
    ##  8   21211            94.8           0.0892            0.255
    ##  9   21212            93.5           0.119             0.257
    ## 10   21213            95.4           0.125             0.292
    ## # … with 16 more rows, and 3 more variables: medicaid_asthma_prev <dbl>,
    ## #   medicaid_heart_disease_prev <dbl>, medicaid_diabetes_prev <dbl>

### Fact: Asthma rates in Baltimore \[cq\]

“Asthma rates in low-income areas like McElderry Park and Broadway East
are higher than in more affluent areas.”

#### Explanation \[cq\]

There is a high degree of correlation between a geographic population’s
asthma prevelence rate in Baltimore, and that populations level of
affluence. Using detailed records of hospital admissions and emergency
room visits, we determined the percentage of patients from each ZIP Code
who had an asthma diagnosis, and compared it to that ZIP Code’s poverty
rate and median household income from U.S. Census data. There was a
strong positive relationship (r = .73) between a ZIP code’s emergency
room asthma rate and the percentage of people below the poverty line;
the higher the asthma rate, the higher the poverty rate, and vice versa.
There was a strong negative relationship (r = -.71) between a ZIP code’s
asthma rate and the median household income; the lower the median
household income, the higher the asthma rate, and vice versa. This is
not a causal relationship we are describing here. It’s just that, in
Baltimore, rich neighborhoods tend to have lower prevelence of asthma,
compared with poorer ones.

McElderry Park is mostly contained in 21205 and Broadway East is mostly
in 21213. 21205 had the third highest asthma rate (12.6 percent) in the
city and the second highest poverty rate (37.13), 21213 had the second
highest asthma rate (13.2 percent) and the sixth-highest poverty rate
(28.2 percent). ZIP Code 21209, in a much wealthier part of town, had
the city’s lowest poverty rate (7.6 percent) and the city’s second
lowest asthma prevelence (4.8 percent).

#### Supporting code and output \[cq\]

``` r
  op_er_full_zip_correlation_matrix %>%
    filter(rowname == "asthma_prev") %>%
    select(rowname, median_household_income_d, `poverty_%`)
```

    ## # A tibble: 1 x 3
    ##   rowname     median_household_income_d `poverty_%`
    ##   <chr>                           <dbl>       <dbl>
    ## 1 asthma_prev                    -0.708       0.737

``` r
  op_er_full_zip_disease_heat %>%
    select(ZIPCODE, asthma_prev, median_household_income_d, `poverty_%`) %>%
    arrange(desc(`asthma_prev`))
```

    ## # A tibble: 26 x 4
    ##    ZIPCODE asthma_prev median_household_income_d `poverty_%`
    ##      <dbl>       <dbl>                     <dbl>       <dbl>
    ##  1   21223       0.149                     26899        38.5
    ##  2   21213       0.132                     34917        28.2
    ##  3   21205       0.126                     28675        37.1
    ##  4   21217       0.126                     28116        36.7
    ##  5   21229       0.124                     47422        18.6
    ##  6   21216       0.123                     37314        26.2
    ##  7   21231       0.121                     69979        19.8
    ##  8   21218       0.117                     43352        24.5
    ##  9   21201       0.114                     33877        30.8
    ## 10   21225       0.113                     41904        24.9
    ## # … with 16 more rows

### Fact: Baltimore’s hottest neighborhood \[cq\]

“..McElderry Park, Baltimore’s hottest neighborhood, has three young
sons with the disease.”

#### Explanation \[cq\]

The mean afternoon temperature, as measured in late August 2018 by
researchers studying Baltimore’s urban heat island, in McElderry Park
was 99.4 degrees, the highest in the city.

#### Supporting code and output \[cq\]

``` r
nsa_tree_temp %>%
  select(nsa_name, temp_mean_aft) %>%
  arrange(desc(temp_mean_aft))
```

    ## # A tibble: 278 x 2
    ##    nsa_name              temp_mean_aft
    ##    <chr>                         <dbl>
    ##  1 mcelderry park                 99.4
    ##  2 milton-montford                99.3
    ##  3 patterson place                98.6
    ##  4 dunbar-broadway                98.3
    ##  5 ellwood park/monument          98.3
    ##  6 penn-fallsway                  98.3
    ##  7 pleasant view gardens          98.3
    ##  8 madison-eastend                97.9
    ##  9 old goucher                    97.9
    ## 10 biddle street                  97.9
    ## # … with 268 more rows

### Fact: Extreme heat in Stephanie Pingley’s house \[cq\]

“Pingley says that room can get as “hot as Hades,” which is reflected in
readings of a sensor placed there. The heat index in that room climbed
as high as 118 degrees and never dropped below 88 degrees over a
seven-day stretch."

#### Explanation \[ca\]

The sensor readings taken inside of Stephanie Pingley’s home reflect the
extreme heat her family experienced. The minimum indoor heat index value
between July 16 and July 22 was 88 degrees, and the max was 118. These
values reflect minute by minute readings.

#### Supporting code and output \[ca\]

``` r
stephanie_day_minute_averages %>%
  mutate(date=date(date_hour_minute)) %>%
  filter(date >= "2019-07-16",
         date <= "2019-07-22") %>%
  summarise(max_heat_index = max(mean_indoor_heat_index),
            min_heat_index = min(mean_indoor_heat_index))
```

    ## # A tibble: 1 x 2
    ##   max_heat_index min_heat_index
    ##            <dbl>          <dbl>
    ## 1            118             88

### Fact: EMS calls for psychiatric/drug disorders \[cq\]

“In Baltimore, the rate of emergency medical calls for psychiatric
disorders and drug and alcohol overdoses increased dramatically when the
heat index hit 103 degrees, the Howard Center data analysis found.”

#### Explanation \[cq\]

Using emergency medical call records from Baltimore City, we examined
calls during Summer 2018. They were aligned to heat index data captured
at the Inner Harbor and adjusted for the urban heat island using the ZIP
Code of each call location. The statistics in the table below represent
the number of hours that passed between calls for select conditions when
the temperature was in a given heat index bucket.

For example, in Summer 2018, when the heat index was under 80 degrees,
there was a medical call for a behavioral and psychiatric disorder every
1.77 hours (1 hour, 46 minutes). When the heat index hit 103 degrees,
the rate of calls increased dramatically – to one call every 1.29 hours
(1 hour, 17 minutes). The increase for drug and alcohol (ETOH) related
calls was even sharper in extreme heat, as the table below shows. For
example, drug overdoses happened at a rate of one call every 2.24 hours
when it was under 80, but increased to about one call per hour over 103.

#### Supporting code and output \[cq\]

``` r
# Select conditions
conditions <- c("Substance/Drug Abuse","Substance/Drug Abuse", "Withdrawal/Overdose Drugs", "Withdrawal/Overdose ETOH", "Behavioral/Psychiatric Disorder")

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

    ## # A tibble: 4 x 3
    ## # Groups:   primary_impression_group [4]
    ##   primary_impression_group        not_unsafe_under_80 danger_103_124
    ##   <chr>                                         <dbl>          <dbl>
    ## 1 Behavioral/Psychiatric Disorder                1.77           1.29
    ## 2 Substance/Drug Abuse                           2.96           1.36
    ## 3 Withdrawal/Overdose Drugs                      2.24           1.06
    ## 4 Withdrawal/Overdose ETOH                       7.11           2.94

-30-
