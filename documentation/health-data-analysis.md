-   [Introduction](#introduction)
-   [Setup](#setup)
    -   [Load packages](#load-packages)
    -   [Load variables and data](#load-variables-and-data)
-   [Line-By-Line Fact-Check](#line-by-line-fact-check)
    -   [Fact: 96 Degrees in Michael Thomas and Alberta Wilkerson’s
        Apartment](#fact-96-degrees-in-michael-thomas-and-alberta-wilkersons-apartment)
    -   [Fact: July 2019 Heat Wave](#fact-july-2019-heat-wave)
    -   [Fact: Heat Index in Michael Thomas and Alberta Wilkerson’s
        Apartment](#fact-heat-index-in-michael-thomas-and-alberta-wilkersons-apartment)
    -   [Fact: EMS calls for certain conditions spike when it gets very
        hot](#fact-ems-calls-for-certain-conditions-spike-when-it-gets-very-hot)
    -   [Explanation](#explanation-3)
    -   [Fact: Morning temperature on July
        19](#fact-morning-temperature-on-july-19)
    -   [Fact: Extreme heat in Audrey DeWitt’s
        House](#fact-extreme-heat-in-audrey-dewitts-house)
    -   [Fact: Data for Graphic on Temperature and Health
        Conditions](#fact-data-for-graphic-on-temperature-and-health-conditions)
    -   [Fact: Asthma rates in
        Baltimore](#fact-asthma-rates-in-baltimore)
    -   [Fact: Baltimore’s Hottest
        Neighborhood](#fact-baltimores-hottest-neighborhood)
    -   [Fact: Extreme heat in Stephanie Pingley’s
        house](#fact-extreme-heat-in-stephanie-pingleys-house)
    -   [Fact: EMS calls for certain conditions spike when it gets very
        hot](#fact-ems-calls-for-certain-conditions-spike-when-it-gets-very-hot-1)
    -   [Explanation](#explanation-10)

Introduction
------------

This R markdown document describes the methodology and results of a
portion of the data analysis for a reporting project examining the
effects of climate change and temperature increases on the health of
people in Baltimore’s hottest neighborhoods.

This analysis describes data findings used in the story [“Heat and
Health: For people with chronic health conditions, heat and humidity is
more than a summer
nuisance,”](https://cnsmaryland.org/interactives/summer-2019/code-red/heat-health.html)
published in August 2019.

Setup
-----

Before running this file, **please view and run the [Code Red Data
Cleaning
document](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/code-red-data-cleaning.md)**
for this project. As well as outputting necessary cleaned data for the
following ananlysis, that document also includes the following items
necessary to understand this analysis:

-   definitions
-   source data citation and information
-   cleaning methodology
-   software tools used

### Load packages

``` r
#######################
#### Load Packages ####
#######################

library(tidyverse) # For general data science goodness
library(DescTools) # For %like% operator
library(corrr) # For correlation matrices
library(colorspace) # For improved color palettes
library(ggplot2) # For graphing
library(ggrepel) # For graph labeling
require(scales) # For percent labeling on distribution tables

# Turn off scientific notation in RStudio (prevents coersion to character type)
options(scipen = 999)
```

### Load variables and data

``` r
#########################
#### Store Variables ####
#########################

###################
#### Load Data ####
###################

### Sensor Data

# Michael
path_to_data <- "../data/output-data/temperature_sensors/michael/"
michael_day_hourly_averages <- read_csv(paste0(path_to_data, "michael_day_hourly_averages.csv"))
michael_day_minute_averages <- read_csv(paste0(path_to_data, "michael_day_minute_averages.csv"))

# Stephanie
path_to_data <- "../data/output-data/temperature_sensors/stephanie/"
stephanie_day_minute_averages <- read_csv(paste0(path_to_data, "stephanie_day_minute_averages.csv"))

# Audrey
path_to_data <- "../data/output-data/temperature_sensors/audrey/"
audrey_day_minute_averages <- read_csv(paste0(path_to_data, "audrey_day_minute_averages.csv"))
audrey_day_hourly_averages <- read_csv(paste0(path_to_data, "audrey_day_hourly_averages.csv"))

### Inner Harbor temperature data
path_to_data <- "../data/output-data/baltimore_weather_stations/"
dmh <- read_csv(paste0(path_to_data, "dmh.csv"))

### EMS data
path_to_data <- "../data/output-data/ems/"
EMS_all <- read_csv(paste0(path_to_data, "EMS_all.csv"))
dmh_ems <- read_csv(paste0(path_to_data, "dmh_ems.csv"))

### Hospital Data
# ER Visits
path_to_data <- "../data/output-data/hospital_data/op_er/"
op_er_full_zip_correlation_matrix <- read_csv(paste0(path_to_data, "op_er_full_zip_correlation_matrix.csv"))
op_er_full_zip_disease_heat <- read_csv(paste0(path_to_data, "op_er_full_zip_disease_heat.csv"))

# Medicaid Inpatient
path_to_data <- "../data/output-data/hospital_data/ip/"
ip_full_zip_medicaid_correlation_matrix <- read_csv(paste0(path_to_data, "ip_full_zip_medicaid_correlation_matrix.csv"))
ip_full_zip_medicaid_disease_heat <- read_csv(paste0(path_to_data, "ip_full_zip_medicaid_disease_heat.csv"))

# Neighborhood Temperature Data
path_to_data <- "../data/output-data/tree_temp_demographics/"
nsa_tree_temp <- read_csv(paste0(path_to_data, "nsa_tree_temp.csv"))
```

Line-By-Line Fact-Check
-----------------------

### Fact: 96 Degrees in Michael Thomas and Alberta Wilkerson’s Apartment

“As the temperature in their rowhouse apartment rose to a humid 96
degrees F during a summer heat wave, Michael Thomas and Alberta
Wilkerson sat on their bed, in front of fans, wiping sweat and drinking
water, trying to keep their minds off the heat.”

#### Explanation

Reporter Ian Round spent time with Michael Thomas and Alberta Wilkerson
in the afternoon of July 20, 2019, and observed this scene. On July 20,
2019, the average temperature between 4 and 5 p.m. in their home was
96.0 degrees, with a heat index of 109.2.

#### Supporting Code

``` r
 michael_day_hourly_averages %>%
  mutate(date = date(date_hour)) %>%
  mutate(hour = hour(date_hour)) %>%
  filter(date == "2019-07-20",
         hour == 16) 
```

    ## # A tibble: 1 x 9
    ##   date_hour           mean_indoor_tem… mean_indoor_hea… mean_outdoor_te…
    ##   <dttm>                         <dbl>            <dbl>            <dbl>
    ## 1 2019-07-20 16:00:00               96             109.               99
    ## # … with 5 more variables: mean_outdoor_heat_index <dbl>,
    ## #   indoor_temperature_difference <dbl>,
    ## #   indoor_heat_index_difference <dbl>, date <date>, hour <int>

### Fact: July 2019 Heat Wave

“…the scorching 11-day stretch in July, including two days when outdoor
temperatures hit 100 degrees…”

#### Explanation

July 12-22 had maximum temperatures at the NWS’s Inner Harbor weather
station of at least 90 degrees F and max heat indexes of at least 92 F.
This uses hourly averages. The table below shows on only one day was
there an hourly average of 100 degrees, July 21. Using a different
dataset from the NCDC, the official maximum for each day, which is
calculated using minute-by-minute readings, shows it 100 on July 20 and
101 on July 21.

#### Supporting Code

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

### Fact: Heat Index in Michael Thomas and Alberta Wilkerson’s Apartment

“At 10:30 p.m. on July 18, the heat index in their living quarters
reached a high of 116 degrees, according to a sensor they allowed
reporters from the University of Maryland’s Howard Center for
Investigative Journalism and Capital News Service to place in their home
for several weeks.”

“That was 22 degrees hotter than the heat index outdoors.”

#### Explanation

At 10:30 p.m. inside their apartment on July 18, the temperature was
94.5 degrees, but the humidity inside pushed the heat index to 116
degrees. The heat index at the time at the NWS’ Inner Harbor monitoring
station was 94 degrees. It was 22 degrees hotter inside their apartment
than it was at the Inner Harbor, using the heat index as a metric.

#### Supporting Code

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

    ## # A tibble: 1 x 10
    ##   date_hour_minute    mean_indoor_tem… mean_indoor_hea… mean_outdoor_te…
    ##   <dttm>                         <dbl>            <dbl>            <dbl>
    ## 1 2019-07-18 22:30:00             94.5              116               86
    ## # … with 6 more variables: mean_outdoor_heat_index <dbl>,
    ## #   indoor_temperature_difference <dbl>,
    ## #   indoor_heat_index_difference <dbl>, date <date>, hour <int>,
    ## #   minute <int>

### Fact: EMS calls for certain conditions spike when it gets very hot

“That helps explain why, during the summer in Baltimore, emergency
medical calls for dehydration, respiratory distress, kidney disease,
diabetes complications, heart attacks and heart failure spiked when the
heat index rose above 103 degrees, according to a Howard Center data
analysis.”

### Explanation

Using emergency medical call records from Baltimore City, we examined
calls during Summer 2018. They were aligned to heat index data captured
at the Inner Harbor and adjusted for the urban heat island using the ZIP
Code of each call location. The statistics in the table below represent
the number of hours that passed between calls for select conditions when
the temperature was in a given heat index bucket.

For example, in Summer 2018, when the heat index was under 80 degrees,
there was a medical call for dehydration every 41 hours. When the heat
index hit 103 degrees, the rate of calls increased dramatically – to one
every 2.2 hours. We saw less dramatic increases for cardiac arrest (a
call every 5 hours to a call every 3 hours) and other chronic
conditions.

#### Supporting Code

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
  spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
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

### Fact: Morning temperature on July 19

“It was the morning of July 19, and the outdoor temperature would rise
to 98 degrees.”

#### Explanation

July 19 was particularly hot in Baltimore. By 2 p.m., the temperature at
the Inner Harbor hit 98.1 degrees, using hourly averages, the hottest it
would get all day.

#### Supporting Code

``` r
 dmh %>%
  filter(`date` == date("2019-07-19")) %>%
  group_by(`date`) %>%
  summarise(max_temp = max(avg_hourly_temperature_dmh),
            max_heat_index = max(avg_hourly_heat_index_dmh)) 
```

    ## # A tibble: 1 x 3
    ##   date       max_temp max_heat_index
    ##   <date>        <dbl>          <dbl>
    ## 1 2019-07-19     98.1            108

### Fact: Extreme heat in Audrey DeWitt’s House

“DeWitt has two units, one on the first floor near where she sleeps and
another upstairs, but they don’t cool the entire house, she says.
Despite the units, a sensor placed on the first floor by University of
Maryland journalists recorded a heat index of 92 degrees at 6 a.m. on
July 20.”

“That was two degrees hotter than the heat index outdoors.”

#### Explanation

Air conditioning window units, not uncommon in our reporting, often
weren’t enough to combat the heat. In Audrey DeWitt’s home, the heat
index hit 91.8 degrees at 6 a.m. on July 20.

#### Supporting Code

``` r
audrey_day_hourly_averages %>%
  mutate(date=date(date_hour)) %>%
  mutate(hour=hour(date_hour)) %>%
  filter(date == "2019-07-20",
         hour == 6) 
```

    ## # A tibble: 1 x 9
    ##   date_hour           mean_indoor_tem… mean_indoor_hea… mean_outdoor_te…
    ##   <dttm>                         <dbl>            <dbl>            <dbl>
    ## 1 2019-07-20 06:00:00             87.8             91.8               84
    ## # … with 5 more variables: mean_outdoor_heat_index <dbl>,
    ## #   indoor_temperature_difference <dbl>,
    ## #   indoor_heat_index_difference <dbl>, date <date>, hour <int>

### Fact: Data for Graphic on Temperature and Health Conditions

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

#### Explanation

We examined rates of chronic conditions among low-income people in
different parts of Baltimore by examining in-patient hospital admissions
by people on Medicaid in Baltimore, and discovered that low-income
people in different parts of the city had different prevalance rates for
chronic conditions affected by heat – asthma, COPD, heart disease,
kidney disease and diabetes. And, we found, those differences varied in
line with temperature differences in the area in which they lived.

There were moderate to strong positive relationships (kidney disease, r
= .6; copd, r=.75; asthma, r=.51; heart\_disease, r=.71; diabetes, r=.5)
between a ZIP code’s prevalance rate for chronic medical conditions as
diagnosed in inpatient hospital visits and a ZIP code’s median afternoon
temperature as measured by urban heat island researchers in August 2018.
That is to say: the higher the neighborhood temperature, the higher the
disease rate amongst the poorest inhabitants, and vice versa. This is
not a causal relationship we are describing.

We’ve also output the table for the graphic here.

#### Supporting Code

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

### Fact: Asthma rates in Baltimore

“Asthma rates in low-income areas like McElderry Park and Broadway East
are higher than in more affluent areas.”

#### Explanation

There is a high degree of correlation between a genographic population’s
asthma prevalance rate in Baltimore, and that populations level of
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
Baltimore, rich neighborhoods tend to have lower prevalance of asthma,
compared with poorer ones.

McElderry Park is mostly contained in 21205 and Broadway East is mostly
in 21213. 21205 had the third highest asthma rate (12.6 percent) in the
city and the second highest poverty rate (37.13), 21213 had the second
highest asthma rate (13.2 percent) and the sixth-highest poverty rate
(28.2 percent). ZIP Code 21209, in a much wealthier part of town, had
the city’s lowest poverty rate (7.6 percent) and the city’s second
lowest asthma prevalance (4.8 percent).

#### Supporting Code

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

``` r
  op_er_full_zip_disease_heat %>%
    select(ZIPCODE, asthma_prev, median_household_income_d, `poverty_%`) %>%
    arrange(desc(`poverty_%`))
```

    ## # A tibble: 26 x 4
    ##    ZIPCODE asthma_prev median_household_income_d `poverty_%`
    ##      <dbl>       <dbl>                     <dbl>       <dbl>
    ##  1   21223      0.149                      26899        38.5
    ##  2   21205      0.126                      28675        37.1
    ##  3   21217      0.126                      28116        36.7
    ##  4   21201      0.114                      33877        30.8
    ##  5   21202      0.0999                     44656        29.3
    ##  6   21213      0.132                      34917        28.2
    ##  7   21216      0.123                      37314        26.2
    ##  8   21215      0.0763                     36500        25.6
    ##  9   21225      0.113                      41904        24.9
    ## 10   21218      0.117                      43352        24.5
    ## # … with 16 more rows

``` r
  op_er_full_zip_disease_heat %>%
    select(ZIPCODE, asthma_prev, median_household_income_d, `poverty_%`) %>%
    arrange(`poverty_%`)
```

    ## # A tibble: 26 x 4
    ##    ZIPCODE asthma_prev median_household_income_d `poverty_%`
    ##      <dbl>       <dbl>                     <dbl>       <dbl>
    ##  1   21209      0.0476                     78900        7.65
    ##  2   21237      0.0785                     64758        7.90
    ##  3   21210      0.0467                     93203        8.35
    ##  4   21211      0.0725                     63733       10.8 
    ##  5   21239      0.108                      52811       11.9 
    ##  6   21214      0.0954                     65792       12.1 
    ##  7   21207      0.0793                     59013       13.0 
    ##  8   21212      0.101                      73244       13.3 
    ##  9   21206      0.113                      50975       14.0 
    ## 10   21222      0.0777                     50644       14.4 
    ## # … with 16 more rows

``` r
  op_er_full_zip_disease_heat %>%
    select(ZIPCODE, asthma_prev, median_household_income_d, `poverty_%`) %>%
    arrange(`asthma_prev`)
```

    ## # A tibble: 26 x 4
    ##    ZIPCODE asthma_prev median_household_income_d `poverty_%`
    ##      <dbl>       <dbl>                     <dbl>       <dbl>
    ##  1   21210      0.0467                     93203        8.35
    ##  2   21209      0.0476                     78900        7.65
    ##  3   21211      0.0725                     63733       10.8 
    ##  4   21215      0.0763                     36500       25.6 
    ##  5   21222      0.0777                     50644       14.4 
    ##  6   21237      0.0785                     64758        7.90
    ##  7   21207      0.0793                     59013       13.0 
    ##  8   21224      0.0878                     65501       17.7 
    ##  9   21226      0.0918                     73438       15.4 
    ## 10   21227      0.0923                     64692       16.5 
    ## # … with 16 more rows

### Fact: Baltimore’s Hottest Neighborhood

"..McElderry Park, Baltimore’s hottest neighborhood, has three young
sons with the disease.

#### Explanation

The median afternoon temperature, as measured in late August 2018 by
researchers studying Baltimore’s urban heat island, in McElderry Park
was 99.4 degrees, the highest in the city.

#### Supporting Code

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

### Fact: Extreme heat in Stephanie Pingley’s house

“Pingley says that room can get as “hot as Hades,” which is reflected in
readings of a sensor placed there. The heat index in that room climbed
as high as 118 degrees and never dropped below 88 degrees for a
seven-day stretch."

#### Explanation

The sensor readings taken inside of Stephanie Pingley’s home reflect the
extreme heat her family experienced. The minimum indoor heat index value
between July 16 and July 22 was 88 degrees, and the max was 118. These
values reflect minute by minute readings.

#### Supporting Code

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

### Fact: EMS calls for certain conditions spike when it gets very hot

“In Baltimore, the rate of emergency medical calls for psychiatric
disorders and drug and alcohol overdoses increased dramatically when the
heat index hit 103 degrees during Summer 2018, the Howard Center data
analysis found.”

### Explanation

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

#### Supporting Code

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
  spread(adjusted_heat_index_nws_five_scale_bucket, hours_per_call) %>%
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
