---
title: "Role of Trees Analysis"
author: "Roxanne Ready, Sean Mussenden | Howard Center for Investigative Journalism"
date: "8/6/2019"
output: 
  html_document: 
    keep_md: true
---

## Introduction

This R markdown document describes the methodology and results of a portion of the data analysis we conducted in support of a reporting project examining the effects of tree canopy inequity across the city of Baltimore, especially as it relates to climate change.

## Setup

Before running this file, **please view and run the [cleaning script](https://github.com/smussenden/2019-baltimore-climate-health-project-data-repo/blob/master/documentation/code-red-data-cleaning.Rmd)** for this project. As well as outputting necessary cleaned data for the following ananlysis, that document also includes the following items necessary to understand this analysis: 

* definitions
* information about the source data 
* methodology 
* software tools


```r
#######################
#### Load Packages ####
#######################

library(tidyverse)
require(scales) # For percent labeling on distribution tables
library(DescTools) # For %like% operator
#library(here) # For cleaner file path writing

# Turn off scientific notation in RStudio (prevents coersion to character type)
options(scipen = 999)

#########################
#### Store Variables ####
#########################

#### Common save path ####
path_to_data <- "../data/output-data/cleaned/"

###################
#### Load Data ####
###################

blocks_tree_temp_demographics <- 
  read_csv(paste0(path_to_data, "blocks_tree_temp_demographics.csv")) %>%
  mutate_at(vars(matches("geoid10")), as.character) # Recast non-calculable variables as characters

csa_tree_temp_demographics <- 
  read_csv(paste0(path_to_data, "csa_tree_temp_demographics.csv"))

nsa_tree_temp <- 
  read_csv(paste0(path_to_data, "nsa_tree_temp.csv"))

zcta_tree_temp_demographics <- 
  read_csv(paste0(path_to_data, "zcta_tree_temp_demographics.csv")) %>%
  mutate_at(vars(matches("zcta")), as.character) # Recast non-calculable variables as characters

street_trees_nsa_categorized <- 
  read_csv(paste0(path_to_data, "street_trees_nsa_categorized.csv"))
```

## Temperature Analysis

### Blocks by temperature 

The following arranges and ranks blocks across the city by temperature in the afternoon of August 29, 2018:


```r
blocks_tree_temp_demographics %>%
  select(geoid10, temp_mean_aft) %>%
  mutate(rank = rank(-temp_mean_aft)) %>%
  arrange(rank)
```

```
## # A tibble: 13,598 x 3
##    geoid10         temp_mean_aft  rank
##    <chr>                   <dbl> <dbl>
##  1 245100602005002          101.     1
##  2 245100602004000          101.     2
##  3 245100602005001          100.     3
##  4 245100602004001          100.     4
##  5 245100702004006          100.     5
##  6 245100702004005          100.     6
##  7 245100702004002          100.     7
##  8 245100603001013          100.     8
##  9 245100401002000          100.     9
## 10 245100702004007          100.    10
## # … with 13,588 more rows
```

Below, we see the block on N. Milton Avenue between Oliver and Federal is one of the city's hottest, ranking at 236 out of 13,598 blocks. The block GEOID for this block was pulled from QGIS and breaks down into the following codes:

* State: 24
* County: 510
* Tract: 080301
* Block: 1000


```r
blocks_tree_temp_demographics %>%
  select(geoid10, temp_mean_aft) %>%
  mutate(rank = rank(-temp_mean_aft)) %>%
  filter(geoid10 == "245100803011000")
```

```
## # A tibble: 1 x 3
##   geoid10         temp_mean_aft  rank
##   <chr>                   <dbl> <dbl>
## 1 245100803011000          98.3   236
```

### Neighborhood Statistical Areas by temperature

The following arranges and ranks NSAs across the city by temperature in the afternoon of August 29, 2018.

First, we can see that all of the top 10 hottest NSAs are located in the south of the city, and many are in the south-east.


```r
# Top 10 hottest neighborhoods
nsa_tree_temp %>%
  select(nsa_name, temp_mean_aft) %>%
  mutate(rank = rank(-temp_mean_aft)) %>%
  arrange(rank)
```

```
## # A tibble: 278 x 3
##    nsa_name              temp_mean_aft  rank
##    <chr>                         <dbl> <dbl>
##  1 mcelderry park                 99.4     1
##  2 milton-montford                99.3     2
##  3 patterson place                98.6     3
##  4 dunbar-broadway                98.3     4
##  5 ellwood park/monument          98.3     5
##  6 penn-fallsway                  98.3     6
##  7 pleasant view gardens          98.3     7
##  8 madison-eastend                97.9     8
##  9 old goucher                    97.9     9
## 10 biddle street                  97.9    10
## # … with 268 more rows
```

Looking at the 10 coolest NSAs, we see it is generally true that they are located far to the west and north, on the outskirts of Baltimore.


```r
# Top 10 coolest neighborhoods
nsa_tree_temp %>%
  select(nsa_name, temp_mean_aft) %>%
  mutate(rank = rank(-temp_mean_aft)) %>%
  arrange(desc(rank))
```

```
## # A tibble: 278 x 3
##    nsa_name                 temp_mean_aft  rank
##    <chr>                            <dbl> <dbl>
##  1 gwynns falls/leakin park          90.7   278
##  2 dickeyville                       91.0   277
##  3 fairmont                          91.1   276
##  4 purnell                           91.2   275
##  5 wakefield                         91.4   274
##  6 mount washington                  92.4   273
##  7 ten hills                         92.4   272
##  8 franklintown                      92.4   271
##  9 west forest park                  92.6   270
## 10 windsor hills                     92.7   269
## # … with 268 more rows
```

Below we see:

* McElderry Park was the hottest, at 99.4 degrees Fahrenheit.
* Gwynns Falls/Leakin Park was the coolest, at 90.8 degrees Fahrenheit.


```r
nsa_tree_temp %>%
  select(nsa_name, temp_mean_aft) %>%
  filter((temp_mean_aft == min(temp_mean_aft)) | (temp_mean_aft == max(temp_mean_aft))) %>%
  arrange(desc(temp_mean_aft))
```

```
## # A tibble: 2 x 2
##   nsa_name                 temp_mean_aft
##   <chr>                            <dbl>
## 1 mcelderry park                    99.4
## 2 gwynns falls/leakin park          90.7
```

The difference in temperatures between the city's hottest and coolest neighborhoods is -8.65 degrees Fahrenheit.

Below, we see the relative ranks of the Broadway East and Roland Park neighborhoods, placing Roland Park at 263 out of 278 neighborhoods, while Roland Park is ranked at 16.


```r
nsa_tree_temp %>%
  select(nsa_name, temp_mean_aft) %>%
  mutate(rank = rank(-temp_mean_aft)) %>%
  arrange(rank) %>%
  filter((nsa_name %like% "roland park") | nsa_name %like% ("broadway east"))
```

```
## # A tibble: 2 x 3
##   nsa_name      temp_mean_aft  rank
##   <chr>                 <dbl> <dbl>
## 1 broadway east          97.4    16
## 2 roland park            93.3   263
```

These data are visualized in the following choropleth map, which was exported from QGIS and prettified slightly in Illustrator:

<img src="../documentation/choropleth-temperature-full-withlines-01.png" width="400" alt="Choropleth map of temperatures across Baltimore NSAs" />

## Tree Canopy Analysis



## Demographics Analysis



## Street Trees Analysis



## North Milton Street Trees



## Plots and Graphs

