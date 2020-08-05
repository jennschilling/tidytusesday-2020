# Author : Jenn Schilling
# Title: #TidyTuesday European Energy
# Date: 8/4/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(ggridges) 
#https://www.datanovia.com/en/blog/elegant-visualization-of-density-distribution-in-r-using-ridgeline/ 
library(skimr)


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals


#### Explore the Data ####
skim(energy_types)

# Country Name is Missing for 8 rows
energy_types %>% filter(is.na(country_name))

# Fix UK Country Name
energy_types <- energy_types %>%
  mutate(country_name = ifelse(country == 'UK', 
                               'United Kingdom',
                               country_name))

skim(country_totals)

# Country Name is Missing for 8 rows
country_totals %>% filter(is.na(country_name))

# Fix UK Country Name
country_totals <- country_totals %>%
  mutate(country_name = ifelse(country == 'UK', 
                               'United Kingdom',
                               country_name))

# 2016 is missing one value
country_totals %>% filter(is.na(`2016`)) # Malta has Exports of NA in 2016


# Pumped hydro (level 2 measure) is a sub measure of hydro (level 1 measure), 
# if you include it in sum it's like double counting.
# Remove Level 2 from Energy Types
energy_types <- energy_types %>%
  filter(level != 'Level 2')

# Make Data Long
energy_types_long <- energy_types %>%
  pivot_longer(cols = `2016`:`2018`, names_to = 'year', values_to = 'gwh')

country_totals_long <- country_totals %>%
  pivot_longer(cols = `2016`:`2018`, names_to = 'year', values_to = 'gwh')
