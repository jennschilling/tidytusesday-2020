# Author : Jenn Schilling
# Title: #TidyTuesday Toronto Shelters
# Date: 12/01/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-12-01')

hike_data <- tuesdata$hike_data %>% 
  unique(.)

#### Explore the Data ####

skim(hike_data)
