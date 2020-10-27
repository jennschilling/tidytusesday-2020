# Author : Jenn Schilling
# Title: #TidyTuesday Canadian Wind Turbines
# Date: 10/27/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)

#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-10-27')

wind_turbines <- tuesdata$`wind-turbine`

#### Explore Data ####

glimpse(wind_turbines)

skim(wind_turbines)

table(wind_turbines$commissioning_date)
