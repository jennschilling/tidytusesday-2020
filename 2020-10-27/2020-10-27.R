
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

# Look at commissioning dates 

table(wind_turbines$commissioning_date)

wind_turbines %>%
  ggplot() +
  geom_bar(aes(x = commissioning_date))

# Create single year numeric field
wind_turbines <- wind_turbines %>%
  mutate(commission_year = parse_number(commissioning_date))

# Provinces

table(wind_turbines$province_territory)

# Manufacturers

table(wind_turbines$manufacturer)

ggplot(wind_turbines) +
  geom_bar(aes(x = manufacturer)) # 5 manufacturers dwarf the rest, out of those 1 stands out

# Turbine number in project

table(wind_turbines$turbine_number_in_project)

# Separate into turbine number and number of turbines in project
wind_turbines <- wind_turbines %>%
  separate(turbine_number_in_project, 
           c("turbine_number_in_project", 
             "number_turbines_in_project"),
           sep = "/")
