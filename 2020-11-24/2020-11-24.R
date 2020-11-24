# Author : Jenn Schilling
# Title: #TidyTuesday Washington Hiking
# Date: 11/24/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-11-24')

hike_data <- tuesdata$hike_data

#### Explore the Data ####

skim(hike_data)



#### Data manipulation ####

# Make columns numeric and separate out length from the descriptors (miles, rountrip/one-way)
hike_data_num <- hike_data %>%
  mutate(gain = as.numeric(gain),
         highpoint = as.numeric(highpoint),
         rating = as.numeric(rating)) %>%
  separate(length, into = c("length", "length_type"), sep = ",") %>%
  separate(length, into = c("length", "length_dist_type"), sep = "\\s") %>%
  mutate(length = as.numeric(length))

skim(hike_data_num)

table(hike_data_num$location)

# Separate out location
hike_data_num <- hike_data_num %>%
  mutate(location2 = location) %>%
  separate(location2, into = c("area", "region"), sep = " --")

table(hike_data_num$area)

#### Plotting ####

hike_data_num %>%
  filter(length < 50) %>%
  ggplot() +
  geom_point(aes(x = length, y = rating, color = length_type)) +
  facet_wrap(~area)
