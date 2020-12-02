# Author : Jenn Schilling
# Title: #TidyTuesday Toronto Shelters
# Date: 12/01/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(skimr)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-12-01')

shelters <- tuesdata$shelters 

#### Explore the Data ####

skim(shelters)

# Compute percent capacity
# Add month and year date parts
shelters <- shelters %>%
  mutate(perc_capacity = occupancy / capacity) %>%
  mutate(month = month(occupancy_date),
         year = year(occupancy_date))

# Look at percent capacity by sector and year
shelters %>%
  filter(perc_capacity <= 1) %>%
  ggplot +
  geom_histogram(aes(x = perc_capacity,
                     fill = sector)) +
  facet_wrap(sector ~ year)

shelters %>%
  filter(perc_capacity <= 1) %>%
  group_by(year, month, sector) %>%
  summarise(median_perc_cap = median(perc_capacity),
            mean_perc_cap = mean(perc_capacity)) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = mean_perc_cap, 
                color = sector)) +
  facet_wrap(sector~year)

shelters %>%
  filter(capacity > 0) %>%
  group_by(year, month, sector) %>%
  summarise(total_cap = sum(capacity),
            total_occ = sum(occupancy),
            total_perc_cap = total_occ / total_cap) %>%
  ggplot(.) +
  geom_line(aes(x = month, y = total_perc_cap, 
                color = sector)) +
  facet_wrap(sector~year)

shelters %>%
  filter(capacity > 0) %>%
  group_by(year, month, sector) %>%
  summarise(total_cap = sum(capacity),
            total_occ = sum(occupancy),
            total_perc_cap = total_occ / total_cap) %>%
  ggplot(.) +
  geom_bar(aes(x = month, y = total_cap, 
                fill = sector),
           stat = "identity") +
  facet_wrap(sector~year)


