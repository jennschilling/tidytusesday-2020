# Author : Jenn Schilling
# Title: #TidyTuesday Ninja Warrior
# Date: 12/15/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-12-15')

ninja_warrior <- tuesdata$ninja_warrior 

#### Explore the Data ####

skim(ninja_warrior)

# There are not too many locations - Las Vegas is a popular location
table(ninja_warrior$location)

# There are many obstacles
table(ninja_warrior$obstacle_name)

# Which obstacles appear 10 or more times?
ninja_warrior %>%
  group_by(obstacle_name) %>%
  count(.) %>%
  filter(n >= 10)

# Which obstacles only appear once?
only_once <- ninja_warrior %>%
  group_by(obstacle_name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n == 1)

only_once_details <- left_join(only_once, ninja_warrior)

