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

only_once_details %>%
  count(round_stage) %>%
  ggplot() +
  geom_bar(aes(x = reorder(round_stage, -n), y = n), 
           stat = "Identity") +
  labs(title = "Number of Single Use Obstacles by Round",
       x = "",
       y = "") +
  theme_classic()


# Obstacles used multiple times
ninja_warrior %>%
  group_by(obstacle_name) %>%
  count(.) %>%
  filter(n >= 10) %>%
  ggplot() +
  geom_bar(aes(x = reorder(obstacle_name, -n),
               y = n),
           stat = "identity") +
  labs(title = "Number of Times Obstacles were Used",
       subtitle = "Filtered to obstacles used 10+ times",
       x = "",
       y = "") +
  theme_classic()

# Warped Wall is used 86 times 

# How many individual seasons/locations/rounds are there?
ninja_warrior %>%
  distinct(season, location, round_stage) %>%
  nrow(.) # 122

# How many individual seasons/locations are there?
ninja_warrior %>%
  distinct(season, location) %>%
  nrow(.) # 51

# When is Warped Wall used?
ninja_warrior %>%
  filter(obstacle_name == "Warped Wall") %>%
  count(round_stage, obstacle_order) %>%
  ggplot() +
  geom_bar(aes(x = reorder(round_stage, -n),
               y = n,
               fill = as.factor(obstacle_order)),
           position = "dodge",
           stat = "identity") +
  labs(title = "Number of Times Warped Wall is Used by Round",
       x = "",
       y = "") +
  theme_classic()

# How do the obstacles and their order change through time?

# Number of obstacle courses by season
ninja_warrior %>%
  distinct(season, location, round_stage) %>%
  count(season)

ninja_warrior %>%
  filter(season == 1) %>%
  ggplot() +
  geom_label(aes(x = round_stage,
                 y = obstacle_order,
                 label = obstacle_name))


