# Author : Jenn Schilling
# Title: #TidyTuesday Extinct Plants
# Date: 8/18/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggtext)
library(tidytext)

# Load  fonts
loadfonts(quiet = TRUE)


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-08-18')

plants <- tuesdata$plants

#### Explore the Data ####

skim(plants)

# 500 records, 500 species
# 15 missing year_last_seen (3%)
# red_list_category - Extinct, Extinct in the Wild
# 75% have unknown action
# 21% have unknown threat

year_missing <- plants %>%
  filter(is.na(year_last_seen))

#### Exploratory Plots ####

# Number by country
plants %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(country))) +
  coord_flip()

# Number by continent
plants %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(continent))) +
  coord_flip()

# Number by year_last_seen
plants %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(year_last_seen))) +
  coord_flip()

# Number by year_last_seen and continent
plants %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(year_last_seen))) +
  coord_flip() +
  facet_wrap(~ continent)

# Number by taxonomic group
plants %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(group))) +
  coord_flip()
# Almost all are flowering plants

# Number by Threat
plants %>%
  pivot_longer(threat_AA:threat_NA, names_to = "threat") %>%
  separate(threat, c("threat", "threat_type"), sep = "_") %>%
  select(-threat) %>% 
  filter(value == 1) %>%
  select(-value) %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(threat_type))) +
  coord_flip()

# Number by Action
plants %>%
  pivot_longer(action_LWP:action_NA, names_to = "action") %>%
  separate(action, c("action", "action_type"), sep = "_") %>%
  select(-action) %>% 
  filter(value == 1) %>%
  select(-value) %>%
  ggplot() +
  geom_bar(aes(x = forcats::fct_infreq(action_type))) +
  coord_flip()

#### Final Plot ####

# Idea
# Timeline by last year seen, continent, threat, action, group
# Dots for each plant
# Color/position by continent
# How to encode group, threat, action ???