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

# 500 records
# 15 missing year_last_seen
# red_list_category - Extinct, Extinct in the Wild

year_missing <- plants %>%
  filter(is.na(year_last_seen))
