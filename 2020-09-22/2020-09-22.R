# Author : Jenn Schilling
# Title: #TidyTuesday Himalayan Climbing Expeditions
# Date: 9/22/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggtext)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-22')

members <- tuesdata$members

#### Explore Data ####

skim(members)

ggplot(members) +
  geom_jitter(aes(x = year,
                  y = age))

