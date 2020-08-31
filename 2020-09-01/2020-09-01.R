# Author : Jenn Schilling
# Title: #TidyTuesday Global Crop Yiels
# Date: 8/25/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggtext)
library(stringr)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-01')

key_crop_yields <- tuesdata$key_crop_yields
arable_land <- tuesdata$arable_land_pin
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production
tractor <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture

#### Explore the Data ####

skim(key_crop_yields) # Year is numeric, 1961 - 2018

skim(arable_land) # Year is numeric, 1961 - 2014

skim(fertilizer) # Year is numeric, 1961 - 2018

skim(land_use) # Year is character and goes back to BCE

skim(tractor) # Year is character and goes back to BCE
