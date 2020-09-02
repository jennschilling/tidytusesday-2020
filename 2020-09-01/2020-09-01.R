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
library(janitor)

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

# Make Crop Yield Data Long
key_crop_yields_long <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production",
               values_drop_na = TRUE) %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())

# Look at fertilizer use
fertilizer_clean <- clean_names(fertilizer) %>% # clean up column names
  mutate(year = as.numeric(year))

fertilizer_clean %>%
  filter(!is.na(code)) %>% # remove entities that are not countries
  filter(year >= 2007 & year <= 2017) %>% # get last 10 years of data
  ggplot() +
  geom_point(aes(x = nitrogen_fertilizer_use_kilograms_per_hectare,
                 y = cereal_yield_tonnes_per_hectare)) +
  facet_wrap(~year) +
  theme_classic()

# Look at top increases and decreases in production

# Compute percent change between 1961 and 2018
key_crop_yields_chg <- key_crop_yields_long %>%
  pivot_wider(names_from = year,
              names_prefix = "yr_",
              values_from = crop_production) %>%
  mutate(perc_chg = (yr_2018 - yr_1961) / yr_1961) 

# Get top 10 by percent change
key_crop_yield_chg_top <- key_crop_yields_chg %>% 
  filter(!is.na(code)) %>%
  top_n(10, perc_chg) %>%
  pivot_longer(cols = yr_1961:yr_2018,
               names_to = "year",
               values_to = "crop_production",
               values_drop_na = TRUE) %>%
  mutate(year = sub("yr_", "", year))
 
# Get bottom 10 by percent change 
key_crop_yield_chg_bottom <- key_crop_yields_chg %>% 
  filter(!is.na(code)) %>%
  top_n(-10, perc_chg) %>%
  pivot_longer(cols = yr_1961:yr_2018,
               names_to = "year",
               values_to = "crop_production",
               values_drop_na = TRUE) %>%
  mutate(year = sub("yr_", "", year))

# Graph top 10
ggplot(key_crop_yield_chg_top) +
  geom_point(aes(x = year, 
                 y = crop_production,
                 color = crop)) +
  facet_wrap(~entity) +
  theme_classic()

# Greece - Bananas looks interesting
# Jordan - Maize and Malawi - Bananas also show big increases
# 5 of 10 are Maize

# Graph bottom 10
ggplot(key_crop_yield_chg_bottom) +
  geom_point(aes(x = year, 
                 y = crop_production,
                 color = crop)) +
  facet_wrap(~entity) +
  theme_classic()

# Timor - Bananas is interesting 
# Burkina Faso - Cassava shows a bump in the decrease
# 5 of 10 are Cocoa Beans