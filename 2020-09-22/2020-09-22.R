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

members %>% 
  filter(hired == TRUE) %>%
  select(expedition_role) %>%
  unique(.) %>%
  View(.)

members %>%
  filter(hired == TRUE) %>%
  group_by(year) %>%
  count(age) %>%
  View(.)

staff <- members %>%
  filter(hired == TRUE) %>%
  mutate(age_bin = cut_interval(age, n = 5),
         age_bin_name = case_when(
           age_bin == "[15,25.4]" ~ "15 to 25",
           age_bin == "(25.4,35.8]" ~ "26 to 35",
           age_bin == "(35.8,46.2]" ~ "36 to 46",
           age_bin == "(46.2,56.6]" ~ "47 to 56",
           age_bin == "(56.6,67]" ~ "57 to 67",
           TRUE ~ "Unknown"
         ))

staff %>%
  ggplot() +
  geom_bar(aes(x = age_bin_name,
               fill = died))

died <- members %>%
  filter(died == TRUE)

died %>%
  ggplot() +
  geom_bar(aes(x = hired,
               fill = hired))
