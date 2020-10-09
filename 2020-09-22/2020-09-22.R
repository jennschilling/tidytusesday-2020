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

peaks <- tuesdata$peaks

expeditions <- tuesdata$expeditions

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

# Female expedition members

female <- members %>%
  filter(sex == "F")

table(female$expedition_role)

table(female$year)

table(female$hired)

table(female$solo)

female %>% filter(solo == TRUE) %>% View(.)

table(female$peak_name)

skim(peaks)

# Join peaks data with female member data and expedition data to find the
# expeditions and peaks that had first ascents with female members

female_plus <- left_join(female, peaks, by = "peak_id") 

female_first <- female_plus %>% 
  filter(expedition_id == first_ascent_expedition_id) %>%
  group_by(expedition_id) %>%
  dplyr::summarize(num_female = n()) %>%
  ungroup() %>%
  left_join(., expeditions, by = "expedition_id") %>%
  mutate(perc_female = num_female / members)

female_first_count <- female_first %>%
  group_by(year) %>%
  summarise(count_female_peaks = n()) %>%
  ungroup()

first_ascent_count <- peaks %>%
  group_by(first_ascent_year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(!is.na(first_ascent_year)) %>%
  filter(first_ascent_year != '201') %>%
  mutate(year = first_ascent_year) %>%
  left_join(., female_first_count, by = "year") %>%
  select(first_ascent_year, count, count_female_peaks) %>%
  mutate(count_female_peaks = ifelse(is.na(count_female_peaks), 
                                     0, 
                                     count_female_peaks))

#### Plot ####


ggplot(first_ascent_count) +
  geom_bar(aes(x = first_ascent_year, y = count),
           stat = "Identity", fill = "#969696") +
  geom_bar(aes(x = first_ascent_year, y = count_female_peaks), 
           stat = "Identity", fill = "#7a0177") +
  labs(x = "Year of First Ascent",
       y = "Number of First Ascents") +
  theme_classic() 

            