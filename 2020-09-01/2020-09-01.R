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

#### Look at top increases and decreases in production ####

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

#### Uganada Focus ####

uganda_crops <- key_crop_yields_long %>% 
  filter(entity == 'Uganda')

uganda_fertilizer <- fertilizer_clean %>%
  filter(entity == 'Uganda')

uganda_arable_land <- clean_names(arable_land) %>%
  filter(entity == 'Uganda')

uganda_land_use <- clean_names(land_use) %>%
  filter(entity == 'Uganda')

uganda_tractor <- clean_names(tractor) %>%
  filter(entity == 'Uganda')

uganda_crops %>%
  ggplot() +
  geom_line(aes(x = year,
                y = crop_production)) +
  facet_wrap(~crop) +
  theme_classic()

uganda_fertilizer %>%
  ggplot() +
  geom_point(aes(x = nitrogen_fertilizer_use_kilograms_per_hectare,
                 y = cereal_yield_tonnes_per_hectare)) +
  geom_text(aes(x = nitrogen_fertilizer_use_kilograms_per_hectare,
                y = cereal_yield_tonnes_per_hectare,
                label = year)) +
  theme_classic()

uganda_arable_land %>%
  ggplot() +
  geom_line(aes(x = year,
                y = arable_land_needed_to_produce_a_fixed_quantity_of_crops_1_0_1961)) +
  theme_classic()

uganda_land_use %>%
  filter(!is.na(change_to_land_area_used_for_cereal_production_since_1961)) %>%
  ggplot() +
  geom_point(aes(x = year,
                 y = change_to_land_area_used_for_cereal_production_since_1961))+
  theme_classic()
  
uganda_land_use %>%
  filter(!is.na(cereal_yield_index)) %>%
  ggplot() +
  geom_point(aes(x = year,
                 y = cereal_yield_index))+
  theme_classic()

uganda_land_use %>%
  filter(!is.na(cereal_yield_index)) %>%
  ggplot() +
  geom_point(aes(x = year,
                 y = total_population_gapminder))+
  theme_classic()

uganda_tractor %>%
  filter(!is.na(tractors_per_100_sq_km_arable_land)) %>%
  ggplot() +
  geom_point(aes(x = tractors_per_100_sq_km_arable_land,
                 y = cereal_yield_kilograms_per_hectare_kg_per_hectare)) +
  geom_text(aes(x = tractors_per_100_sq_km_arable_land,
                y = cereal_yield_kilograms_per_hectare_kg_per_hectare,
                label = year)) +
  theme_classic()

#### Explore Bananas ####

bananas <- key_crop_yields_long %>%
  filter(crop == "Bananas")

banana_old_new <- bananas %>%
  filter(year %in% c(1961, 2018)) %>%
  filter(!is.na(code)) %>%
  filter(entity != 'World') %>%
  pivot_wider(names_from = year, 
              names_prefix = "yr_", 
              values_from = crop_production) %>%
  filter(!is.na(yr_1961)) %>%
  filter(!is.na(yr_2018)) %>%
  mutate(rank = rank(-yr_2018)) %>%
  mutate(color = ifelse(rank <= 10, "Top 10", 
                        ifelse(yr_1961 >= 30 &rank > 10, "Prior Top", ""))) %>%
  pivot_longer(cols = yr_1961:yr_2018,
               names_to = "year",
               values_to = "crop_production",
               values_drop_na = TRUE) %>%
  mutate(year = sub("yr_", "", year)) %>%
  mutate(year = as.numeric(year))

ggplot(banana_old_new) +
  geom_point(aes(x = year, y = crop_production,
                 group = entity, color = color),
             show.legend = FALSE) +
  scale_color_manual(values = c("#969696", "#969696", "#7a0177")) +
  geom_line(data = subset(banana_old_new, color == ""),
            aes(x = year, y = crop_production, 
                group = entity),
            color = "#969696",
            size = 0.5,
            show.legend = FALSE) +
  geom_line(data = subset(banana_old_new, color == "Prior Top"),
            aes(x = year, y = crop_production, 
                group = entity),
            color = "#969696",
            size = 1.5,
            show.legend = FALSE) +
  geom_line(data = subset(banana_old_new, color == "Top 10"),
            aes(x = year, y = crop_production, 
                group = entity),
            color = "#7a0177",
            size = 1.5,
            show.legend = FALSE) +
  geom_text(data = subset(banana_old_new, year == 2018 & rank <= 10),
            aes(x = year, y = crop_production, label = entity),
            vjust = 0.2,
            hjust = -0.1,
            color = "#7a0177") +
  geom_text(data = subset(banana_old_new, 
                          year == 1961 & crop_production >= 30 & 
                            rank > 10 & entity != 'Timor' &
                            entity != 'Portugal'),
            aes(x = year, y = crop_production, label = entity),
            vjust = 0.2,
            hjust = 1.1,
            color = "#969696") +
  geom_text(data = subset(banana_old_new, 
                          year == 1961 & crop_production >= 30 & 
                            rank > 10 & entity == 'Timor'),
            aes(x = year, y = crop_production, label = entity),
            vjust = 1.3,
            hjust = 1.1,
            color = "#969696") +
  geom_text(data = subset(banana_old_new, 
                          year == 1961 & crop_production >= 30 & 
                            rank > 10 & entity == 'Portugal'),
            aes(x = year, y = crop_production, label = entity),
            vjust = 0.4,
            hjust = 1.1,
            color = "#969696") +
  labs(y = "",
       x = "",
       title = "Banana Yield (tonnes per hectare) 1961 to 2018",
       caption = "TidyTuesday 01 Sep 2020 | Data: Our World in Data | Designer: Jenn Schilling | jennschilling.me") +
  scale_x_continuous(limits = c(1958, 2022)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color = "#969696"),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color = "#cccccc"),
        panel.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
        plot.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
        plot.title.position = "plot",
        text = element_text(family = 'Verdana'))

ggsave("2020-09-01\\banana.yield.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       dpi = 500)
