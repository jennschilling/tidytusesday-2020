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

# Leave out 15 with missing year

# Use geom_point with 
# y = increment of count to stack points / index of species by year and continent
# x = year_last_seen combined with continent somehow
# shape could by group since there are only 6 and most are flowering plant
# threat could be size 
#  or if I use all the same shape (circles) I could use a line position off the circle to indicate threat
# action could be color - only 6, most are unknown, could make unknown a grey/neutral color

# PROBLEM - multiple threats and multiple actions for a single plant

plants.long <- plants %>%
  filter(!is.na(year_last_seen)) %>%
  mutate(year_last_seen = factor(year_last_seen, levels = c("Before 1900",
                                                            "1900-1919",
                                                            "1920-1939",
                                                            "1940-1959",
                                                            "1960-1979",
                                                            "1980-1999",
                                                            "2000-2020"))) %>%
  pivot_longer(threat_AA:threat_NA, names_to = "threat") %>%
  separate(threat, c("threat", "threat_type"), sep = "_") %>%
  select(-threat) %>% 
  filter(value == 1) %>%
  select(-value) %>%
  pivot_longer(action_LWP:action_NA, names_to = "action") %>%
  separate(action, c("action", "action_type"), sep = "_") %>%
  select(-action) %>% 
  filter(value == 1) %>%
  select(-value)
  
  
# Create data frame with factor levels and index of each plant needed for creating my own dot plot
plants.dat <-  plants %>%
    filter(!is.na(year_last_seen)) %>%
    mutate(num_threats = select(., threat_AA:threat_NA) %>% rowSums()) %>%
    mutate(year_last_seen = factor(year_last_seen, levels = c("Before 1900",
                                                              "1900-1919",
                                                              "1920-1939",
                                                              "1940-1959",
                                                              "1960-1979",
                                                              "1980-1999",
                                                              "2000-2020"))) %>%
    mutate(continent = factor(continent, levels = c("Africa",
                                                    "Asia",
                                                    "Europe",
                                                    "North America",
                                                    "Oceania",
                                                    "South America"))) %>%
    mutate(group = factor(group, levels = c("Flowering Plant",
                                            "Algae",
                                            "Conifer",
                                            "Cycad",
                                            "Ferns and Allies",
                                            "Mosses"))) %>%
    arrange(group, num_threats) %>%
    group_by(year_last_seen, continent) %>%
    mutate(index = row_number()) %>%
    ungroup()

# Create data frame of mean number of threats (didn't end up using this)
plants.agg <- plants.dat %>%
  group_by(year_last_seen, continent) %>%
  summarise(max_index = max(index) + 10,
            avg_num_threats = mean(num_threats))

# Final plot
ggplot(data = plants.dat) +
    geom_point(aes(x = fct_rev(year_last_seen), 
                   y = index, 
                   group = desc(continent),
                   color = continent,
                   shape = group#,
                  # size = num_threats
                   ),
               position =  position_dodge(width = 1),
               size = 3.5
               ) +
    # geom_text(data = plants.agg,
    #           aes(x = fct_rev(year_last_seen),
    #               y = max_index,
    #               group = desc(continent),
    #               label = paste(round(avg_num_threats, digits = 1),
    #                             "number of threats")),
    #           position = position_dodge(width = 1)) +
    geom_vline(xintercept = seq(1.5, 6.5, 1), color = "#666666") +
    scale_x_discrete(position = "bottom") +
    scale_y_continuous(expand = c(0,1)) +
   # scale_color_brewer(palette = "Greens", aesthetics = "color") +
    scale_color_manual(values = c("#a1d99b",
                                  "#74c476",
                                  "#41ab5d",
                                  "#238b45",
                                  "#006d2c",
                                  "#00441b")) +
    scale_shape_manual(values = c(19, 12, 13, 10, 14, 7)) +
    guides(color = guide_legend(title = "Continent of Origin"),
           shape = guide_legend(title = "Taxonomic Group"),
           size = guide_legend(title = "Number of Threats")) +
    labs(title = "Plant extinction over time by continent of origin and taxonomic group",
         subtitle = "Each shape represents a single plant.",
         caption = "#TidyTuesday 18 Aug 2020 | Data: International Union for Conservation of Nature | Designer: Jenn Schilling | jennschilling.me") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
          plot.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
          legend.background = element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
          legend.key =  element_rect(fill = "#F8F8F8", color = "#F8F8F8"),
          plot.caption = element_text(hjust = 1),
          plot.title.position = "plot",
          plot.caption.position =  "plot"
          ) 
  
ggsave("2020-08-18\\extinct.plants.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 8,
       dpi = 100)
  