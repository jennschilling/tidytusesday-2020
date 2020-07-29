# Author : Jenn Schilling
# Title: #TidyTuesday Penguins
# Date: 7/28/2020
# Artwork by @allison_horst

#### Packages ####
library(tidytuesdayR)
library(tidyverse)
library(palmerpenguins) # Another way to get the data
library(skimr)
library(explore)

#### Data ####
tuesdata <- tidytuesdayR::tt_load('2020-07-28')
penguins <- tuesdata$penguins # cleaned data

skim(penguins)
View(penguins)

#### Plotting ####

penguins %>%
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_jitter()

# Gentoo seems to be heavier species

penguins %>%
  ggplot(aes(x = body_mass_g, y = bill_length_mm, color = sex)) +
  geom_jitter()

# Seems to be positive relationship between bill length and body mass
# Seems that males are heavier than females

penguins %>%
  ggplot(aes(x = body_mass_g, y = bill_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~ sex)

# Nice clusters by sex and species

penguins %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~ sex)

penguins %>%
  ggplot(aes(x = body_mass_g, y = bill_depth_mm, color = species)) +
  geom_point() +
  facet_wrap(~ sex)

# Normalize by body mass and try adding a regression line
penguins %>%
  ggplot(aes(x = bill_length_mm / body_mass_g, 
             y = bill_depth_mm / body_mass_g, 
             color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

# Can see positive relationship, intercepts are very different - would want to use a mixed model

penguins %>%
  ggplot(aes(x = species)) +
  geom_bar() +
  facet_wrap(~ island)

# Species are on different islands - adelie is on all, but gentoo and chinstrap are only each on one


#### Explore Raw Data ####
skim(penguins_raw)

View(penguins_raw)

penguins_raw %>%
  count(Region, Island)

unique(penguins_raw$`Date Egg`)

unique(penguins_raw$Comments)

unique(penguins_raw$`Date Egg`)

penguins_raw %>%
  group_by(`Individual ID`) %>%
  count() %>%
  arrange(-n) %>%
  View()

# Penguins have 1-3 records; there are 190 unique penguins in the dataset

penguins_raw %>%
  group_by(`Date Egg`, Island) %>%
  count() %>%
  arrange(-n) %>%
  View()

explore(penguins_raw)

#### Analysis ####

# Make decision tree for identifying penguin species
# Perform clustering analysis to identify different groups?
# Visualize.....


