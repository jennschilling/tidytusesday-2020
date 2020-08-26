# Author : Jenn Schilling
# Title: #TidyTuesday Chopped
# Date: 8/25/2020


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
tuesdata <- tidytuesdayR::tt_load('2020-08-25')

chopped <- tuesdata$chopped

#### Explore the Data ####

skim(chopped)

#### Get List of Ingredients by Course ####

# Separate out the list of ingredients, after making them all lowercase
ingredients <- chopped %>%
  select(season, season_episode, 
         series_episode, episode_rating,
         appetizer, entree, dessert) %>%
  pivot_longer(appetizer:dessert, 
               names_to = "course",
               values_to = "ing_list") %>%
  mutate(ing_list = tolower(ing_list)) 

ingredients$ingredient <- str_split(ingredients$ing_list, pattern = ", ")

ingredients_list <- ingredients %>%
  unnest(cols = ingredient)

# Count number of times ingredient appeared and get average rating
ingredients_agg <- ingredients_list %>%
  group_by(course, ingredient) %>%
  summarise(n = n(),
            mean_rating = mean(episode_rating, na.rm = TRUE)) %>%
  arrange(-n)

# Get the top five ingredients for each course
ingredients_agg_filter <- ingredients_agg %>%
  group_by(course) %>%
  arrange(-n) %>%
  filter(row_number() <= 5)

#### Graph ####

ingredients_agg_filter %>%
  filter(course == 'appetizer') %>%
ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity') +
  facet_wrap(~course) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#bcbddc',
                      high = '#54278f')

ingredients_agg_filter %>%
  filter(course == 'entree') %>%
  ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity') +
  facet_wrap(~course) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#bcbddc',
                      high = '#54278f')

ingredients_agg_filter %>%
  filter(course == 'dessert') %>%
  ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity') +
  facet_wrap(~course) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#bcbddc',
                      high = '#54278f')

