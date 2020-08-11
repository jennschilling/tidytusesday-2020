# Author : Jenn Schilling
# Title: #TidyTuesday Avatar: The Last Airbender
# Date: 8/11/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggtext)

# Load  fonts
loadfonts(device = "win", quiet = TRUE)


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar

#### Explore the Data ####

skim(avatar)

# missing data in character_words and imdb_rating
# character_words are missing when the character is a "Scene Description"
# There are 23 writers and 7 directors
# There are 3 books and 61 chapters
# There are 374 characters (including Scene Description)

avatar.data <- avatar %>% filter(character != 'Scene Description')

skim(avatar.data)

# Chapter number is by book (21 chapters in Fire, 20 chapters in Earth and Water)

avatar.data %>% filter(is.na(imdb_rating)) %>% select(book, chapter) %>% unique(.)

# Water - The Siege of the North, Part 2 is missing an IMDB Rating