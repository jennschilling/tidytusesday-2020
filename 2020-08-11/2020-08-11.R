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

# Put the books in order from 1st to 3rd
avatar.data <- avatar.data %>% mutate(book = factor(book, levels=unique(book[order(book_num)]), ordered=TRUE))

# Exploratory graphs
ggplot(avatar.data %>%
         filter(!is.na(imdb_rating)) %>%
         select(director, book, chapter, chapter_num, imdb_rating) %>%
         unique(.) ) +
  geom_point(aes(x = chapter_num, y = imdb_rating, color = director)) +
  facet_grid(book ~ director) +
  theme_bw()

ggplot(avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(director, chapter, book, imdb_rating) %>%
  unique(.) %>%
  group_by(director, book) %>%
  summarise(avg_rating = mean(imdb_rating),
            num_chapters = n()) %>%
  mutate(num_books = n_distinct(book))) +
  geom_point(aes(x = num_chapters, y = avg_rating, size = num_books, color = director)) +
  facet_grid(~ book) +
  theme_bw()


ggplot(avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(character, chapter, book, imdb_rating) %>%
  unique(.) %>%
  group_by(character, book) %>%
  summarise(avg_rating = mean(imdb_rating),
            num_chapters = n()) %>%
  mutate(num_books = n_distinct(book)) %>%
  filter(num_chapters > 5)) +
  geom_jitter(aes(x = num_chapters, y = avg_rating, color = character), width = 1) +
  facet_wrap(~ book) +
  theme_bw()

ggplot(avatar.data %>%
         filter(!is.na(imdb_rating)) %>%
         select(character, chapter_num, book, imdb_rating) %>%
         unique(.) %>%
         group_by(book, chapter_num) %>%
         summarise(avg_rating = mean(imdb_rating),
                   num_characters = n())) +
  geom_jitter(aes(x = num_characters, y = avg_rating)) +
  facet_wrap(~ book) +
  theme_bw()


