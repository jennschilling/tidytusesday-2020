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
library(tvthemes)
library(tidytext)

# Load  fonts
loadfonts(quiet = TRUE)


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

# Episode ratings by director and season
avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(director, book, chapter, chapter_num, imdb_rating) %>%
  unique(.) %>%
ggplot() +
  geom_point(aes(x = chapter_num, y = imdb_rating, color = director)) +
  scale_color_avatar() +
  facet_grid(book ~ director) +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer")

# Average episode rating by director and season
avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(director, chapter, book, imdb_rating) %>%
  unique(.) %>%
  group_by(director, book) %>%
  summarise(avg_rating = mean(imdb_rating),
            num_chapters = n()) %>%
  mutate(num_books = n_distinct(book)) %>%
ggplot() +
  geom_point(aes(x = num_chapters, y = avg_rating, 
                 size = num_books, color = director)) +
  facet_grid(~ book) +
  scale_color_avatar() +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer")

# Average rating by character and book
avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(character, chapter, book, imdb_rating) %>%
  unique(.) %>%
  group_by(character, book) %>%
  summarise(avg_rating = mean(imdb_rating),
            num_chapters = n()) %>%
  mutate(num_books = n_distinct(book)) %>%
  filter(num_chapters > 5) %>%
ggplot() +
  geom_jitter(aes(x = num_chapters, y = avg_rating, color = character), 
              width = 1) +
  facet_wrap(~ book) +
#  scale_color_avatar() +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer")

# Ratings versus number of characters in an episode, by book
avatar.data %>%
  filter(!is.na(imdb_rating)) %>%
  select(character, chapter_num, book, imdb_rating) %>%
  unique(.) %>%
  group_by(book, chapter_num) %>%
  summarise(avg_rating = mean(imdb_rating),
            num_characters = n()) %>%
ggplot() +
  geom_jitter(aes(x = num_characters, y = avg_rating)) +
  facet_wrap(~ book) +
  scale_color_avatar() +
  theme_avatar(title.font = "Slayer",
               text.font = "Slayer")


### Let's Do Some Text Analysis ####

# Get words in each line, but first get characters who speak the most

avatar.main.characters <- avatar.data %>%
  group_by(character) %>%
  summarise(num_lines = n()) %>%
  ungroup() %>%
  arrange(-num_lines) %>%
  top_n(10) %>%
  select(character) %>%
  unique(.)

main.characters <- tribble(
  ~character, ~nation, ~gender,
  'Aang', 'air', 'male',
  'Sokka', 'water', 'male',
  'Katara', 'water', 'female',
  'Zuko', 'fire', 'male',
  'Toph', 'earth', 'female',
  'Iroh', 'fire', 'male',
  'Azula', 'fire', 'female',
  'Jet', 'earth', 'male',
  'Suki', 'earth', 'female',
  'Zhao', 'fire', 'male',
)


# Get words for main characters
avatar.data.tokenize.main <- avatar.data %>%
  inner_join(main.characters, by = "character") %>%
  unnest_tokens(input = character_words,
                output = word)

# Get word counts by character, excluding stop words
avatar.words.main <- avatar.data.tokenize.main %>%
  anti_join(stop_words, by = "word") %>%
  count(character, nation, gender, word, sort = TRUE) %>%
  arrange(-n) 

# Get total words by character
avatar.total.words.main <- avatar.words.main %>%
  group_by(character, nation, gender) %>%
  summarise(total = sum(n))

# Get word frequencies
avatar.word.freq.main <- avatar.words.main %>%
  left_join(., avatar.total.words.main, by = c("character", "gender", "nation")) %>%
  mutate(freq = n / total)
  
ggplot(avatar.word.freq.main, aes(freq, fill = character)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~character, ncol = 2, scales = "free_y")

# Who talks the most?
ggplot(avatar.total.words.main) +
  geom_bar(aes(x = reorder(character, total), y = total, fill = gender), 
           stat = "identity") +
  coord_flip() +
  labs(y = "Number of Words",
       x = "",
       title = "Who says the most in Avatar: The Last Airbender?",
       subtitle = "Male characters say more than female characters.") +
  scale_fill_avatar(palette = "EarthKingdom") +
  theme_avatar(text.font = "Slayer")

# What are they saying?

avatar.word.freq.main %>%
  filter(freq >= 0.005) %>%
  filter(character == "Sokka") %>%
  ggplot() +
  geom_bar(aes(x = reorder(word, n), y = n, fill = nation), stat = "identity") +
  coord_flip() +
  labs(y = "Count",
       x = "",
       title = "Sokka") +
  scale_fill_avatar(palette = "WaterTribe") +
  theme_avatar(text.font = "Slayer")


avatar.word.freq.main %>%
  filter(freq >= 0.005) %>%
  filter(character == "Aang") %>%
ggplot() +
  geom_bar(aes(x = reorder(word, n), y = n, fill = nation), stat = "identity") +
  coord_flip() +
  labs(y = "Count",
       x = "",
       title = "Aang") +
  scale_fill_avatar(palette = "AirNomads") +
  theme_avatar(text.font = "Slayer")

# Which of the main characters talks the most in each episode?
# Include stop words
avatar.char.episode <- avatar.data.tokenize.main %>%
  count(book, chapter_num, imdb_rating, character, nation, gender, word) %>%
  group_by(book, chapter_num, imdb_rating, character, nation, gender) %>%
  summarise(word_count = sum(n)) %>%
  ungroup() %>%
  group_by(book, chapter_num, imdb_rating) %>%
  filter(word_count == max(word_count))

# Ratings by Chapter, Color by Character who Speaks Most, Graph for each Book

avatar.char.episode %>%
  filter(book == "Water") %>%
ggplot() +
  geom_bar(aes(x = chapter_num, y = imdb_rating, fill = character), stat = "identity", position = "dodge") +
  scale_fill_avatar(palette = "WaterTribe") +
  labs(x = "Chapter",
       y = "IMDB Rating",
       title = "Ratings by Chapter for Water Book",
       subtitle = "Color shows Character Who Said the Most") +
  theme_avatar(text.font = "Slayer")

avatar.char.episode %>%
  filter(book == "Earth") %>%
  ggplot() +
  geom_bar(aes(x = chapter_num, y = imdb_rating, fill = character), stat = "identity", position = "dodge") +
  scale_fill_avatar(palette = "EarthKingdom") +
  labs(x = "Chapter",
       y = "IMDB Rating",
       title = "Ratings by Chapter for Earth Book",
       subtitle = "Color shows Character Who Said the Most") +
  theme_avatar(text.font = "Slayer")


avatar.char.episode %>%
  filter(book == "Fire") %>%
  ggplot() +
  geom_bar(aes(x = chapter_num, y = imdb_rating, fill = character), stat = "identity", position = "dodge") +
  scale_fill_avatar(palette = "FireNation") +
  labs(x = "Chapter",
       y = "IMDB Rating",
       title = "Ratings by Chapter for Fire Book",
       subtitle = "Color shows Character Who Said the Most") +
  theme_avatar(text.font = "Slayer")

# Heatmap View

# Get characters who speak the most to make levels
char.most <- avatar.char.episode %>%
  ungroup() %>%
  count(character) %>%
  arrange(n) %>%
  select(character) %>%
  as.list(.)

avatar.char.episode %>%
  mutate(character = factor(character, levels=char.most$character, ordered=TRUE)) %>%
  ggplot() +
  geom_tile(aes(x = chapter_num, y = character, fill = imdb_rating)) +
  facet_wrap(~book) +  
  labs(x = "Chapter",
       y = "Character who Spoke the Most",
       title = "Ratings by Chapter and Book",
       subtitle = "") +
  theme_avatar(text.font = "Slayer")

