# Author : Jenn Schilling
# Title: #TidyTuesday Toronto Shelters
# Date: 12/08/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(tidytext)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-12-08')

women <- tuesdata$women 

#### Explore the Data ####

skim(women)

table(women$category)

#### Text Analysis ####

# What words are used to describe women in different categories?

women_text <- women %>%
  filter(name != 'Unsung hero') %>% # remove unsung hero
  unnest_tokens(input = description,
                output = word) %>% # tokenize
  anti_join(stop_words, by = "word") # remove stop words

women_text_summary <- women_text %>%
  filter(word != "women" & word != "woman" & word != "female" & 
           word != "19" & word != "fang" & word != "zambian") %>%
  mutate(word = ifelse(word == "films", "film", word),
         word = ifelse(word == "covid", "coronavirus", word),
         word = ifelse(word == "activists", "activist", word),
         word = ifelse(word == "dr", "doctor", word),
         word = ifelse(word == "children’s", "children", word)) %>%
  count(category, word) %>%
  filter(n > 2) %>%
  ungroup() %>%
  arrange(category, -n)

ggplot(women_text_summary) +
  geom_bar(aes(y = reorder(word, n), x = n), stat = "identity") +
  facet_wrap(~category, scales = "free")
