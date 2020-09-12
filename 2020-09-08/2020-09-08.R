# Author : Jenn Schilling
# Title: #TidyTuesday Friends
# Date: 9/08/2020


#### Libraries ####
library(friends)
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
friends <- friends
friends_emotions <- friends_emotions
friends_info <- friends_info


#### Explore Data ####

# Join emotions with dialogue

friends_joined <- friends %>%
  left_join(., friends_emotions, 
            by = c("season", "episode", "scene", "utterance"))

friends_emotions_char <- friends_joined %>%
  count(season, speaker, emotion) %>%
  filter(!is.na(emotion)) 

unique(friends_emotions_char$speaker)

main_chars <- c("Chandler Bing", "Joey Tribbiani", "Phoebe Buffay",
                "Ross Geller", "Monica Geller", "Rachel Green")

friends_emotions_char %>%
  filter(speaker %in% main_chars) %>%
ggplot() +
  geom_bar(aes(x = speaker, y = n, fill = speaker), stat = "identity") +
  facet_wrap(~emotion)

# What about relationships between characters?
# Source: http://varianceexplained.org/r/love-actually-network/

speaker_scene <- friends %>%
  count(season, episode, scene, speaker) %>%
  filter(speaker != "#ALL#") %>% # remove all
  filter(!is.na(speaker)) # remove NAs

library(reshape2)
speaker_scene_matrix <- speaker_scene %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

speaker_scene_matrix_s2 <- speaker_scene %>%
  filter(speaker %in% main_chars) %>%
  filter(season == 2) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm <- speaker_scene_matrix_s2 / rowSums(speaker_scene_matrix_s2)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)
