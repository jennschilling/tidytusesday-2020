# Author : Jenn Schilling
# Title: #TidyTuesday Friends
# Date: 9/08/4040


#### Libraries ####
library(friends)
library(tidyverse)
library(skimr)
library(animation)
library(reshape2)


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
  filter(speaker != 'Scene Directions') %>% # remove scene directions
  filter(!is.na(speaker)) # remove NAs

speaker_scene_matrix <- speaker_scene %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

speaker_scene_matrix_s4 <- speaker_scene %>%
  filter(speaker %in% main_chars) %>%
  filter(season == 4) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm <- speaker_scene_matrix_s4 / rowSums(speaker_scene_matrix_s4)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)

#### Get Clusters of Main Characters by Season ####

# Season 1
speaker_scene_1 <- speaker_scene %>% 
  filter(season == 1)

# chars_1 <- speaker_scene_1 %>%
#   group_by(speaker) %>%
#   summarise(num_lines = sum(n)) %>%
#   top_n(10, num_lines)

# speaker_scene_1 <- inner_join(speaker_scene_1, chars_1, by = "speaker")

speaker_scene_matrix_1 <- speaker_scene_1 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_1 <- speaker_scene_matrix_1 / rowSums(speaker_scene_matrix_1)

h_1 <- hclust(dist(norm_1, method = "manhattan"))

plot(h_1, main = "Season 1", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 2
speaker_scene_2 <- speaker_scene %>% 
  filter(season == 2)

speaker_scene_matrix_2 <- speaker_scene_2 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_2 <- speaker_scene_matrix_2 / rowSums(speaker_scene_matrix_2)

h_2 <- hclust(dist(norm_2, method = "manhattan"))

plot(h_2, main = "Season 2", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 3
speaker_scene_3 <- speaker_scene %>% 
  filter(season == 3)

speaker_scene_matrix_3 <- speaker_scene_3 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_3 <- speaker_scene_matrix_3 / rowSums(speaker_scene_matrix_3)

h_3 <- hclust(dist(norm_3, method = "manhattan"))

plot(h_3, main = "Season 3", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 4
speaker_scene_4 <- speaker_scene %>% 
  filter(season == 4)

speaker_scene_matrix_4 <- speaker_scene_4 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_4 <- speaker_scene_matrix_4 / rowSums(speaker_scene_matrix_4)

h_4 <- hclust(dist(norm_4, method = "manhattan"))

plot(h_4, main = "Season 4", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 5
speaker_scene_5 <- speaker_scene %>% 
  filter(season == 5)

speaker_scene_matrix_5 <- speaker_scene_5 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_5 <- speaker_scene_matrix_5 / rowSums(speaker_scene_matrix_5)

h_5 <- hclust(dist(norm_5, method = "manhattan"))

plot(h_5, main = "Season 5", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 6
speaker_scene_6 <- speaker_scene %>% 
  filter(season == 6)

speaker_scene_matrix_6 <- speaker_scene_6 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_6 <- speaker_scene_matrix_6 / rowSums(speaker_scene_matrix_6)

h_6 <- hclust(dist(norm_6, method = "manhattan"))

plot(h_6, main = "Season 6", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 7
speaker_scene_7 <- speaker_scene %>% 
  filter(season == 7)

speaker_scene_matrix_7 <- speaker_scene_7 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_7 <- speaker_scene_matrix_7 / rowSums(speaker_scene_matrix_7)

h_7 <- hclust(dist(norm_7, method = "manhattan"))

plot(h_7, main = "Season 7", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 8
speaker_scene_8 <- speaker_scene %>% 
  filter(season == 8)

speaker_scene_matrix_8 <- speaker_scene_8 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_8 <- speaker_scene_matrix_8 / rowSums(speaker_scene_matrix_8)

h_8 <- hclust(dist(norm_8, method = "manhattan"))

plot(h_8, main = "Season 8", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 9
speaker_scene_9 <- speaker_scene %>% 
  filter(season == 9)

speaker_scene_matrix_9 <- speaker_scene_9 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_9 <- speaker_scene_matrix_9 / rowSums(speaker_scene_matrix_9)

h_9 <- hclust(dist(norm_9, method = "manhattan"))

plot(h_9, main = "Season 9", xlab = "", ylab = "", sub = "", yaxt = 'n')

# Season 10
speaker_scene_10 <- speaker_scene %>% 
  filter(season == 10)

speaker_scene_matrix_10 <- speaker_scene_10 %>%
  filter(speaker %in% main_chars) %>%
  mutate(id = paste(season, episode, scene, sep = ".")) %>%
  select(id, speaker, n) %>%
  acast(speaker ~ id, fun.aggregate = sum)

norm_10 <- speaker_scene_matrix_10 / rowSums(speaker_scene_matrix_10)

h_10 <- hclust(dist(norm_10, method = "manhattan"))

plot(h_10, main = "Season 10", xlab = "", ylab = "", sub = "", yaxt = 'n')


#### Create GIF ####

saveGIF(
  expr = {
    plot(h_1, main = "Season 1", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_2, main = "Season 2", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_3, main = "Season 3", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_4, main = "Season 4", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_5, main = "Season 5", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_6, main = "Season 6", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_7, main = "Season 7", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_8, main = "Season 8", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_9, main = "Season 9", xlab = "", ylab = "", sub = "", yaxt = 'n')
    plot(h_10, main = "Season 10", xlab = "", ylab = "", sub = "", yaxt = 'n')
  },
  movie.name = "friends_seasons.gif"
)
