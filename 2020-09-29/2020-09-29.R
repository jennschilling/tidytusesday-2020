# Author : Jenn Schilling
# Title: #TidyTuesday Beyonce and Taylor Swift Lyrics
# Date: 9/29/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(ggtext)
library(tidytext)
library(lubridate)
library(stringr)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-29')

beyonce <- tuesdata$beyonce_lyrics

taylor <- tuesdata$taylor_swift_lyrics %>%
  rename_all(~str_replace_all(., "\\s+", ""))


charts <- tuesdata$charts

sales <- tuesdata$sales


#### Explore Data ####

unique(beyonce$song_name) # Beyonce dataframe is by song and line

unique(taylor$Title) # Taylor Swift dataframe is by song

skim(charts)

skim(sales)

# U.S. sales by year and artist
sales %>%
  filter(country == "US") %>%
  mutate(released = gsub("\\s*\\([^\\)]+\\)\\[[^\\)]+\\]", "", released)) %>%
  mutate(year = substr(released, nchar(released) - 3, nchar(released))) %>%
ggplot(.) +
  geom_line(aes(x = year, y = sales, color = artist, group = artist))

#### Text Analysis ####

beyonce_lyrics <- beyonce %>%
  filter(!str_detect(song_name, "Live")) %>% 
  filter(!str_detect(song_name, "Alternate")) %>%
  filter(!str_detect(song_name, "Mix")) %>%
  filter(!str_detect(song_name, "Speech")) %>%
  filter(!str_detect(song_name, "Remix")) %>%
  filter(!str_detect(song_name, "Edit")) %>%
  filter(!str_detect(song_name, "Dub")) %>%
  filter(!str_detect(song_name, "Version")) %>%
  filter(!str_detect(song_name, "live")) %>%
  filter(!str_detect(song_name, "Script")) %>%
  filter(!str_detect(song_name, "translation")) %>%
  filter(!str_detect(song_name, "BET Awards")) %>%
  filter(!str_detect(song_name, "Extended")) %>%
  filter(!str_detect(song_name, "Poetry")) %>%
  filter(!str_detect(song_name, "Rap")) %>%
  filter(!str_detect(song_name, "Hompage")) %>%
  filter(!str_detect(song_name, "VMA")) %>%
  unnest_tokens(word, line)

