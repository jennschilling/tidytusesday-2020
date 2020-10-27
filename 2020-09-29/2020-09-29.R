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
library(reshape2)
library(wordcloud)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-29')

beyonce <- tuesdata$beyonce_lyrics

taylor <- tuesdata$taylor_swift_lyrics %>%
  rename_all(~str_replace_all(., "\\s+", ""))


charts <- tuesdata$charts

sales <- tuesdata$sales %>%
  mutate(country = ifelse(country == "WW", "World", country))



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

# Remove some songs and then get words
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
  unnest_tokens(word, line) %>% 
  anti_join(stop_words, by=c("word"="word")) 

# Count words
beyonce_lyrics %>%
  count(word, sort = TRUE) %>%
  View(.)

# Sentiment
beyonce_sentiment <- beyonce_lyrics %>%
  inner_join(get_sentiments("nrc"))

ggplot(beyonce_sentiment) +
  geom_bar(aes(x = sentiment))

beyonce_sentiment %>%
  count(song_name, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ggplot() +
  geom_bar(aes(x = song_name, y = n, group = sentiment), stat = "identity") +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

beyonce_neg_pos <- beyonce_lyrics %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#417E5D", "#6A5DC9"),
                   max.words = 100, scale=c(3.5,0.50))

#### Sales & Charts ####

sales %>%
  filter(country %in% c("UK", "US", "World")) %>%
  mutate(released = gsub("\\s*\\([^\\)]+\\)\\[[^\\)]+\\]", "", released)) %>%
  mutate(year = substr(released, nchar(released) - 3, nchar(released))) %>%
  ggplot(.) +
  geom_point(aes(x = year, y = sales, color = artist, group = artist)) +
  geom_text(aes(x = year, y = sales, color = artist, group = artist, label = title)) +
  facet_wrap(~country)

charts %>%
  filter(chart_position != '—') %>%
  count(artist, chart, chart_position) %>%
  ggplot(.) +
  geom_bar(aes(x = chart_position, y = n, fill = artist), 
           stat = "identity", position = "dodge") +
  facet_wrap(~chart, scales = "free")

# Worldwide sales

xlabs <- c(0, 5, 10)

sales %>%
  filter(country == "World") %>%
  mutate(released = gsub("\\s*\\([^\\)]+\\)\\[[^\\)]+\\]", "", released)) %>%
  mutate(year = substr(released, nchar(released) - 3, nchar(released))) %>%
  ggplot(.) +
  geom_bar(aes(y = reorder(paste(title, paste0("(", year, ")"), sep = "\n"), 
                           sales), 
               x = sales,
               fill = artist), 
           stat = "identity") + 
  geom_text(aes(y = reorder(paste(title, paste0("(", year, ")"), sep = "\n"), 
                            sales), 
                x = sales, 
                label = paste0("$", sales / 1000000, "M")),
            family = "Verdana",
            size = 3.5,
            hjust = 0) +
  scale_fill_manual(values = c("#D4AF37", "red"),
                    guide = FALSE) +
  facet_wrap(~artist, scales = "free_y") +
  scale_x_continuous(expand = expansion(mult = c(0, .1)),
                     labels = paste0("$", xlabs, "M"),
                     breaks = 1000000 * xlabs ) +
  labs(x = "",
       y = "",
       title = "Worldwide Album Sales",
       subtitle = "Both Beyoncé and Taylor Swift made the most in worldwide sales from their first albums.",
       caption = "TidyTuesday 29 Sep 2020 | Designer: Jenn Schilling | jennschilling.me") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, family = "Verdana", color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, hjust = 0, family = "Verdana"),
        text = element_text(family = "Verdana", size = 12),
        plot.title.position = "plot")

ggsave("2020-09-29\\worldwide_sales.png",
       plot = last_plot(),
       device = "png",
       width = 10.7,
       height = 5,
       dpi = 300)
