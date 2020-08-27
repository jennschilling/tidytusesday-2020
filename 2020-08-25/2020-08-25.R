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
library(stringr)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


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
  filter(row_number() <= 5) %>%
  ungroup() %>%
  mutate(ingredient = str_to_title(ingredient),
         course = str_to_title(course))

#### Graph ####

app_plot <- ingredients_agg_filter %>%
  filter(course == 'Appetizer') %>%
ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity',
           show.legend = FALSE) +
  scale_x_continuous(limits = c(0,14), 
                     expand = c(0,0.02),
                     breaks = c(2, 4, 6, 8, 10, 12)) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#E9A343',
                      high = '#D55D28') +
  labs(title = "<b>Appetizer</b>") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        text = element_text(family = 'Bahnschrift'))


ent_plot <- ingredients_agg_filter %>%
  filter(course == 'Entree') %>%
  ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity',
           show.legend = FALSE) +
  scale_x_continuous(limits = c(0,14), 
                     expand = c(0,0.02),
                     breaks = c(2, 4, 6, 8, 10, 12)) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#E9A343',
                      high = '#D55D28') +
  labs(title = "<b>Entree</b>") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        text = element_text(family = 'Bahnschrift'))

des_plot <- ingredients_agg_filter %>%
  filter(course == 'Dessert') %>%
  ggplot() +
  geom_bar(aes(y = reorder(ingredient, n),
               x = n,
               fill = mean_rating),
           stat = 'identity',
           show.legend = FALSE) +
  scale_x_continuous(limits = c(0,14), 
                     expand = c(0,0.02),
                     breaks = c(2, 4, 6, 8, 10, 12)) +
  scale_fill_gradient(limits = c(8.14, 8.56),
                      low = '#E9A343',
                      high = '#D55D28') +
  labs(title = "<b>Dessert</b>",
       caption = "") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        text = element_text(family = 'Bahnschrift'))

title_theme <- ggplot() +  
  labs(title ="Most Frequently Used Ingredients by Course",
       subtitle = "Color represents the average rating of episodes using the ingredient.<br>
       <b style='color:#D55D28'>Higher ratings.</b>
       <b style='color:#E9A343'>Lower ratings.</b><br><br>
       <b style='color:#D55D28'>The highest average rating was 8.56.</b><br>
        <b style='color:#E9A343'>The lowest average rating was 8.14.</b>") +
    theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Font and Positioning
      plot.title = element_markdown(),
      plot.subtitle = element_markdown(),
      plot.caption = element_markdown(),
      text = element_text(family = 'Helvetica')
     # plot.title.position = "plot",
    #  plot.caption.position =  "plot"
    )

plot_grid(title_theme,
          app_plot,
          ent_plot,
          des_plot,
          ncol = 1,
          rel_heights = c(0.4, 1, 1, 1)
          )
