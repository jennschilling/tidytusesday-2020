# Author : Jenn Schilling
# Title: #TidyTuesday IKEA 
# Date: 11/3/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(showtext)
library(sysfonts) # add fonts

#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-11-3')

ikea <- tuesdata$ikea

# Add font
font_add_google("Noto Sans")

showtext_auto()

#### Explore Data ####

glimpse(ikea)

skim(ikea)

table(ikea$other_colors)

table(ikea$category)

# Play
ikea %>%
ggplot() +
  geom_jitter(aes(x = category, y = price, color = category))

ikea %>%
  ggplot() +
  geom_jitter(aes(x = category, y = width, color = category))

ikea %>%
  ggplot() +
  geom_jitter(aes(x = width, y = price, color = category))

ikea %>%
  ggplot() +
  geom_jitter(aes(x = height, y = price, color = category))

ikea %>%
  ggplot() +
  geom_jitter(aes(x = depth, y = price, color = category))

# Box plot
ikea <- ikea %>%
  mutate(price_usd = price * 0.27)

ikea_category_summary <- ikea %>%
  mutate(category = fct_reorder(category, price, median, .desc = FALSE)) %>%
  group_by(category) %>%
  summarise(n = n(),
            y = quantile(price_usd, 0.75) * 0.90) %>%
  mutate(n_text = paste0("N = ", n)) %>%
  ungroup()

ikea %>%
  ggplot() +
  geom_boxplot(aes(x = fct_reorder(category, price, median, .desc = TRUE), 
                   y = price_usd)) +
  labs(title = "Price Distribution of IKEA Furniture by Category",
       y = "Price in USD",
       x = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Noto Sans"),
        plot.title.position = "plot")

# ikea %>%
#   ggplot() +
#   geom_violin(aes(x = fct_reorder(category, price, median, .desc = TRUE), 
#                   y = price_usd))

ikea_category_summary %>%
  ggplot() +
  geom_bar(aes(x = n, y = category), stat = "identity", fill = "#0051BA") +
  labs(title = "Number of Items in Each Category",
       subtitle = "Categories ordered from highest to lowest median price.",
       y = "",
       x = "") +
  theme_classic() +
  theme(text = element_text(family = "Noto Sans"),
        plot.title.position = "plot")
