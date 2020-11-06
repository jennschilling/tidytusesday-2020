# Author : Jenn Schilling
# Title: #TidyTuesday IKEA 
# Date: 11/3/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(showtext)
library(sysfonts) # add fonts
library(gridExtra)

#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-11-3')

ikea <- tuesdata$ikea

# Add IKEA font
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

#### Plots ####

# Convert to USD
ikea <- ikea %>%
  mutate(price_usd = price * 0.27)

# Get number of records by category
ikea_category_summary <- ikea %>%
  mutate(category = fct_reorder(category, price, median, .desc = FALSE)) %>%
  group_by(category) %>%
  summarise(n = n(),
            y = quantile(price_usd, 0.75) * 0.90) %>%
  mutate(n_text = paste0("N = ", n)) %>%
  ungroup()

# Box plot of price distribution by category
price_dist <- ikea %>%
  ggplot() +
  geom_boxplot(aes(x = fct_reorder(category, price, median, .desc = FALSE), 
                   y = price_usd),
               color = "#0051BA") +
  labs(title = "Price Distribution of IKEA Furniture by Category",
       subtitle = "Prices from the IKEA Saudi Arabia website, converted into USD.",
       y = "Price in USD",
       x = "",
       caption = "") +
  coord_flip() +
  theme_classic() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Noto Sans"),
        plot.title.position = "plot")

# Bar plot of number of records by category
cat_count <- ikea_category_summary %>%
  ggplot() +
  geom_bar(aes(x = n, y = category), stat = "identity", fill = "#0051BA") +
  labs(title = "Number of Items in Each Category",
       subtitle = "Categories ordered from highest to lowest median price.",
       y = "",
       x = "Number of Items",
       caption = "TidyTuesday 03 Nov 2020 | Data: Kaggle | Designer: Jenn Schilling | jennschilling.me") +
  scale_x_continuous(expand = expansion(mult = c(0, .1))) + # pull bars back to the y-axis
  theme_classic() +
  theme(text = element_text(family = "Noto Sans"),
        plot.title.position = "plot")

# Put graphs together
final_plot <- grid.arrange(price_dist, 
                           cat_count, 
                           ncol = 2)

# Check min prices
ikea %>%
  group_by(category) %>%
  summarise(min_price = min(price_usd))

# All are above $0 - good

# Save plot
ggsave("2020-11-03\\ikea_category_prices.png",
       plot = final_plot,
       device = "png",
       width = 10,
       height = 6,
       dpi = 300)
