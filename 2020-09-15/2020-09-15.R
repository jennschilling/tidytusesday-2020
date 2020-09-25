# Author : Jenn Schilling
# Title: #TidyTuesday US Spending on Kids
# Date: 9/15/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggtext)
library(ggrepel)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-15')

kids <- tuesdata$kids

#### Explore Data ####

skim(kids)

# Years 1997 - 2016
# 23 different variables

unique(kids$variable)

# Arizona
# Data is in $1000s
arizona <- kids %>% 
  filter(state == 'Arizona') %>%
  mutate(raw = raw * 1000,
         inf_adj = inf_adj * 1000,
         inf_adj_perchild = inf_adj_perchild * 1000)

arizona %>%
  filter(variable %in% c("PK12ed", "highered", "lib")) %>%
  ggplot() +
  geom_line(aes(x = year, y = inf_adj_perchild, color = variable)) +
  theme_classic()

# PK-12 Ed spending
# Elementary and secondary education expenditures per child, in $2016
kids %>%
  filter(variable == 'PK12ed') %>%
  ggplot() +
  geom_line(aes(x = year, y = inf_adj_perchild, group = state)) +
  geom_text(data = kids %>% filter(variable == 'PK12ed' & year == 2016),
            aes(x = year, y = inf_adj_perchild, label = state),
            hjust = -0.1,
            vjust = 0.2) +
  scale_x_continuous(limits = c(1997, 2021)) +
  theme_classic()

# Higher Ed Spending
# Higher education expenditures per child, in $2016
kids %>%
  filter(variable == 'highered') %>%
  ggplot() +
  geom_line(aes(x = year, y = inf_adj_perchild, group = state)) +
  geom_text(data = kids %>% filter(variable == 'highered' & year == 2016),
            aes(x = year, y = inf_adj_perchild, label = state),
            hjust = -0.1,
            vjust = 0.2) +
  scale_x_continuous(limits = c(1997, 2021)) +
  theme_classic()

# States in regions
# Source: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
kids <- kids %>%
  mutate(region = 
           case_when(
             state %in% c('Connecticut', 
                          'Maine',
                          'Massachusetts',
                          'New Hampshire',
                          'Rhode Island',
                          'Vermont',	
                          'New Jersey',
                          'New York',
                          'Pennsylvania') ~ 'Northeast',
             state %in% c('Indiana',
                          'Illinois',
                          'Michigan',
                          'Ohio',
                          'Wisconsin',
                          'Iowa',
                          'Nebraska',
                          'Kansas', 
                          'North Dakota',
                          'Minnesota', 
                          'South Dakota',
                          'Missouri') ~ 'Midwest',
             state %in% c('Delaware',
                          'District of Columbia',
                          'Florida',
                          'Georgia',
                          'Maryland',
                          'North Carolina',
                          'South Carolina',
                          'Virginia',
                          'West Virginia',
                          'Alabama',
                          'Kentucky',
                          'Mississippi',
                          'Tennessee',
                          'Arkansas',
                          'Louisiana',
                          'Oklahoma',
                          'Texas') ~ 'South',
             state %in% c('Arizona',
                          'Colorado',
                          'Idaho',
                          'New Mexico',
                          'Montana',
                          'Utah',
                          'Nevada',
                          'Wyoming',
                          'Alaska',
                          'California',
                          'Hawaii',
                          'Oregon',
                          'Washington') ~ 'West',
             TRUE ~ 'Other'
           ))

# PK-12 Ed spending
# Elementary and secondary education expenditures per child, in $2016
kids %>%
  filter(variable == 'PK12ed') %>%
  ggplot() +
  geom_line(aes(x = year, y = inf_adj_perchild, group = state)) +
  geom_text(data = kids %>% filter(variable == 'PK12ed' & year == 2016),
            aes(x = year, y = inf_adj_perchild, label = state),
            hjust = -0.1,
            vjust = 0.2)+
  scale_x_continuous(limits = c(1997, 2021)) +
  facet_wrap(~region) +
  theme_classic()

# Higher Ed Spending
# Higher education expenditures per child, in $2016
kids %>%
  filter(variable == 'highered') %>%
  ggplot() +
  geom_line(aes(x = year, y = inf_adj_perchild, group = state)) +
  geom_text(data = kids %>% filter(variable == 'highered' & year == 2016),
            aes(x = year, y = inf_adj_perchild, label = state),
            hjust = -0.1,
            vjust = 0.2) +
  scale_x_continuous(limits = c(1997, 2021)) +
  facet_wrap(~region) +
  theme_classic()


# PK-12 Ed spending
# Elementary and secondary education expenditures per child, in $2016

# Average Spending
avg_kids <- kids %>%
  filter(variable == "PK12ed") %>%
  group_by(year) %>%
  summarise(avg_inf_adj_perchild = mean(inf_adj_perchild))

# Top and Bottom Spenders for Labels
label_kids <- kids %>%
  filter(variable == "PK12ed" & year == 2016) %>%
  filter(state %in% c('District of Columbia', # Top 5
                      'New York',
                      'Vermont',
                      'New Jersey',
                      'Connecticut',
                      'Utah', # Bottom 5
                      'Arizona', 
                      'Idaho', 
                      'Oklahoma',
                      'North Carolina'))
  
# Labels for y-axis, to format labels as $K
ylab <- c(5, 10, 15)

kids %>%
  filter(variable == 'PK12ed') %>%
  ggplot() +
  # State lines
  geom_line(aes(x = year, y = inf_adj_perchild * 1000, group = state),
            size = 0.7,
            color = "#969696") +
  # Label for Top and Bottom 5
  geom_text(data = filter(label_kids, 
                          state != 'Idaho' & state != 'Arizona' & state != 'Utah'),
            aes(x = year, y = inf_adj_perchild * 1000, label = state),
            color = "#969696",
            fontface = "bold",
            hjust = -0.02) +
  geom_text(data = filter(label_kids, 
                          state == 'Utah'),
            aes(x = year, y = inf_adj_perchild * 1000, label = state),
            color = "#969696",
            fontface = "bold",
            hjust = -0.02,
            vjust = 1.3) +
  geom_text(data = filter(label_kids, 
                          state == 'Arizona'),
            aes(x = year, y = inf_adj_perchild * 1000, label = state),
            color = "#969696",
            fontface = "bold",
            hjust = -0.02,
            vjust = 0.6) +
  geom_text(data = filter(label_kids, 
                          state == 'Idaho'),
            aes(x = year, y = inf_adj_perchild * 1000, label = state),
            color = "#969696",
            fontface = "bold",
            hjust = -0.02,
            vjust = -0.1) +
  # U.S. Average Line
  geom_line(data = avg_kids,
            aes(x = year, y = avg_inf_adj_perchild * 1000),
            size = 1.2,
            color = '#0F4C81') +
  # Label for U.S. Average Line
  geom_text(data = filter(avg_kids, year == 2016),
            aes(x = year, y = avg_inf_adj_perchild * 1000),
            label = "U.S. Average",
            color = '#0F4C81',
            fontface = "bold",
            hjust = -0.02) +
  labs(title = "Annual elementary and secondary education expenditures per child",
       subtitle = "1997 through 2016 in 2016 dollars",
       x = "",
       y = "",
       caption = "TidyTuesday 15 Sep 2020 | Data: Urban Institute via tidykids | Designer: Jenn Schilling | jennschilling.me") +
  scale_y_continuous(labels = paste0("$", ylab, "K"),
                     breaks = 1000 * ylab) +
  scale_x_continuous(limits = c(1997, 2019.5),
                     breaks = c(1998, 2000, 2002, 2004, 2006, 2008, 
                                2010, 2012, 2014, 2016)) +
  theme_classic() +
  theme(plot.title.position = "plot",
        text = element_text(family = 'Verdana'))


ggsave("2020-09-15\\k12_spend_per_child.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 8,
       dpi = 300)
