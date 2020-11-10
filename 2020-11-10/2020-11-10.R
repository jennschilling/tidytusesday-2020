# Author : Jenn Schilling
# Title: #TidyTuesday Historical Phone Usage 
# Date: 11/10/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(gganimate) # for plot animation

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-11-10')

mobile <- tuesdata$mobile

landline <- tuesdata$landline


#### Explore the Data ####

skim(mobile)

skim(landline)


#### Graph the Data ####

# Density plot of the most recent mobile data by continent
mobile %>%
  filter(year == 2017) %>%
  ggplot() +
  geom_density(aes(x = mobile_subs, fill = continent),
               alpha = 0.5) 


# Gapminder style
mobile %>%
  filter(year == 2013) %>%
  ggplot() +
  geom_point(aes(x = gdp_per_cap, y = mobile_subs, size = total_pop, color = continent))

yr_plot <- ggplot(mobile,
                  aes(x = gdp_per_cap, y = mobile_subs, size = total_pop, color = continent, frame = year)) +
  geom_point() +
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(yr_plot, width = 450, height = 450)

anim_save("2020-11-10\\test.gif")

# TO DO: make years a factor so that there aren't decimals in the year title, remove years with no data,
# make graph larger and/or add landline animation next to it