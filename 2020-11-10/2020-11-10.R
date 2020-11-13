# Author : Jenn Schilling
# Title: #TidyTuesday Historical Phone Usage 
# Date: 11/10/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(gganimate) # for plot animation
library(gapminder) # for population data

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-11-10')

mobile <- tuesdata$mobile

landline <- tuesdata$landline


#### Explore the Data ####

skim(mobile)

skim(landline)

#### Data manipulation ####

# Total population does not match between the datasets

# Combine data together
phone_data <- full_join(landline, mobile, by = c("entity", "code", "year", 
                                                 "continent", "gdp_per_cap")) %>%
  select(-total_pop.x, -total_pop.y)

# Get population data

summary(gapminder_unfiltered)

phone_data <- phone_data %>%
  rename(country = entity) %>%
  left_join(gapminder_unfiltered, by = c("country", "continent", "year"))

# Population data is only available in certain years, and GDP per cap is not
# aligning, so I guess I will go back to using each dataset separately with the
# different population numbers

# Leave out population and just compare landlines and mobile together
phone_data_long <- phone_data %>%
  select(1:7) %>%
  pivot_longer(c("mobile_subs", "landline_subs"), names_to = "phone_type", 
               values_to = "num_subs")

#### Graph the Data ####

# Density plot of the most recent mobile data by continent
mobile %>%
  filter(year == 2017) %>%
  ggplot() +
  geom_density(aes(x = mobile_subs, fill = continent),
               alpha = 0.5) 


# Gapminder style

# Plot GDP vs. Mobile Subscriptions with size as population
mobile %>%
  filter(year == 2013) %>%
  ggplot() +
  geom_point(aes(x = gdp_per_cap, y = mobile_subs, 
                 size = total_pop, color = continent))

# Create animation of plot for mobile
yr_plot <- mobile %>%
  filter(year <= 2013) %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = gdp_per_cap, y = mobile_subs, 
                      size = total_pop, color = continent, 
                      frame = year)) +
  geom_point() +
  labs(x = "GDP per Capita",
       y = "Mobile Subscriptions per 100 People",
       color = "Continent",
       size = "Total Population") +
  theme_classic() +
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(yr_plot)

anim_save("2020-11-10\\mobile.gif")

# Create animation of plot for landlines
yr_plot <- landline %>%
  filter(year <= 2013) %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = gdp_per_cap, y = landline_subs, 
             size = total_pop, color = continent, 
             frame = year)) +
  geom_point() +
  labs(x = "GDP per Capita",
       y = "Landline Subscriptions per 100 People",
       color = "Continent",
       size = "Total Population") +
  theme_classic() +
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(yr_plot)

anim_save("2020-11-10\\landline.gif")

# Compare landlines to mobile phones
phone_data_long %>%
  ggplot() +
  geom_jitter(aes(x = year, y = num_subs, color = phone_type)) +
  facet_wrap(~continent)

phone_data %>%
  filter(year == 2016) %>%
  ggplot() +
  geom_jitter(aes(x = landline_subs, y = mobile_subs, color = continent))

phone_data %>%
  filter(year == 1990) %>%
  ggplot() +
  geom_jitter(aes(x = landline_subs, y = mobile_subs, color = continent))


# Create animation of plot for landlines vs. mobile
yr_plot <- phone_data %>%
  filter(year <= 2016) %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = landline_subs, y = mobile_subs, 
             color = continent, frame = year,
             shape = continent)) +
  geom_point() +
  labs(x = "Landline Subscriptions per 100 People",
       y = "Mobile Subscriptions per 100 People",
       shape = "Continent",
       color = "Continent") +
  theme_classic() +
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

animate(yr_plot,
        duration = 20)  

anim_save("2020-11-10\\landline_v_mobile.gif")
