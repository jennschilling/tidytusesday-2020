# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest
library(tidytuesdayR)
library(tidyverse)

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-07-28')

penguins <- tuesdata$penguins # cleaned data

# Another way to get the data
# install.packages("palmerpenguins")

glimpse(penguins)
