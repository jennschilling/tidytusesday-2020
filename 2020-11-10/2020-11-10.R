# Author : Jenn Schilling
# Title: #TidyTuesday Historical Phone Usage 
# Date: 11/10/2020

#### Libraries ####

library(tidytuesdayR)
library(tidyverse)
library(skimr)

#### Get the Data ####

tuesdata <- tidytuesdayR::tt_load('2020-11-10')

mobile <- tuesdata$mobile

landline <- tuesdata$landline


#### Explore the Data ####

skim(mobile)

skim(landline)
