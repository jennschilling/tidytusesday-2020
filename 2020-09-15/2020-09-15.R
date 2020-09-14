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
library(janitor)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-09-15')

kids <- tuesdata$kids

#### Explore Data ####
