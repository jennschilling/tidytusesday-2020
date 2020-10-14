# Author : Jenn Schilling
# Title: #TidyTuesday NCAA Women's Basketball Tournament
# Date: 10/62020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(ggtext)
library(explore)
library(corrplot)
library(caret)
library(rattle)

# Load  fonts
#font_import()
loadfonts(device = "pdf")


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-10-06')

tournament <- tuesdata$tournament

#### Explore the Data ####

skim(tournament)

explore(tournament)

#### Predict Tournament Finish ####

# Dependent Variables
# seed, conference, conf_place, how_qual, x1st_game_at_home,
# conf_percent, reg_percent

# Independent Variable
# tourney_finish

model_data <- tournament %>%
  select(seed, conference, conf_percent, conf_place,
         reg_percent, how_qual, x1st_game_at_home, tourney_finish) %>%
  filter(complete.cases(.))

# Check correlations
cor.matrix <- cor(select(model_data, c("seed", "conf_percent", "reg_percent")))

corrplot(cor.matrix, method = "color", type = "lower")

# Check percentage of outcome variable
model_data %>% group_by(tourney_finish) %>% dplyr::summarise(n = n()) %>% mutate(perc = n/sum(n))

# 27% Fail, 44% Pass, 29% Exceptional

# Split data into train and testing sets (80% train, 20% test)
set.seed(246)

# Stratified Sampling 
split_ids <- createDataPartition(model_data$tourney_finish, p = 0.8, list = F)

data.train <- model_data[split_ids,] 
data.test <- model_data[-split_ids,] 

# Double check proportions of testing and training datasets
prop.table(table(data.train$tourney_finish))
prop.table(table(data.test$tourney_finish))

# Decision Tree

# Cross Validation - 10-Fold, Repeated 10 Times
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(246)

# Create Decision Tree
decision.tree <- train(tourney_finish ~ ., 
                       data = data.train, 
                       method = "rpart",
                       trControl = trControl) 

# Examine Results
fancyRpartPlot(decision.tree$finalModel, caption = NULL)

decision.tree # Accuracy 0.58 Kappa 0.37

#### Maybe Try Something Else... ####

elite_8_teams <- tournament %>%
  filter(tourney_finish %in% c("RF", "NSF", "N2nd", "Champ"))

elite_8_teams %>%
  ggplot(.) +
  geom_line(aes(x = year, y = seed, group = school)) +
  facet_wrap(~tourney_finish)
