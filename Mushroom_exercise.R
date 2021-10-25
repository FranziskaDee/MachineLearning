# Machine Learning with R - Chapter 5 
## Second example - mushroom data

library(C50)
library(gmodels)
#install.packages("RWeka")
library(RWeka)

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

### Exploring and preparing the data
str(mushrooms)
mushrooms$veil_type <- NULL
table(mushrooms$type)

###Model training with only 1R
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
summary(mushroom_1R)

###Model training with RIPPER
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

