# Machine Learning with R - Chapter 6 
## Third example - wine data

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(RWeka)

wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

###Training the model
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

###Visualizing decision trees
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

###evaluating model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)

###mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)

###Regression tree
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
