# Machine Learning with R - Chapter 6 
## Second example - Medical insurance data

install.packages("psych")
library(psych)

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)

###Exploring the data
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses) 
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

###OLS Model
ins_model <- lm(expenses ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
ins_model

###Evaluating model performance
summary(ins_model)

###Improving the model
insurance$age2 <- insurance$age^2

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                  smoker + region, data = insurance)
summary(ins_model2)
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

ins_model3 <- lm(expenses ~ age + age2 + children + bmi + bmi30 + sex +
                   smoker + region, data = insurance)
summary(ins_model3)

ins_model4 <- lm(expenses ~ age + age2 + children + bmi + bmi30 + sex +
                   smoker + bmi30*smoker + region, data = insurance)
summary(ins_model4)

