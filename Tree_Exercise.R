# Machine Learning with R - Chapter 5 
## First example - credit data

#install.packages("C50")
#install.packages("gmodels")

library(C50)
library(gmodels)

###Data exploration
credit <- read.csv("credit.csv", stringsAsFactors = T)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

###Data preparation
#Randomly split dataset to generate training and testing dfs
set.seed(12345) #for replicability
credit_rand <- credit[order(runif(1000)), ]
#Test that randomization worked and did not somehow affect dataframe
summary(credit$amount)
summary(credit_rand$amount) #The two fcts generate same outcome
head(credit$amount)
head(credit_rand$amount)
#Split datasets
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

###Builiding a decision tree
 
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

###Evaluating model performance
credit_pred <- predict(credit_model, credit_test)

CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

###Improving model performance
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

###Introducing penalties
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)

credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
