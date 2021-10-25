# Machine Learning with R - Chapter 7 
## Second example - OCR data in support vector machine

#install.packages("kernlab")
library(kernlab)

letters <- read.csv("letterdata.csv", stringsAsFactors = TRUE)
str(letters)
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

###Training the model
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")
letter_classifier

###Test the model
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

###Improving model performance
# Using the Gaussian RBF kernel function in the ksvm() -> actually always good to start out with this one
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))


