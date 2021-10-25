# Machine Learning with R - Chapter 8 
## First example - Groceries data -> association

###Cannot simple read the data with read.csv since that will set it into a df, not useful in the case of market basket analysis
###"need a dataset that does not treat a transaction as a set of positions 
###to be filled (or not filled) with specific items, but rather as a market 
###basket that either contains or does not contain each particular item."
###Solution: Sparse Matrix

#install.packages("arules")
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)
inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])

###Visualizing item support – item frequency plots
itemFrequencyPlot(groceries, support = 0.1)  #m showing the eight items in the groceries data with at least 10 percent support
itemFrequencyPlot(groceries, topN = 20) #limit the plot to a specific number of items
image(groceries[1:5]) #Sparse Matrix for first five transactions
image(sample(groceries, 100)) #a random selection of 100 transactions

###Model training
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
groceryrules

###Evaluating model performance
summary(groceryrules)
inspect(groceryrules[1:3])

###Improving model performance
inspect(sort(groceryrules, by = "lift")[1:5]) #Best five rules according to lift
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
