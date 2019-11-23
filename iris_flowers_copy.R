library(tidyverse)
library(caret)

#Load in the iris flower data.
dataset <- read_csv("iris_flowers.csv")

#take a look at the data
view(dataset)

#Now let's check the classes of the data.
sapply(dataset, class)

#Use table() to get a idea of factors in the class column.
table(dataset$class)

#Convert class column (a character vector) to a factor.
dataset$class <- as.factor(dataset$class)

#You could use head(dataset) to see any changes

#Now we need to isolatate 75% of the data for testing/training and the rest for validation.

#First use set.seed so we can reproduce the results with the random number generator.
set.seed(3142)

#The function createDataPartition can be used to create balanced splits of the data. 
#If the y argument to this function is a factor, the random sampling occurs within each class 
#and should preserve the overall class distribution of the data.
test_data <- createDataPartition(dataset$class, times = 1, p = 0.80, list = FALSE)

#Now select 25% of the data for validation.
validation <- dataset[-test_data,]

#Now select 75% of the data for testing/training.
dataset <- dataset[test_data,]

#Confirm we have an equal amount of data from each class.
percentage <- prop.table(table(dataset$class)) * 100

#To present the results in a table format.
percent_table <- cbind(freq=table(dataset$class),percentage=percentage)

# Summarize the attribute distributions
summary(dataset)

#Let's start visualizing the data.




