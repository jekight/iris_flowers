library(tidyverse)
library(caret)

file_name <- "iris_flowers.csv"
dataset <- read_csv(file_name)
colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# create a list of 75% of the rows in the original dataset to use for training.

test_data <- createDataPartition(dataset$class, p = 0.75, times = 1, list = FALSE)

# Select 25% of the data for validation.

validation <- dataset[-test_data,]

#Use the Remaining 75% of data to training and testing the models

dataset <- dataset[test_data,]

# Dimensions of dataset

dim(dataset)

# List types for each attribute

sapply(dataset, class)

# Take a peek at the first 5 rows of the data

head(dataset)

# Summarize the class distribution

percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# Summarize the attribute distributions

summary(dataset)

# Split input and output

x <- dataset[,1:4]
y <- dataset[,5]

# Boxplot for each attribute on one image

par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(x[,i], main=names(iris)[i])
}
 # Bar plot for class breakdown

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# Summarize accuracy of models

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


       

