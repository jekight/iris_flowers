library(tidyverse)
library(caret)
library(ggplot2)

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

#Now we need to isolatate 80% of the data for testing/training and the rest for validation.

#First use set.seed so we can reproduce the results with the random number generator.
set.seed(3142)

#The function createDataPartition can be used to create balanced splits of the data. 
#If the y argument to this function is a factor, the random sampling occurs within each class 
#and should preserve the overall class distribution of the data.
test_data <- createDataPartition(dataset$class, times = 1, p = 0.80, list = FALSE)

#Now select 20% of the data for validation.
validation <- dataset[-test_data,]

#Now select 80% of the data for testing/training.
dataset <- dataset[test_data,]

#Confirm we have an equal amount of data from each class.
percentage <- prop.table(table(dataset$class)) * 100

#To present the results in a table format.
percent_table <- cbind(freq=table(dataset$class),percentage=percentage)

# Summarize the attribute distributions
summary(dataset)

#Let's start visualizing the data.
ggplot(data = dataset) +
  geom_point(mapping = aes(x = sepal_length, y = sepal_width, color = class))

ggplot(data = dataset) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = class))

ggplot(data = dataset) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = class))

ggplot(data = dataset) +
  geom_point(mapping = aes(x = sepal_length, y = petal_length, color = class))


ggplot(data = dataset) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = class)) +
  facet_grid(~class)

ggplot(data = dataset) +
  geom_point(mapping = aes(x = sepal_length, y = sepal_width, color = class)) +
  facet_grid(~class)

#Now that we have seen some scatter plots, let's look at boxplots.

ggplot(data = dataset, mapping = aes(x = class, y = sepal_length)) +
  geom_boxplot()

ggplot(data = dataset, mapping = aes(x = class, y = sepal_width)) +
  geom_boxplot()

ggplot(data = dataset, mapping = aes(x = class, y = petal_length)) +
  geom_boxplot()

ggplot(data = dataset, mapping = aes(x = class, y = petal_width)) +
  geom_boxplot()

#Now let's look at some density plots.
ggplot(data = dataset, mapping = aes(x = sepal_length, color = class)) +
  geom_density()

ggplot(data = dataset, mapping = aes(x = sepal_width, color = class)) +
  geom_density()

ggplot(data = dataset, mapping = aes(x = petal_length, color = class)) +
  geom_density() 

ggplot(data = dataset, mapping = aes(x = petal_width, color = class)) +
  geom_density()

#This will split our dataset into 10 parts, train in 9 and test on 1 and 
#release for all combinations of train-test splits. We will also repeat the 
#process 3 times for each algorithm with different splits of the data into 10 groups, 
#in an effort to get a more accurate estimate.

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Time to build some models.

# a) linear algorithms
set.seed(8)
fit.lda <- train(class~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(8)
fit.cart <- train(class~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(8)
fit.knn <- train(class~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(8)
fit.svm <- train(class~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(8)
fit.rf <- train(class~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#box plot of the results
dotplot(results)

#From the results it appears the most accurate model was the lda (linear model).

#Summarize the best model.
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$class)




