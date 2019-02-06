# XGBoost - Most powerful version of gradient boosting with decision trees

# This algorithm works very fast and is quite accurate with large datasets
# in terms of performance -- great accuracy and fast computation speed

# No need to feature scale and you can keep all the interpretations of the model

## Importing the datset

dataset <- read.csv("Churn_Modelling.csv", header = TRUE)
dataset <- dataset[, 4:14]
sdf
# Encoding the categorical variables as factors and as numeric (Deep learning package requires this)

dataset$Gender <- as.numeric(factor(dataset$Gender,
                                    levels = c("Female", "Male"),
                                    labels = c(1,2)))

dataset$Geography <- as.numeric(factor(dataset$Geography,
                                       levels = c("France", "Spain", "Germany"),
                                       labels = c(1,2,3)))


# Splitting the dataset into the Training and Test Set

install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)


# Fitting XGBoost to the training set

#install.packages('xgboost')
library(xgboost)

classifier <- xgboost(data = as.matrix(training_set[-11]),
                      label = training_set$Exited,
                      nrounds = 10) # label is the dependent variable, but needs to be a vector of response values
# nrounds is the number of interations to train the model

# We get the information of the root mean square error. Relevent computation of the error. The lower the error the better the model
# Error decreased from the first to the tenth error 


library(caret)
folds <- createFolds(training_set$Exited, k = 10) # ten is a good rule of thumb --this part just creates a folds list, doesn't apply a function
# the first argument is how to split the dataset (do it based off the dependent variable)
# second argument is the number of folds
# lapply applies a function to elements of a list
cv <- lapply(folds, function(x) { # x is a local variable
  training_fold <- training_set[-x, ] # new local variable. tAKING THE whole trainingset minus the test fold
  test_fold <- training_set[x,] # x represents all the observations for the 
  classifier <- xgboost(data = as.matrix(training_set[-11]),
                     label = training_set$Exited,
                     nrounds = 10)
  y_pred <- predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred <- (y_pred >= 0.5)
  cm <- table(test_fold[,11], y_pred)
  accuracy <- (cm[1,1] + cm[2,2])/(sum(cm))
  return(accuracy)
  
  
  
} ) 

accuracy = mean(as.numeric(cv))
# Pretty good without parameter tuning like Kfold cross validation

