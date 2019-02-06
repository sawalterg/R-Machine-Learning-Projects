# k-fold cross validation
# Two types of parameters
# 1. Learned parameters through the actual model
# 2. Hyperparameters are the ones we choose (kernel parameters (used to improve the model)). Since these are not learned by the models, we need to optimize how we select these parameters

# The Variance problem--if we only test our model on one test set, we can see a large difference in the accuracy on the test set and another corpus of data

# k-fold fixes variance problem. You split the data into ten folds and train the model on 9 folds and test on 1 fold. Then perform 10 iterations. Then we can take an average of the 
# ten different accuracies and get the mean and standard deviation. This will give us a more relevant score

# Kernel SVM
# Classification Template


## Importing the datset

dataset <- read.csv("Social_Network_Ads.csv", header = TRUE)
dataset <- dataset[, 3:5]



# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling

training_set[,-3] <- scale(training_set[,-3])
test_set[,-3] <- scale(test_set[,-3])

# Fitting Logistic Regression to Training set

library(e1071)

classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "radial")

# Predicting Test Set Results

y_pred = predict(classifier, newdata = test_set[-3])


# Make confusion matrix





cm <- table(test_set[,3], y_pred)


# k-fold cross validation for evaluating the performance of the model

#install.packages('caret')
library(caret)

folds <- createFolds(training_set$Purchased, k = 10) # ten is a good rule of thumb --this part just creates a folds list, doesn't apply a function
# the first argument is how to split the dataset (do it based off the dependent variable)
# second argument is the number of folds
# lapply applies a function to elements of a list
cv <- lapply(folds, function(x) { # x is a local variable
  training_fold <- training_set[-x, ] # new local variable. tAKING THE whole trainingset minus the test fold
  test_fold <- training_set[x,] # x represents all the observations for the 
  classifier <- svm(formula = Purchased ~ .,
                    data = training_fold,
                    type = "C-classification",
                    kernel = "radial")
  y_pred <- predict(classifier, newdata = test_fold[-3])
  cm <- table(test_fold[,3], y_pred)
  accuracy <- (cm[1,1] + cm[2,2])/(sum(cm))
  return(accuracy)
  
  
  
} ) 

accuracy <- mean(as.numeric(cv))

# first the data, then a function to be applied to a list



# Visualizing the Training Set Results
# install.packages('ElemStatLearn')
# library(ElemStatLearn)
set <- training_set # Assigning this as a local variable helps with automation
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('Age', 'EstimatedSalary')  # This column gives the matrix columns names
y_grid <- predict(classifier, newdata = grid_set) # This then predicts the color for the imaginary users (the colors you should but don't see)
plot(set[,-3],
     main = 'SVM (Training Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
point(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato')) 
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3')) 




# Visualizing the Test Set Results

set <- test_set
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM (Test Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
point(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

