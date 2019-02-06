# Principle Componenet Analysis

# This feature extraction technique (as oppossed to feature selection (like backwards elimination)) selects p independent variables from your 
# these are new variables that explain the most variance, independent of the dependent variable
# This is unsupervised because we don't consider the dependent variable

# We can reduce number of important variables for visualization purposes






## Importing the datset

dataset <- read.csv("Wine.csv", header= T)


# Splitting the dataset into the Training and Test Set
# install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# Feature Scaling

training_set[-14] <- scale(training_set[-14])
test_set[-14] <- scale(test_set[-14])





# PCA Analysis (Post Pre-processing)
library(caret)
library(e1071)

pca <- preProcess(x = training_set[-14], method = "pca", pcaComp = 2) # Creates PCA object


training_set <- predict(pca, training_set) # This original training set will become a new dataset with two new independent variables


# Move the dependent variable in the last position

training_set <- training_set[c(2,3,1)] # These numbers reference the columns


# Now for the test set

test_set <- predict(pca, test_set) # This original training set will become a new dataset with two new independent variables


# Move the dependent variable in the last position

test_set <- test_set[c(2,3,1)] # These numbers reference the columns

# Fitting SVM to Training set

classifier <- svm(formula = Customer_Segment ~ .,
                  data = training_set,
                  type = 'C-classification',
                  kernel = 'linear')

# Predicting Test Set Results

y_pred <- predict(classifier, type = 'response', newdata = test_set[-3]) # We don't want the dependent variables involved in the prediction
# This just gives the predicted probability of purchase, instead of actual predictions

y_pred <- ifelse(prob_pred > 0.5, 1, 0) # First argument is condition, next is if condition met and last is if condition not met
# Make confusion matrix

# Rows are actual, columns are predicted, doesn't matter how you align the two

cm <- table(test_set[,3], y_pred)
cmpunk <- table(y_pred, test_set[,3])
cm
cmpunk


# Visualizing the Training Set Results
# install.packages('ElemStatLearn')
# library(ElemStatLearn)
set <- training_set
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('PC1', 'PC2')  # This column gives the matrix columns names
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM (Training Set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato'))) # Pixel results for imaginary users (the whole grid)
points(set, pch = 21, bg = ifelse(set[,3] == 2, 'blue3', ifelse(set[,3] == 1, 'green4', 'red3'))) # Pixel results of the actual results (points)


#


# Visualizing the Test Set Results

set <- test_set
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('PC1', 'PC2')
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM (Test Set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,3] == 2, 'blue3', ifelse(set[,3] == 1, 'green4', 'red3')))
