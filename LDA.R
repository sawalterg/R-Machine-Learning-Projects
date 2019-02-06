# Linear Discriminant Analysis

# New independent variables that will seperate the classes the most (supervised model)

# Another dimensionality reduction technique


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
library(MASS)

lda <- lda(formula = Customer_Segment ~ ., data = training_set)   # LDA is supervised. Since it takes into account the number of discriminants (class separators). The number of discriminants is 
# k - 1 (two in this case). K being the number of classes




training_set <- as.data.frame(predict(lda, training_set)) # This original training set will become a new dataset with two new independent variables

# We need to make this a dataframe. LDA creates a matrix

training_set <- training_set[c(5,6,1)]
# Move the dependent variable in the last position


# Now for the test set

test_set <- as.data.frame(predict(lda, test_set)) # This original training set will become a new dataset with two new independent variables


# Move the dependent variable in the last position

test_set <- test_set[c(5,6,1)] # These numbers reference the columns

# Fitting SVM to Training set

classifier <- svm(formula = customer_segment ~ .,
                  data = training_set,
                  type = 'C-classification',
                  kernel = 'linear')

# Predicting Test Set Results

y_pred <- predict(classifier, type = 'response', newdata = test_set[-3]) # We don't want the dependent variables involved in the prediction
# This just gives the predicted probability of purchase, instead of actual predictions

# Make confusion matrix

# Rows are actual, columns are predicted, doesn't matter how you align the two

cm <- table(test_set[,3], y_pred)


# Visualizing the Training Set Results
# install.packages('ElemStatLearn')
# library(ElemStatLearn)
set <- training_set
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('x.LD1', 'x.LD2')  # This column gives the matrix columns names
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM (Training Set)',
     xlab = 'LD1', ylab = 'LD2',
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
colnames(grid_set) <- c('x.LD1', 'x.LD2')
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM (Test Set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,3] == 2, 'blue3', ifelse(set[,3] == 1, 'green4', 'red3')))
