# Kernel PCA
# Feature Extraction for Non-Linear Problems

# Logistic Regression



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

training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])


# Applying Kernel PCA

install.packages('kernlab')
library(kernlab)
kpca <- kpca(~.,
             data = training_set[-3],
             kernel = 'rbfdot',
             features = 2)
training_set_pca <- as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased <- training_set$Purchased

test_set_pca <- as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased <- test_set$Purchased
# Fitting Logistic Regression to Training set

classifier <- glm(formula = Purchased ~ ., 
                  family = binomial,
                  data = training_set_pca)

# Predicting Test Set Results

prob_pred <- predict(classifier, type = 'response', newdata = test_set_pca[-3]) # We don't want the dependent variables involved in the prediction
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
set <- training_set_pca
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('V1', 'V2')  # This column gives the matrix columns names
prob_set <- predict(classifier, type = 'response', newdata = grid_set) # This then predicts the color for the imaginary users (the colors you should but don't see)
y_grid <- ifelse(prob_set > 0.50, 1, 0)
plot(set[,-3],
     main = 'Logistic Regression (Training Set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato')) # Pixel results for imaginary users (the whole grid)
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3')) # Pixel results of the actual results (points)


#


# Visualizing the Test Set Results

set <- test_set_pca
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('V1', 'V2')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.50, 1, 0)
plot(set[,-3],
     main = 'Logistic Regression (Test Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
point(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

