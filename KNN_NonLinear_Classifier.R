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

training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

# Fitting KNN  to Training set AND PREDICTING test set results
# Create Classifier

#install.packages('class')
#library(class)

y_pred = knn(train = training_set[,-3],
             test = test_set[,-3,],
             cl = training_set[,3],
             k = 5)# Only needs the dependent variables and not the indpendent variables



# Make confusion matrix





cm <- table(test_set[,3], y_pred)



# Visualizing the Training Set Results
# install.packages('ElemStatLearn')
# library(ElemStatLearn)

# We need to change the template a bit because the knn function includes part of the test set. No predict function


set <- training_set # Assigning this as a local variable helps with automation
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('Age', 'EstimatedSalary')  # This column gives the matrix columns names
y_grid <- knn(train = training_set[,-3],
                test = grid_set,
                cl = training_set[,3],
                k = 5)#, type = 'response', newdata = grid_set) # This then predicts the color for the imaginary users (the colors you should but don't see)
plot(set[,-3],
     main = 'KNN (Training Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
point(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato')) 
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3')) 




# Visualizing the Test Set Results

set <- test_set # Assigning this as a local variable helps with automation
x1 <- seq(min(set[,1]) -1, max(set[,1]) + 1, by = 0.1) # By adding the plus and minus one we are making sure the points aren't squeezed on the plot for both independent variables
x2 <- seq(min(set[,2]) -1, max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2) # This makes the two sequences above into a grid/matrix
colnames(grid_set) <- c('Age', 'EstimatedSalary')  # This column gives the matrix columns names
y_grid <- knn(train = training_set[,-3],
              test = grid_set,
              cl = training_set[,3],
              k = 5)#, type = 'response', newdata = grid_set) # This then predicts the color for the imaginary users (the colors you should but don't see)
plot(set[,-3],
     main = 'KNN (Training Set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
point(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato')) 
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3')) 

