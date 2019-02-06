# Random Forest


## Importing the datset

dataset <- read.csv("Position_Salaries.csv", header = TRUE)
dataset <- dataset[, 2:3]


# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
# library("caTools")
# set.seed(123)
 #split <- sample.split(dataset$Purchased, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

# training_set <- subset(dataset, split == TRUE)
# test_set <- subset(dataset, split == FALSE)

# Feature Scaling

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])



# Import Packages
# install.packages('randomForest')
#library(randomForest)


# Fitting Random Forest to the dataset
set.seed(1234)
regressor = randomForest(x = dataset[1], # This will give you a dataframe (necessary)
                         y = dataset$Salary,  # This will produce a vector
                        ntree = 500)

# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# install.package('ggplot2')

# Visualizing the Random Forest  Regression Results - this doesn't work, straight line. Decision tree based on non-Euclidean distances, so we don't need to feature scale. This problem is related to the number of splits -- too few. This just averaged the levels
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.001) # Change the 0.x add zero to increase resolution
ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle("Truth of Bluff (Random Forest Regression)") +
  xlab("Level") +
  ylab("Salary")