# Regression Template


## Importing the datset

dataset <- read.csv("Data.csv", header = TRUE)
dataset <- dataset[, 2:3]


# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])



# Import Packages


# Fitting Regression to the dataset

regressor = svm(formula = Salary ~., 
                data = dataset)

# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# install.package('ggplot2')

# Visualizing the Regression Results
ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle("Truth of Bluff (SVR Regression)") +
  xlab("Level") +
  ylab("Salary")

# Visualizing the  Regression Results - this doesn't work, straight line. Decision tree based on Euclidean distances, so we don't need to feature scale. This problem is related to the number of splits -- too few. This just averaged the levels
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle("Truth of Bluff (Decision Tree Regression)") +
  xlab("Level") +
  ylab("Salary")