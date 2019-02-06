# Polynomial Regression

# Data Preprocessing

## Importing the datset


dataset <- read.csv("Position_Salaries.csv", header = TRUE)
dataset <- dataset[, 2:3]



# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
# library("caTools")
# set.seed(123)
# split <- sample.split(dataset$Purchased, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

# training_set <- subset(dataset, split == TRUE)
# test_set <- subset(dataset, split == FALSE)

# Feature Scaling

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

# Fitting Polynomial Linear Regression to the dataset (for base comparison)

lin_reg <- lm(formula = Salary ~ Level, 
              data = dataset)
summary(lin_reg)


# Fitting Polynomial Regression to the dataset

dataset$Level2 <- dataset$Level ^ 2 
dataset$Level3 <- dataset$Level ^ 3
dataset$Level4 <- dataset$Level ^ 4


poly_reg <- lm(formula = Salary ~ .,
               data = dataset)
summary(poly_reg)
poly_reg

# Visualizing the Linear Regression Results
ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle("Truth of Bluff (Linear Regression)") +
  xlab("Level") +
  ylab("Salary")

# Visualizing the Polynomial  Regression Results

ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = "blue") +
  ggtitle("Truth or Bluff(Polynomial Regression)") +
  xlab("Level") +
  ylab("Salary")

# Predicting a new result with Linear Regression (Can't predict like in python, must make cell)
Y_pred <- predict(lin_reg, data.frame(Level = 6.5))
Y_pred # Shit is innacurate

# Predicting a new result with Polynomial Regression (4 degrees)

Y_pred_pol <- predict(poly_reg, data.frame(Level = 6.5, 
                                       Level2 = 6.5^2, 
                                       Level3 = 6.5^3,
                                       Level4 = 6.5^4))


