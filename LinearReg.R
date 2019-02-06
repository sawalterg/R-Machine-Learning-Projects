# Simple Linear Regression


## Importing the datset

dataset <- read.csv("Salary_Data.csv", header = TRUE)



# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])


## Fitting Simple Linear Regression to the Training Set

regressor <- lm(formula = Salary~YearsExperience, data = training_set)  # The tilde always has Y on the left and x on the right
summary(regressor)
### Note the three stars in the Co-efficients, that means high significance


## Predicting the Test Set Results
y_pred <- predict(regressor, newdata = test_set)
y_pred 

# Visualize the Training Set Results

library(ggplot2)

ggplot()+
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), colour = 'blue') + # Make sure we take the predictions from train set and not test set
  ggtitle('Salary vs. Experience (Training Set)')+
  xlab('Years of Experience')+
  ylab('Salary')
  

ggplot()+
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), colour = 'blue') + # We don't change the input into test set because because the regressor is trained on the same set, becasue we have one unique model equation
  ggtitle('Salary vs. Experience (Test Set)')+
  xlab('Years of Experience')+
  ylab('Salary')   # Note that the blue line (regressor) doesn't change at all. There is certainly a linear dependency, but not a perfect amount
