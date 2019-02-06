# Multiple Linear Regression

# Importing the dataset

dataset <- read.csv("50_Startups.csv")

# dataset <- dataset[,1:4]

# Encoding Categorical data

dataset$State <- factor(dataset$State,
                        levels = c('New York', 'California', 'Florida'),
                        labels = c(1,2,3))

dataset <- dataset[,1:4]

# Splitting the dataset into Training and Test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# No need for manually applying feature scaling
# Period indicates all independent variables
regressor = lm(formula = Profit ~ .,    
               data = training_set,)
summary(regressor)
## Note the stars! That is significance level. Profit is governed by the R & D spend. If you remove all the other independent variables, you will still get the same, as there is only
## one variable of significance



y_pred <- predict(regressor, newdata = test_set)
print(y_pred)

summary(regressor)

# Backward Elimination

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,    
               data = dataset)

summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,    
               data = dataset)

summary(regressor)


regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,    
               data = dataset)

summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend,    
               data = dataset)

summary(regressor)
