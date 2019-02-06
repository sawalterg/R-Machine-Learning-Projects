# SVR

## Importing the datset

dataset <- read.csv("Position_Salaries.csv", header = TRUE)
dataset <- dataset[, 2:3]


# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
#library("caTools")
#set.seed(123)
#split <- sample.split(dataset$Purchased, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

#training_set <- subset(dataset, split == TRUE)
#test_set <- subset(dataset, split == FALSE)

# Feature Scaling

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])



# Fitting SVR to the dataset
# install.packages('e1071')
# library(e1071)

## SVM is for classification and SVR is for regression

regressor = svm(formula = Salary ~., 
                data = dataset,
                type = "eps-regression")# This specifies eps regression for regression, as opposed to classification. Kernel is automatically gausian

                 
# Predicting a new result

y_pred = predict(regressor, data.frame(Level = 6.5)) #You the input to be a dataframe
                 

# install.package('ggplot2')

# Visualizing the Linear Regression Results
ggplot() + geom_point(aes(x = dataset$Level, y = dataset$Salary),
                      colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle("Truth of Bluff (SVR Regression)") +
  xlab("Level") +
  ylab("Salary")