# Data Preprocessing

## Importing the datset

dataset <- read.csv("Data.csv", header = TRUE)


# We don't need to separate independent (features) and dependent variable like we do in Python

dataset$Age <- ifelse(is.na(dataset$Age), mean(dataset$Age, na.rm = TRUE), dataset$Age)

## 

dataset$Salary <- ifelse(is.na(dataset$Salary), mean(dataset$Salary, na.rm = TRUE), dataset$Salary)


### Note the na.rm = TRUE so the mean does not factor in missing data


## Encoding Categorical Data (No need for one hot encoding, R views as factors/levels)

str(dataset)

dataset$Country <- as.factor(dataset$Country)
dataset$Purchased <- as.factor(dataset$Purchased)


# Or as this convoluted bullshit if we want to follow instructions
dataset$Country <- factor(dataset$Country, 
                          levels = c("France", "Spain", "Germany"),
                          labels = c(1, 2, 3))
dataset$Purchased <- factor(dataset$Purchased,
                            levels = c("Yes", "No"),
                            labels = c(1, 0))

# Splitting the dataset into the Training and Test Set

# install.packages("caTools")
library("caTools")

## Set seed is how get some reproducability

set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling

training_set[,2:3] <- scale(training_set[,2:3])
test_set[,2:3] <- scale(test_set[,2:3])

## These don't work because all columns are not numeric, we need to then select only the relevent categories. 

