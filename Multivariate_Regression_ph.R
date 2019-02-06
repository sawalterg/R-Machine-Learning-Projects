## 1. PREPROCESSING


# Import the data

dataset <- read.csv("data.csv") # this should be whatever you name your dataset. Make sure its a csv

# View the data

View(dataset)

# Rename the target variable (column name corrupted by file format). This might not be the case for your document

names(dataset)[1] <- "Capacity.utilization"



## 2. EXPLORATORY DATA ANALYSIS



# Quick check to determine all variables are numeric. R's linear model function requires all values be in this form

str(dataset)

# Compute summary statistics

summary(dataset)



## 3. GRAPHICAL EDA




# Check for outliers

hist(dataset$Capacity.utilization,
     xlab = 'Capacity Utilization',
     ylab = 'Frequency',
     col = 'lightblue',
     prob = TRUE)
lines(density(dataset$Capacity.utilization))


# Check for correlation between the variables for potential multicolinearity of independent variables, as well as correlation for dependent variable

# install.packages("corrplot") # If not preinstalled, use this line
library(corrplot)

data_cor <- cor(dataset)
corrplot(data_cor, method = "number")



## 4.  TRAIN-TEST SPLIT (FOR VALIDATION PURPOSES). We split the data here so we can test the assumptions of the model
# and see how it fits on unseen data


# install.packages("caTools") # If not preinstalled, use this line
library("caTools")

# Set random number seed reproducability

set.seed(123)

# Set split pararameter for train test sets

split <- sample.split(dataset$Capacity.utilization, SplitRatio = 0.8) 


training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)



## 5.  General Linear Model (OLS)



# Create regressor object with all variables included

regressor1 = lm(formula = Capacity.utilization  ~ .,    
               data = training_set)


# Generate summary statistics

summary(regressor1)


# Backward Elimination (t test to remove lowest p value above SL)

regressor2 = lm(formula = Capacity.utilization ~ Price.flexibility + Overall.service + Size.of.firm,   
               data = training_set)

summary(regressor2)

# It looks like after removing the variable with the highest p value, the Adjusted R Squared decreased. We will
# keep this variable for our final model (side note - it may be worth normalizing your variables so they are on the same scale
# right, think of size of firm vs. price flexibility.)




## 6. Check assumptions of linearity -- normal distribution of the errors and homscedasity



# Checking fitted vs actual values to check for homoscedasity

plot(fitted(regressor1), 
     residuals(regressor1))

# install.packages("olsrr")
library(olsrr)


# QQ plot for determining normality - if the points are essentially all on the plot line, then normal distribution

ols_plot_resid_qq(regressor1)



# 7. PREDICT ON TEST SET - Remember we split our data here, so we could validate our data

y_pred <- predict(regressor1, newdata = test_set)

summary(y_pred)


ggplot() + geom_point(aes(x = y_pred, y = test_set$Capacity.utilization), color = 'blue') +
     xlab("Predicted") +
     ylab("Actual")

# This is a pretty good fit for our data. Stick with these four variables
