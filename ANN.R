# Artificial Neural Network (ANN)


## Importing the datset

dataset <- read.csv("Churn_Modelling.csv", header = TRUE)
dataset <- dataset[, 4:14]

# Encoding the categorical variables as factors and as numeric (Deep learning package requires this)

dataset$Gender <- as.numeric(factor(dataset$Gender,
                         levels = c("Female", "Male"),
                         labels = c(1,2)))

dataset$Geography <- as.numeric(factor(dataset$Geography,
                            levels = c("France", "Spain", "Germany"),
                            labels = c(1,2,3)))


# Splitting the dataset into the Training and Test Set

install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)   # Note syntax differences, TRUE means training set, FALSE means test set

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Feature Scaling - Everything except the target/outcome variable

training_set[,-11] <- scale(training_set[,-11])
test_set[,-11] <- scale(test_set[,-11])



# Importing the deep learning library: h20 (open-source software platform, allows you to connect to an instance in a computer system, makes this quick like GPU package in tensorflow for paralell computations)
# Lots of options and contains a parameter tuning argument

#install.packages("h2o")
library(h2o)

# Initialize an h20 instance

h2o.init(nthreads = -1 ) # -1 means all the available cores, nthreads is the number of cores to run these computations (gpus have more cores)
# Fitting Classifier to Training set

classifier <- h2o.deeplearning(y = 'Exited',
                               training_frame = as.h2o(training_set),
                               activation = 'Rectifier',
                               hidden = c(6,6),
                               epochs = 100,
                               train_samples_per_iteration = -2) # This is not a complex data set, like images with pixels. We don't need many hiden layers and numbers of neurons

# This auto tunes parameters
# train_samples_per_iteration = batch size. How many samples used before updating the weights
# Predicting Test Set Results

prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set[-11])) 


y_pred <- ifelse(prob_pred > 0.5, 1, 0) # It is not always 50%, like in medicine where we predict if a tumor is malignant, will be closer to something like 80%


#### OOOORRRRR

y_pred2 <- (prob_pred > 0.5)  # This returns Boolean

y_pred2 <- as.vector(y_pred2)
# Make confusion matrix


Out[41]: (0.949, 0.953)Out[41]: (0.949, 0.953)


cm <- table(test_set[,11], y_pred2)



accuracy = (cm[1,1] + cm[2,2]) / sum(cm)
accuracy 
# Pretty good without parameter tuning like Kfold cross validation

# disconnect fro h2o session

h2o.shutdown()
Yes
