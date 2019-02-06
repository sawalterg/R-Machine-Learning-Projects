# Natural Language Processing - analyzing texts using Machine Learning


## Importing dataset

# We will use a tsv because there are many commas within the reviews themselves 


dataset <- read.delim("Restaurant_Reviews.tsv", header = T, sep = "\t", quote = '', stringsAsFactors = F) # This quote parameter means the commas are taken as nothing
# Must not view each review as a single entity (should be a character)

## Clean the text/simplify the corpus

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC) # Necessary for stopwords

corpus <- VCorpus(VectorSource(dataset$Review))       # Create a vcorpus to clean the text
corpus <- tm_map(corpus, content_transformer(tolower))# Make all the text lower case
corpus <- tm_map(corpus, removeNumbers)# Remove all numbers
corpus <- tm_map(corpus, removePunctuation) # Remove all punctuation
corpus <- tm_map(corpus, removeWords, stopwords())# Remove all stopwords (the, this, is, etc)
corpus <- tm_map(corpus, stemDocument)   # Stemming to get only the root of the word (words essentially mean the same thing minus syntax)
corpus <- tm_map(corpus, stripWhitespace)# Remove the extra spaces created from cleaning the dataset
  

as.character(corpus[[1]]) # This is how to review just one line
as.character(corpus[[841]]) # This is how to review just one line


## Create bag of words model 



# Create the sparse matrix

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)  # Remove non-frequent words, we must take a high proportion

dataset <- as.data.frame(as.matrix(dtm))  # Since we need a dataframe, lets do this
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


