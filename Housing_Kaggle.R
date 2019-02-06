# Import the datasets

train <- read.csv("train (1).csv", header = T, stringsAsFactors = F)
test <- read.csv("test (1).csv", header = T, stringsAsFactors = F)

# Dealing with Missing Values

naTotal <- sapply(candidateFeatures, function(x){sum(is.na(train[[x]]))})

naTotal <- sort(naTotal, decreasing = T)


# Taking Care of the NAs in order of missing data

train$PoolQC <- ifelse(is.na(train$PoolQC), "None", train$PoolQC)

train$MiscFeature <- ifelse(is.na(train$MiscFeature), "None", train$MiscFeature)

train$Fence <- ifelse(is.na(train$Fence), "No Fence", train$Fence)

train$Alley <- ifelse(is.na(train$Alley), "NoAlley", train$Alley)

train$FireplaceQu <- ifelse(is.na(train$FireplaceQu), "NoFire", train$FireplaceQu)

str(train$LotFrontage)

ggplot(train) + geom_bar(aes(x = as.factor(LotFrontage), fill = LotConfig))

train$LotFrontage <- ifelse(is.na(train$LotFrontage), 0, train$LotFrontage)

train$GarageType <- ifelse(is.na(train$GarageType), "None", train$GarageType)
train$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt), "None", train$GarageYrBlt)
train$GarageCond <- ifelse(is.na(train$GarageCond), "None", train$GarageCond)
train$GarageQual <- ifelse(is.na(train$GarageQual), "None", train$GarageQual)
train$GarageFinish <- ifelse(is.na(train$GarageFinish), "None", train$GarageFinish)

# Basement replace - Na represents no basement

train$BsmtExposure <- ifelse(is.na(train$BsmtExposure), "NoBas", train$BsmtExposure)
train$BsmtFinType2 <- ifelse(is.na(train$BsmtFinType2), "NoBas", train$BsmtFinType2)
train$BsmtQual <- ifelse(is.na(train$BsmtQual), "NoBas", train$BsmtQual)
train$BsmtCond <- ifelse(is.na(train$BsmtCond), "NoBas", train$BsmtCond)

train$BsmtFinType1 <- ifelse(is.na(train$BsmtFinType1), "NoBas", train$BsmtFinType1)

noBasement <- subset(train, train$BsmtUnfSF == 0 & train$TotalBsmtSF == 0)

garageVect <- subset(train, is.na(GarageType))



#### FEATURE SELECTION - BORUTA (RF WRAPPER)





# Import libraries


library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)

# Data Preparation

idVar <- "Id"
targetVar <- "SalePrice"

candidateFeatures <- setdiff(colnames(train), c(idVar, targetVar))


dataType <- sapply(candidateFeatures, function(x) {class(train[[x]])})

# [[]] refers to the column


explanatoryAttributes <- setdiff(names(train), c(idVar, targetVar))
dataClasses <- sapply(explanatoryAttributes, function(x) {class(train[[x]])})

# sapply takes the form sapply(x, function (of x))


uniqueClasses <- unique(dataClasses)



attrDataType <- lapply(uniqueClasses, function(x) {names(dataClasses[dataClasses == x])}) # This creates two lists which contain all numeric and character variables


names(attrDataType) <- uniqueClasses



# Create Actual Frames for Boruta Analysis

response <- train$SalePrice

trainBor <- train[candidateFeatures]

# For numeric set missing values to -1 for purpose of the random forest run


for (x in attrDataType$integer) {
  train[[x]][is.na(trainBor[[x]])] <- -1
}

for (x in attrDataType$character) {
  trainBor[[x]][is.na(trainBor[[x]])] <- "*MISSING*"
}





# Run Boruta Analysis

set.seed(13)
bor.results <- Boruta(trainBor, response,
                      maxRuns =  101,
                      doTrace = 0)


plot(bor.results)







### MODEL CREATION

# Create a vector for confirmed attributes, tentative attributes and rejected

confirmedAttributes <- getSelectedAttributes(bor.results, withTentative = F)

tempAttr <- getSelectedAttributes(bor.results, withTentative = T)


tentAttributes <- setdiff(tempAttr, confirmedAttributes)

rejectedAttr <- setdiff(explanatoryAttributes, tempAttr)










# Cleaning up missing values


for (x in attrDataType$integer) {
  train[[x]][is.na(train[[x]])] <- -1
}

for (x in attrDataType$character) {
  train[[x]][is.na(train[[x]])] <- "*MISSING*"
}










# Convert all character variables to factors


train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                       as.factor)


# Always remember to classify missing variables as being either missing completely at random (faulty sensor)
# or Missing Not A Random. Serious issue. Dive deeper and determine if you can extrapolate why something is missing

install.packages("randomForest")
library(randomForest)


# Check the na values

train$GarageYrBlt <- as.numeric(train$GarageYrBlt)
str(train[sapply(train, is.factor)])


# Random Forest requires dataframes as input


set.seed(123)
rf.regressor <- randomForest(x = train[confirmedAttributes],
                             y = train$SalePrice,
                             importance = T,
                             ntree = 100)


y_pred <- predict(rf.regressor, test)

test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], 
                                             as.factor)




# Preprocessing the Test data set (this is why its best to do the preprocessing in one data frame)


test$PoolQC <- ifelse(is.na(test$PoolQC), "None", test$PoolQC)

test$MiscFeature <- ifelse(is.na(test$MiscFeature), "None", test$MiscFeature)

test$Fence <- ifelse(is.na(test$Fence), "No Fence", test$Fence)

test$Alley <- ifelse(is.na(test$Alley), "NoAlley", test$Alley)

test$FireplaceQu <- ifelse(is.na(test$FireplaceQu), "NoFire", test$FireplaceQu)

str(test$LotFrontage)

ggplot(test) + geom_bar(aes(x = as.factor(LotFrontage), fill = LotConfig))

test$LotFrontage <- ifelse(is.na(test$LotFrontage), 0, test$LotFrontage)

test$GarageType <- ifelse(is.na(test$GarageType), "None", test$GarageType)
test$GarageYrBlt <- ifelse(is.na(test$GarageYrBlt), "None", test$GarageYrBlt)
test$GarageCond <- ifelse(is.na(test$GarageCond), "None", test$GarageCond)
test$GarageQual <- ifelse(is.na(test$GarageQual), "None", test$GarageQual)
test$GarageFinish <- ifelse(is.na(test$GarageFinish), "None", test$GarageFinish)

# Basement replace - Na represents no basement

test$BsmtExposure <- ifelse(is.na(test$BsmtExposure), "NoBas", test$BsmtExposure)
test$BsmtFinType2 <- ifelse(is.na(test$BsmtFinType2), "NoBas", test$BsmtFinType2)
test$BsmtQual <- ifelse(is.na(test$BsmtQual), "NoBas", test$BsmtQual)
test$BsmtCond <- ifelse(is.na(test$BsmtCond), "NoBas", test$BsmtCond)

test$BsmtFinType1 <- ifelse(is.na(test$BsmtFinType1), "NoBas", test$BsmtFinType1)

test$GarageYrBlt <- as.numeric(test$GarageYrBlt)


  
  
  

for (x in attrDataType$integer) {
  test[[x]][is.na(test[[x]])] <- -1
}

for (x in attrDataType$character) {
  test[[x]][is.na(test[[x]])] <- "*MISSING*"
}


test[sapply(test, is.factor)] <- lapply(test[sapply(test, is.factor)], as.character)

test[test == "*MISSING*"] <- NA

test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], as.factor)

# predict the results





# We have a factor level issue. Lets cross check the two



testConf <- test[confirmedAttributes]
testConf <- testConf[sapply(testConf, is.factor)]
levelTest <- sapply(testConf, levels)

trainConf <- train[confirmedAttributes]
trainConf <-trainConf[sapply(trainConf, is.factor)]

levelTrain <- sapply(trainConf, levels)


yPred <- predict(rf.regressor,
               newdata = test[confirmedAttributes],
               r)

test <- test(lapply(test, function(x) {
  +                  gsub("*MISSING*", NA, x)}))
naTotalTest <- sapply(candidateFeatures, function(x){sum(is.na(test[[x]]))})
naTotalTest <- sort(naTotalTest)
