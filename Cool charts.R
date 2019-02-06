load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()] # Load libraries that are not in installed.packages
for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)


train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE )
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

setDT(train)
setDT(test)

str(train)
str(test)


cat_var <- names(train[which(sapply(train, is.character))])
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', "KitchenAbvGr", 'BsmtFullBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]

dim(train)

str(train)

head(train)

# Missing Data

missing_values <- colSums(sapply(train, is.na)) # iterate the number of each nas in column and return sum
missing_values <- sort(missing_values, decreasing = TRUE)  # Sort by highest number of missing values
missing_values

## These are data table functions, this requires turning turning a data frame into a data table, # setDT

colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))

## Interesting Utility Function for Visualizing missing data
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[, order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill = factor(m))) + scale_fill_manual(values = c("white", "black"), 
  name = "Missing\n(0 = Yes, 1 = No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)}

plot_Missing(train[,colSums(is.na(train)) > 0, with = FALSE])

sum(train[, "YearRemodAdd", with = FALSE] != train[,'YearBuilt', with = FALSE])

cat('Percentage of houses remodeled', sum(train[, 'YearRemodAdd', with = FALSE] != train[, 'YearBuilt', with = FALSE])/dim(train)[1])


train %>% select(YearBuilt, YearRemodAdd) %>% mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>%ggplot(aes(x = factor(x = Remodeled, labels = c('No', 'Yes')))) + geom_bar() + xlab('Remodeled') + theme_light()



summary(train[,.SD, .SDcols = numeric_var])

cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns') # Note that the index numbers here are outside of the paranthesis. This is because we are looking at the overall dimensions
# which in this case means the overall numner of rows and the overall number of columns

cat('Test has', dim(test)[1], 'rows and', dim(test)[2], 'columns')

sum(is.na(test)) / (nrow(test) * ncol(test))
sum(is.na(train)) / (nrow(train) * ncol(train))


train %>% select()

cat("The number of duplicatd rows is", nrow(train) - nrow(unique(train)))

train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]



### Correlation Plot - The mother of all plots for feature selection

train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

train_cont <- data.frame(train_cont)

correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")
