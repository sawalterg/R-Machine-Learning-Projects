# Apriori 

# Data Preprocessing

dataset = read.csv("Market_Basket_Optimisation.csv", header = F)


# This is a sparse matrix (most columns contain o)

# We are going to assign a column to each product (120 products). Lines (rows) are still going to be each of purchases (1 if purchased, 0 if not)

install.packages('Rcmdr')
library(RcmdrMisc)

dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T) # The 1 and 5 means there are five duplicates (if the first number is 2, its triplicates)

# We also remove the duplicates from the transactions 


summary(dataset)

itemFrequencyPlot(dataset, topN = 100)


# Training Apriori on the dataset

rules = apriori(dataset, parameter = list(support = 0.004 , confidence = 0.2 )) # Set up minimum support (how often the products are bought/frequency (# products bought/total number of transactions)) and confidence


# Visualizing the results

# Lift is the best metric to sort

inspect(sort(rules, by = 'lift')[1:10])    # Notice that some of these rules have high confidence, some products are bought oftem regardless. The confidence is likely too high