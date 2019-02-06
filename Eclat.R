# Eclat - useful for quick model

dataset <- read.csv("Market_Basket_Optimisation.csv", header = F)

dataset <- read.transactions('Market_Basket_Optimisation', sep = ',', rm.duplicates = T)
library(arules)

summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Eclat on the dataset

set <- eclat(data = dataset, parameter = list(support = 0.003, minlen = 2))


# Visualizing the results

inspect(sort(set, by = 'support')[1:10])
