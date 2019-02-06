# KMeans Clustering 

dataset <- read.csv("Mall_Customers.csv", header = T)

x <- dataset[,4:5]   # Just need two columns

# Using the elbow method to find the optimal number of clusters

set.seed(6)

wcss <- vector() # Initialize empty vector
for (i in 1:10) wcss[i] <- sum(kmeans(x,i)$withinss) # Lower bound and upward bound are included in R
plot(1:10, wcss, type = "b", main = paste('Clusters of clients'), xlab = "Number of clusters", ylab = "WCSS")

# Applying k-means to the mall dataset

set.seed(29)
kmeans <- kmeans(x, 5, iter.max = 300, nstart = 10)

install.packages("cluster")
library(cluster)


clusplot(x,
         kmeans$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste('Clusters of Clients'),
         xlab = "Annual Income",
         ylab = "Spending Score")

# Remember this is only for two dimensional models

# Ctrl L clears everything

