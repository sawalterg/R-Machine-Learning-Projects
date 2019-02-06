# Hierarchical Clustering

# Import the dataset

dataset <- read.csv("Mall_Customers.csv", header = T)

x = dataset[,4:5]

# Using the dendrogram

dendrogram <- hclust(dist(x, method = 'euclidean'), method = "ward.D")      # Method tries to minimize the variance between clusters (variance as opposed to cluster)

plot(dendrogram, 
     main = paste("Dendrogram"),
     xlab = "Customers",
     ylab = "Euclidean Distanced")
# Optimal number of clusters is 5


# Fitting the Hierarchical clustering to the mall dataset

hc <- hclust(dist(x, method = 'euclidean'), method = "ward.D")
y_hc <- cutree(hc, k = 5) # We are cutting the tree, to make the clusters



# Visualize the clusters


clusplot(x,
         y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste('Clusters of Clients'),
         xlab = "Annual Income",
         ylab = "Spending Score")