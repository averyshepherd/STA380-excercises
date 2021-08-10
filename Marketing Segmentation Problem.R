rm(list=ls())
library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(tidyverse)

marketing = read.csv('social_marketing.csv', header=TRUE)

summary(marketing)
library(foreach)
# Center and scale the data
X = marketing[,-c(1,2,6,36,37)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

marketing_new = scale(marketing[,-c(1)]) # cluster on measurables
k_grid = seq(2, 20, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(marketing_new, k, nstart=50)
  cluster_k$tot.withinss
}

#Elbow plot
fig.width = 9
fig.height = 5
fig.align='center'
plot(k_grid, SSE_grid)

## Elbow looks like it is at k=7
clust1 = kmeans(X, 7, nstart=25)
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[3,]*sigma + mu
clust1$center[4,]*sigma + mu
clust1$center[5,]*sigma + mu
clust1$center[6,]*sigma + mu
clust1$center[7,]*sigma + mu

# # Which users are in which clusters?
# which(clust1$cluster == 1)
# which(clust1$cluster == 2)
# which(clust1$cluster == 3)
# which(clust1$cluster == 4)
# which(clust1$cluster == 5)
# which(clust1$cluster == 6)
# which(clust1$cluster == 7)


# A few plots with cluster membership shown
# qplot is in the ggplot2 library

# Cluster 1 has a a high center for cooking and photo_sharing
# these users can be segmented into a users that look online recipes group
qplot(cooking, photo_sharing, data=marketing, 
      color=factor(clust1$cluster))

# Cluster 2 has a high center for politics and travel
# these users can be segmented into an international politics group
qplot(politics,travel, data=marketing, color=factor(clust1$cluster))

# Cluster 3 has a high center for photo-sharing and current events
# these users can be segmented into a social media news reader group
qplot(shopping,photo_sharing, data=marketing, 
      color=factor(clust1$cluster))

# Cluster 4 has a high center for art and tv_film
# these users can be segmented into a fine arts group
qplot(art,tv_film, data=marketing, 
      color=factor(clust1$cluster))

# Cluster 5 has a high center for personal_fitness and health_nutrition
# these users can be segmented into a healthy living user group
qplot(personal_fitness, health_nutrition, data=marketing, 
      color=factor(clust1$cluster))

# Cluster 6 has a high center for religion and sports_fandom
# ???
qplot(religion, sports_fandom, data=marketing, 
      color=factor(clust1$cluster))

# Cluster 7 has a high center for online_gaming and college_uni
# these users can be segementd into a college student group
qplot(online_gaming, college_uni, data=marketing, 
      color=factor(clust1$cluster))

center<-sort(cluster1,decreasing = TRUE)
center
##### THE END ####



#CH Index

N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = kmeans(X, k, nstart=50)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}
plot(k_grid,CH_grid)

#best K is at k=1 or 2


rm(list=ls())

marketing = read.csv("social_marketing.csv", row.names=1)

# Center/scale the data
marketing_scaled= scale(marketing, center=TRUE, scale=TRUE) 

# Form a pairwise distance matrix using the dist function
marketing_distance_matrix = dist(marketing_scaled, method='euclidean')


# Now run hierarchical clustering
marketing_protein = hclust(marketing_distance_matrix, method='average')


# Plot the dendrogram
plot(marketing_protein, cex=0.8)

# Cut the tree into 5 clusters
cluster1 = cutree(marketing_protein, k=5)
summary(factor(cluster1))

# Examine the cluster members
which(cluster1 == 1)
which(cluster1 == 2)
which(cluster1 == 3)


