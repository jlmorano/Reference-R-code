# Assignment #5

# use stream reach properties to determine whether brook trout are likely to reside within a given stream reach

data <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/data/Maryland.Trout.csv", header = TRUE)

# a) Calculate a distance matrix between scale reach properties (X.Ag, LOG_RD,TEMP_FLD,RIFQUAL,DO_FLD)

# function to standardize
scale2 <- function(x) {
  x <- (x- mean(x))/sd(x)
  return(x)
} 

# Standardize each reach property in the dataset
# MARGIN = 2 applies the function scale2 to each column
scale.covariates <- apply(data[,3:7], MARGIN = 2, FUN=scale2)
# head(scale.covariates)

d <- dist(scale.covariates) # will be a lower triangular matrix of distances between each of 7 variables


# b) hclust() function, generate 4 different dendrograms based on the distance matrix, using four different methods for clustering groups with more than one member (“centroid”, "single linkage”, “complete linkage”, and “average”).
h.centroid <- hclust(d, "centroid")
h.single <- hclust(d, "single")
h.complete <- hclust(d, "complete")
h.average <- hclust(d, "average")

par(mfrow = c(2,2))
plot(h.centroid)
plot(h.single)
plot(h.complete)
plot(h.average)
par(mfrow = c(1,1))


# c) INTERPRET


# d) Using the complete-linkage dendrogram and the cutree function(), cut the tree to allocate different observations to two different clusters, #row id
cluster <- cutree(h.complete, k = 2) #row id
obs <- data.frame(cbind(cluster, scale.covariates, data$bkt))

# rename
library(dplyr)
obs <- obs %>%
  rename(bkt = V7)


# e) mean across 2 clusters
cluster1 <- obs %>%
  filter(cluster == 1)

cluster2 <- obs %>%
  filter(cluster == 2)

#mean of variables for both clusters
cluster1.mean <- apply(cluster1[,2:6], MARGIN = 2, FUN = mean)
cluster2.mean <- apply(cluster2[,2:6], MARGIN = 2, FUN = mean)
mean.cluster <- rbind(cluster1.mean, cluster2.mean)

# print mean of clusters
print(mean.cluster)

# 2x2 contigency table; how trout presence/absence is distributed (for each cluster, how many have trout, how many don't)
# total 1, 0 (in bkt) for each cluster
table(cluster = obs$cluster, trout = obs$bkt)

### Interpret this.


# 2. K-Means Clustering to Explore Partitioning
# f) run kmeans for groups of k=1:10
kmeans.list <- vector(mode = "list", length = 10)
for (i in 1:10) {
  kmeans.list[[i]] <- kmeans(scale.covariates, centers= i, nstart = 10)
}  

  
# g) Plot the variance explained by the clusters against the number of clusters for each of the different values of K tested.
evar <- c()
for (i in 1:10) {  
  evar[i] <- kmeans.list[[i]]$betweenss/kmeans.list[[i]]$totss
}
clustevar <- cbind(rep(1:10), evar)
plot(clustevar[,1], clustevar[,2], xlab="clusters", ylab="variance explained")


# h) show how the mean characteristics of reaches differ across the two clusters. Also similar to (e) above, present a table that shows how trout presence/absence is distributed across the clusters. What can you conclude from these results, and how do they compare to the results using the hierarchical clustering approach?
kmeans2 <- data.frame(cbind(kmeans.list[[2]]$cluster, scale.covariates, data$bkt))
kmeans2 <- kmeans2 %>%
  rename(cluster = V1,
         bkt = V7)
kmeans2.clst1 <- kmeans2 %>%
  filter(cluster == 1)
kmeans2.clst2 <- kmeans2 %>%
  filter(cluster == 2)

#mean of variables for both clusters
kmeans2.clst1.mean <- apply(kmeans2.clst1[,2:6], MARGIN = 2, FUN = mean)
kmeans2.clst2.mean <- apply(kmeans2.clst2[,2:6], MARGIN = 2, FUN = mean)
kmeans2.means <- rbind(kmeans2.clst1.mean, kmeans2.clst2.mean)

# print mean of clusters
print(kmeans2.means)

# contingency table
table(cluster = kmeans2$cluster, trout = kmeans2$bkt)


# 3. Discrimination and Classification

# i) First, split the original dataset into a training and testing dataset. Let the training dataset be composed of the first 60 observations, and the testing data be composed of the remaining 24 observations.

# Take scaled covariates, but need also presence/absence of brook trout
trout.scaled <- apply(data[,3:7], MARGIN = 2, FUN=scale2)
trout.scaled <- cbind(trout.scaled, data$bkt)

# Training data, first 60 obs
training <- trout.scaled[1:60,]
# Testing data, last 24 obs
testing <- trout.scaled[61:84,]


# j) Now, split the training data into two groups, one with and one without the presence of trout.
training <- data.frame(training)
training0 <- training[training$V6 == 0,]
training0 <- as.matrix(training0[,1:5])
training1 <- training[training$V6 == 1,]
training1 <- as.matrix(training1[,1:5])

# First, find the number of observations associated with each group.
n0 <- nrow(training0)
n1 <- nrow(training1)

# Second, find the pooled covariance matrix across the two groups. Spool = within group var-cov
# ...by first getting var-cov matrix for each group
cov0 <- cov(training0) #could add use="pairwise.complete.obs" but isn't neccessary here
cov1 <- cov(training1)

# ...then calculating the Spool (pooled variance)
Spool <- ( (n0-1) / (n0+n1-2) ) * cov0 + ( (n1-1) / (n0+n1-2) ) * cov1 

# Third, calculate the discrimination function (alpha).
# ...get the mean of each group
mean0 <- apply(training0, MARGIN = 2, FUN=mean)
mean1 <- apply(training1, MARGIN = 2, FUN=mean)

# ...then, alpha = (Spool)^-1 * (mean group 1 - mean group 2)
alpha <- solve(Spool) %*% ( mean0 - mean1 )


# k) Finally, classify observations in testing based on alpha

# ...Find sigma for each group (presence or absence) = t(alpha) %*% mean of training data
sigma.0 <- t(alpha) %*% mean0
sigma.1 <- t(alpha) %*% mean1
#  This is what will be compared to sigma.star for each obs

# ...Then find sigma* for each obs in testing data...
testing.matrix <- as.matrix(testing[,1:5])

predict <- c()
# i <-2
for (i in 1:nrow(testing.matrix)) {
  sigma.star <- t(alpha) %*% testing.matrix[i,]
  # ...then compare sigma* to sigma.0 and sigma.1
  dis1 <- abs(sigma.star - sigma.0)
  dis2 <- abs(sigma.star - sigma.1)
  
  # Compare dis1 and dis2,
  # if dis1 < dis2, that means dis1 is more similar to sigma (or the "score" of the variables) and therefore sigma.star is absence of trout
  if (dis1 < dis2) {
  predict[i] <- 0
  } else {
    predict[i] <- 1
    }

}

testing.predict <- cbind(testing, predict)

# Table of 
# 1: No trout present, and you estimate no trout present
# 2: No trout present, and but you estimate trout present
# 3: Trout present, but you estimate no trout present
# 4: Trout present, and you estimate trout present

table(true.trout = testing.predict[,6], predict.trout = testing.predict[,7])

