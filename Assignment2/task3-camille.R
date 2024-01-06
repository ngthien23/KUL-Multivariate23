library(plfm)
library(psych)
library(ca)

#a)

# anger data is a 3D array
# Aggregate across situations (the second dimension is for situations)
person_behavior_aggregated <- apply(anger$data, MARGIN =3, FUN = rowSums)

# Check the dimensions of the aggregated matrix
dim(person_behavior_aggregated)
# We have 101 persons and 8 behaviors, the result is a 101 x 8 matrix

# compute squared Euclidean distances
EuclideanDistance <- dist(person_behavior_aggregated, method = "euclidean", 
                         diag = TRUE, upper = TRUE)
EuclideanDistance
#SquaredEuclideanDistance <- EuclideanDistance^2
#SquaredEuclideanDistance 

# hierarchical clustering Ward bimodal data 
# cluster on squared Euclidean distance 
hiclust_ward<- hclust(EuclideanDistance, "ward.D2") 
par(pty="s") 
plot(hiclust_ward,hang=-1) 
#Ward’s method fails to capture the difference in the true modality of the two samples. 

#Save the cluster membership variable of the 2-cluster solution
clusters <- cutree(hiclust_ward, k = 2)
clusters
nclust <- 2

#centroid
stat<-describeBy(person_behavior_aggregated, clusters, mat=TRUE)
stat
hcenter <- matrix(stat[,5],nrow=nclust)
hcenter
rownames(hcenter) <- paste("c_",rep(1:nclust),sep="")
colnames(hcenter) <- c(colnames(anger$freq2))
round(hcenter,2)

#b.
anger$freq1
dim(anger$freq1)

# Compute profile vectors for each cluster
# we are looking for a 2*8 matrix where 2 unique clusters with 8 different behaviours with cells as frequency count
# Combine the aggregated matrix and cluster assignments
data_with_clusters <- data.frame(person_behavior_aggregated, cluster = clusters)
profile_vectors <- aggregate(. ~ cluster, data = data_with_clusters, sum)
profile_vectors <- profile_vectors[, -1]
dim(profile_vectors)
profile_vectors

# Define the new column names
new_column_names <- c(
  "fly off the handle",
  "quarrel",
  "leave",
  "avoid",
  "pour out one's hart",
  "tell one's story",
  "make up",
  "clear up the matter"
)

# Assign the new column names to 'profile_vectors'
colnames(profile_vectors) <- new_column_names

final_freq1 <- rbind(anger$freq1, profile_vectors)
final_freq1



#c

#H0: bahabior and situations are statistically independent
#if the Pearson-Chi square test indicates that Xand Y are statistically
#dependent, it is meaningful to use CA to further study the nature of the
#relation between Xand Y.

chisq.test(final_freq1)
#p-value is small enough to reject null
ca.out <- ca(final_freq1)
#slide 19 of ch10

summary(ca.out)
#slide 20
par(pty="s", cex=0.9)
plot(
  ca.out,
  mass = TRUE,
  arrows = c(TRUE, FALSE),
)
