library(stats)
library(MASS)
library(mclust)
library(HDclassif)

# loading the data and getting the true values 
load('mnist_task2.Rdata')
true_labels <- as.numeric(as.factor(target))
data= scale(data,center=TRUE, scale=FALSE)


#Hierarchical clustering on squared Euclidean distances using the method of Ward
dist_data <- dist(data, method = "euclidean",diag = TRUE, upper = TRUE)
hier_clust <- hclust(dist_data,"ward.D2")

#Cutting the tree to 4 clusters
clust_hier_4 <- cutree(hier_clust, k = 4)
table(clust_hier_4)


#K-means clustering 
kmeans_result <-kmeans(data,4,nstart=500,iter.max=2000)
varciance_k<-kmeans_result$betweenss/kmeans_result$totss
varciance_k

#HDDC clustering AkjBkQkD with hierarchical clustering 
hddc_AkjBkQkD_hier <- hddc(data, K = 4, model = "AkjBkQkD",d_select = "Cattell" ,init.vector = clust_hier_4, 
                           threshold = 0.05)

#HDDC clustering AkjBkQkD with kmeans
hddc_AkjBkQkD_means <- hddc(data, K = 4, model = "AkjBkQkD",d_select = "Cattell" ,init.vector = kmeans_result, 
                            threshold = 0.05)

#HDDC clustering AkjBQkD with hierarchical clustering 
hddc_AkjBQkD_hier<-hddc(data, K = 4, model = "AkjBQkD",d_select = "Cattell" ,init.vector = clust_hier_4, 
                        threshold = 0.05)

#HDDC clustering AkjBkQkD with kmeans
hddc_AkjBQkD_means<-hddc(data, K = 4, model = "AkjBQkD",d_select = "Cattell" ,init.vector = kmeans_result, 
                         threshold = 0.05)

#Calculating the ARI for each method
ari_hier <- adjustedRandIndex(true_labels, clust_hier_4)
ari_kmeans <- adjustedRandIndex(true_labels, kmeans_result$cluster)
ari_hddc_AkjBkQkD_hier <- adjustedRandIndex(true_labels, hddc_AkjBkQkD_hier$class)
ari_hddc_AkjBQkD_hier<- adjustedRandIndex(true_labels, hddc_AkjBQkD_hier$class)
ari_hddc_AkjBkQkD_k<- adjustedRandIndex(true_labels, hddc_AkjBkQkD_means$class)
ari_hddc_AkjBQkD_k<- adjustedRandIndex(true_labels, hddc_AkjBQkD_means$class)


#Creating a dataframe with the ARI values 
ari_values <- c(ari_hier, ari_kmeans, ari_hddc_AkjBkQkD_hier, ari_hddc_AkjBQkD_hier, ari_hddc_AkjBkQkD_k, ari_hddc_AkjBQkD_k)
method_names <- c('Hierarchical', 'K-means', 'HDDC AkjBkQkD Hierarchical', 'HDDC AkjBQkD Hierarchical', 'HDDC AkjBkQkD K-means', 'HDDC AkjBQkD K-means')
ari_df <- data.frame(Method = method_names, ARI = ari_values)
print(ari_df)

# Visualizing the higest ARI with 2 PCA on centerd data 
prcomp<- prcomp(data, center = TRUE, scale. = FALSE)
pc2_data <- prcomp$x[, 1:2]
plot(pc2_data, col = clust_hier_4, main = "Hierarchical clustering", xlab = "PC1", ylab = "PC2")
