#Load libraries
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#Import dataset
data<-read.csv('C://Users//annic//Desktop//MyProjects//Clustering_Mall//Mall_Customers.csv')
head(data)

#To remove any missing data
data<-na.omit(data)

#Keep only the numerical values with meaning to cluster
customer<-data[,3:5]

#Feature Scaling
scaled_data<-scale(customer)

#Randomly select K-means with 3 clusters
clust_cust<-kmeans(scaled_data,3,nstart=25)
clust_cust

#Find total withiness and betweenness of the clusters
clust_cust$totss
clust_cust$tot.withinss #total within-cluster sum of square #we want a small value
clust_cust$betweenss #we want a big value
clust_cust$tot.withinss+clust_cust$betweenss #same as $totss
clust_cust$withinss

sum(clust_cust$withinss) #same as $tot.withinss, gives the total  within sum of squared
class(clust_cust)

#To get the cluster number of each customer
clust_cust$cluster

#Draw the Tot_within_SS curve vs No. of Clusters ( Elbow curve)
kmax<-40 #suppose 40

tot_wss<-sapply(1:kmax,function(k){kmeans(scaled_data,k)$tot.withinss})
between_ss<-sapply(1:kmax,function(k){kmeans(scaled_data,k)$betweenss})
tot_ss<-sapply(1:kmax,function(k){kmeans(scaled_data,k)$totss})

#Plot the elbow curve (where the distance sharply becomes smaller)
plot(1:kmax,tot_wss,type='b',xlab='No. of Clusters',ylab='Total Within Sum of Squares')

#Plot an -inverse- elbow curve (where the distance sharply becomes bigger)
plot(1:kmax,between_ss,type = "b",xlab="No. of Clusters",ylab="Between Sum of Squares")

#Cluster Visualization (based on the k=3 parameter we gave above)
fviz_cluster(clust_cust,scaled_data)

#Cluster Visualization (the optimal) - Elbow method
fviz_nbclust(scaled_data,kmeans,method='wss',k.max=40) 
?fviz_nbclust
#Alternative -> Average Silhouette Method (better not use it and stick to the wss method above)
fviz_nbclust(scaled_data,kmeans,method='silhouette',k.max=40)

#Conclusion: k=5 looks more optimal, since the WSS difference from 4-5 is smaller comparing to the previous ones

#Compute k-means clustering with k=5
set.seed(123)
final<-kmeans(scaled_data,5,nstart = 25)
print(final)

#Visualization
fviz_cluster(final, data = scaled_data) #some overlapping is ok, no method is perfect
