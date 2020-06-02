library(dplyr)
library(ggplot2)
source("featurescaling.R")
diamondsscaled<-featurescaling(diamonds)
diamondsscaled<-select(diamondsscaled, -(x:z))
diamondsscaledmatrix<-as.matrix(select_if(diamondsscaled, is.numeric))

### Subseting 1000 randoms rows from the scaled dataset
diamondssubsample<-diamondsscaledmatrix[sample(nrow(diamondsscaledmatrix), 1000, replace = FALSE),]

### Heatmap
heatmap(diamondssubsample)

### Dendrogram
distances<-dist(diamondssubsample)
dendrogram<-as.dendrogram(hclust(distances, method = "average"))
plot(dendrogram)
library(rafalib)
myplclust(hclust(distances, method = "complete"), lab.col = 2) ## replace lab.col=2 by factor of outcome of interest to get the bottom of the dendrogram colored

### K-means with 2 centroids, 1000 starts and max 1000 iteration per start
str(kmeans(diamondssubsample, centers = 2, iter.max = 1000, nstart = 1000))

### K-means distortion function over a defined number of clusters
distorionoverncluster<-function(data, n_cluster, iter_max = 100, n_start = 100){
        x<-rep(-1, n_cluster)
        for (i in 1:n_cluster){
                kmeanobject<-kmeans(data, centers = i, iter.max = iter_max, nstart = n_start)
                x[i]<-kmeanobject$tot.withinss
        }
        print(x)
}

### Elbow method : Plotting the distortion function over number of clusters
distorionoverncluster(diamondssubsample, 10)
plot(distorionoverncluster(diamondssubsample, 10), type="l",xlab = "Number of Clusters", ylab="Distortion Function")

