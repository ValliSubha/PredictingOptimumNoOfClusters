iris_ds= read.csv("Iris.csv")

str(iris_ds)

summary(iris_ds) 

#(without normalization)

library(ggplot2)

tot.withinss = NULL
for (i in 1:10){
  iris_cluster = kmeans(iris_ds[,2:5], center=i, nstart=20)
  tot.withinss[i] = iris_cluster$tot.withinss
}

plot(x=1:10, y=tot.withinss, type="b", pch=19, col= "red",
     xlab = "Number of clusters(k)", 
     ylab = "Within Cluster Sum of Squares")

library(cluster)
iris_cluster = kmeans(iris_ds[,2:5], center=3, nstart=20)
clusplot(x=iris_ds, clus= iris_cluster$cluster,
         color=T, shade=T, labels=0, lines=0) 

table(iris_cluster$cluster, iris_ds$Species)

ggplot(iris_ds,aes(x = SepalLengthCm, y = SepalWidthCm, 
                   col= as.factor(iris_cluster$cluster),size=10))+ 
  geom_point() + scale_color_discrete()



############################################



iris_ds= read.csv("Iris.csv")

str(iris_ds)

summary(iris_ds)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris_ds$SepalLengthCm = normalize(iris_ds$SepalLengthCm)
iris_ds$SepalWidthCm = normalize(iris_ds$SepalWidthCm)
iris_ds$PetalLengthCm = normalize(iris_ds$PetalLengthCm)
iris_ds$PetalWidthCm = normalize(iris_ds$PetalWidthCm)

library(ggplot2)

tot.withinss = NULL
for (i in 1:10){
  iris_cluster = kmeans(iris_ds[,2:5], center=i, nstart=20)
  tot.withinss[i] = iris_cluster$tot.withinss
}

plot(x=1:10, y=tot.withinss, type="b", pch=19, col= "red",
     xlab = "Number of clusters(k)", 
     ylab = "Within Cluster Sum of Squares")

library(cluster)
iris_cluster = kmeans(iris_ds[,2:5], center=3, nstart=20)
clusplot(x=iris_ds, clus= iris_cluster$cluster,
         color=T, shade=T, labels=0, lines=0) 

table(iris_cluster$cluster, iris_ds$Species)

ggplot(iris_ds,aes(x = SepalLengthCm, y = SepalWidthCm, 
                   col= as.factor(iris_cluster$cluster),size=10))+ 
  geom_point() + scale_color_discrete()


