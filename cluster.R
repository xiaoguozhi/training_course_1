########################################################
# Description:
# 1.for the course "clustering and  classification"
# 2.Section: 1
# 3.Author: Zhifeng Guo
# 4.Date: 31 10, 2017.
########################################################

#install.packages("factoextra")
library(factoextra)
# Loading the data set
data("USArrests") 
# Scaling the data
df <- scale(USArrests) 

# View the firt 3 rows of the data
head(df, n = 5)


fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)

aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster
head(km.res$cluster, 4)

# Cluster size
km.res$size


fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



#############################################################################

#k-medoids algorithm
#The most common k-medoids clustering methods is the PAM algorithm ( Partitioning Around Medoids, Kaufman & Rousseeuw, 1990).

library(cluster)
library(factoextra)
 # Load the data set
data("USArrests")
# Scale the data
df <- scale(USArrests) 
 # View the firt 3 rows of the data
head(df, n = 3)

fviz_nbclust(df, pam, method = "silhouette")+ theme_classic()

pam.res <- pam(df, 2)
print(pam.res)

dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)

# Cluster medoids: New Mexico, Nebraska
pam.res$medoids

# Cluster numbers
head(pam.res$clustering)

fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)




##############################################################################
#Hierarchical clustering [or hierarchical cluster analysis ( HCA )

#Agglomerative Clustering

# Load the data
data("USArrests")
# Standardize the data
df <- scale(USArrests)
# Show the first 6 rows
head(df, nrow = 6)

# Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]


res.hc <- hclust(d = res.dist, method = "ward.D2")
# cex: label size
library("factoextra")
fviz_dend(res.hc, cex = 0.5)


# Compute cophentic distance
res.coph <- cophenetic(res.hc)
# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)

res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)
## Alabama Alaska Arizona Arkansas
## 1 2 2 3
# Number of members in each cluster
table(grp)

rownames(df)[grp == 1]

# Cut in 4 groups and color by groups

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE# Add rectangle around groups,
          
)
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)


fviz_cluster(list(data = df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal()
             )


##################################################################################
#data visualizing
#install.packages(c("igraph", "dendextend"))
# Load data
data(USArrests)
# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

library(factoextra)


fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)


fviz_dend(hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)



fviz_dend(hc, cex = 0.5, k = 4, # Cut in four groups
          k_colors = "jco")


fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco", type = "circular")

require("igraph")
fviz_dend(hc, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)


####################################################################
#Density-Based Clustering DBSCAN

library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]
df0<-multishapes
df0$shape<-as.factor(df0$shape)
ggplot(df0,aes(x=x,y=y,colour=shape))+geom_point()
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

#install.packages("fpc")
#install.packages("dbscan")
#install.packages("factoextra")


library("fpc")
library("dbscan")
library("factoextra")
# Load the data
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]
# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)


fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())



################################################################################
#Model-Based Clustering
# Load the data
library("MASS")

# Scatter plot
library("ggpubr")
library(ggplot2)
data("geyser")
ggplot(geyser,aes(x=duration,y=waiting))+geom_point()

ggscatter(geyser, x = "duration", y = "waiting")+
  geom_density2d() # Add 2D density


#install.packages("mclust")


library(mclust)
library(factoextra)
data("diabetes")
head(diabetes, 3)
df <- scale(diabetes[, -1]) # Standardize the data
mc <- Mclust(df) 

set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())


fviz_mclust(mc, "classification", geom = "point",pointsize = 1.5, palette = "jco")


# Load the data
library("MASS")
data("geyser")

# Scatter plot
library("ggpubr")
ggscatter(geyser, x = "duration", y = "waiting")+geom_density2d() # Add 2D density

df<-geyser
set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

mc <- Mclust(df) 
fviz_mclust(mc,"classification", geom = "point",pointsize = 1.5, palette = "jco")
df$class<-as.factor(mc$classification)

ggplot(df,aes(x=waiting,y=duration,colour=class))+geom_point()

##############################################################################
#Spectral clustering
rm(list=ls())
library("kernlab")#SVM
library("ggplot2")#ploting
data(spirals)

df<-as.data.frame(spirals)

names(df)<-c("x1","x2")

ggplot(df,aes(x=x1,y=x2))+geom_point()

sc <- specc(spirals, centers=2)
df1<-df
df1$class<-as.factor(sc@.Data)

ggplot(df1,aes(x=x1,y=x2,colour=class))+geom_point()

set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())


############################################################################################
