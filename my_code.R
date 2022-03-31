###############
### Cluster analysis in R
###############


###############
### Mammals milk compositions
###############


###############
### Descriptive Statistics
###############


load("/cloud/project/MammalsMilk.Rdata")
# Visual inspection by constituents

suppressPackageStartupMessages(library(reshape)) # Useful to re-arrange data set
milk.long <- melt(milk,id.vars=c("Name")) # from wide to long format
names(milk.long)[1:3] <- c("Mammal","Constituent","Value")

# Summary statistics
summary(milk[,2:5])
apply(milk[,2:5],2,sd) # Standard deviations

### To standardise or not to standardise?

###############
### Hierarchical clustering analysis
###############


milk2 <- scale(milk[,2:5])
d <- dist(milk2, method = "euclidean") # Distance matrix: Euclidean on standarised variables
hc <- hclust(d, method="ward.D") # Ward's clustering method
plot(hc,labels=milk$Name) # dendrogram

### 4-cluster Solution
plot(hc,labels=milk$Name)
rect.hclust(hc, k=4, border="red") # Show them on the dendrogram

###############
### Cluster Validation
###############


install.packages("fpc")
library(fpc)

### CH Index
clusval3 <- cluster.stats(d,ward3) # Validation stats for a 3-cluster solution
clusval3$ch # CH index for a 3-cluster solution

ward4 <- cutree(hc, k=4) # Extract a 4-cluster solution
clusval4 <- cluster.stats(d,ward4) # Validation stats for a 4-cluster solution
clusval4$ch # CH index for a 4-cluster solution

### Average Silhouette Width (ASW)
clusval3$avg.silwidth # Average silhouette width for a 3-cluster solution
clusval4$avg.silwidth # Average silhouette width for a 4-cluster solution

###############
### Cluster Characteristics
###############


### Cluster centroids (standarised data)
hc4 <- cutree(hc, k=4) # Cluster assignments
st <- by(milk2,hc4,colMeans) # Mean vector by cluster
st <- matrix(unlist(st), nrow = 4)
colnames(st) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")
rownames(st) <- levels(milk.long$Constituent)
st <- as.data.frame(st)
print(st)

### Scatterplot matrix using the groups
milkdat <- cbind(hc4,milk2) # Append grouping information
milkdat <- as.data.frame(milkdat)
library(lattice) # This package simplifies using group colours
a <- splom(~milkdat[c(2:5)], groups = hc4, data = milkdat,
           par.settings = simpleTheme(col = 1:4, pch = 1:4),
           auto.key = list(columns = 4, title = "Cluster"))
print(a)

### Distances to the final cluster centroids (within-group distances)
d1 <- apply(t(milk2[hc4==1,]),2,function(x) dist(rbind(x,st[,1]))) # Cluster 1
d2 <- apply(t(milk2[hc4==2,]),2,function(x) dist(rbind(x,st[,2]))) # Cluster 2
d3 <- apply(t(milk2[hc4==3,]),2,function(x) dist(rbind(x,st[,3]))) # Cluster 3
d4 <- apply(t(milk2[hc4==4,]),2,function(x) dist(rbind(x,st[,4]))) # Cluster 4

c1 <- rep(1:4,times=c(length(d1),length(d2),length(d3), length(d4)))
c2 <- c(as.character(milk$Name[hc4==1]), as.character(milk$Name[hc4==2]),
        as.character(milk$Name[hc4==3]), as.character(milk$Name[hc4==4]))

distances <- data.frame(Cluster=c1,Mammal=c2,Dist=c(d1,d2,d3,d4))

### Plot distances to the cluster centroids
plot(distances$Cluster,distances$Dist,axes=F,ylab="Distance to cluster centroid",
     xlab="Cluster",col="white",xlim=c(0.5,4), ylim=c(0,2))
text(distances$Cluster,distances$Dist,labels=distances$Mammal)
axis(1,at=1:4,labels=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")); axis(2)
