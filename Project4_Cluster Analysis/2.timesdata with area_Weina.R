################################################Timesdata with area feature
times <- read.csv("C:/data/timesData.csv")
summary(times)
#clean 
#Duplicated
i <- duplicated(times);i 
which(i)
times[i,]##If no duplicates, ignore
clean.data <- unique(times[complete.cases(times),])
summary(clean.data)

#Data cleaning
#rank---useless
times$world_rank
rnk <- as.character(times$world_rank)
unique(rnk)
cbind(rnk, as.numeric(rnk))
#get rid of = and ranges (look up regular expressions!)
rnk <- sub(pattern = "=", "", rnk)
rnk <- sub(pattern = "-.*", "", rnk)
rnk <- as.numeric(rnk)
rnk
summary(rnk)
times$world_rank <- rnk

#Intl
intl <- as.character(times$international)
unique(intl)
intl[intl == '-'] <- NA
unique(intl)
intl <- as.numeric(intl)
summary(intl)
times$international <- intl

#inc
inc <- as.character(times$income)
unique(inc)
inc[inc == '-'] <- NA
unique(inc)
inc <- as.numeric(inc)
summary(inc)
times$income <- inc

#totalscore
totalscore <- as.character(times$total_score)
unique(totalscore)
totalscore[totalscore == '-'] <- NA
unique(totalscore)
totalscore <- as.numeric(totalscore)
summary(totalscore)
times$total_score <- totalscore

#Students
ns <- as.character(times$num_students)
unique(ns)
cbind(ns, as.numeric(ns))
ns <- sub(pattern = ",", "", ns)
ns <- as.numeric(ns)
summary(ns)
times$num_students <- ns

# international_students
times$international_students
instu <- as.character(times$international_students)
unique(instu)
cbind(instu, as.numeric(fe==instu))
instu <- sub(pattern = "%", "", instu)
instu <- as.numeric(instu)
instu
summary(instu)
times$international_students <-instu

# female_male_ratio--female
fe <- as.character(times$female_male_ratio)
unique(fe)
cbind(fe, as.numeric(fe))
fe <- sub(pattern = "-", "", fe)
fe <- sub(pattern = ":.*", "", fe)
fe <- as.numeric(fe)
fe
summary(fe)

#female_male_ratio--male
ma <- as.character(times$female_male_ratio)
unique(ma)
cbind(ma, as.numeric(ma))
ma <- sub(pattern = "-", "", ma)
ma <- sub(pattern = ".*:", "", ma)
ma <- as.numeric(ma)
ma
summary(ma)

#ratio
ratio<-fe/ma
ratio
times$female_male_ratio <- ratio
summary(ratio)

summary(times)
table(times$year)

#Data cleaning end
times$year <- as.numeric(times$year)
times$female_male_ratio <- NULL
#times$Continent < as.factor(times$Continent)
##########split the data by university, 56 repeat once from 2005 to 2015

summary(times)
colnames(times)
dim(times)
############################################################################### data processing done
#Select some data for clustering
data <- times[,c("university_name","teaching",
                 "research", "citations", "income","student_staff_ratio",
                 "international", "num_students","international_students","year", "English_speaking","Continent","world_rank")]
data[,-c(1,10:13)] <- scale(data[,-c(1,10:13)])
#kmeans does not like missing data!
library(dbscan)
lof <- lof(data[,-c(1,10:13)], k = 3)
lof
plot(data[,-c(1,10:13)], pch = ".", main = "LOF (k=3)")
points(data[,-c(1,10:13)], cex = (lof-1)*3, pch = 1, col="red")

plot(sort(lof), type = "l")
abline(h = 1.7, col = "red")

plot(data[,-c(1,10:13)][lof < 1.7,], main = "Data and outliers")
points(data[,-c(1,10:13)][lof >= 1.3,], col = "grey", pch = 4)

data<-data[-lof,]
############################ outliers done
data <- na.omit(data)
colnames(data)
summary(data)
# pairs(data[,-c(1,10:13)])
############################################# tendency
d_shapes <- dist(data[,"research"])
library(seriation)
#Visual Analysis for Cluster Tendency Assessment (VAT) reorders the objects to show potential clustering tendency as a block structure (dark blocks along the main diagonal).
VAT(d_shapes)
iVAT(d_shapes)
############################################################################ visualization
pairs(data[,-c(1,10:12)], col = km$cluster)
km <- kmeans(data[,-c(1,10:12)], centers = 5)
pairs(data[,-c(1,10:12)], col = km$cluster)
library("GGally")
data2 <- data[,-c(1,10:12)]
data2$cluster <- as.factor(km$cluster)
ggpairs(data2, mapping = ggplot2::aes(color = cluster))
###################################################################

#Cluster just teaching and research
data_tr <- data[, c("teaching", "research")]
plot(data_tr)
cor(data_tr)
km <- kmeans(data_tr, centers = 3)
plot(data_tr, col = km$cluster)
# do PCA
pr <- prcomp(data_tr)
biplot(pr)
plot(pr$x, col = km$cluster)
##########################################
############################################################Find Optimal Number of Clusters for k-means
set.seed(1234)
ks <- 2:10
#Within Sum of Squares
#Use within sum of squares and look for the knee (nstart=5 repeats k-means 5 times and returns the best solution)

WSS <- sapply(ks, FUN=function(k) {
  kmeans(data[,c("teaching","research")], centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=5, col="red", lty=2)

#Average Silhouette Width
#Use average silhouette width (look for the max)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(data[,c("teaching","research")], centers=k, nstart=5)$cluster)$avg.silwidth
})
plot(ks, ASW, type="l")
ks[which.max(ASW)]
abline(v=ks[which.max(ASW)], col="red", lty=2)
############################################################
#Dunn Index
#Use Dunn index (another internal measure given by min. separation/ max. diameter)
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(data[,c("teaching","research")], centers=k, nstart=5)$cluster)$dunn
})
plot(ks, DI, type="l")
ks[which.max(DI)]
## 10
abline(v=ks[which.max(DI)], col="red", lty=2)
#########################
#Gap Statistic
#Compares the change in within-cluster dispersion with that expected from a null model (see ? clusGap). 
#The default method is to choose the smallest k such that its value Gap(k) is not more than 1 standard error away from the first local maximum.
library(cluster)
k <- clusGap(data[,c("teaching","research")], FUN = kmeans,  nstart = 10, K.max = 10)
k
plot(k)
#############################################################
k <- clusGap(data[,c("teaching","research")], FUN = dbscan,  nstart = 10, K.max = 10)
k
plot(k)
k <- clusGap(data[,c("world_rank","research")], FUN = hcut,  nstart = 10, K.max = 10)
k
plot(k)
############################
#################
require(cluster)
library(factoextra)
fviz_nbclust(data[,c("world_rank","research")], hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(data[,c("world_rank","research")], hcut, method = "silhouette",
             hc_method = "complete")

###################################################modeling
#Create clusters kmeans
km <- kmeans(data[,-c(1,10:13)], centers = 4,nstart=10);km
################
plot(data[,c("research", "num_students")], col = km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID

library(cluster)
clusplot(data[,c("world_rank","research")], km$cluster)
km$centers

def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))
plot(data[,c("world_rank","research")], col=kmeans(data[,c("world_rank","research")], centers=6)$cluster)
plot(data[,c("Area","world_rank")], col=kmeans(data[,c("Area","world_rank")], centers=6)$cluster)
plot(data[,c("world_rank","research")], col=kmeans(data[,c("Area","world_rank")], centers=6)$cluster)

#######################

############################################################# Hierarchical Clustering
# dist defaults to method="Euclidean"

d <- dist(data[,c("world_rank","research")])
# We cluster using complete link

hc <- hclust(d, method="complete")
#Dendrogram

plot(hc)
rect.hclust(hc, k=5)

plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels
cluster_complete <- cutree(hc, k=10)
plot(data[,c("world_rank","research")], col=cluster_complete)
###############################
############################################################### Density-based clustering with DBSCAN
library(dbscan)
# Parameters: minPts is often chosen as dimensionality of the data +1. Decide on epsilon using the knee in the kNN distance plot (seems to be around eps = .25).

kNNdistplot(data[,c("world_rank","research")], k = 3)
abline(h=.07, col="red")
#run dbscan

db <- dbscan(data[,c("world_rank","research")], eps=.07, minPts=3)
db
str(db)
plot(data[,c("world_rank","research")], col=db$cluster+1L)
hullplot(data[,c("world_rank","research")], db)
######################################
# Play with eps (neighborhood size) and MinPts (minimum of points needed for core cluster)
###################################################################    Gaussian Mixture Models
library(mclust)
m <- Mclust(data[,c("world_rank","research")])
summary(m)
plot(m, what = "classification")

#########################
############################################################################ Internal Cluster Validation
# Compare the Clustering Quality
# Look at the within.cluster.ss and the avg.silwidth
#library(fpc)
fpc::cluster.stats(d, km$cluster)
#Silhouette plot
plot(silhouette(km$cluster, d))
##############################

# Visualize the Distance Matrix
# Visualizing the unordered distance matrix does not show much structure.

library(seriation)
pimage(d, colorkey=TRUE)
pimage(d, order=order(km$cluster), colorkey=TRUE)
#Use dissplot which rearranges clusters, adds cluster labels, and shows average dissimilarity in the lower half of the plot.
dissplot(d, labels=km$cluster, options=list(main="k-means with k=4"))
#################
dissplot(d, labels=db$cluster+1, options=list(main="DBSCAN"))

#############################################################################   external cross validation
#Prepare data
truth <- as.integer(data$Area)
shapes <- scale(data$research)

plot(shapes)
km <- kmeans(shapes, centers=5, nstart = 10)
plot(shapes, col=km$cluster)

d <- dist(shapes)
hc <- hclust(d, method="single")
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, cutree(hc, k))$avg.silwidth
})
plot(ks, ASW, type="l")

hc_4 <- cutree(hc, 5)
plot(shapes, col=hc_4)

#define entropy and purity
entropy <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(truth, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  sum(rowSums(e, na.rm = TRUE) * mi/m)
}

purity <- function(cluster, truth) {
  k <- max(cluster, truth)
  cluster <- factor(cluster, levels = 1:k)
  truth <- factor(truth, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(truth, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(apply(p, 1, max) * mi/m)
}
#calculate measures (for comparison we also use random "clusterings" with 4 and 6 clusters)

random4 <- sample(1:4, nrow(shapes), replace = TRUE)
random6 <- sample(1:6, nrow(shapes), replace = TRUE)

r <- rbind(
  kmeans = c(
    unlist(fpc::cluster.stats(d, km$cluster, truth, compareonly = TRUE)),
    entropy = entropy(km$cluster, truth),
    purity = purity(km$cluster, truth)
  ),
  hc = c(
    unlist(fpc::cluster.stats(d, hc_4, truth, compareonly = TRUE)),
    entropy = entropy(hc_4, truth),
    purity = purity(hc_4, truth)
  ),
  random4 = c(
    unlist(fpc::cluster.stats(d, random4, truth, compareonly = TRUE)),
    entropy = entropy(random4, truth),
    purity = purity(random4, truth)
  ),
  random6 = c(
    unlist(fpc::cluster.stats(d, random6, truth, compareonly = TRUE)),
    entropy = entropy(random6, truth),
    purity = purity(random6, truth)
  )
)
r
###################### done



