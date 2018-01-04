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
data <- na.omit(data)
###
library(dbscan)
lof <- lof(data[,-c(1,10:12)], k = 3)
lof
plot(data[,-c(1,10:12)], pch = ".", main = "LOF (k=3)")
points(data[,-c(1,10:12)], cex = (lof-1)*3, pch = 1, col="red")

plot(sort(lof), type = "l")
abline(h = 1.5, col = "red")

plot(data[,-c(1,10:12)][lof < 1.5,], main = "Data and outliers")
points(data[,-c(1,10:12)][lof >= 1.5,], col = "grey", pch = 4)
##############

################################ remove outliers
# x[!x %in% boxplot.stats(x)$out]
# summary(x)
# data<-x
data[,2][!data[,2] %in% boxplot.stats(data[,2])$out]
data[,3][!data[,3] %in% boxplot.stats(data[,3])$out]
data[,4][!data[,4] %in% boxplot.stats(data[,4])$out]
data[,5][!data[,5] %in% boxplot.stats(data[,5])$out]
data[,6][!data[,6] %in% boxplot.stats(data[,6])$out]
data[,7][!data[,7] %in% boxplot.stats(data[,7])$out]
data[,8][!data[,8] %in% boxplot.stats(data[,8])$out]
data[,9][!data[,9] %in% boxplot.stats(data[,9])$out]
summary(data)
############################ outliers done
summary(data[2:9])
colnames(data)
summary(data)
# pairs(data[,-c(1,10:13)])
############################################# tendency
d_shapes <- dist(data[,c(2,13)])
library(seriation)
#Visual Analysis for Cluster Tendency Assessment (VAT) reorders the objects to show potential clustering tendency as a block structure (dark blocks along the main diagonal).
VAT(d_shapes)
iVAT(d_shapes)
############################################################################ visualization
pairs(data[,-c(1,10:12)], col = km$cluster)
km <- kmeans(data[,-c(1,10:12)], centers = 5)
km
#pairs(data[,-c(1,10:12)], col = km$cluster)
library("GGally")
data2 <- data[,-c(1,10:12)]
data2$cluster <- as.factor(km$cluster)
ggpairs(data2, mapping = ggplot2::aes(color = cluster))
###################################################################

##########################################
############################################################Find Optimal Number of Clusters for k-means
set.seed(1234)
ks <- 2:10

#Use Dunn index (another internal measure given by min. separation/ max. diameter)
# 1 Within Sum of Squares
#Use within sum of squares and look for the knee (nstart=5 repeats k-means 5 times and returns the best solution)
WSS <- sapply(ks, FUN=function(k) {
  kmeans(data[,c("teaching","research")], centers=k, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=5, col="red", lty=2)

# 2Average Silhouette Width
#Use average silhouette width (look for the max)
library(factoextra)
require(cluster)
fviz_nbclust(data[,c("world_rank","research")], hcut, method = "silhouette",
             hc_method = "complete")

#########################
#3 Gap Statistic
#Compares the change in within-cluster dispersion with that expected from a null model (see ? clusGap). 
#The default method is to choose the smallest k such that its value Gap(k) is not more than 1 standard error away from the first local maximum.
library(cluster)
k <- clusGap(data[,c("world_rank","teaching")], FUN = dbscan,  nstart = 10, K.max = 10)
k
plot(k)

#4 Average Silhouette Width
#Use average silhouette width (look for the max)
library(factoextra)
require(cluster)
fviz_nbclust(data[,c("student_staff_ratio","international_students")], pam, method = "silhouette")
############################################################

###################################################modeling
#Create clusters kmeans
#Cluster just teaching and research
data_tr <- data[, c("teaching", "research")]
plot(data_tr)
cor(data_tr)
km <- kmeans(data_tr, centers = 5)
plot(data_tr, col = km$cluster)

# do PCA
pr <- prcomp(data_tr)
biplot(pr)
plot(pr$x, col = km$cluster)

################
plot(data[,c("teaching", "research")], col = km$cluster)

points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID

library(cluster)
clusplot(data[,c("teaching", "research")], km$cluster)
km$centers

def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))
plot(data[,c("teaching", "research")], col=kmeans(data[,c("teaching", "research")], centers=5)$cluster)



km.res <- eclust(data[,c("teaching", "research")], "kmeans", k = 5,
                 nstart = 25, graph = FALSE)
################################## k-means group number of each observation
km.res$cluster
fviz_silhouette(km.res)
fviz_cluster(km.res, geom = "point", frame.type = "norm")
#######################

############################################################# Hierarchical Clustering
# dist defaults to method="Euclidean"
d <- dist(data[,c("world_rank","research")])
# We cluster using complete link
hc <- hclust(d, method="complete")
#Dendrogram
plot(hc)
rect.hclust(hc, k=3)
plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels
cluster_complete <- cutree(hc, k=3)
rect.hclust(hc, k=10)
plot(data[,c("world_rank","research")], col=cluster_complete)

####################
res.hc <- eclust(data[,c("world_rank","research")], "hclust", k = 3,
                 method = "complete", graph = FALSE) 
head(res.hc$cluster, 15)
# Dendrogram
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE) 
###############################
############################################################### Density-based clustering with DBSCAN
library(dbscan)
# Parameters: minPts is often chosen as dimensionality of the data +1. Decide on epsilon using the knee in the kNN distance plot (seems to be around eps = .25).

kNNdistplot(data[,c("world_rank","teaching")], k = 4)
abline(h=.5, col="red")
#run dbscan
db <- dbscan(data[,c("world_rank","teaching")], eps=.5, minPts=4)
db
str(db)
plot(data[,c("world_rank","teaching")], col=db$cluster+1L)
hullplot(data[,c("world_rank","teaching")], db)
######################################
# Play with eps (neighborhood size) and MinPts (minimum of points needed for core cluster)
##################################### pam
library("cluster")
pam.res <- pam(data[,c("student_staff_ratio","international_students")], k=3)
pam.res$medoids
head(pam.res$cluster)
clusplot(pam.res, main = "Cluster plot, k = 3", 
         color = TRUE)
library(factoextra)
fviz_cluster(pam.res)
plot(silhouette(pam.res),  col = 2:5) 
fviz_silhouette(silhouette(pam.res)) 
############################################################# Hierarchical Clustering
# dist defaults to method="Euclidean"
d <- dist(data[,c("student_staff_ratio","international_students")])
# We cluster using complete link
hc <- hclust(d, method="complete")
#Dendrogram
plot(hc)
rect.hclust(hc, k=3)
plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels
cluster_complete <- cutree(hc, k=3)
rect.hclust(hc, k=10)
plot(data[,c("student_staff_ratio","international_students")], col=cluster_complete)

####################
res.hc <- eclust(data[,c("student_staff_ratio","international_students")], "hclust", k = 3,
                 method = "complete", graph = FALSE) 
head(res.hc$cluster, 15)
# Dendrogram
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE) 

###################################################################    Gaussian Mixture Models
library(mclust)
m <- Mclust(data[,c("income","citations")])
summary(m)
plot(m, what = "classification")
#########################
############################################################################ part Internal Cluster Validation
# Compare the Clustering Quality
# Look at the within.cluster.ss and the avg.silwidth
#library(fpc)
fpc::cluster.stats(d, km$cluster)
#Silhouette plot
plot(silhouette(km$cluster, d))
########################################################################### 
# Visualize the Distance Matrix
# Visualizing the unordered distance matrix does not show much structure.

library(seriation)
d <- dist(data[,c("teaching","research")])
#pimage(d, colorkey=TRUE)
pimage(d, order=order(km$cluster), colorkey=TRUE)
#Use dissplot which rearranges clusters, adds cluster labels, and shows average dissimilarity in the lower half of the plot.
dissplot(d, labels=km$cluster, options=list(main="k-means with k=5"))
#################
dissplot(d, labels=db$cluster+1, options=list(main="DBSCAN"))

#############################################################################  5.1 external cross validation
#Prepare data
truth <- as.integer(data$world_rank)
#############################################
shapes <- data[,c("teaching","research")]
plot(shapes)
km <- kmeans(shapes, centers=5, nstart = 10)
plot(shapes, col=km$cluster)

d <- dist(shapes)
hc <- hclust(d, method="complete")
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
###################### 

#Prepare data
truth <- as.integer(data$continent)
#
shapes <- data[,c("student_staff_ratio","international_students")]
plot(shapes)
km <- kmeans(shapes, centers=3, nstart = 10)
plot(shapes, col=km$cluster)

d <- dist(shapes)
hc <- hclust(d, method="complete")
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, cutree(hc, k))$avg.silwidth
})
plot(ks, ASW, type="l")

hc_4 <- cutree(hc, 3)
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
    entropy = e
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
###################### donentropy(random4, truth),
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



