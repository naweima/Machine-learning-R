################################################cwurData_improvement
times <- read.csv("C:/data/cwurData.csv")
summary(times)
#clean 
#Duplicated
i <- duplicated(times);i 
which(i)
times[i,]##If no duplicates, ignore
clean.data <- unique(times[complete.cases(times),])
summary(clean.data)

times1<-times
summary(times)
table(times$year)
colnames(times)
#Data cleaning end
##########split the data by university, 56 repeat once from 2005 to 2015
l <- split(times, times$institution)
year <- sapply(l, nrow)
l[1:3]
table(year)

############################### improved features
#pick two years for comparison
r <- lapply(l, FUN = function(x) {
  d_2014 <- x[x$year==2014,]
  d_2015 <- x[x$year==2015,]
  merge(d_2014, d_2015, by = "institution",
        all = TRUE, suffix = c("_X2014", "_X2015"))
})
r <- do.call(rbind, r)
head(r)
summary(r)
colnames(r)
#calculate improvement
########################################### quality_of_education
improved_quality_of_education <- r$'quality_of_education_X2015' - r$'quality_of_education_X2014'
table(improved_quality_of_education, useNA = "always")
# add improvement as a variable 
r$improved_quality_of_education <- as.numeric(improved_quality_of_education)
dim(r)
summary(r)
########################################### alumni_employment
improved_alumni_employment <- r$'alumni_employment_X2015' - r$'alumni_employment_X2014'
table(improved_alumni_employment, useNA = "always")
# add improvement as a variable 
r$improved_alumni_employment <- as.numeric(improved_alumni_employment)
dim(r)
summary(r)
########################################### quality_of_faculty
improved_quality_of_faculty <- r$'quality_of_faculty_X2015' - r$'quality_of_faculty_X2014'
table(improved_quality_of_faculty, useNA = "always")
# add improvement as a variable 
r$improved_quality_of_faculty <- as.numeric(improved_quality_of_faculty)
dim(r)
summary(r)

########################################### publications
improved_publications <- r$'publications_X2015' - r$'publications_X2014'
hist(improved_publications, breaks = 100)
table(improved_publications, useNA = "always")
# add improvement as a variable 
r$improved_publications <- as.numeric(improved_publications)
dim(r)
summary(r)
########################################### influence
improved_influence <- r$'influence_X2015' - r$'influence_X2014'
hist(improved_influence, breaks = 100)
table(improved_influence, useNA = "always")
# add improvement as a variable 
r$improved_influence <- as.numeric(improved_influence)
dim(r)
summary(r)
########################################### citations
improved_citations <- r$'citations_X2015' - r$'citations_X2014'
hist(improved_citations, breaks = 100)
table(improved_citations, useNA = "always")
# add improvement as a variable 
r$improved_citations <- as.numeric(improved_citations)
dim(r)
summary(r)
########################################### broad_impact
improved_broad_impact <- r$'broad_impact_X2015' - r$'broad_impact_X2014'
hist(improved_broad_impact, breaks = 100)
table(improved_broad_impact, useNA = "always")
# add improvement as a variable 
r$improved_broad_impact <- as.numeric(improved_broad_impact)
dim(r)
summary(r)
########################################### patents
improved_patents <- r$'patents_X2015' - r$'patents_X2014'
hist(improved_patents, breaks = 100)
table(improved_patents, useNA = "always")
# add improvement as a variable 
r$improved_patents <- as.numeric(improved_patents)
dim(r)
summary(r)
colnames(r)

########################################## add features end

##delete useless features
r4 <- r3[, c(1,15:17,28:35)];summary(r4);colnames(r4)
r4$country<-r4$country_X2015
r4$country_X2015<-NULL
times<-r4
colnames(times)
summary(times)


# impute missing values
for(i in which(sapply(times, is.numeric))){
  times[is.na(times[,i]), i] <- mean(times[,i], na.rm = TRUE)
}

##################################################data preprocessing done

summary(times[,4:12])
colnames(times)
dim(times)
library(psych)
describe(times[,4:11])

boxplot(times[,-c(1:3,12)],main='cwur-boxplot of features')
################################# data processing done
cc<-cor(times[sapply(times, is.numeric)])
pimage(cc, main = "Correlation between features")
library(corrplot)
corrplot(cc, method = "ellipse")


##################################
times<-merge(times, data.frame(table(country = times$country)), by = c("country"))

library(ggplot2)
library(gridExtra)

times$Freq<-as.numeric(times$Freq)
times<-times[,c(1,16)]
times[!duplicated(times), ]

times$country <- factor(times$country, levels = times[order(times$Freq),'country'])
x <- ggplot(times, aes(y = country, x = Freq)) + geom_point(stat = "identity")
y <- ggplot(times, aes(x = country, y = Freq)) + geom_bar(stat = "identity") + 
  coord_flip()
grid.arrange(x, y, ncol = 2)
##################### 

plot(times$world_rank_X2015, times$national_rank_X2015,col = times$country,main = "Country Comparison in 2015",xlab = "World Rank", ylab="National Rank")



###################### smu
cwur2014 <- times1[times1$year=="2014",]
summary(cwur2014$year)
colnames(cwur2014)

m <- sapply(cwur2014[, -c(1:3, 14)], median, na.rm = TRUE)
oldpar <- par(mar = c(4,10,1,1))
barplot(m, las =2, horiz = TRUE, xlab = "Median Rank in 2014")
par(oldpar)
##Add SMU
smu <- cwur2014[cwur2014$institution=="Southern Methodist University",-c(1:3, 14)]
data <- as.matrix(rbind(SMU = smu, Median = m))
oldpar <- par(mar = c(4,10,1,1))
#barplot(unlist(smu[,-(2:3)]), las =2, horiz = TRUE)
barplot(data, las =2, horiz = TRUE, beside = TRUE, xlab = "Median Rank")

##################### 
which(cwur2014$institution == 'Southern Methodist University')
cwur2014[cwur2014$institution %in% 'Southern Methodist University',]

##
cwur2014$education<-cwur2014$quality_of_education
cwur2014$alumni<-cwur2014$alumni_employment
boxplot(cwur2014[,-c(1:6,14)],main='cwur-boxplot of features')
##################################
hist(cwur2014$world_rank, ylim=c(0,120)) 
abline(lwd=3, v=171, lty=1)
#mtext('Southern Methodist University',side=1,at=700, line=-14)
hist(cwur2014$national_rank, ylim=c(0,120)) 
abline(lwd=3, v=74, lty=1)

hist(cwur2014$quality_of_education, ylim=c(0,120)) 
abline(lwd=3, v=157, lty=1)

hist(cwur2014$alumni_employment,ylim=c(0,600)) 
abline(lwd=3, v=34, lty=1)

hist(cwur2014$quality_of_faculty,xlim=c(0,250)) 
abline(lwd=3, v=210, lty=1)

hist(cwur2014$publications,ylim=c(0,120)) 
abline(lwd=3, v=632, lty=1)

hist(cwur2014$influence,ylim=c(0,120)) 
abline(lwd=3, v=677, lty=1)

hist(cwur2014$citations,ylim=c(0,250)) 
abline(lwd=3, v=406, lty=1)

hist(cwur2014$broad_impact) 
abline(lwd=3, v=628, lty=1)

hist(cwur2014$patents,ylim=c(0,300),xlim=c(0,800)) 
abline(lwd=3, v=737, lty=1)

hist(cwur2014$score,ylim=c(0,500)) 
abline(lwd=3, v=48.69, lty=1)


#
