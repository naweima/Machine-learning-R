################################################Shanghai
shanghai <- read.csv("C:/data/shanghaiData.csv")
summary(shanghai)
#clean data
#world rank
rnk <- as.character(shanghai$world_rank)
rnk <- sub(pattern = "-.*", "", rnk)
rnk <- as.numeric(rnk)
summary(rnk)
shanghai$world_rank <- rnk

#national rank 
rnk <- as.character(shanghai$national_rank)
rnk <- sub(pattern = "-.*", "", rnk)
rnk <- as.numeric(rnk)
summary(rnk)
shanghai$national_rank <- rnk
#data cleaning end

##########split the data by university, 56 repeat once from 2005 to 2015
l <- split(shanghai, shanghai$university_name)
years <- sapply(l, nrow)
l[1:3]
table(years)

# impute missing values
for(i in 1:ncol(shanghai)){
  r4[is.na(shanghai[,i]), i] <- mean(shanghai[,i], na.rm = TRUE)
}
summary(shanghai)
library(psych)
describe(shanghai[,4:10])
################################################################## Data cleaning end
###################################################### SMU
library("DT")
library("plotly")
colnames(shanghai)
age <- shanghai[shanghai$university_name =="Southern Methodist University",]
datatable(age) %>% formatRound(2,2)


#################
times<-shanghai
colnames(times)
summary(times[,4:11])
#############
boxplot(times[,-c(1,2,3,11)],main='Shanghai-boxplot of features')

plot(shanghai$world_rank, shanghai$national_rank, col=shanghai$university_name)


#library(seriation)
#cc<-cor(shanghai[,c(4:10)]);cc
#######cor(shanghai[sapply(shanghai, is.numeric)])


pimage(cc, main = "Correlation between features")
library(corrplot)
corrplot(cc, method = "ellipse")

times<-shanghai
colnames(times)
#############
boxplot(times[,-c(1,2,11)],main='Shanghai-boxplot of features')
