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

summary(times[,c(3:12,14:16)])
table(times$year)
colnames()
#Data cleaning end
times$year <- as.numeric(times$year)

library(psych)
describe(times[,3:12])

##########split the data by university, 56 repeat once from 2005 to 2015
l <- split(times, times$university_name)
year <- sapply(l, nrow)
l[1:3]
table(year)
#

times$English_speaking <-as.factor(times$English_speaking)

#Get rid of total score (is derived) and university name
times$female_male_ratio <- NULL
# impute missing values
for(i in which(sapply(times, is.numeric))){
  times[is.na(times[,i]), i] <- mean(times[,i], na.rm = TRUE)
}
# delete rows with missing values
row.has.na <- apply(times, 1, function(x){any(is.na(x))})
sum(row.has.na)
times <- times[!row.has.na,]
summary(times)
colnames(times)
#times$studentstaff<-times$student_staff_ratio
#times$student_staff_ratio<-NULL
#times$interstudents<-times$international_students
#times$international_students<-NULL
######################################
boxplot(times[,-c(1:3,11:13)],main='Times-boxplot of features')
##########split the data by university, 344 repeat once from 2005 to 2015
l <- split(times, times$university_name)
year <- sapply(l, nrow)
l[1:3]
table(year)
#range, mean, median, variance, counts
library(psych)
describe(times)

############
library(Matrix)
library(arules)
library(arulesViz)
library(plotly)

summary(times)
colnames(times)
dim(times)
################################# data processing done

############### hist/dist & scatter & r^2
plot(times, col=times$year)
hist(times$world_rank, breaks = 30) ######## This is weird!!!

hist(times$teaching)
hist(times$research)
hist(times$citations,breaks = 30)
hist(times$income)
hist(times$num_students)
hist(times$student_staff_ratio)
hist(times$international_students,breaks = 30)

############################################## scatter plot

plot(times$world_rank, times$num_students)
# plot(times$world_rank, times$num_students, log = "y")
plot(times$world_rank, times$international)

######################################################
plot(times$world_rank, times$teaching, log ="x")
#cor(times$world_rank, times$teaching)
#cor(times$world_rank, times$teaching, method = "spearman")
cor(log(times$world_rank), times$teaching)
l <- lm(teaching ~ log(world_rank), data = times[times$world_rank>10,])
summary(l)
x <- 1:600
lines(x, predict(l, data.frame(world_rank=x)), col = "red")

plot(times$world_rank, times$teaching)
lines(x, predict(l, data.frame(world_rank=x)), col = "red")
##############


plot(times$world_rank, times$research)
plot(times$world_rank, times$income)
plot(times$world_rank, times$student_staff_ratio,log = "y")
plot(times$world_rank, times$international_students)

# Some figure aggregate together, so I made a log transformation for y.
colnames(times)

#######################
times2016 <- times[times$year=="2016",]
summary(times2016$year)

########## Look at correlation
library(seriation)
m <- na.omit(times2016)
rownames(m) <- m[,2]
m <- m[,-c(2,3,13,14,15)]
hmap(cor(m), margins=c(10,10))
hmap(t(scale(m[1:20,])), margins=c(17,10), method = "OLO")
cc<-cor(times[,c(1,4:12)]);cc
library("seriation") # for pimage
pimage(cc, main = "Correlation between features")
library(corrplot)
corrplot(cc, method = "ellipse")


##########################################

########################################################## PCA
pr <- prcomp(scale(m))
plot(pr,main='principal component 2016')
biplot(pr, col = c("grey", "red"), main = "All Universities (2016)")

pr <- prcomp(scale(m[1:10,]))
plot(pr,main='principal component 2016')
biplot(pr, col = c("grey", "red"), xlim=c(-1,1), ylim=c(-.5,1), main = "Top 10 (2016)")
pr <- prcomp(scale(m[1:50,]))
biplot(pr, col = c("grey", "red"), main = "Top 50 (2016)")
########################################################## PCA end

############################
library("DT")
library("plotly")
colnames(times)
age <- times[times$Area =="NorthAMERICA",]
age2 <- apply(age[,-(1:3)], MARGIN =1, mean, na.rm = TRUE)
age <- data.frame(country=age$country_name, college_age_pop = age2)
datatable(age) %>% formatRound(2,2)

colnames(times)
age <- times[times$country =="United States of America",]
datatable(age) %>% formatRound(2,2)

###########################################
plot(times2016[,4:5])
text(times2016[,4], times2016[,5], labels = times2016[,14], pos = 3,
     col = rgb(.5,0,0, alpha = .5))
#########################

########################## continents

barplot(prop.table(table(times$Area)), ylim=(c(0,0.5)),main="The distribution of continents", col=c('Red','Orange','Yellow','Green','Blue','Purple'))
barplot(prop.table(table(times$English_speaking)), ylim=(c(0,0.6)),main="The distribution of English_speaking", col=c('Red','Yellow'))
###################

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

plot_ly(times, x = research, y = teaching,
        text = university_name, 
        mode = "markers")


plot_ly(times, x = student_staff_ratio, y = income,
        text = university_name, 
        mode = "markers")

