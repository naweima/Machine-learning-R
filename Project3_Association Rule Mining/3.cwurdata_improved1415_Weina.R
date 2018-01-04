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
improved_quality_of_education <- r$'quality_of_education_X2015' - r$'quality_of_education_X2014'>0
table(improved_quality_of_education, useNA = "always")
# add improvement as a variable 
r$improved_quality_of_education <- as.factor(improved_quality_of_education)
dim(r)
summary(r)
########################################### alumni_employment
improved_alumni_employment <- r$'alumni_employment_X2015' - r$'alumni_employment_X2014'>0
table(improved_alumni_employment, useNA = "always")
# add improvement as a variable 
r$improved_alumni_employment <- as.factor(improved_alumni_employment)
dim(r)
summary(r)
########################################### quality_of_faculty
improved_quality_of_faculty <- r$'quality_of_faculty_X2015' - r$'quality_of_faculty_X2014'>0
table(improved_quality_of_faculty, useNA = "always")
# add improvement as a variable 
r$improved_quality_of_faculty <- as.factor(improved_quality_of_faculty)
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

########################################## add features end

###############################pick two years for comparison, create y
r2 <- lapply(l, FUN = function(x) {
  d_2014 <- x[x$year==2014,]
  d_2015 <- x[x$year==2015,]
  merge(d_2014, d_2015, by = "institution",
        all = TRUE, suffix = c("_Y2014", "_Y2015"))
})
r2 <- do.call(rbind, r2)
head(r2)
#calculate improvement of y
improved_rank14_15 <- r2$'world_rank_Y2014' - r2$'world_rank_Y2015'
hist(improved_rank14_15, breaks = 100)
##
improved_rank14_15 <- improved_rank14_15 < 0
table(improved_rank14_15, useNA = "always")
### add improvement as the class variable (has to be a factor)
r2$improved_rank14_15 <- as.factor(improved_rank14_15)
dim(r2)
summary(r2)
############## get target end

r3<-merge(r,r2, by="institution")
summary(r3)
colnames(r3)
##delete useless features
r4 <- r3[, -c(1:27)];summary(r4);colnames(r4)
r4 <- r4[, -c(9:34)]
times<-r4
colnames(times)
summary(times)


# impute missing values
for(i in which(sapply(times, is.numeric))){
  times[is.na(times[,i]), i] <- mean(times[,i], na.rm = TRUE)
}
# delete rows with missing values
row.has.na <- apply(times, 1, function(x){any(is.na(x))})
sum(row.has.na)
times <- times[!row.has.na,]
summary(times)
##################################################data preprocessing done

#Discretize all other continuous variables
for(i in which(sapply(times, is.numeric)))
  times[[i]] <- discretize(times[[i]], method = "frequency", categories = 5,
                           labels = c("very low", "low", "average", "high", "very high"))

summary(times)
colnames(times)
dim(times)
################################# data processing done
#
library(Matrix)
library(arules)
library(arulesViz)
library(plotly)

################################# Create Transactions
trans <- as(times, "transactions");trans
summary(trans)
################################# Mine Some Rules
10/nrow(trans)
rules <- apriori(trans, parameter = list(support = 0.01))
inspect(head(rules, 10, by = "lift"))
itemFrequencyPlot(trans, topN=20)
inspectDT(rules)
plotly_arules(rules, jitter = 2)


# Look at Rules About China
rules_china <- subset(rules, items %pin% "improved_rank14_15")
rules_china
inspect(head(rules_china, 10, by="lift"))
plotly_arules(rules_china, jitter = 1)
# Create a China Data Subset
trans_china <- subset(trans, items %pin% "improved_rank14_15")
itemFrequencyPlot(trans_china, topN=20)
10/nrow(trans_china)
rules_china <- apriori(trans_china, parameter = list(support = 0.12))
inspect(head(rules_china, 10, by ="lift"))
inspectDT(rules_china)


#
as(trans, "matrix")[1:3,]
inspect(trans[1:3])
colnames(trans)

image(trans)
itemFrequencyPlot(trans,topN=20)
plot(sort(itemFrequency(trans, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")


# Mine Frequent Itemsets
2^ncol(trans)
is <- apriori(trans, parameter=list(target="frequent"));is

10/nrow(trans)
is <- apriori(trans, parameter=list(target="frequent", support=0.01));is
is <- sort(is, by="support")
inspect(head(is, n=10))

barplot(table(size(is)), xlab="itemset size", ylab="count")
inspect(is[size(is)>8])

# Concise Representation of Itemsets
is_max <- is[is.maximal(is)]
inspect(head(sort(is_max, by="support")))

is_closed <- is[is.closed(is)]
inspect(head(sort(is_closed, by="support")))

barplot(c(
  frequent=length(is),
  closed=length(is_closed),
  maximal=length(is_max)
), ylab="count", xlab="itemsets")

# Mine Association Rules
rules <- apriori(trans, parameter=list(support=0.05, confidence=.9))
length(rules)
inspect(head(rules))

quality(head(rules))

rules <- sort(rules, by="lift")
inspect(head(rules, n=10))

# Additional Interest Measures
interestMeasure(rules[1:10], measure=c("phi", "gini"),
                trans=trans)
quality(rules) <- cbind(quality(rules),
                        interestMeasure(rules, measure=c("phi", "gini"),
                                        trans=trans))
inspect(head(rules, by="phi"))
plot(quality(head(rules, n=1000, by = "lift")))

##############################Mine using Templates_rank
rank <- grep("improved_rank14_15=", itemLabels(trans), value = TRUE);rank
rules_rank <- apriori(trans,
                      appearance= list(rhs=rank, default="lhs"))
inspect(head(sort(rules_rank, by="lift")))
##############################Mine using Templates_rank done


##############################Mine using Templates_research done
#write(rules, file="rules.csv", quote=TRUE)

#Association rule visualization
plot(rules)
plot(rules, control=list(jitter=10))
plot(rules, shading="order", control=list(jitter=10))

plot(rules_research)

plot(rules, method="grouped")
plot(sample(rules, 100), method="graph", control=list(type="items"))
plot(sample(rules, 50), method="graph", control=list(type="items"))

plot(sort(rules, by="phi")[1:100], method="graph", control=list(type="items"))
