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
times$year <- as.factor(times$year)

##########split the data by university, 56 repeat once from 2005 to 2015
l <- split(times, times$university_name)
year <- sapply(l, nrow)
l[1:3]
table(year)
#

times$year <- as.factor(times$year)
times$English_speaking <-as.factor(times$English_speaking)

#Get rid of total score (is derived) and university name
times$total_score <- NULL
times$university_name <- NULL
times$year <- NULL
times$female_male_ratio <- NULL
times$country <- NULL
# impute missing values
for(i in which(sapply(times, is.numeric))){
  times[is.na(times[,i]), i] <- mean(times[,i], na.rm = TRUE)
}
# delete rows with missing values
row.has.na <- apply(times, 1, function(x){any(is.na(x))})
sum(row.has.na)
times <- times[!row.has.na,]
summary(times)

#
library(Matrix)
library(arules)
library(arulesViz)
library(plotly)

#Discretize rank
times$world_rank <- discretize(times$world_rank, method = "frequency", categories = 5,
                               labels = c("very high", "high", "average", "low", "very low"))

#Discretize all other continuous variables
for(i in which(sapply(times, is.numeric)))
  times[[i]] <- discretize(times[[i]], method = "frequency", categories = 5,
                           labels = c("very low", "low", "average", "high", "very high"))

summary(times)
colnames(times)
dim(times)
################################# data processing done


################################# Create Transactions
trans <- as(times, "transactions");trans
summary(trans)
################################# Mine Some Rules
10/nrow(trans)
rules <- apriori(trans, parameter = list(support = 0.004))
inspect(head(rules, 10, by = "lift"))

inspectDT(rules)
plotly_arules(rules, jitter = 2)

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
is <- apriori(trans, parameter=list(target="frequent", support=0.004));is
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
rank <- grep("world_rank=", itemLabels(trans), value = TRUE);rank
rules_rank <- apriori(trans,
                      appearance= list(rhs=rank, default="lhs"))
inspect(head(sort(rules_rank, by="lift")))
##############################Mine using Templates_rank done

##############################Mine using Templates_area
area <- grep("Area=", itemLabels(trans), value = TRUE);area
rules_area <- apriori(trans,
                      appearance= list(rhs=area, default="lhs"))
inspect(head(sort(rules_area, by="lift")))
##############################Mine using Templates_rank done

##############################Mine using Templates_teaching
teaching <- grep("teaching=", itemLabels(trans), value = TRUE);teaching
rules_teaching <- apriori(trans,
                          appearance= list(rhs=teaching, default="lhs"))
inspect(head(sort(rules_teaching, by="lift")))
##############################Mine using Templates_teaching done

##############################Mine using Templates_research
research <- grep("research=", itemLabels(trans), value = TRUE);research
rules_research <- apriori(trans,
                          appearance= list(rhs=research, default="lhs"))
inspect(head(sort(rules_research, by="lift")))
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
