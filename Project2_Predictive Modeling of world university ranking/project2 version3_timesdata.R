################################################Timesdata
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
#Data cleaning end


##########split the data by university, 56 repeat once from 2005 to 2015
l <- split(times, times$university_name)
year <- sapply(l, nrow)
l[1:3]
table(year)

############################### improved features
#pick two years for comparison
r <- lapply(l, FUN = function(x) {
  d_2013 <- x[x$year==2013,]
  d_2014 <- x[x$year==2014,]
  merge(d_2013, d_2014, by = "university_name",
        all = TRUE, suffix = c("_X2013", "_X2014"))
})
r <- do.call(rbind, r)
head(r)
summary(r)
colnames(r)
#calculate improvement
########################################### teaching
improved_teaching <- r$'teaching_X2014' - r$'teaching_X2013'
hist(improved_teaching)
hist(improved_teaching, breaks = 100)
improved_teaching
table(improved_teaching, useNA = "always")
# add improvement as a variable 
r$improved_teaching <- as.numeric(improved_teaching)
dim(r)
summary(r)
########################################### international
improved_international <- r$'international_X2014' - r$'international_X2013'
hist(improved_international)
hist(improved_international, breaks = 100)
improved_international
table(improved_international, useNA = "always")
# add improvement as a variable 
r$improved_international <- as.numeric(improved_international)
dim(r)
summary(r)
########################################### research
improved_research <- r$'research_X2014' - r$'research_X2013'
hist(improved_research)
hist(improved_research, breaks = 100)
improved_research
table(improved_research, useNA = "always")
# add improvement as a variable 
r$improved_research <- as.numeric(improved_research)
dim(r)
summary(r)
########################################### citations
improved_citations <- r$'citations_X2014' - r$'citations_X2013'
hist(improved_citations)
hist(improved_citations, breaks = 100)
improved_citations
table(improved_citations, useNA = "always")
# add improvement as a variable 
r$improved_citations <- as.numeric(improved_citations)
dim(r)
summary(r)
########################################### income
improved_income <- r$'income_X2014' - r$'income_X2013'
hist(improved_income)
hist(improved_income, breaks = 100)
improved_income
table(improved_income, useNA = "always")
# add improvement as a variable 
r$improved_income <- as.numeric(improved_income)
dim(r)
summary(r)

########################################## add features end

###############################pick two years for comparison, create y
r2 <- lapply(l, FUN = function(x) {
  d_2014 <- x[x$year==2014,]
  d_2015 <- x[x$year==2015,]
  merge(d_2014, d_2015, by = "university_name",
        all = TRUE, suffix = c("_Y2014", "_Y2015"))
})
r2 <- do.call(rbind, r2)
head(r2)
#calculate improvement of y
improved_rank14_15 <- r2$'world_rank_Y2014' - r2$'world_rank_Y2015'
hist(improved_rank14_15)
hist(improved_rank14_15, breaks = 100)
##
improved_rank14_15 <- improved_rank14_15 > 0
table(improved_rank14_15, useNA = "always")
### add improvement as the class variable (has to be a factor)
r2$improved_rank14_15 <- as.factor(improved_rank14_15)
dim(r2)
summary(r2)
############## get target end


r3<-merge(r,r2, by="university_name")
summary(r3)
colnames(r3)
##delete useless features
r4 <- r3[, -c(33:58)]
#delete useless features
r4$'university_name' <- NULL
r4$'year_X2014' <- NULL
r4$'year_X2013' <- NULL
r4$'world_rank_X2014' <- NULL
r4$'world_rank_X2013' <- NULL
r4$'country_X2014' <- NULL
r4$'country_X2013' <- NULL
r4$'total_score_X2014' <- NULL
r4$'total_score_X2013' <- NULL
r4$'female_male_ratio_X2014' <- NULL
r4$'female_male_ratio_X2013' <- NULL

colnames(r4)
summary(r4)

# impute missing values
for(i in 1:ncol(r4)){
  r4[is.na(r4[,i]), i] <- mean(r4[,i], na.rm = TRUE)
}
# delete rows with missing values
row.has.na <- apply(r4, 1, function(x){any(is.na(x))})
sum(row.has.na)
r4 <- r4[!row.has.na,]
summary(r4)
################################# Decision tree
#Create Tree With Default Settings (uses pre-pruning)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library("caret")

tree <- rpart(improved_rank14_15 ~ ., data=r4)
tree
rpart.plot(tree, extra = 2, under = TRUE, varlen=0, faclen=0)
#Do in-sample testing (resubstitution error). NOTE: You should use a training and test sample.
#Won't use it
pred <- predict(tree, r4, type="class")
confusionMatrix(data=pred, reference=r4$improved_rank14_15, positive = "TRUE")

########### Create a Full Tree
tree2 <- rpart(improved_rank14_15 ~ ., data=r4, control=rpart.control(minsplit=2, cp=0))
rpart.plot(tree2, extra = 2, under = TRUE,  varlen=0, faclen=0)
tree2
pred <- predict(tree2, r4, type="class")
confusionMatrix(data=pred, reference=r4$improved_rank14_15, positive = "TRUE")
######################################
#############  10-Fold Cross Validation
library(lattice)
library(ggplot2)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
registerDoParallel()
library(rpart)
library(rpart.plot)
library(caret)
fit <- train(improved_rank14_15 ~ ., data=r4, method = "rpart",
             control=rpart.control(minsplit=2),
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=5)
fit


fit$resample
rpart.plot(fit$finalModel, extra = 2, under = TRUE,  varlen=0, faclen=0)
varImp(fit)
varImp(fit, compete = FALSE)
dotPlot(varImp(fit, compete=FALSE))
########################### CV end
######################## Repeated Bootstrap Sampling
fit <- train(improved_rank14_15 ~ ., data=r4, method = "rpart",
             control=rpart.control(minsplit=2),
             na.action = na.pass,
             trControl = trainControl(method = "boot", number = 10),
             tuneLength=5)
fit

############################ Holdout Sample
# Partition data 66%/34%
inTrain <- createDataPartition(y=r4$improved_rank14_15, p = .66, list=FALSE)
training <- r4[ inTrain,]
testing <- r4[-inTrain,]

fit <- train(improved_rank14_15 ~ ., data = training, method = "rpart",
             control=rpart.control(minsplit=2),
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=20)
fit
plot(fit)
# Use the best model on the test data
fit$finalModel
pred <- predict(fit, newdata = testing)
head(pred)

##########################################  Model Comparison         !!!!!!!!!!!!!!!!
####################### rename class variables y
r4$improved_rank14_15 <- as.character(r4$improved_rank14_15)
r4$improved_rank14_15[r4$improved_rank14_15 == "TRUE"] <- "1"
r4$improved_rank14_15[r4$improved_rank14_15 == "FALSE"] <- "0"
r4$improved_rank14_15 <- as.factor(r4$improved_rank14_15)
####################### rename class variables improved_pcp_discretize
r4$improved_pcp_discretize <- as.character(r4$improved_pcp_discretize)
r4$improved_pcp_discretize[r4$improved_pcp_discretize == "TRUE"] <- "1"
r4$improved_pcp_discretize[r4$improved_pcp_discretize == "FALSE"] <- "0"
r4$improved_pcp_discretize <- as.factor(r4$improved_pcp_discretize)
summary(r4)
##################################

########################################################### Feature selection
library(FSelector)
#Univariate Feature Importance Score
weights <- chi.squared(improved_rank14_15 ~ ., data=r4)
weights
#plot importance (ordered)
str(weights)

o <- order(weights$attr_importance)
dotchart(weights$attr_importance[o], labels = rownames(weights)[o],
         xlab = "Importance")
# Get the 5 best features
subset <- cutoff.k(weights, 5)
subset
#Use only the best 5 features to build a model
f <- as.simple.formula(subset, "improved_rank14_15")
f
m <- rpart(f, data=r4)
rpart.plot(m, extra = 2, under = TRUE,  varlen=0, faclen=0)
#####

########################################### Feature Subset Selection
cfs(improved_rank14_15 ~ ., data=r4)
consistency(improved_rank14_15 ~ ., data=r4)
evaluator <- function(subset) {
  m <- train(as.simple.formula(subset, "improved_rank14_15"), data = r4, method = "rpart",
             trControl = trainControl(method = "boot", number = 5), tuneLength = 0)
  results <- m$resample$Accuracy
  print(subset)
  print(mean(results))
  mean(results)
}

features <- names(r4)[1:16]
subset <- backward.search(features, evaluator)

#################################### Dealing With the Class Imbalance Problem
barplot(table(r4$improved_rank14_15), xlab = "improved_rank14_15", ylab="Count")
# Balance Data With Resampling
library(sampling)
library(caret)
id <- strata(r4, stratanames="improved_rank14_15", size=c(100,100), method="srswr")
r4_balanced <- r4[id$ID_unit, ]
table(r4$improved_rank14_15)
fit <- train(improved_rank14_15 ~ ., data = r4_balanced, method = "rpart",
             trControl = trainControl(method = "cv"),
             control = rpart.control(minsplit = 5))
fit

rpart.plot(fit$finalModel, extra = 2, under = TRUE,  varlen = 0, faclen = 0)

confusionMatrix(data = predict(fit, r4_balanced),
                ref = r4_balanced$improved_rank14_15, positive = "TRUE")

confusionMatrix(data = predict(fit, r4),
                ref = r4$improved_rank14_15, positive = "TRUE")

############### Option 3: Build A Larger Tree and use Predicted Probabilities
fit <- train(improved_rank14_15 ~ ., data = r4, method = "rpart",
             tuneLength=20,
             trControl = trainControl(method = "cv",
                                      classProbs = TRUE,                 ## necessary for predict with type="prob"
                                      summaryFunction=twoClassSummary),  ## necessary for ROC
             metric = "ROC",
             control = rpart.control(minsplit = 5))
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)
confusionMatrix(data = predict(fit, r4),
                ref = r4$improved_rank14_15, positive = "TRUE")
####################
prob <- predict(fit, r4, improved_rank14_15 = "TRUE")
tail(prob)

pred <- as.factor(ifelse(prob[,"TRUE"]>=0, "TRUE", "FALSE"))

confusionMatrix(data = pred,
                ref = Zoo_reptile$type, positive = "reptile")

library(ROCR)
plot(performance(ROCRpred, measure = 'tpr', x.measure = 'fpr'))

library(stats)
library("pROC")

p <- roc(r4$improved_rank14_15 == "TRUE", prob[,"TRUE"])
p
plot(p)

citation("pROC")
################################ model
library(lattice)
library(ggplot2)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
registerDoParallel()
getDoParWorkers()
library(rpart)
library(rpart.plot)

library(caret)
library(lattice)
library(ggplot2)
train <- createFolds(r4$improved_rank14_15, k=10)

######################################## Fitting Different Classification Models

# predict on the test data using "best" number of trees
f.predict.test = matrix(predict(gbm1, testdata, best.iter), ncol=1)
colnames(f.predict.test) = "forecastgbm"

# export forecast to a txt file
write.table(f.predict.test, outpath, sep="\t")

f.predict.test = matrix(predict(rm1, newdata=testdata.roughfix), ncol=1)
colnames(f.predict.test) = "forecastrf"

# gradient boosting
library(survival)
library(caret)
library(splines)
library(gbm)

gbm1 =gbm(improved_rank14_15 ~ ., # formula
          data = r4, 
          distribution = "gaussian", 
          n.trees = 5000, # number of trees
          shrinkage = 0.01, # shrinkage or learning rate, 0.001 to 0.1 usually work
          cv.folds = 5) # do 5-fold cross-validation
gbm1
# check performance using 5-fold cross-validation
best.iter = gbm.perf(gbm1,method="cv")
print(best.iter)

# plot variable influence
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees


#Conditional Inference Tree (Decision Tree)
ctreeFit <- train(improved_rank14_15 ~ ., method = "ctree", data = r4,
                  tuneLength = 5,
                  
                  trControl = trainControl(
                    method = "cv", indexOut = train))
ctreeFit
plot(ctreeFit$finalModel)
ctreeFit$finalModel

#C 4.5 Decision Tree
library(RWeka)
C45Fit <- train(improved_rank14_15 ~ ., method = "J48", data = r4,
                tuneLength = 5,
                
                trControl = trainControl(
                  method = "cv", indexOut = train))
C45Fit
C45Fit$finalModel

#PART (Rule-based classifier)
rulesFit <- train(improved_rank14_15 ~ ., method = "PART", data = r4,
                  tuneLength = 5,
                  
                  trControl = trainControl(
                    method = "cv", indexOut = train))
rulesFit
rulesFit$finalModel

#Linear Support Vector Machines
library(ggplot2)
library(kernlab)
svmFit <- train(improved_rank14_15 ~., method = "svmLinear", data = r4,
                tuneLength = 5,
                
                trControl = trainControl(
                  method = "cv", indexOut = train))
svmFit
svmFit$finalModel

# Artificial Neural Network
library(nnet)
nnetFit <- train(improved_rank14_15 ~ ., method = "nnet", data = r4,
                 tuneLength = 5,
                 
                 trControl = trainControl(
                   method = "cv", indexOut = train))
nnetFit
nnetFit$finalModel

# random forest
library(randomForest)
randomForestFit <- train(improved_rank14_15 ~ ., method = "rf", data = r4,
                         tuneLength = 5,
                         
                         importance = TRUE,
                         trControl = trainControl(
                           method = "cv", indexOut = train))
randomForestFit
randomForestFit$finalModel


library(gplots)
library(ROCR)
r4$improved_rank14_15<- as.numeric(r4$improved_rank14_15== "TRUE")
mod <- glm(improved_rank14_15 ~ ., data=r4)
pred1 <- prediction(predict(mod), r4$improved_rank14_15)
plot(performance(pred1, measure = 'tpr', x.measure = 'fpr'))


library(ROCR)
pred1 <- prediction(predict(mod), iris$isv)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1)
##############################################Compare Models
resamps <- resamples(list(

  ctree=ctreeFit,
  C45=C45Fit,
  SVM=svmFit,
  rules=rulesFit,
  NeuralNet=nnetFit,
  randomForest=randomForestFit))
resamps
summary(resamps)
difs <- diff(resamps)
difs
summary(difs)
############################################## model end

##########split the data by university
l <- split(times, times$university_name)
year <- sapply(l, nrow)
l[1:3]
table(year)

############################### improved features
#pick two years for comparison
r <- lapply(l, FUN = function(x) {
  d_2014 <- x[x$year==2014,]
  d_2015 <- x[x$year==2015,]
  merge(d_2014, d_2015, by = "university_name",
        all = TRUE, suffix = c("_X2014", "_X2015"))
})
r <- do.call(rbind, r)
head(r)
summary(r)
colnames(r)
#calculate improvement
########################################### teaching
improved_teaching <- r$'teaching_X2015' - r$'teaching_X2014'
hist(improved_teaching)
hist(improved_teaching, breaks = 100)
improved_teaching
table(improved_teaching, useNA = "always")
# add improvement as a variable 
r$improved_teaching <- as.numeric(improved_teaching)
dim(r)
summary(r)
########################################### international
improved_international <- r$'international_X2015' - r$'international_X2014'
improved_international
table(improved_international, useNA = "always")
# add improvement as a variable 
r$improved_international <- as.numeric(improved_international)
dim(r)
summary(r)
########################################### research
improved_research <- r$'research_X2015' - r$'research_X2014'
improved_research
table(improved_research, useNA = "always")
# add improvement as a variable 
r$improved_research <- as.numeric(improved_research)
dim(r)
summary(r)
########################################### citations
improved_citations <- r$'citations_X2015' - r$'citations_X2014'
improved_citations
table(improved_citations, useNA = "always")
# add improvement as a variable 
r$improved_citations <- as.numeric(improved_citations)
dim(r)
summary(r)
########################################### income
improved_income <- r$'income_X2015' - r$'income_X2014'
improved_income
table(improved_income, useNA = "always")
# add improvement as a variable 
r$improved_income <- as.numeric(improved_income)
dim(r)
summary(r)

########################################## add features end
r[r$university_name == 'Southern Methodist University', ]
r[r$university_name == 'University of Southern California', ]
which(r$university_name == 'University of Southern California')
which(r$university_name == 'Southern Methodist University')
##delete useless features

#delete useless features
r$'university_name' <- NULL
r$'year_X2014' <- NULL
r$'year_X2015' <- NULL
r$'world_rank_X2014' <- NULL
r$'world_rank_X2015' <- NULL
r$'country_X2014' <- NULL
r$'country_X2015' <- NULL
r$'total_score_X2014' <- NULL
r$'total_score_X2015' <- NULL
r$'female_male_ratio_X2014' <- NULL
r$'female_male_ratio_X2015' <- NULL

testdata <- r
colnames(testdata)
summary(testdata)

# impute missing values
for(i in 1:ncol(testdata)){
  testdata[is.na(testdata[,i]), i] <- mean(testdata[,i], na.rm = TRUE)
}

# rename variables
names(testdata)[names(testdata) == 'teaching_X2014'] <- 'teaching_X2013'
names(testdata)[names(testdata) == 'international_X2014'] <- 'international_X2013'
names(testdata)[names(testdata) == 'research_X2014'] <- 'research_X2013'
names(testdata)[names(testdata) == 'citations_X2014'] <- 'citations_X2013'
names(testdata)[names(testdata) == 'income_X2014'] <- 'income_X2013'
names(testdata)[names(testdata) == 'num_students_X2014'] <- 'num_students_X2013'
names(testdata)[names(testdata) == 'student_staff_ratio_X2014'] <- 'student_staff_ratio_X2013'
names(testdata)[names(testdata) == 'international_students_X2014'] <- 'international_students_X2013'
names(testdata)[names(testdata) == 'female_male_ratio_X2014'] <- 'female_male_ratio_X2013'

#
names(testdata)[names(testdata) == 'teaching_X2015'] <- 'teaching_X2014'
names(testdata)[names(testdata) == 'international_X2015'] <- 'international_X2014'
names(testdata)[names(testdata) == 'research_X2015'] <- 'research_X2014'
names(testdata)[names(testdata) == 'citations_X2015'] <- 'citations_X2014'
names(testdata)[names(testdata) == 'income_X2015'] <- 'income_X2014'
names(testdata)[names(testdata) == 'num_students_X2015'] <- 'num_students_X2014'
names(testdata)[names(testdata) == 'student_staff_ratio_X2015'] <- 'student_staff_ratio_X2014'
names(testdata)[names(testdata) == 'international_students_X2015'] <- 'international_students_X2014'
names(testdata)[names(testdata) == 'female_male_ratio_X2015'] <- 'female_male_ratio_X2014'

summary(testdata)
############################### predict on the test data

testdata.roughfix = na.roughfix(testdata)
f.predict.test = matrix(predict(randomForestFit, newdata=testdata.roughfix), ncol=1)
colnames(f.predict.test) = "forecastrf"
f.predict.test


# export forecast to a txt file
outpath = "C:/p/forecastrf.txt"
write.table(f.predict.test, outpath, sep="\t")





