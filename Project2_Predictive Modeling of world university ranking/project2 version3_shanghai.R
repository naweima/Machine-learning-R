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

############################### improved features
#pick two years for comparison
r <- lapply(l, FUN = function(x) {
  d_2010 <- x[x$year==2010,]
  d_2014 <- x[x$year==2014,]
  merge(d_2010, d_2014, by = "university_name",
        all = TRUE, suffix = c("_X2010", "_X2014"))
})
r <- do.call(rbind, r)
head(r)
summary(r)
#calculate improvement
########################################### alumni
improved_alumni <- r$'alumni_X2014' - r$'alumni_X2010'
hist(improved_alumni)
hist(improved_alumni, breaks = 100)
improved_alumni
table(improved_alumni, useNA = "always")
# add improvement as a variable 
r$improved_alumni <- as.numeric(improved_alumni)
dim(r)
summary(r)
########################################### award
improved_award <- r$'award_X2014' - r$'award_X2010'
hist(improved_award)
hist(improved_award, breaks = 100)
improved_award
table(improved_award, useNA = "always")
# add improvement as a variable 
r$improved_award <- as.numeric(improved_award)
dim(r)
summary(r)
########################################### hici
improved_hici <- r$'hici_X2014' - r$'hici_X2010'
hist(improved_hici)
hist(improved_hici, breaks = 100)
improved_hici
table(improved_hici, useNA = "always")
# add improvement as a variable 
r$improved_hici <- as.numeric(improved_hici)
dim(r)
summary(r)
########################################### ns
improved_ns <- r$'ns_X2014' - r$'ns_X2010'
hist(improved_ns)
hist(improved_ns, breaks = 100)
improved_ns
table(improved_ns, useNA = "always")
# add improvement as a variable 
r$improved_ns <- as.numeric(improved_ns)
dim(r)
summary(r)
########################################### pub
improved_pub <- r$'pub_X2014' - r$'pub_X2010'
hist(improved_pub)
hist(improved_pub, breaks = 100)
improved_pub
table(improved_pub, useNA = "always")
# add improvement as a variable 
r$improved_pub <- as.numeric(improved_pub)
dim(r)
summary(r)
########################################### pcp
improved_pcp <- r$'pcp_X2014' - r$'pcp_X2010'
hist(improved_pcp)
hist(improved_pcp, breaks = 100)
improved_pcp
table(improved_pcp, useNA = "always")
# add improvement as the  variable 
r$improved_pcp<- as.numeric(improved_pcp)
dim(r)
summary(r)
########################################### aggregate
improved_aggregate <- r$'improved_alumni' + r$'improved_award'+ r$'improved_hici'+ r$'improved_ns'+ r$'improved_pub'
hist(improved_aggregate)
hist(improved_aggregate, breaks = 100)
improved_aggregate
table(improved_aggregate, useNA = "always")
# add improvement as a variable 
r$improved_aggregate <- as.numeric(improved_aggregate)
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
##delete useless features
r4 <- r3[, -c(29:48)]
#delete useless features
r4$'university_name' <- NULL
r4$'year_X2014' <- NULL
r4$'year_X2010' <- NULL
r4$'world_rank_X2014' <- NULL
r4$'world_rank_X2010' <- NULL
r4$'national_rank_X2014' <- NULL
r4$'national_rank_X2010' <- NULL
r4$'total_score_X2014' <- NULL
r4$'total_score_X2010' <- NULL
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
##########################################  Model Comparison        
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


#Conditional Inference Tree (Decision Tree)
ctreeFit <- train(improved_rank14_15 ~ ., method = "ctree", data = r4,
                  tuneLength = 5,
            
                  trControl = trainControl(
                    method = "cv", indexOut = train))
ctreeFit
ctreeFit$finalModel
plot(ctreeFit$finalModel)

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

set.seed(1)
rm1 = randomForest(improved_rank14_15 ~ ., # formula
                   data = r4, 
                   na.action=na.roughfix,
                   n.trees = 5000, # number of trees
                   mtry = floor(254/3), # Number of variables randomly sampled as candidates at each split
                   importance = TRUE) # show importance of variables

# plot variable influence
importance(rm1)
varImpPlot(rm1)

importance(randomForestFit)
varImpPlot(randomForestFit)
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


################################ create test data 2015-2016
l <- split(shanghai, shanghai$university_name)
years <- sapply(l, nrow)
l[1:3]
table(years)
############################### improved features
#pick two years for comparison
r <- lapply(l, FUN = function(x) {
  d_2011 <- x[x$year==2011,]
  d_2015 <- x[x$year==2015,]
  merge(d_2011, d_2015, by = "university_name",
        all = TRUE, suffix = c("_X2011", "_X2015"))
})
r <- do.call(rbind, r)
head(r)
#calculate improvement
########################################### alumni
improved_alumni <- r$'alumni_X2015' - r$'alumni_X2011'
hist(improved_alumni)
hist(improved_alumni, breaks = 100)
improved_alumni
table(improved_alumni, useNA = "always")
# add improvement as a variable 
r$improved_alumni <- as.numeric(improved_alumni)
dim(r)
summary(r)
########################################### award
improved_award <- r$'award_X2015' - r$'award_X2011'
hist(improved_award)
hist(improved_award, breaks = 100)
improved_award
table(improved_award, useNA = "always")
# add improvement as a variable 
r$improved_award <- as.numeric(improved_award)
dim(r)
summary(r)
########################################### hici
improved_hici <- r$'hici_X2015' - r$'hici_X2011'
hist(improved_hici)
hist(improved_hici, breaks = 100)
improved_hici
table(improved_hici, useNA = "always")
# add improvement as a variable 
r$improved_hici <- as.numeric(improved_hici)
dim(r)
summary(r)
########################################### ns
improved_ns <- r$'ns_X2015' - r$'ns_X2011'
hist(improved_ns)
hist(improved_ns, breaks = 100)
improved_ns
table(improved_ns, useNA = "always")
# add improvement as a variable 
r$improved_ns <- as.numeric(improved_ns)
dim(r)
summary(r)
########################################### pub
improved_pub <- r$'pub_X2015' - r$'pub_X2011'
hist(improved_pub)
hist(improved_pub, breaks = 100)
improved_pub
table(improved_pub, useNA = "always")
# add improvement as a variable 
r$improved_pub <- as.numeric(improved_pub)
dim(r)
summary(r)
########################################### pcp
improved_pcp <- r$'pcp_X2015' - r$'pcp_X2011'
hist(improved_pcp)
hist(improved_pcp, breaks = 100)
improved_pcp
table(improved_pcp, useNA = "always")
# add improvement as the  variable 
r$improved_pcp<- as.numeric(improved_pcp)
dim(r)
summary(r)
########################################### aggregate
improved_aggregate <- r$'improved_alumni' + r$'improved_award'+ r$'improved_hici'+ r$'improved_ns'+ r$'improved_pub'
hist(improved_aggregate)
hist(improved_aggregate, breaks = 100)
improved_aggregate
table(improved_aggregate, useNA = "always")
# add improvement as a variable 
r$improved_aggregate <- as.numeric(improved_aggregate)
dim(r)
summary(r)

r[r$university_name == 'Southern Methodist University', ]
r[r$university_name == 'University of Southern California', ]
which(r$university_name == 'University of Southern California')
which(r$university_name == 'Southern Methodist University')

########################################## add features end

##delete useless features

#delete useless features
r$'university_name' <- NULL
r$'year_X2011' <- NULL
r$'year_X2015' <- NULL
r$'world_rank_X2011' <- NULL
r$'world_rank_X2015' <- NULL
r$'national_rank_X2011' <- NULL
r$'national_rank_X2015' <- NULL
r$'total_score_X2011' <- NULL
r$'total_score_X2015' <- NULL
testdata <- r
colnames(testdata)
summary(testdata)

# impute missing values
for(i in 1:ncol(testdata)){
  testdata[is.na(testdata[,i]), i] <- mean(testdata[,i], na.rm = TRUE)
}

# rename variables
names(testdata)[names(testdata) == 'alumni_X2011'] <- 'alumni_X2010'
names(testdata)[names(testdata) == 'award_X2011'] <- 'award_X2010'
names(testdata)[names(testdata) == 'hici_X2011'] <- 'hici_X2010'
names(testdata)[names(testdata) == 'ns_X2011'] <- 'ns_X2010'
names(testdata)[names(testdata) == 'pub_X2011'] <- 'pub_X2010'
names(testdata)[names(testdata) == 'pcp_X2011'] <- 'pcp_X2010'
#
names(testdata)[names(testdata) == 'alumni_X2015'] <- 'alumni_X2014'
names(testdata)[names(testdata) == 'award_X2015'] <- 'award_X2014'
names(testdata)[names(testdata) == 'hici_X2015'] <- 'hici_X2014'
names(testdata)[names(testdata) == 'ns_X2015'] <- 'ns_X2014'
names(testdata)[names(testdata) == 'pub_X2015'] <- 'pub_X2014'
names(testdata)[names(testdata) == 'pcp_X2015'] <- 'pcp_X2014'
summary(testdata)
############################### predict on the test data
testdata.roughfix = na.roughfix(testdata)
f.predict.test = matrix(predict(randomForestFit, newdata=testdata.roughfix), ncol=1)
colnames(f.predict.test) = "forecastrf"
f.predict.test


# export forecast to a txt file
outpath = "C:/p/forecastrf.txt"
write.table(f.predict.test, outpath, sep="\t")
































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



