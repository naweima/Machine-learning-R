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

# delete rows with missing values
times <- na.omit(times)
summary(times)
##################################################data preprocessing done

#Discretize all other continuous variables
#for(i in which(sapply(times, is.numeric)))
#   times[[i]] <- discretize(times[[i]], method = "frequency", categories = 5,
#                            labels = c("very low", "low", "average", "high", "very high"))
# 
# summary(times)
colnames(times)
dim(times)
################################# data processing done

# Select some data for clustering
data <- times[,c("institution","world_rank","national_rank","quality_of_education",
                 "alumni_employment","quality_of_faculty","publications" ,
                 "influence","citations","broad_impact","patents"   )]
data[,-1] <- scale(data[,-1])
data <- na.omit(times)
summary(data)

pairs(data[,-1])

# Create clusters
km <- kmeans(data[,-1], centers = 3)
plot(data[,c("publications", "influence")], col = km$cluster)

library("GGally")
library(scales)

