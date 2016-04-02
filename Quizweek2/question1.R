library(ggplot2)
library(ISLR)
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
library(plyr)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
dim(training)
summary(training)
dim(testing)
summary(training)

#featurePlot(x = training[,c("index","FlyAsh","Age")],y = training$CompressiveStrength, plot="pairs")

#training <- mutate(training, index=1:nrow(training))
#cutIndex <- cut2(training$index, g=10)
#breaks <- 10
#a <- qplot(index, CompressiveStrength, data=training, color= cut2(training$Cement, g=breaks))
#b <-qplot(index, CompressiveStrength, data=training, color= cut2(training$BlastFurnaceSlag, g=breaks))
#c <- qplot(index, CompressiveStrength, data=training, color= cut2(training$FlyAsh, g=breaks))
#d <- qplot(index, CompressiveStrength, data=training, color= cut2(training$Water, g=breaks))
#e <- qplot(index, CompressiveStrength, data=training, color= cut2(training$Superplasticizer, g=breaks)) 
#f <- qplot(index, CompressiveStrength, data=training, color= cut2(training$CoarseAggregate, g=breaks))
#g <- qplot(index, CompressiveStrength, data=training, color= cut2(training$FineAggregate, g=breaks))
#h <- qplot(index, CompressiveStrength, data=training, color= cut2(training$Age, g=breaks))
#i <- qplot(index, CompressiveStrength, data=training, color= cut2(training$CompressiveStrength, g=breaks))
#j <- qplot(index, CompressiveStrength, data=training, color= cut2(training$index, g=breaks))

hist(as.numeric(training$superplasticizer),breaks=20)



