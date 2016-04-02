## week 3 quiz question 1

library(ggplot2)
library(AppliedPredictiveModeling)
library(rattle)
data(segmentationOriginal)
names(segmentationOriginal)
library(caret)
#1. Subset the data to a training set and testing set based on the Case variable in the data set.

#2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.

#3. In the final model what would be the final model prediction for cases with the following variable values:
set.seed(125)
table(segmentationOriginal$TotalIntenCh2)
inTrain = createDataPartition(y = segmentationOriginal$Class , p = 3/4,list = FALSE)
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[-inTrain,]
dim(training)
dim(testing)

modfit <- train(Class ~.,method="rpart",data = training)
print(modfit$finalModel)
fancyRpartPlot(modfit$finalModel) 
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100

# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100

# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
