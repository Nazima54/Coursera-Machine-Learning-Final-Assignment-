---
title: "Fitness Activity Impact Prediction"
author: "Coursera Student"

output: html_document
---
## Objective

**Instructions**

One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.


**Background of the Project**

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

**Data**

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

**Goal of the Project**

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.






```r
#Load the required Packages.
library(knitr)
library(markdown)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)
library(ElemStatLearn)
knit("C:/Users/MohammedAbdul/Desktop/Coursera/MachineLearning/FitnessActivityImpact.Rmd")  # produces the md file
```

```
## 
## 
## processing file: C:/Users/MohammedAbdul/Desktop/Coursera/MachineLearning/FitnessActivityImpact.Rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |..                                                               |   4%
##   ordinary text without R code
## 
##   |                                                                         |.....                                                            |   8%
## label: unnamed-chunk-170
```

```
##   |                                                                         |........                                                         |  12%
##   ordinary text without R code
## 
##   |                                                                         |..........                                                       |  15%
## label: unnamed-chunk-171
##   |                                                                         |............                                                     |  19%
##   ordinary text without R code
## 
##   |                                                                         |...............                                                  |  23%
## label: unnamed-chunk-172
##   |                                                                         |..................                                               |  27%
## label: unnamed-chunk-173
##   |                                                                         |....................                                             |  31%
##   ordinary text without R code
## 
##   |                                                                         |......................                                           |  35%
## label: unnamed-chunk-174
##   |                                                                         |.........................                                        |  38%
##   ordinary text without R code
## 
##   |                                                                         |............................                                     |  42%
## label: unnamed-chunk-175
##   |                                                                         |..............................                                   |  46%
##   ordinary text without R code
## 
##   |                                                                         |................................                                 |  50%
## label: unnamed-chunk-176
##   |                                                                         |...................................                              |  54%
##   ordinary text without R code
## 
##   |                                                                         |......................................                           |  58%
## label: unnamed-chunk-177
```

```
##   |                                                                         |........................................                         |  62%
##   ordinary text without R code
## 
##   |                                                                         |..........................................                       |  65%
## label: unnamed-chunk-178
##   |                                                                         |.............................................                    |  69%
##   ordinary text without R code
## 
##   |                                                                         |................................................                 |  73%
## label: unnamed-chunk-179
##   |                                                                         |..................................................               |  77%
##   ordinary text without R code
## 
##   |                                                                         |....................................................             |  81%
## label: unnamed-chunk-180
##   |                                                                         |.......................................................          |  85%
##   ordinary text without R code
## 
##   |                                                                         |..........................................................       |  88%
## label: unnamed-chunk-181
##   |                                                                         |............................................................     |  92%
##   ordinary text without R code
## 
##   |                                                                         |..............................................................   |  96%
## label: unnamed-chunk-182
##   |                                                                         |.................................................................| 100%
##   ordinary text without R code
```

```
## output file: FitnessActivityImpact.md
```

```
## [1] "FitnessActivityImpact.md"
```



```r
#Load the seed for any Random Number Generation
set.seed(3241)
```
**Getting the data from the source**


```r
# Load and read  the dataset 

# set the URL for the download
UrlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
UrlTest  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# download the datasets
train <- read.csv(url(UrlTrain))
test  <- read.csv(url(UrlTest))
dim(train)
```

```
## [1] 19622   160
```

```r
dim(test)
```

```
## [1]  20 160
```

```r
# Find the  observations with  total missing values
sum(complete.cases(train))
```

```
## [1] 406
```

```r
# This command gives the colums with names and its number of NA's values
#colSums(is.na(train))
```


```r
#clean the columns containing missing NA's

training <- train[, colSums(is.na(train)) == 0] 
testing <- test[, colSums(is.na(test)) == 0] 
```


```r
#Get rid of columns with all NA's from the new data set

classe <- training$classe
Removedtrain <- grepl("^X|timestamp|window", names(training))
Newtrain <- training[, ! Removedtrain]
Cleanedtrain <- training[, sapply(training, is.numeric)]
Cleanedtrain$classe <- classe
Removedtest <- grepl("^X|timestamp|window", names(testing))
Newtest <- testing[, !Removedtest]
Cleanedtest <- testing[, sapply(testing, is.numeric)]
dim(Cleanedtrain)
```

```
## [1] 19622    57
```

```r
dim(Cleanedtest)
```

```
## [1] 20 57
```

**Slice the training dataset into 70:20**


```r
inTrain <- createDataPartition(y = Cleanedtrain$classe, p=0.70, list=F)
trainingData <- Cleanedtrain[inTrain,]
testingData <- Cleanedtrain[-inTrain,]
```

**ModelFit Data**


```r
# show fancyRpartPlot
modfit <- train(classe ~.,method="rpart",data = trainingData)
print(modfit$finalModel)
```

```
## n= 13737 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 13737 9831 A (0.28 0.19 0.17 0.16 0.18)  
##   2) X< 5581 3906    0 A (1 0 0 0 0) *
##   3) X>=5581 9831 7173 B (0 0.27 0.24 0.23 0.26)  
##     6) X< 9377.5 2658    0 B (0 1 0 0 0) *
##     7) X>=9377.5 7173 4648 E (0 0 0.33 0.31 0.35) *
```

```r
fancyRpartPlot(modfit$finalModel) 
```

![plot of chunk unnamed-chunk-164](figure/unnamed-chunk-164-1.png)

**Implement RandomForest Algorithm using 5 fold cross validation**


```r
#Predict model using Random Forest Algorithm
Rf <- trainControl(method="cv", 5)
Rfmodel <- train(classe ~ ., data=trainingData, method="rf", trControl= Rf, ntree=250)
Rfmodel
```

```
## Random Forest 
## 
## 13737 samples
##    56 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10988, 10989, 10991, 10991, 10989 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD   Kappa SD    
##    2    0.9984712  0.9980663  0.0005983137  0.0007568213
##   29    1.0000000  1.0000000  0.0000000000  0.0000000000
##   56    0.9999272  0.9999079  0.0001627415  0.0002058324
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 29.
```
**Using the Validation set predict the performance of the model**

```r
Rfpredict <- predict(Rfmodel, testingData)
confusionMatrix(testingData$classe, Rfpredict)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    1 1138    0    0    0
##          C    0    0 1026    0    0
##          D    0    0    0  964    0
##          E    0    0    0    0 1082
## 
## Overall Statistics
##                                      
##                Accuracy : 0.9998     
##                  95% CI : (0.9991, 1)
##     No Information Rate : 0.2846     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 0.9998     
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9994   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   0.9998   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   0.9991   1.0000   1.0000   1.0000
## Neg Pred Value         0.9998   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2846   0.1934   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1934   0.1743   0.1638   0.1839
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9997   0.9999   1.0000   1.0000   1.0000
```


```r
accuracy <- postResample(Rfpredict,testingData$classe)
accuracy
```

```
##  Accuracy     Kappa 
## 0.9998301 0.9997851
```


```r
finalPredict <- 1 - as.numeric(confusionMatrix(testingData$classe,Rfpredict)$overall[1])
finalPredict
```

```
## [1] 0.0001699235
```
 
**Estimated Accuracy is 99.97% and out of samplet error is 0.16%**

**Test the Dataset**

```r
Finalresult <- predict(Rfmodel, Cleanedtest[, -length(names(Cleanedtest))])
Finalresult
```

```
##  [1] A A A A A A A A A A A A A A A A A A A A
## Levels: A B C D E
```

##Reference

Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6. Cited by 2 (Google Scholar)

**Read more: http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz3jOpnStGb**









