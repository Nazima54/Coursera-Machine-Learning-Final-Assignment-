setwd("C:\\Users\\MohammedAbdul\\Desktop\\Coursera\\Machine Learning")

library(caret)
library(kernlab)


trainingData = as.data.frame(read.csv(file = "pml-training.csv"))
dim(trainingData)
set.seed(1234)
modelFit1 <- train(type ~.,data=trainingData, method="glm")
modelFit1


testingData <- read.csv("pml-testing.csv")
dim(testingData)
modelFit2 <- train(type ~.,data = testingData, methos = "glm")


