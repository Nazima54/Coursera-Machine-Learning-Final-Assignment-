library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

dim(vowel.train)
dim(vowel.train)
names(vowel.train)
names(vowel.test)
 
set.seed(33833)
modeltrain <- train(y ~ ., method = "rf",data = vowel.train, importance = TRUE)
varImp(modeltrain)
modeltest <- train (y ~.,method = "rf",data = vowel.test,importance = TRUE)
varImp(modeltest)
