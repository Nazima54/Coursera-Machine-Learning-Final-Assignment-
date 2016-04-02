library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
names(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
str(AlzheimerDisease)

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj

IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess (training[, IL_str], method = "pca", thresh = 0.8)
preProc$rotation)G