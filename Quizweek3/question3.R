# week 3 quiz question 3

library(pgmm)
data(olive)
names(olive)

dim(olive)
# These data contain information on 572 different Italian olive oils from multiple regions in Italy. Fit a classification tree where Area is the outcome variable. Then predict the value of area for the following data frame using the tree command with all defaults
olive = olive[,-1]
modfit <- train(Area ~.,method="rpart",data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modfit,newdata)