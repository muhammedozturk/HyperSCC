###This code is designed to obtain average prediction success of XGBoost
##and random forest for one-class programming language prediction
###########xgboost process##################
library(caret)
library(tidyverse)
library("xgboost")


###get 300 random instances
set.seed(123)
randomN <- sample(1:1000,300)
dataGroup <- read.csv("processedPosts.csv")
dataGroup <- dataGroup[,-c(1:2)]


fileM1 <- dataGroup[,1:50]
fileM2 <- dataGroup[,10557:10577]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[randomN,]


###The code part below is devised for y17
####repat the operation for y1:y21
trainIndex <- createDataPartition(fileM$y17,p=.7,list=FALSE)
trainData <- fileM[trainIndex,]
testData  <- fileM[-trainIndex,]
trainData$y17[trainData$y17==1] <- 'T'
trainData$y17[trainData$y17==0] <- 'F'
trainData$y17 <- as.factor(trainData$y17)
testData$y17[testData$y17==1] <- 'T'
testData$y17[testData$y17==0] <- 'F'
testData$y17 <- as.factor(testData$y17)
tune_grid <- expand.grid(
					nrounds= c(50, 75, 100), 
                         max_depth = c(3:7),
                         eta = c(0.05,0.3, 0.075),
                         gamma = c(0.01),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         subsample = c(0.50),
                          min_child_weight = c(2.0, 2.25)
)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(y17~., data=trainData, method="xgbTree", trControl=control,tuneGrid=tune_grid)
plot(model)
predictions <- predict(model,testData)
result <- confusionMatrix(predictions,testData$y17)


########################################
#######################################





fileM <- dataGroup

trainIndex <- createDataPartition(fileM$y19,p=.7,list=FALSE)
trainData <- fileM[trainIndex,]
testData  <- fileM[-trainIndex,]



bstDense <- xgboost(data = as.matrix(trainData), label = trainData$y19, max.depth = 3, eta = 0.04, nthread = 2, nrounds = 50, objective = "binary:logistic")
pred <- predict(bstDense, as.matrix(testData))
  pred = ifelse(pred > 0.5, "1", "0")
    pred = as.factor(pred)
pred <- as.factor(pred)
result <- confusionMatrix(pred,as.factor(testData$y19))

#####randomForest process##################################
library("randomForest")


###get 100 random instances
set.seed(123)
randomN <- sample(1:1000,100)
dataGroup <- read.csv("islenmisDosya3.csv")
dataGroup <- dataGroup[,-c(1:2)]

###############################
fileM1 <- dataGroup[,1:50]
fileM2 <- dataGroup[,10557:10577]
fileM <- data.frame(fileM1,fileM2)
randomN <- sample(1:5000,1000)
fileM <- fileM[randomN,]
trainIndex <- createDataPartition(fileM$y21,p=.7,list=FALSE)
trainData <- fileM[trainIndex,]
testData  <- fileM[-trainIndex,]
trainData$y21[trainData$y21==1] <- 'T'
trainData$y21[trainData$y21==0] <- 'F'
trainData$y21 <- as.factor(trainData$y21)
testData$y21[testData$y21==1] <- 'T'
testData$y21[testData$y21==0] <- 'F'
testData$y21 <- as.factor(testData$y21)
ntrees<-c(250)
grid <- expand.grid(mtry=c(1:3))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
model <- train(y21~., data=trainData, method="rf", trControl=control,tuneGrid=grid)
plot(model)
predictions <- predict(model,testData)
result <- confusionMatrix(predictions,as.factor(testData$y21))

################################