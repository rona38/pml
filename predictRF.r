setwd("c:/pml")
library(caret)

# load data
trainRawData <- read.csv("pml-training.csv",na.strings=c("NA",""))

# make training set
trainData<-trainRawData[ , !apply(trainRawData, 2, function(x) any(is.na(x)) ) ]
trainIndex <- createDataPartition(y = trainData$classe, p=0.7,list=FALSE) # 70% of the data
trainData <- trainData[trainIndex,]

# remove unuseful columns
removeindex<- grep("timestamp|X|user_name|new_window|num_window", colnames(trainData))
trainData <- trainData[,-removeindex]

# make RF model
modRFFit <- train(trainData$classe ~.,data = trainData,method="rf", trControl=cv_opts)
modRFFit

# load test data
testRawData <- read.csv("pml-testing.csv",na.strings=c("NA",""))
testData<-testRawData[ , !apply(testRawData, 2, function(x) any(is.na(x)) ) ]
testData <- testData[,-removeIndex]

# answers
answers <- predict(modRFFit, testData)