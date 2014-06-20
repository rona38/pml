
#set working directory
setwd(DataDir)

#load caret
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

# cross validation data
cvData <- trainData[-trainIndex,]

# make CART model
cv_opts = trainControl(method = "cv", number = 4)
modCARTFit <- train(trainData$classe ~.,data = trainData,method="rpart", trControl=cv_opts)
modCARTFit

# make RF model
modRFFit <- train(trainData$classe ~.,data = trainData,method="rf", trControl=cv_opts)
modRFFit

# make Boosting model
modGBMFit <- train(trainData$classe ~.,data = trainData,method="gbm", trControl=cv_opts)
modGBMFit


# make model based prediction
modLDA <- train(trainData$classe ~.,data = trainData,method="lda", trControl=cv_opts)
modNB  <- train(trainData$classe ~.,data = trainData,method="nb", trControl=cv_opts)


# comparison 
print ("CART")
cvResultsCART<-predict(modCARTFit,cvData)
confusionMatrix(cvResultsCART, cvData$classe)

print("Random Forest")
cvResultsRF<-predict(modRFFit,cvData)
confusionMatrix(cvResultsRF, cvData$classe)

print("General Boosting Mode")
cvResultsGBM<-predict(modGBMFit,cvData)
confusionMatrix(cvResultsGBM, cvData$classe)

print("model bases predition
plda<-predict(modLDA,cvData)
plnb<-predict(modNB,cvData)
confusionMatrix(plda, plnb)
