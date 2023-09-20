library("mlbench")
library("tidyverse")
library("caret")
library(rpart)
library(randomForest)
library(xgboost)

#Load the data
dataset = read.csv("topS with complex.csv")
dataset = dataset[2:20]
sample_n(dataset,3)
dataset <-na.omit(dataset)

##Split the data 
set.seed(123)
training.samples <- dataset$complex %>%
  createDataPartition(p=0.8, list=FALSE)
train.data <- dataset[training.samples,]
test.data <- dataset[-training.samples,]

set.seed(123)
model <- train(complex ~., data=train.data, method="xgbTree",
               trControl=trainControl("cv",number=10))
model$bestTune
model$finalModel
print(model)

predicted.classes <-model %>% predict(test.data)

#Model n accuracy
mean(predicted.classes == test.data$complex)

###Variable importance###########################################

#importance(model$finalModel)
#varImp(model)

################## Regression###########################################
