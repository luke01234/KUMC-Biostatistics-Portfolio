library("mlbench")
library("tidyverse")
library("caret")

dataset = read.csv('bayes/topS with complex fixed.csv')
dataset = dataset[2:20]

dataset$complex = factor(dataset$complex, levels = c(0, 1))

##Split the data 
set.seed(123)
training.samples <- dataset$complex %>%
  createDataPartition(p=0.8, list=FALSE)
train.data <- dataset[training.samples,]
test.data <- dataset[-training.samples,]

## Computing the Naive Bayes 

library("klaR")
model <-NaiveBayes(complex ~., data=train.data)

#Make prediction
predictions <- model %>% predict(test.data)

#Model accuracy
mean(predictions$class == test.data$complex)

cm = table(test.data$complex, predictions$class)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
precision=cm[1,1]/(cm[1,1]+cm[1,2])
recall=cm[1,1]/(cm[1,1]+cm[2,1])
F_score=2*precision*recall/(precision+recall)

##Using caret package can automatically train the model and asses the model 
##accuracy using k-fold cross validation (CV) 
library("klaR")
set.seed(123)
model <- train(complex ~., data=train.data, method="nb",
               trControl=trainControl("cv",number=10))
print(model)

predicted.classes <-model %>% predict(test.data)

#Model n accuracy
mean(predicted.classes == test.data$complex)

#same accuracy as above 0.86
####################################################################