# Random Forest Classification

# Importing the dataset
dataset = read.csv('input_file2_ML.csv')
dataset = dataset[2:20]

# Encoding the target feature as factor
dataset$complex = factor(dataset$complex, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$complex, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

finalCol = ncol(dataset)

# Feature Scaling
training_set[-finalCol] = scale(training_set[-finalCol])
test_set[-finalCol] = scale(test_set[-finalCol])

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-finalCol],
                          y = training_set$complex,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-finalCol])

# Making the Confusion Matrix
cm = table(test_set[, finalCol], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
precision=cm[1,1]/(cm[1,1]+cm[1,2])
recall=cm[1,1]/(cm[1,1]+cm[2,1])
F_score=2*precision*recall/(precision+recall)

# Choosing the number of trees
plot(classifier)
