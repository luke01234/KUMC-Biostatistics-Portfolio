# K-Nearest Neighbors (K-NN)

# Importing the dataset
dataset = read.csv('bayes/topS with complex fixed.csv')
dataset = dataset[2:20]
finalCol = ncol(dataset)

# Encoding the target feature as factor
dataset$complex = factor(dataset$complex, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$complex, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-finalCol] = scale(training_set[-finalCol])
test_set[-finalCol] = scale(test_set[-finalCol])

# Fitting K-NN to the Training set and Predicting the Test set results on independent var
library(class)
y_pred = knn(train = training_set[, -finalCol],
             test = test_set[, -finalCol],
             cl = training_set[, finalCol],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm = table(test_set[, finalCol], y_pred)
accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
precision=cm[1,1]/(cm[1,1]+cm[1,2])
recall=cm[1,1]/(cm[1,1]+cm[2,1])
F_score=2*precision*recall/(precision+recall)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -finalCol], test = grid_set, cl = training_set[, finalCol], k = 5)
plot(set[, -finalCol],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, finalCol] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -finalCol], test = grid_set, cl = training_set[, finalCol], k = 5)
plot(set[, -finalCol],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, finalCol] == 1, 'green4', 'red3'))