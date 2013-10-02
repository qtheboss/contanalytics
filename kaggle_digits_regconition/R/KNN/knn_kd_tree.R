# makes the KNN submission

library(FNN)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- train[,1]
train <- train[,-1]

results <- (0:9)[knn(train, test, labels, k = 10, algorithm="kd_tree")]

write(results, file="kd_tree.csv", ncolumns=1) 

