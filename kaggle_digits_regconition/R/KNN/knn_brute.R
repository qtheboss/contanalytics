# makes the KNN submission

library(FNN)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

labels <- train[,1]
train <- train[,-1]

ptm <- proc.time()
results <- (0:9)[knn(train, test, labels, k = 10, algorithm="brute")]
proc.time() - ptm

id <- c(1:length(results))
output <- cbind(id, results)
colnames(output) <- c("ImageId", "Label")

write.csv(output, file="knn_brute.csv", row.names = FALSE, quote = FALSE) 