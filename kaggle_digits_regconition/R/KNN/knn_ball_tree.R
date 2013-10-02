# makes the KNN submission

library(FNN)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

label <- train[,1]
train <- train[,-1]

# sample first 1000 labels
sample.label <- label[1:1000]

# sample first 1000 training data
sample.train <- train[1:1000,]

# get next 100 datum for test
sample.test <- train[1001:1100,]

# run sample data
#results <- (0:9)[knn(sample.train, sample.test, sample.label, k = 10, algorithm="kd_tree")]

# Start the clock
ptm <- proc.time()

# run all data
results <- (0:9)[knn(train, test, label, k = 10, algorithm="kd_tree")]

# Stop the clock
proc.time() - ptm

id <- c(1:length(results))
output <- cbind(id, results)
colnames(output) <- c("ImageId", "Label")

write.csv(output, file="knn_kd_tree.csv", row.names = FALSE, quote = FALSE) 
