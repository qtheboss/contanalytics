# makes the random forest submission

library(randomForest)

train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

label <- as.factor(train[,1])
train <- train[,-1]

# Start the clock
ptm <- proc.time()

rf <- randomForest(train, label, xtest=test, ntree=1000)

# Stop the clock
proc.time() - ptm

predictions <- levels(label)[rf$test$predicted]

id <- c(1:length(predictions))
output <- cbind(id, predictions)
colnames(output) <- c("ImageId", "Label")


write.csv(output, file="random_forest.csv", row.names = FALSE, quote = FALSE) 
