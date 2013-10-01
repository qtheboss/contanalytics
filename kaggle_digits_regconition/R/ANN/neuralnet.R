# Digital Recognizer with Neural network
# 23/9/2013
# 

library(neuralnet)
library(psych)

library(FactoMineR)



train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

n <- 5000
labels <- train[1:n,1]
traininginput <- train[1:n,-1]
 

# try with PCA

pca3  <- PCA(traininginput, graph=FALSE)

# convert data to matrix

datatrain <- as.matrix(pca3$ind$coord)
trainingdata <- cbind(datatrain, labels == 0)
trainingdata <- cbind(trainingdata, labels == 1)
trainingdata <- cbind(trainingdata, labels == 2)
trainingdata <- cbind(trainingdata, labels == 3)
trainingdata <- cbind(trainingdata, labels == 4)
trainingdata <- cbind(trainingdata, labels == 5)
trainingdata <- cbind(trainingdata, labels == 6)
trainingdata <- cbind(trainingdata, labels == 7)
trainingdata <- cbind(trainingdata, labels == 8)
trainingdata <- cbind(trainingdata, labels == 9)
colnames(trainingdata)[6] <- 'label0'
colnames(trainingdata)[7] <- 'label1'
colnames(trainingdata)[8] <- 'label2'
colnames(trainingdata)[9] <- 'label3'
colnames(trainingdata)[10] <- 'label4'
colnames(trainingdata)[11] <- 'label5'
colnames(trainingdata)[12] <- 'label6'
colnames(trainingdata)[13] <- 'label7'
colnames(trainingdata)[14] <- 'label8'
colnames(trainingdata)[15] <- 'label9'
ptm <- proc.time()
net.digit <- neuralnet(label0+label1+label2+label3+label4+label5+label6+label7+label8+label9~Dim.1+Dim.2+Dim.3+Dim.4+Dim.5,
                       trainingdata, hidden = 4, act.fct = "logistic")
# train model with neural net
proc.time()

testdata <- train[5001: 5100, -1]
testlabels <- train[5001:5100, 1]
# PCA with data test
pca4  <- PCA(testdata, graph=FALSE)

# convert data test to matrix
datatest <- as.matrix(pca4$ind$coord)

# results test
net.results <- compute(net.digit,datatest)

ls(net.results)

output <- net.results$net.result

maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(output, c(1), maxidx)
idx <- idx-1

cleanoutput <- cbind(testlabels, as.data.frame(idx))
colnames(cleanoutput) <- c("Expected Output", "Neural Net Output")

print (cleanoutput)
c1 <- 0
for(i in 1:100)
{
  if(idx[i]== testlabels[i])
  {
    c1 <- c1+1
  }
  
}
print(c1)

write(results, file="knn_benchmark.csv", ncolumns=1)
