# Day: 23/10/2013
# Author: Big data team
# Some functions for main 
# 

calc_accuracy <- function(predict, actual, lowest) {
  n = length(predict)
  sum = 0
  for (i in 1:n) {
    sum = sum + (log(predict[i] + lowest + 1 ) - log(actual[i] + lowest + 1)) ^ 2
  }
  e = sqrt(sum/n)
  return (e)
}  
# function combine row data train with data test
combine_table = function(file_name_train, file_name_test) {
  data_train = read.csv(file_name_train, stringsAsFactors = FALSE)
  data_test = read.csv(file_name_test, stringsAsFactors = FALSE)
  
  data_train$Sales. = as.numeric(gsub(",", "", data_train$Sales.))
  Sales = data_train$Sales.
  data_train = data_train[, -7]
  data_train = cbind(data_train, Sales)
  colnames(data_train) = c("Year.Month", "Market", "Product.Market.Combi", "Application.Code", "Sales.Area", "PdtLine", "Cust", "Material", "Sales")
  colnames(data_test) = c("Year.Month", "Market", "Product.Market.Combi", "Application.Code", "Sales.Area", "PdtLine", "Cust", "Material", "Sales")
  
  year = substring(data_train$Year.Month, 1, 4)
  month = substring(data_train$Year.Month, 5, 6)  
  data_train = data_train[, -1]
  data_train = cbind(year, month, data_train)
  
  year = substring(data_test$Year.Month, 1, 4)
  month = substring(data_test$Year.Month, 5, 6)  
  data_test = data_test[, -1]
  data_test = cbind(year, month, data_test)
  
  return (data = rbind(data_train, data_test))
}
# Encode function to prepare for dummy coding
encode_data = function(data) {

  # change variables to factor 
  data$Market = factor(data$Market)
  data$Sales.Area = factor(data$Sales.Area)
  data$PdtLine = factor(data$PdtLine)
  data$Product.Market.Combi = factor(data$Product.Market.Combi)
  data$Application.Code = factor(data$Application.Code)
  data$Cust = factor(data$Cust)
  data$Material = factor(data$Material)
  # chang variables to numeric
  data$Market = as.numeric(data$Market)
  data$Product.Market.Combi = as.numeric(data$Product.Market.Combi)
  data$PdtLine = as.numeric(data$PdtLine)
  data$Sales.Area = as.numeric(data$Sales.Area)
  data$Application.Code = as.numeric(data$Application.Code)
  data$Cust = as.numeric(data$Cust)
  data$Material = as.numeric(data$Material)
  
  
  return (data)
}

# Dummy coding for categorical variables
dummy_coding = function(data, col_name) {
  num_row = nrow(data)
  num_col = length(unique(col_name))
  labels = unique(col_name)
  my_mat = matrix(rep(0, num_row * num_col), num_row, num_col)
  
  col_name = as.numeric(col_name)
  for (i in 1 : nrow(data)) {
    for (j in 1 : length(labels)) {
      if (col_name[i] == j) {
        my_mat[i, j] = 1
      }
    }
  }
  return (my_mat)
}

# if prediction value <0 we need to convert prediction value = 0
convert_prediction = function (predictions) {
  for(i in 1: length(predictions)){
    
    if(predictions[i] < 0 )
      predictions[i] = 0
    
  }
  return (predictions)
}

save_file_submisstion = function (predictions,data_submisstion){
  Prediction = convert_prediction(predictions$fit)
  data_sub = cbind(data_submisstion[,-9], Prediction)
  rownames(data_sub) <- NULL
  write.csv(data_sub, file = "data_submission.csv", row.names = FALSE)
  
}
# Model linear regresstion 
model_linear_regresstion = function(data_dummy, r_begin, r_end){
  train = data_dummy[r_begin : r_end,]
  time_process = proc.time()
  model = lm(train$Sales ~  ., train)
  print("Time of process training: ")
  print(proc.time() - time_process)
  
  return(model)
  
}
# fuction predict on train data
acc_model_train = function(model, data){
  predictions_train = fitted(model)
  prediction_train = convert_prediction(predictions_train)
  actual = data$Sales
  l <- abs(min(min(actual), min(predictions_train)))
  acc <- calc_accuracy(predictions_train, actual, l)
  print("Accuracy is: ")
  print(acc)
  return (acc)
  
}
