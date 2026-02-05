# 4.3 BOSTON HOUSING (Regression)

#loading the keras library
library(keras3)

# loading the dataset 
boston <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% boston

#getting the structres of the train data, test data and train targets
str(train_data)
str(test_data)
str(train_targets)
