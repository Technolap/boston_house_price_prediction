# Estimating the price of a house, given real estate data (scalar regression)

#loading the keras library
library(keras3)

# loading the dataset 
boston <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% boston

#getting the structres of the train data, test data and train targets
str(train_data)
str(test_data)
str(train_targets)

# Normalizing with mean and standard deviation
mean <- apply(train_data,2,mean)
sd <- apply(train_data,2, sd)

train_data <- scale(train_data, center = mean, scale = sd)
test_data <- scale(test_data, center = mean, scale = sd)

#function to build the model 
#since we have few samples we'll use a very small model with 2 intermediate layers each with 64 units,
build_model <- function(){
    model <- keras_model_sequential() %>%
        layer_dense(64, activation = "relu") %>%
        layer_dense(64, activation = "relu") %>%
        layer_dense(1)  
    
    model %>% compile(
        optimizer = "rmsprop",
        loss = "mse",
        metrics = "mae"
    )
    model
}
# K fold cross validation
k <- 4
fold_id <- sample (rep(1:k, lenght.out = nrow(train_data)))
num_epochs <-100
all_scores <- numeric()

for (i in 1:k){
    cat("Processing fold #", i, "\n")
    
    val_indices <- which(fold_id == i)
    val_data <- train_data[val_indices]
    val_targets <- train_data[val_indices]

    partial_train_data <- train_data[-val_indices, ]
    partial_train_targets <- train_targets[-val_indices]

    model <- build_model()

    model %>% fit(
        partial_train_data,
        partial_train_targets,
        epochs = num_epochs,
        batch_size = 16,
        verbose = 0
    )

    results <- model %>% evaluate(val_data, val_targets, verbose = 0)

    all_scores[[i]] <- results[['mae']]
}

all_scores
mean(all_scores)



