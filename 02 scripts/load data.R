library(data.table)
library(tidyverse)
library(spatstat)
library(randomforest)
library(xgboost)

train <- fread(file = "01 data/train.csv")
train$label <- as.factor(train$label)
test <- fread(file = "01 data/test.csv")

colnames(train)
print(table(train$label))

## Explore and plot the data set #####
plot_digits= function(pixeldata){
  
  pixelmatrix <- matrix(
    sapply(pixeldata[1, c(2:785)], as.numeric), 
    nrow=28, ncol=28)
  
  pixelmatrix <- apply(pixelmatrix, 1, rev)
  
  image(t(pixelmatrix),
      axes = FALSE, col = grey(seq(0, 1, length = 256)))
  
  title(main=pixeldata$label)
}

par(mfrow=c(4,4))
for (idx in c(1:16)){
  plot_digits(train[idx, ])
  }


## Try a minimum solution with a random forest  #####
rf <- randomForest::randomForest(label~., train)

## Try a minimum solution using xgboost  #####
train <- fread(file = "01 data/train.csv") 
sample_ids <- sample(c(1:42000))[1:40000]

train_train <- train[sample_ids, ]
train_test <- train[-sample_ids, ]

train_mat <-as.matrix(train_train)
train_mat_xgb <- xgb.DMatrix(train_mat[,2:785], label=train_mat[,1])

## continue here: Next step is to add one-hot encoding

xgp_params <- c()
xgb_result <- xgboost(train_mat_xgb, nrounds=20)

test_matrix <- xgb.DMatrix(data = test_data, label = test_label)




