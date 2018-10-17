library(data.table)
library(tidyverse)
library(spatstat)
library(randomforest)
library(xgboost)
library(caret)

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
train <- fread(file = "01 data/train.csv") # relaod file, to make sure all cols are numeric
sample_ids <- sample(c(1:42000))[1:40000]

train_train <- train[sample_ids, ]
train_test <- train[-sample_ids, ]

train_mat <-as.matrix(train_train)
train_mat_xgb <- xgb.DMatrix(train_mat[,2:785], label=train_mat[,1])

## continue here
xgb_params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 10, eval_metric = "mlogloss")
#xgb_result <- xgboost(train_mat_xgb, nrounds=20, params=xgb_params)

# Do cross-validated xgboost
xgbcv <- xgb.cv(params = xgb_params, data = train_mat_xgb, nrounds = 100, nfold = 5, showsd = TRUE, stratified = TRUE, 
                print_every_n = 2, early_stop_round = 20, maximize = FALSE, prediction = TRUE)

# retreive predictions from class probabilities
xgb_train_preds <- data.frame(xgbcv$pred) %>% mutate(pred_digit = max.col(., ties.method = "last") -1) 
xgb_train_preds$real_digit =   as.vector(train_train[[1]])          

xgb_conf_mat <- table(real=xgb_train_preds$real_digit, pred=xgb_train_preds$pred_digit)

xgb_conf_mat_2 <- caret::confusionMatrix(factor(xgb_train_preds$real_digit),
                                  factor(xgb_train_preds$pred_digit),
                                  mode = "everything")

print(xgb_conf_mat_2)

#save.image()
git mv "load data.R" "load_and_analyse.R"

