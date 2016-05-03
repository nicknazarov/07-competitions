library(caret)
library(xgboost)
#library(readr)
library(dplyr)
#library(tidyr)
library('Matrix')

setwd("/home/nick/01-projects/07-competitions/02/scripts/")
load('tun_test.RDA')
load('tun_train.RDA')

# load in the training data
df_train <- tun_train

modelLookup("xgbTree")


# ---------------------------------------------------
# Features
feature.names <- names(df_train)
#feature.names <- feature.names[-grep('^ID$', feature.names)]
feature.names <- feature.names[-grep('^TARGET$', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))


# ---------------------------------------------------
# Matrix

indexes <- sample(seq_len(nrow(df_train)), floor(nrow(df_train)*1))

data <- sparse.model.matrix(feature.formula, data = df_train[indexes, ])
sparseMatrixColNamesTrain <- colnames(data)
dtrain <- xgb.DMatrix(data, label = as.integer(as.character(df_train[indexes, 'TARGET'])))
rm(data)

set.seed(1234)
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                              
  # binary classification
  eta = 0.05,                                                                 
  # learning rate
  max_depth = 4,      #0.829690+0.007963                                                         
  # max tree depth
  eval_metric = "auc" ,                                                         
  # evaluation/loss metric
  # gamma = 
  colsample_bytree = 0.2,#
  min_child_weight = 5,#
  subsample = 0.8, #0.829833+0.008513 0.8
  scale_pos_weight = 0.9 #test-auc:0.828313+0.005539
  #gamma = 0.05
)





# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = dtrain,
                  nrounds = 3000, 
                  nfold = 5,                                                   
                  # number of folds in K-fold
                  prediction = TRUE,                                           
                  # return the prediction using the final model 
                  showsd = TRUE,                                               
                  # standard deviation of loss across folds
                  stratified = TRUE,                                           
                  # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 10, 
                  early.stop.round = 50
)

#10
#[110]	train-auc:0.901426+0.002678	test-auc:0.819965+0.007361
#stopping. Best iteration: 104

#8
#[160]	train-auc:0.900007+0.002350	test-auc:0.821791+0.008278
#Stopping. Best iteration: 156

#6
#[180]	train-auc:0.882933+0.001599	test-auc:0.828930+0.005077
#Stopping. Best iteration: 180

#4
#[180]	train-auc:0.860371+0.002363	test-auc:0.829690+0.007963
#Stopping. Best iteration: 173
#############################################################



#t <- as.matrix(df_train %>% select(-TARGET))

# fit the model with the arbitrary parameters specified above
xgb_1 <- xgboost(data = dtrain,
                params = xgb_params_1,
                nrounds = 426, early.stop.round = 100,
                eval_metric = 'auc', maximize = T,
               # watchlist = watchlist, 
               print.every.n = 100,
               verbose = TRUE)

tun_test$TARGET <- -1                
dtest <- sparse.model.matrix(feature.formula, data = tun_test)                
pred <- predict(xgb_1, dtest)

# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = x_raw$test$ID, TARGET = pred)
write.csv(submission, 'xgboost.csv', row.names=FALSE, quote = FALSE)





# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

############################################################################
#Hyperparameter search using train
############################################################################

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  
  #nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight
  nrounds = 200,
  #eta = c(0.01, 0.001, 0.0001),
  eta = c(0.1),
  max_depth = c(4, 6, 8, 10, 12, 14),
  gamma = 1,
  
  # gamma = 
  colsample_bytree = 0.2 ,
  min_child_weight = 5
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        
  # save losses across all models
  classProbs = TRUE,                                                           
  # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

##################################
train_ <- df_train[, !(names(df_train) %in% c("TARGET"))]

for(i in 1: ncol(train_)){
  if (class(train_[,i])== "factor"){
    train_[,i] <- as.integer(as.character(train_[,i]))
  }
}


target <- factor(df_train$TARGET,                           # factor variable for classification
       labels = c("sut", "unsut"))

#################################

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(train_ ),
  y = target,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")



