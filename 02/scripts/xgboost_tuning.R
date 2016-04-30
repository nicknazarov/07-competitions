
imp_features_xgboost <- function(x, for_seed){
  
  set.seed(for_seed)
  options(scipen=999)
  
  train <- x$train
  test <- x$test
  #str(dtrain)
  
  train$TARGET <- as.integer(as.character(train$TARGET)) 
  test$TARGET <- -1
  # ---------------------------------------------------
  # Features
  feature.names <- names(train)
  feature.names <- feature.names[-grep('^ID$', feature.names)]
  feature.names <- feature.names[-grep('^TARGET$', feature.names)]
  feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))
  
  
  # ---------------------------------------------------
  # Matrix
  indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.85))
  
  data <- sparse.model.matrix(feature.formula, data = train[indexes, ])
  sparseMatrixColNamesTrain <- colnames(data)
  dtrain <- xgb.DMatrix(data, label = train[indexes, 'TARGET'])
  rm(data)
  dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[-indexes, ]),
                        label = train[-indexes, c('TARGET')])
  dtest <- sparse.model.matrix(feature.formula, data = test)
  
  watchlist <- list(valid = dvalid, train = dtrain)
  
  # ---------------------------------------------------
  # XGBOOST
  params <- list(booster = "gbtree", objective = "binary:logistic",
                 max_depth = 8, eta = 0.05,
                 colsample_bytree = 0.65, subsample = 0.95)
  model <- xgb.train(params = params, data = dtrain,
                     nrounds = 500, early.stop.round = 50,
                     eval_metric = 'auc', maximize = T,
                     watchlist = watchlist, print.every.n = 10)
  
  pred <- predict(model, dtest)
  
  # ---------------------------------------------------
  # SAVE
  submission <- data.frame(ID = test$ID, TARGET = pred)
  write.csv(submission, 'imp_xgboost_first_simple.csv', row.names=FALSE, quote = FALSE)
  
  # Compute feature importance matrix
  importance_matrix <- xgb.importance(feature.names, model = model)
  # Nice graph
  #xgb.plot.importance(importance_matrix[1:10,])
  
  #test <- chisq.test(train$Age, output_vector)
  #print(test)
  
  #print(importance_matrix[1:30,])
  save(importance_matrix ,file="importance_xgboost.RDA")
  importance_matrix
}