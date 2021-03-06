
libraryBoot <- function()
{
  library('randomForest')
  library('ggplot2')
  library('caret')
  library('e1071')
  library('pROC')
  library('Matrix')
  library('xgboost')
  library('doMC')
  return (TRUE)
}

getData <- function(PATH){
  train <- read.csv(paste0(PATH,"train.csv"))
  test  <- read.csv(paste0(PATH,"test.csv"))
  
  ncol0 <- ncol(train)
  return (list(train = train, test= test, ncol0=ncol0))
}

xform_data <- function(x, N_cat) {
  #Чистим данные - удаляем столбцы из констант, линейные комбинации столбцов, сильно коррелированные фичи
  # категоризуем столбцы
  train <- x$train
  test <- x$test
  ncol0 <- x$ncol0
  ### 1 #################################################
  #Similarly, you can remove variables with 0 variance using the function nearZeroVar again from caret package
  #zero.var = nearZeroVar(train, saveMetrics=TRUE) train <- train[, !(zero.var$zeroVar)]
  ### Removing constant features 
  cat("removing constant features\n")
  toRemove <- c()
  feature.names <- names(train)
  for (f in feature.names) {
    if (sd(train[[f]])==0) {
      toRemove <- c(toRemove,f)
      cat(f,"is constant\n")
    }
  }
  train.names  <- setdiff(names(train), toRemove)
  train        <- train[,train.names]
  test.names   <- setdiff(names(test), toRemove)
  test         <- test[,test.names]
  toRemove
  cat("-------------------------\n")
  
  ### 2 #################################################
  # Removing features being a specific linear function 
  # of other features where all coefficients
  
  #library(caret)
  lin.comb <- findLinearCombos(train)
  train <- train[, -lin.comb$remove]
  test <- test[, -lin.comb$remove]
  
  ### 3 #################################################
  #Removing hghly correlated variables
  cor_v<-abs(cor(train))
  diag(cor_v)<-0
  cor_v[upper.tri(cor_v)] <- 0
  cor_f <- as.data.frame(which(cor_v > 0.9, arr.ind = T))
  train <- train[,-unique(cor_f$row)]
  test <- test[,-unique(cor_f$row)]
  cat("-------------------------\n")
  ####################################################
  removed <- ncol0-ncol(train)
  cat("\n ",removed," features have been removed\n")
  #### 4 ###############################################
  # Вписываем область значений теста в область значений трейна
  #---limit vars in test based on min and max vals of train
  print('Setting min-max lims on test data')
  for(f in colnames(train[, !(names(train) %in% c("TARGET"))])){
    lim <- min(train[,f])
    test[test[,f]<lim,f] <- lim
    
    lim <- max(train[,f])
    test[test[,f]>lim,f] <- lim  
  }
  ### 5 #################################################
  #Факторизуем категориальные переменные
  factor.index <- c()
  # соединяем тест и трейн 
  test$TARGET <- 2
  all_data <- rbind(train, test)
  
  for(i in 1:ncol(all_data)){
     #if( length(unique(train[,i])) < N_cat){    
    if( grepl("ind_",names(all_data)[i]) || length(unique(all_data[,i])) < N_cat){ 
           factor.index <- c(factor.index, i)
     }
  }
  #factor.index
  for(i in 1:(length(factor.index)-1)){
      all_data[,factor.index[i]] <- as.factor(all_data[,factor.index[i]])
      # train[,factor.index[i]] <- as.factor(train[,factor.index[i]])
      # test[,factor.index[i]] <- as.factor(test[,factor.index[i]])
  }
  train <-  all_data[ all_data$TARGET !=2, ]
  test <-  all_data[ all_data$TARGET ==2, ]
  test <- test[ , !(names(test) %in% c("TARGET"))]
  train$TARGET <- as.factor(train$TARGET)

  #### 6 ###############################################
  ##### 0 count per line
  # Добавляем новую переменную - количество нулей в строке
  count0 <- function(x) {
         return( sum(x == 0) )
  }
  train$n0 <- apply(x$train [, !(names(x$train) %in% c("TARGET"))], 1, FUN=count0)
  test$n0 <- apply(x$test, 1, FUN=count0)
  
  #### 8 ###############################################
  ##### There is Mortage in this bank
  #most popular value
  v38 <- as.data.frame(table(train$var38))
  v38 <- v38[order(v38[,2], decreasing = TRUE),]
  v38[,1] <-  as.numeric(levels(v38[,1]))[v38[,1]]
  train$flag_client_mort <- as.integer(train$var38 == v38[1,1])
  test$flag_client_mort <- as.integer(test$var38 == v38[1,1])
  
  #### 7 ###############################################
  ##### There is Mortage in this bank more than avarege
  #most popular value
  print(as.integer(v38[1,1]))
  train$flag_big_mort <- as.integer(train$var38 > v38[1,1])
  test$flag_big_mort <- as.integer(test$var38 > v38[1,1])
  
 return (list(train = train, test = test))

}

imp_features_rf <- function(x, for_seed){
  # Важность фичей по rf
  #library(randomForest)
  train <- x$train
  set.seed(for_seed)
  #importante features
  rf_importance <- randomForest(TARGET ~ ., train[,!(names(train) %in% c("ID"))] , ntree=500,
                                norm.votes=F,
                                mtry=2)
  
  imp_fact <- as.data.frame(importance(rf_importance))
  imp_fact[,2] <- rownames( imp_fact )
  sort_imp_df <- as.data.frame(imp_fact[ order(-imp_fact[,1]) , ])
  #top 60, others useless I think 
 # most_imp_fact <- rownames( temp_df  )[1:top_n]
  gc(reset=TRUE)
  return (sort_imp_df) 
}


count_levels <- function (dat){
  #Количество уникальных значений в каждом столбце датафрейма
  result <- data.frame(1,1) 
  for( i in 1:ncol(dat)){
   result[i,1] <- colnames(dat)[i]
   result[i,2] <- length(unique(dat[,i]))
  }
  return (result[order(result[,2]),])
}

#t <- count_levels (temp)


subSample <- function (x, pct, list_of_features){
  # Извлекаем стратифицированныю подвыборку
  # х - лист с трейн и тест
  dat <- x$train[,c(list_of_features, "TARGET")]
  inTrain <- createDataPartition(y = dat$TARGET,
                                 ## the outcome data are needed
                                 p = pct,
                                 ## The percentage of data in the
                                 ## training set
                                 list = FALSE)
  ## The format of the results
  ## The output is a set of integers for the rows of dat
  ## that belong in the training set.
  #str(inTrain)
  training <- dat[ inTrain,]
  testing <- dat[-inTrain,]
  
  return (list(training = training, testing = testing, n_train = nrow(training) ))
}



print_to_file_top_fact <- function(PATH_2, train, list_of_factors){
  # Печатаем в файл гистограммы с распределением непрерывных фичей
  pdf(file=PATH_2)
  dat <- train[,list_of_factors]
  for (i in 1:ncol(dat)){
    #boxplot(train[,i]~train[,2], data=train)
    if(class(dat[,i])!= "factor")
      hist(dat[,i])
      title(colnames(dat)[i],adj=1)
  }
  dev.off()   
}

AUC <- function(train_actual, train_predicted, test_actual, test_predicted)
{
  # Подсчет аук для трейн и тест
  train_auc<-auc(as.numeric(train_actual),as.numeric(train_predicted))
  test_auc<-auc(as.numeric(test_actual),as.numeric(test_predicted))
  cat("\n\n*** what ***\ntraining:")
  print(train_auc)
  cat("\ntesting:")
  print(test_auc)
  cat("\n*****************************\n")
  list(train_auc=train_auc,test_auc=test_auc) 
}


feature_eng <- function(x){
  # преобразовываем фчи из датасета, работаем только со значимыми
  #summary(x)
  x$num_var42 <- as.integer(as.character(x$num_var42))
  x$num_var4 <- as.integer(as.character(x$num_var4))
  x$flag_num_var42 <- as.integer(x$num_var42 ==0)
  x$flag_imp_op_var41_efect_ult1 <- as.integer(x$imp_op_var41_efect_ult1 ==0)
  
  #x$flag_saldo_medio_var5_ult3 <- ifelse(x$saldo_medio_var5_ult3<=0, 1, 0 )  
  x$flag_saldo_medio_var5_ult1 <- ifelse(x$saldo_medio_var5_ult1<=0, 1, 0 )  
  x$flag_saldo_medio_var5_hace2 <- ifelse(x$saldo_medio_var5_hace2<=0, 1, 0 )  
  x$flag_saldo_var5 <- ifelse(x$saldo_var5<=0, 1, 0 )  
  x$flag_imp_ent_var16_ult1 <- ifelse(x$imp_ent_var16_ult1==0, 1, 0 ) 
  x$flag_num_med_var22_ult3 <- as.integer(x$num_med_var22_ult3 ==0)
  
  
  
  
  #x$flag_imp_sal_var16_ult1 <- as.integer(x$imp_sal_var16_ult1==0)
  x$flag_num_med_var45_ult3 <- as.integer(x$num_med_var45_ult3==0)
  x$flag_imp_op_var40_comer_ult1 <- as.integer(x$imp_op_var40_comer_ult1==0)
  x$flag_imp_op_var39_comer_ult3 <- as.integer(x$imp_op_var39_comer_ult3==0)
  #x$flag_imp_reemb_var17_ult1 <- as.integer(x$imp_reemb_var17_ult1==0)
  x$flag_imp_trans_var37_ult1 <- as.integer(x$imp_trans_var37_ult1==0)
  x$flag_num_var43_recib_ult1  <- as.integer(x$num_var43_recib_ult1 ==0)
  
  
  x$imp_ent_var16_ult1 <- NULL #3719
  #x$num_med_var22_ult3 <- NULL #11254
 # x$num_med_var45_ult3 <- NULL
  
  x$imp_op_var40_comer_ult1 <- NULL#295
  
 # x$imp_op_var39_comer_ult3 <- NULL#11690
 # x$imp_reemb_var17_ult1 <- NULL#20
  #x$imp_trans_var37_ult1 <- NULL#9830
  
 # x$num_var43_recib_ult1  <- NULL #9830

  
  #sum( x$train$imp_op_var40_comer_ult1     > 0) 
  
  #x$flag_imp_op_var41_efect_ult3 <- ifelse(x$imp_op_var41_efect_ult3==0, 1, 0 )
  #x$flag_num_med_var45_ult3 <- ifelse(x$num_med_var45_ult3==0, 1, 0 )
  
  #x$flag_num_var22_ult1 <- ifelse(x$num_var22_ult1 ==0, 1, 0 )
  #x$flag_num_med_var22_ult3 <- ifelse(x$num_med_var22_ult3 ==0, 1, 0 )
  #x$flag_num_var45_hace3 <- ifelse(x$num_var45_hace3 ==0, 1, 0 )
  #x$flag_num_var22_hace2 <- ifelse(x$num_var22_hace2 ==0, 1, 0 )
  #x$flag_num_var45_ult1 <- ifelse(x$num_var45_ult1 ==0, 1, 0 )
  #x$flag_imp_op_var39_comer_ult1 <- ifelse(x$imp_op_var39_comer_ult1 ==0, 1, 0 )
  #x$flag_num_var22_hace3 <- ifelse(x$num_var22_hace3 ==0, 1, 0 )
  #x$flag_num_op_var41_ult1 <- ifelse(x$num_op_var41_ult1 ==0, 1, 0 )
  #x$flag_imp_op_var39_comer_ult3 <- ifelse(x$imp_op_var39_comer_ult3 ==0, 1, 0 )
 # x$flag_saldo_var37 <- ifelse(x$saldo_var37 ==0, 1, 0 )
  #x$flag_saldo_var8 <- ifelse(x$saldo_var8 <=0, 1, 0 )
  
  
 # x$LowAge <- as.integer(x$var15  < 18)
 # x$Log.age <- log(x$var15)
 # x$Log.age[x$LowAge == 1] <- 0
 # x$var15  <- NULL

  summary(x)
  x
}


imp_features_xgboost <- function(x, for_seed){
  # Важность фичей согласно xgboost
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
