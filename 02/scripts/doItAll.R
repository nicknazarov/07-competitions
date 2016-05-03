

rm(list=ls())
#setwd("/home/nazarov/07-competitions/02/scripts/")
setwd("/home/nick/01-projects/07-competitions/02/scripts/")

source("lib_func.R")
libraryBoot()

#PATH <- "/home/nazarov/07-competitions/02/data/"
#PATH_2 <- "/home/nazarov/Рабочий стол/santander.pdf"
PATH <- "/home/nick/01-projects/07-competitions/02/data/"
PATH_2 <- "/home/nick/Рабочий стол/santander.pdf"

N_CAT <- 15
FOR_SEED <- 1234
PCT <- 0.3
N_FACTORS <- 1:30

cat("Getting data\n")
print(system.time(x_raw <- getData(PATH)))

cat("Transforming data\n")
print(system.time({
  x <- xform_data(x_raw, N_CAT)
}))

##############################################################################################
#str(x$train, list.len =168 )

cat("Feature selection data\n")
print(system.time(list_of_features <- imp_features_rf(x, FOR_SEED)))
top_N_features <- list_of_features[N_FACTORS,2]

cat("Plot features importance\n")
plot(list_of_features[,1])

cat("Print hist for top factors\n")
print_to_file_top_fact (PATH_2, x$train, top_N_features)

#save(list_of_features,file='list_of_features.RData')
#load('list_of_features.RData')

#много сильно коррелированных фичей
#corr <- as.data.frame(cor( x_raw$train[,top_N_features[!(top_N_features %in% c("n0","flag_big_mort")) ]]))
#######################################################################################


load('list_of_features.RData')
load('importance_xgboost.RDA')

imp_rf <- list_of_features [1:15,2]
imp_xgboost <- as.data.frame(importance_matrix)[1:18, 1]  
uniq_features <- union(setdiff(imp_rf ,imp_xgboost ), setdiff(imp_xgboost ,imp_rf )) 
top_N_features <- union(uniq_features, intersect(imp_rf ,imp_xgboost ))
#corr <- as.data.frame(cor( x_raw$train[,all_features [!(all_features  %in% c("n0","flag_big_mort")) ]] ))

top_N_features <- top_N_features[!(top_N_features %in% c("imp_op_var41_efect_ult3",
                                                         "imp_op_var41_ult1","saldo_medio_var5_hace3",
                                                         "imp_op_var39_comer_ult1","saldo_medio_var5_ult3",
                                                         " num_venta_var44_ult1","imp_reemb_var17_ult1 ")) ]
                


#top_N_features               
data2learn <- subSample(x, PCT, top_N_features)

training <- feature_eng(data2learn$training)
testing <- feature_eng(data2learn$testing)

#top_N_features
summary(training)

week_factors <- c(
"flag_imp_ent_var16_ult1",  "flag_imp_op_var41_efect_ult1",  
"flag_imp_op_var39_comer_ult3","flag_num_med_var45_ult3",          
"imp_sal_var16_ult1" ,  "flag_imp_trans_var37_ult1"  ,      
"flag_num_var43_recib_ult1",  "flag_imp_op_var40_comer_ult1" ,
"imp_reemb_var17_ult1",   "ind_var13_largo_0",               
"ind_var31_0", "num_meses_var13_largo_ult3"  ,   "num_venta_var44_ult1")               


tun_train <-feature_eng(x$train[,c(top_N_features, "TARGET" )])
tun_test <-feature_eng(x$test[,top_N_features])

tun_train <- tun_train[ ,!(colnames(tun_train) %in% week_factors) ]
tun_test <- tun_test[ ,!(colnames(tun_test) %in% week_factors) ]

#save(tun_train, file="tun_train.RDA")
#save(tun_test, file="tun_test.RDA")

#######################################################################################

load('rfensemble.RDA')

#str(rfensemble)
load('tun_test.RDA')
load('tun_train.RDA')
#rfensemble$rf1
predicts <- list()
i <-1
  for(rf in rfensemble){
    predicts[i] <- predict(rf, tun_test)
    i <- i+1
  }
  
predict(rfensemble$rf1, tun_test) 


setdiff(names(tun_train),names(tun_test ))
rm(rfensemble)

# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = test$ID, TARGET = pred)
write.csv(submission, 'xgboost_first_simple.csv', row.names=FALSE, quote = FALSE)


#######################################################################################





load(file='rfensemble_mtree_2_classmt.RDA')

varImpPlot(rfensemble$rf2)

print(rfensemble$rf2$mtry )

rfensemble$rf1$confusion[, 'class.error']

OOB.votes <- predict (rfensemble$rf1,tun_train)

formatrix <- table (OOB.votes ,as.factor(tun_train$TARGET))
confusionMatrix(formatrix)



OOB.votes <- predict (rfensemble$rf2,tun_test,type="prob")
OOB.pred <- OOB.votes[,2];
OOB.pred [1:10]
pred[1:10]

train_auc <-auc(as.numeric(as.character(tun_train$TARGET)), OOB.pred )
print(train_auc)
#Area under the curve: 0.8398
#Area under the curve: 0.8398
#Area under the curve: 0.8278
#Area under the curve: 0.8242
#Area under the curve: 0.8221

# ---------------------------------------------------
# SAVE

pred_boost_rf <- pred*0.9+OOB.pred*0.1

pred_boost_rf <- sqrt(pred*OOB.pred)

submission <- data.frame(ID = pred[,1], TARGET = order(pred[,2])/ nrow(pred))
write.csv(submission, 'xgboost_rank.csv', row.names=FALSE, quote = FALSE)

pred_boost_rf[1:10]
pred[1:10]

pred <- predict (rfensemble$rf4,tun_test, type="prob")
# SAVE
submission <- data.frame(ID = x_raw$test$ID, TARGET = pred[,2])
write.csv(submission, 'rf_4.csv', row.names=FALSE, quote = FALSE)

pred <- read.csv('xgboost_rf_9_1.csv')


########################################################################################
modelLookup("xgbTree")
getModelInfo()

xgb_train_1 <- train(
  x = as.matrix(df_train %>% select(-c(Response, Id))),
  y= df_train$Response,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid_1,
  method="xgbLinear"
)


########################################################################################
load('svm_data.RDA')

model <-svm_data$svmensemble$svm1


# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y, probability = TRUE) 

print(model)
summary(model)

# test with train data
pred <- predict(model, data_scaling$test, probability = TRUE)
# (same as:)
pred <- fitted(model)

head(attr(pred, "probabilities"),100)
pred[1:100]

#
auc(as.numeric(as.character(tun_train$TARGET)), attr(pred, "probabilities")[,2] )

#############################################################################

load('tun_train.RDA')
set.seed(1)
sub_train <-createDataPartition(y = tun_train$TARGET,
                                ## the outcome data are needed
                                p = .1,
                                ## The percentage of data in the
                                ## training set
                                list = FALSE)
tr <- trainControl(method = "cv", number = 5)
train(TARGET ~ .,data=tun_train[sub_train,] ,method="rf",trControl= tr)
