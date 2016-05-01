
#install.packages("caret")
#install.packages("doMC")
library('caret')
library('doMC')

load('tun_train.RDA')
#tun_train <- subSample(x, 0.02, top_N_features)$training
#train$TARGET <- as.factor(train$TARGET ) 
set.seed(1234)

# configure multicore
registerDoMC(cores=4)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")

# train the model
tunegrid <- expand.grid(.mtry=c(1:11))
model_mtree <- train(TARGET~., data=tun_train, metric="roc", 
               tuneGrid=tunegrid, trControl=control, allowParallel=TRUE)
# summarize the model
save(model_mtree,"model_mtree.RDA")


print(model_mtree)

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
