
install.packages("caret")
install.packages("doMC")
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
training$TARGET <- as.factor(tun_train$TARGET)
# train the model
tunegrid <- expand.grid(.mtry=c(1:15))
model_mtree <- train(TARGET~., data=tun_train,  method="rf", metric="roc", 
               tuneGrid=tunegrid, trControl=control, allowParallel=TRUE)
# summarize the model
save(model_mtree,"model_mtree.RDA")
