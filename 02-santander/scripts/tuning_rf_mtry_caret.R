
# Ломается, треьует пакет e107
# Все равно не вариант, так как оптимизирует не auc, там точность лог лос и что-то еще


install.packages('iterators', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('caret', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 
install.packages('foreach', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 
install.packages('doMC', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 
install.packages('randomForest', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 

library('iterators', lib.loc = "~/")
library('caret', lib.loc = "~/")
library('foreach', lib.loc = "~/")
library('doMC',  lib.loc = "~/")
library('randomForest', lib.loc = "~/")

registerDoMC(cores=4)  

load('tun_train.RDA')

set.seed(1234)

#tun_train <- subSample(x, 0.02, top_N_features)$training
#train$TARGET <- as.factor(train$TARGET ) 


# prepare training scheme
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")

# train the model
tunegrid <- expand.grid(.mtry=c(1:16))
tun_mtry <- train(TARGET~., data=tun_train, metric="roc", 
               tuneGrid=tunegrid, trControl=control, allowParallel=TRUE)
# summarize the model
save(tun_mtry,"tun_mtry.RDA")


print(tun_mtry)


