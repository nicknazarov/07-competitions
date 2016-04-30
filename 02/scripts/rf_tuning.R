



set.seed(1234)
# configure multicore
library(doMC)

registerDoMC(cores=4)
# load the library
library(caret)


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
training$TARGET <- as.factor(training$TARGET)
# train the model
tunegrid <- expand.grid(.mtry=c(1:15))
model <- train(TARGET~., data=training,  method="rf", metric="roc", 
               tuneGrid=tunegrid, trControl=control, allowParallel=TRUE)
# summarize the model
print(model)





buildRFModel <- function(training, pctDeadbeat) {
  
  # GC multiple times to force memory back to the OS, so we
  # can run multiple processes with as little memory as possible.
  gc(reset=TRUE)
  cat("\n**************\n\nRF pctDeadbeat=",pctDeadbeat,"\n\n***********\n\n")
  RF <- foreach(ntree=rep(200,16), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  training.SeriousDlqin2yrs <- training$SeriousDlqin2yrs
                  training <- training[,-c(1,2)]
                  classwt <- c((1-pctDeadbeat)/sum(training.SeriousDlqin2yrs == 0),
                               pctDeadbeat/sum(training.SeriousDlqin2yrs == 1)) *
                    nrow(training)
                  
                  randomForest(training,
                               factor(training.SeriousDlqin2yrs),
                               ntree=ntree,
                               strata=factor(training.SeriousDlqin2yrs),
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE, classwt=classwt)
                }
  RF
}