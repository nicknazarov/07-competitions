library(randomForest)
library(foreach)

load('tun_train.RDA')
set.seed(1234)


buildRFModel <- function(training, pctDeadbeat) {
  
  # GC multiple times to force memory back to the OS, so we
  # can run multiple processes with as little memory as possible.
  gc(reset=TRUE)
  cat("\n**************\n\nRF pctDeadbeat=",pctDeadbeat,"\n\n***********\n\n")
  RF <- foreach(ntree=rep(200,8), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  tun_train.target <- as.integer(as.character(tun_train$TARGET))
                  training <- tun_train
                  classwt <- c((1-pctDeadbeat)/sum( tun_train.target == 0),
                               pctDeadbeat/sum( tun_train.target == 1)) *
                    nrow(training)
                  
                  randomForest(training,
                               factor( tun_train.target),
                               ntree=ntree,
                               strata=factor( tun_train.target),
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE, classwt=classwt)
                }
  RF
}

buildRFModelEnsemble <- function(training) {
  # rf2 was less important in final model
  rfensemble<-lapply(list(rf1=0.02,
                          rf2=0.04,
                          rf3=0.06,
                          rf4=0.08,
                          rf5=0.2),
                     function(pctDeadbeat) buildRFModel(training, pctDeadbeat))
  save(rfensemble,file='rfensemble.RDA')
  #rfensemble
}


buildRFModelEnsemble(tun_train)

