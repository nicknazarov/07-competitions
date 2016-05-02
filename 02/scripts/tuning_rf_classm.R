install.packages('iterators', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('randomForest', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 
install.packages('foreach', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 
install.packages('doMC', repos='http://cran.us.r-project.org', lib= "~/",dep=TRUE) 

library('iterators', lib.loc = "~/")
library('randomForest', lib.loc = "~/")
library('foreach', lib.loc = "~/")
library('doMC',  lib.loc = "~/")

registerDoMC(cores=4)   
load('tun_train.RDA')
set.seed(1234)


buildRFModel <- function(training, pctDeadbeat) {
  
  # GC multiple times to force memory back to the OS, so we
  # can run multiple processes with as little memory as possible.
  gc(reset=TRUE)
  cat("\n**************\n\nRF pctDeadbeat=",pctDeadbeat,"\n\n***********\n\n")
  RF <- foreach(ntree=rep(200,4), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  tun_train.target <- as.integer(as.character(tun_train$TARGET))
                  training <- tun_train
                  classwt <- c((1-pctDeadbeat)/sum( tun_train.target == 0),
                               pctDeadbeat/sum( tun_train.target == 1)) *
                    nrow(training)
                  
                  randomForest(TARGET ~ .,training,
                               ntree=ntree,
                               strata=factor( tun_train.target), mtry =2,
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE, classwt=classwt)
                }
  RF
}

buildRFModelEnsemble <- function(training) {
  # rf2 was less important in final model
  rfensemble <-lapply(list(rf1=0.005,
                          rf2=0.01,
                          rf3=0.03,
                          rf4=0.05,
                          rf5=0.08),
                     function(pctDeadbeat) buildRFModel(training, pctDeadbeat))
  save(rfensemble,file='rfensemble_mtree_2_classmt.RDA')
  #rfensemble
}


buildRFModelEnsemble(tun_train)


#list(rf1=0.1,
#     rf2=0.25,
#     rf3=0.375,
#     rf4=0.5,
#     rf5=0.625)




