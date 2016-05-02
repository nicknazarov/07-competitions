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


buildRFModel <- function(training, mtry_) {
  
  # GC multiple times to force memory back to the OS, so we
  # can run multiple processes with as little memory as possible.
  gc(reset=TRUE)
  cat("\n**************\n\nRF mtry_=",mtry_,"\n\n***********\n\n")
  RF <- foreach(ntree=rep(200,4), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  tun_train.target <- as.integer(as.character(tun_train$TARGET))
                  training <- tun_train
                  classwt <- c((1-0.06)/sum( tun_train.target == 0),
                               0.06/sum( tun_train.target == 1)) *
                    nrow(training)
                  
                  randomForest(TARGET ~ .,training,
                               ntree=ntree,
                               strata=factor( tun_train.target), mtry =mtry_,
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE, classwt=classwt)
                }
  RF
}

buildRFModelEnsemble <- function(training) {
  # rf2 was less important in final model
  rfensemble <-lapply(list(rf1=2,
                           rf2=5,
                           rf3=8,
                           rf4=10,
                           rf5=16),
                      function(mtry_) buildRFModel(training, mtry_))
  save(rfensemble,file='rfensemble_mtry.RDA')
  #rfensemble
}


buildRFModelEnsemble(tun_train)


#list(rf1=0.1,
#     rf2=0.25,
#     rf3=0.375,
#     rf4=0.5,
#     rf5=0.625)




