
library(dplyr)
library('randomForest')
##############################################################################################
#### Loading data
##############################################################################################
setwd("/home/nick/01-projects/07-competitions/02-santander/scripts")
load('tun_test.RDA')
load('tun_train.RDA')

# load in the training data
df_train <- tun_train
df_train$TARGET <- NULL
set.seed(1234)


result <- rfcv(df_train , tun_train$TARGET, cv.fold=3)

with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

registerDoMC(cores=4)   
load('tun_train.RDA')
set.seed(1234)


buildRFModel <- function(training, mtry_, coeff) {
  
  # GC multiple times to force memory back to the OS, so we
  # can run multiple processes with as little memory as possible.
  gc(reset=TRUE)
  cat("\n**************\n\nRF mtry_=",mtry_,"\n\n***********\n\n")
  RF <- foreach(ntree=rep(200,4), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  tun_train.target <- as.integer(as.character(tun_train$TARGET))
                  training <- tun_train
                  classwt <- c((1-coeff)/sum( tun_train.target == 0),
                               coeff/sum( tun_train.target == 1)) *
                    nrow(training)
                  
                  randomForest(TARGET ~ .,training,
                               ntree=ntree,
                               strata=factor( tun_train.target), mtry =mtry_,
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE, classwt=classwt)
                }
  RF
}

buildRFModelEnsemble <- function(training, coeff) {
  # rf2 was less important in final model
  rfensemble <-lapply(list(rf1=3,
                           rf2=4,
                           rf3=8,
                           rf4=10,
                           rf5=16),
                      function(mtry_) buildRFModel(training, mtry_, coeff))
  save(rfensemble,file='rfensemble_mtry.RDA')
  #rfensemble
}

args <- commandArgs(trailingOnly = TRUE)
coeff <- as.numeric(args[1])

buildRFModelEnsemble(tun_train, coeff)


#list(rf1=0.1,
#     rf2=0.25,
#     rf3=0.375,
#     rf4=0.5,
#     rf5=0.625)


set.seed(647)
myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96))
result <- rfcv(myiris, iris$Species, cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

## Not run:
result <- replicate(5, rfcv(myiris, iris$Species), simplify=FALSE)
error.cv <- sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")
## End(Not run)