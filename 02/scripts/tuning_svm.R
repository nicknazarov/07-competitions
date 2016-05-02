install.packages('iterators', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('foreach', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('doMC', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('e1071', repos='http://cran.us.r-project.org', lib= "~/") 
install.packages('caret', repos='http://cran.us.r-project.org', lib= "~/") 

library('iterators', lib.loc = "~/")
library('foreach', lib.loc = "~/")
library('doMC' , lib.loc = "~/")
library('e1071' , lib.loc = "~/")
library('caret' , lib.loc = "~/")

# configure multicore
registerDoMC(cores=4)   
#load('tun_train.RDA')
#load('tun_test.RDA')
load('tun_all.RDA')

set.seed(1234)

data_preprocess <- function(training, testing) {
  cat("Centering and scaling\n")
  target <- training$TARGET
  training$TARGET <- NULL
  preprocess <- preProcess(rbind(testing,training))

  training <- predict(preprocess, training)
  #print(summary(training$TARGET <- target))
  training$TARGET <- target
  target <- NULL
  
  testing <- predict(preprocess, testing)

  cat("Building models\n")
  gc(reset=TRUE)
  
 return (list(train= training, test = testing, preprocess=preprocess)) 
  
}

buildSVM <- function(training, cost=1) {
  response <- factor(training$TARGET)
  weight <- sum(as.numeric(as.character(training$TARGET)))/nrow(training)
  gc()
  weight <- c(1/(1-weight), 1/weight)
  names(weight) <- levels(response)
  
  # already scaled
  svm(training, response, scale=FALSE, type='C-classification',
      kernel='radial', cachesize=4000, probability=TRUE,
      class.weights=weight, cost=cost)
}

buildSVMEnsemble <- function(training) {
  gc(reset=TRUE)
  mclapply(list(svm1=0.05, svm3=0.1, svm4=0.5, svm5=1),
           function(cost) buildSVM(training, cost=cost))

}

tun_train <- tun_all$train
tun_test <- tun_all$test

data_scaling   <- data_preprocess (tun_train, tun_test)

svmensemble <- buildSVMEnsemble (data_scaling$train )

svm_data <- list(train= data_scaling$train, test = data_scaling$test, preprocess=data_scaling$preprocess, 
     svmensemble=svmensemble) 

save(svm_data,file='svm_data.RDA')

#tun_all <- list(train =tun_train , test= tun_test)
#save(tun_all,file='tun_all.RDA')

training <- data_scaling$train
response <- factor(training$TARGET)
weight <- sum(as.numeric(as.character(training$TARGET)))/nrow(training)
gc()
weight <- c(1/(1-weight), 1/weight)
names(weight) <- levels(response)

# already scaled
model <- svm(training, response, scale=FALSE, type='C-classification',
    kernel='radial', cachesize=4000, probability=TRUE,
    class.weights=weight, cost=1)
