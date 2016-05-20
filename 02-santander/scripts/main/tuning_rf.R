
#Проблем реализации в карет - там нет оптимизации для auc - если классы несбалансированы,
#то  подбирать параметры для точности - совсем не тру

library('iterators')
library('randomForest')
library('foreach')
library('doMC')
library('pROC')
library('dplyr')
library('plyr')

registerDoMC(cores=4)   
set.seed(1234)
##############################################################################################
#### Loading data
##############################################################################################
setwd("/home/nick/01-projects/07-competitions/02-santander/scripts")
#load('tun_test.RDA')
#load('tun_train.RDA')

train <- iris
train$target <- ifelse(as.character(train[,5])=="virginica",0,1)
train.target <- train$target
train$Species <- NULL

##############################################################################################
#### Define parameters
##############################################################################################
#Тьюнить будем 2 параметра classwt - вес класса и mtry - количетсво факторов, по которым происходит расщепление
i <- 1
classwt_grid <- list()

for(pctResponse in c( 0.01, 0.05, 0.1, 0.3, 0.5)){
  classwt_grid[[i]] <- c((1-pctResponse)/sum( train.target == 0),
                         pctResponse/sum( train.target == 1)) * nrow(train)
  i <- i+1
}


mtry_grid <- list(2,3,4)

##############################################################################################
#### stratified k-fold
##############################################################################################
train$id <- 0
k <- 5

m <- matrix(0, ncol = k+1, nrow = 1)
auc_table <- data.frame(m)

names_auc <- list()
for(i in 1:k) {
  nam <- paste("auc_fold_", i, sep = "")
  #names_auc <- list(names_auc, assign(nam, 1))
  colnames(auc_table)[i] <- nam
}

colnames(auc_table)[k+1] <- "auc_mean"

train[train$target==0,]$id <- sample(1:k,nrow(train[train$target==0,]),replace=TRUE)
train[train$target==1,]$id <- sample(1:k,nrow(train[train$target==1,]),replace=TRUE)

#hist(train$id )


#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(length(mtry_grid)*length(classwt_grid))
fold <- 1:k

j <- 1
for(mtry in mtry_grid ){ 
  for(classwt in classwt_grid ){ 
   
    for (i in 1:k){
      # remove rows with id i from dataframe to create training set
      # select rows with id i to create test set
      trainingset <- subset(train, id %in% fold[-i])
      testset <- subset(train, id %in% c(i))
  
       # run a random forest model
      gc(reset=TRUE)
     # cat("\n**************\n\nRF k_=",i,"\n\n***********\n\n")
      #RF <- foreach(ntree=rep(200,4), .combine=combine,
     #               .multicombine=TRUE,
      #              .packages="randomForest") %dopar% {
      #                trainingset.target <- as.integer(as.character(trainingset$target))
#
     #                 randomForest(as.factor(target) ~ ., data = trainingset[,!(names(trainingset)%in%c("id"))],
     #                              ntree=ntree,
     #                              strata=factor( trainingset.target), mtry =mtry,
     #                             do.trace=F, importance=TRUE, forest=TRUE,
     #                              replace=TRUE, classwt=classwt)
    #                }
  
      RF <- randomForest(as.factor(target) ~ ., data = trainingset[,!(names(trainingset)%in%c("id"))], ntree = 100, 
                         classwt=classwt, mtry =mtry)
       prediction <- predict(RF, testset[,!(names(trainingset)%in%c("id","target"))], type = "prob")
       auc_table[j,i] <- auc(as.numeric(testset$target), prediction[,1])
       

       }

    auc_table$auc_mean <- mean(as.numeric(auc_table[j,1:k]))
    progress.bar$step()
    j <- j+1

  }
}


#prediction
#newdat <- expand.grid( x1=0:1, x2=0:5)
#class(newdat)
