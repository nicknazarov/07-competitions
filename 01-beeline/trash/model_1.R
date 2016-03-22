###########################################################################################
#1) Загрузка данных
setwd("/home/nazarov/07-конкурсы/01-beeline/")
mylist <- readRDS("beeline_v1.RDS") # читаем из файла что там есть 

###########################################################################################

data.set<- mylist[[1]]
data.set$train <- if (data.set$y==-1)  0 else 1
library(imputation)
library(randomForest)
data.train <- data.set[data.set$train==1,]

# Заменим пропуски модой
data.set.na1
for(i in 1:ncol(data.set)){
  if(length(is.na(data.set[,i]))>0) {
    data.set.na1 <- data.set[is.na(data.set[,i]),i]
  }

}
#########################################3
rf1 <- randomForest(y ~ ., data.train , ntree=50, norm.votes=FALSE)
#t<- kNNImpute(data.set, 3, x.dist = NULL, impute.fn, verbose = T)
