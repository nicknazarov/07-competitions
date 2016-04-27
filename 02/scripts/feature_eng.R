
library(xgboost)
library(Matrix)

set.seed(1234)


#######################################################
#           Reverse Feature Engineering               #
#                                                     #
#######################################################

### LOADING  ##########################################
PATH <- "/home/nazarov/07-competitions/02/data/"
train <- read.csv(paste0(PATH,"train.csv"))
test  <- read.csv(paste0(PATH,"test.csv"))

ncol0 <- ncol(train)
### 1 #################################################

#Similarly, you can remove variables with 0 variance using the function nearZeroVar again from caret package
#zero.var = nearZeroVar(train, saveMetrics=TRUE) train <- train[, !(zero.var$zeroVar)]
### Removing constant features 
cat("removing constant features\n")
toRemove <- c()
feature.names <- names(train)
for (f in feature.names) {
  if (sd(train[[f]])==0) {
    toRemove <- c(toRemove,f)
    cat(f,"is constant\n")
  }
}
train.names  <- setdiff(names(train), toRemove)
train        <- train[,train.names]
test.names   <- setdiff(names(test), toRemove)
test         <- test[,test.names]
toRemove
cat("-------------------------\n")

### 2 #################################################
# Removing features being a specific linear function 
# of other features where all coefficients

#library(caret)
#lin.comb <- findLinearCombos(train)
#train <- train[, -lin.comb$remove]
#test <- test[, -lin.comb$remove]

#Removing hghly correlated variables
cor_v<-abs(cor(train))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.9, arr.ind = T))
train <- train[,-unique(cor_f$row)]
test <- test[,-unique(cor_f$row)]
cat("-------------------------\n")


####################################################
removed <- ncol0-ncol(train)
cat("\n ",removed," features have been removed\n")
####################################################
#Make factor variables
#общая визуализация и вывод в пдф
'''
pdf(file="/home/nazarov/Рабочий стол/paribas.pdf")
#train[,2] <- as.factor(train[,2])
for (i in 2:ncol(train)){
  #boxplot(train[,i]~train[,2], data=train)
  if(class(train[,i])!= "factor")
    hist(train[,i])
}
dev.off()   
'''


factor.index <- c()

for(i in 1:ncol(train)){
  if( length(unique(train[,i])) < 15){
    factor.index <- c(factor.index, i)
  }
}
factor.index

for(i in 1:(length(factor.index)-1)){
   train[,factor.index[i]] <- as.factor(train[,factor.index[i]])
   test[,factor.index[i]] <- as.factor(test[,factor.index[i]])
}
train$TARGET <- as.factor(train$TARGET)

####################################################
##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train [, !(names(train) %in% c("TARGET"))], 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)
####################################################
#---limit vars in test based on min and max vals of train
print('Setting min-max lims on test data')
for(f in colnames(train[, !(names(train) %in% c("TARGET"))])){
  lim <- min(train[,f])
  test[test[,f]<lim,f] <- lim
  
  lim <- max(train[,f])
  test[test[,f]>lim,f] <- lim  
}
#---


####################################################
#RF modeling

library(randomForest)
set.seed(1234)
#importante features
rf_importance <- randomForest(TARGET ~ ., train[,!(names(train) %in% c("ID"))] , ntree=500,
                    norm.votes=F,
                    mtry=2)

imp_fact <- as.data.frame(importance(rf_importance))
imp_fact[,2] <- rownames( imp_fact )
temp_df <- as.data.frame(imp_fact[ order(-imp_fact[,1]) , ])
#top 60, others useless I think 
most_imp_fact <- rownames( temp_df  )[1:60]




tr <- trainControl(method = "cv", number = 5)
train(TARGET ~ ., data=train[,c("TARGET",most_imp_fact)] ,method="rf",trControl= tr)




### SAVING #########################################

#write.csv(train, "../input/train1.csv", row.names=F, quote=F)
#write.csv(test, "../input/test1.csv", row.names=F, quote=F)
