
library(imputation)
library(randomForest)
# Заменим пропуски модой
library(modeest)
library(caret)
###########################################################################################
#1) Загрузка данных   ##########
###########################################################################################
setwd("/home/nazarov/07-конкурсы/01-beeline/")
mylist <- readRDS("beeline_v1.RDS") # читаем из файла что там есть 
data.set<- mylist[[1]]
###########################################################################################
#2) Заменим пропуски модой   ##########
###########################################################################################
data.set.na1<-data.set
for(i in 1:ncol(data.set)){
  if(length(is.na(data.set[,i]))>0) {
    data.set.na1[is.na(data.set[,i]),i] <- mlv(data.set[!is.na(data.set[,i]),i], method = "mfv") [[1]]
  }
}

#Заменим пропуски методом kNN http://www.unt.edu/rss/class/Jon/Benchmarks/MissingValueImputation_JDS_Nov2010.pdf
# 2
install.packages("DMwR")
library(DMwR)
imputed.mis <- knnImputation(misData, 5)
round(imputed.mis  [,5] )
# 3
install.packages("rrcovNA")
mis <- data.set[1:400,]
library(rrcovNA)
summary(mis)
imputed.mis <- as.data.frame(impSeq(mis))
summary(imputed.mis) 
# 4
library(imputation)
knn.data.set <- knnImputation(data.set, 3)

###########################################################################################
#3) Разбивка на train и test тренировочного множества   ##########
###########################################################################################
data.train <- data.set.na1[data.set.na1$y>-1,]
data.test <- data.set.na1[data.set.na1$y==-1,]
###########################################################################################
#4) Моделирование  ##########
###########################################################################################
# model 1
rf1 <- randomForest(y ~ ., data.train , ntree=50, norm.votes=FALSE)

#Просмотр важности факторов
tt<-importance(rf1)
tt[ order(-tt[,1]) , ]

# Разбивка на train и test тренировочного множества
split <- runif(dim(data.train)[1]) > 0.2
train.rf2 <- data.train[split,]
test.rf2 <- data.train[!split,]

# model 2
rf2 <- randomForest(as.factor(y) ~ .,train.rf2 [,c(23,15,9,31,18,20,24,30,12,57,60,33,25,21,29,61,28,59,65,66,67,63)] , ntree=50, norm.votes=FALSE)
predictions <- predict(rf2, test.rf2)
#predictions 
formatrix <-  table (predictions,as.factor(test.rf2$y))
confusionMatrix(formatrix)

# model 3
set.seed(45)
rf3 <- randomForest(as.factor(y) ~ .,train.rf2 [,c(23,15,9,31,18,20,24,30,12,57,60,33,25,21,29,61,28,59,65,66,67,63)] , ntree=50, norm.votes=FALSE)

# model 4
library(party)
set.seed(415)

# Создание факторных переменных
factor.index <- c(1,2,3,4,5,6,7,8,10,12,13,15,16,17,18,20,21,23)
for(i in 1:length(factor.index)){
  data.set.na1[,factor.index[i]] <-as.factor(data.set.na1[,factor.index[i]])
}

#rf4 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
#                 data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

###########################################################################################
#5) Submit результатов ##########
###########################################################################################
prediction.for.submit <- predict(rf2, data.test[,c(23,15,9,31,18,20,24,30,12,57,60,33,25,21,29,61,28,59,65,66,67,63)])
submit <- data.frame(ID = data.test$ID, y = prediction.for.submit )
write.csv(submit, file = "sol.csv", row.names = FALSE)


