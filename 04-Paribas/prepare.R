#Скачиваем данные

rm(list=ls()) 
setwd("/home/nazarov/07-competitions/04-Paribas/")
#setwd("J:/14 - конкурсы/01 -  beeline_bigdata/")
train <- read.csv(file="data/train.csv", header=TRUE, sep=",")
test <- read.csv(file="data/test.csv", header=TRUE, sep=",")
# 18 21
#train$ID <- 50001:100000


str(train,list.len=ncol(train) )
######################################################################
#общая визуализация и вывод в пдф

pdf(file="/home/nazarov/Рабочий стол/paribas.pdf")
#pdf(file="C:/Users/_/Desktop/Reality check/plots_33 (19.05.2014).pdf")
#train[,2] <- as.factor(train[,2])
for (i in 2:ncol(train)){
  #boxplot(train[,i]~train[,2], data=train)
  if(class(train[,i])!= "factor")
  hist(train[,i])
}
dev.off()   

#111
#####################################################################
#Rid off variables with zero std
### Removing constant features 
cat("removing constant features\n")
toRemove <- c()
feature.names <- names(train)

for (f in feature.names) {
  if (sd(train[[f]], na.rm =T)==0 ) {
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

library(caret)
lin.comb <- findLinearCombos(train, na.actionna.omit) 

train <- train[, -lin.comb$remove]

#sd(train[,15],na.rm=T)


#####################################################################
#create num variables from factors with too many levels
#train$ID <-50001:100000
test$target <- as.factor(-1)
temp <- train$target
train <- train[, c(-2)]
train$target <- temp
all.data <- data.frame(1:(114393+114321))
#Объединяем датасеты
for (i in 1:133){
  all.data[,i]<- c(train[,i], test[,i])
}
colnames(all.data) <- colnames(train)


str(all.data, list.len=ncol(all.data))
#Факторные переменные опять делаем факторными
for (i in 2:ncol(all.data)){
  #boxplot(train[,i]~train[,2], data=train)
  if(class(all.data[,i])== "integer")
    all.data[,i]= as.factor(all.data[,i])
}


#Меняем хэшированные значения на логарифм от частоты встречаемости значения
uni.factor.v22 <- unique(all.data[,24])

stat.v22 <- data.frame(1,1)

for(i in 1:length(uni.factor.v22)){
  stat.v22[i,1]<- as.numeric(uni.factor.v22)[i]
  stat.v22[i,2]<- length(all.data[which(all.data[,24]== stat.v22[i,1]),24]) 
}

v22<-stat.v22[order(-stat.v22[,2]),]
v22[,3]<-log10(v22[,2])
#hist(x18[,10],600)

for(i in 1:nrow(all.data)){
  all.data[i,length(colnames(all.data))+1] <- v22[which(all.data[i,24]==v22[,1]),3] 
}
colnames(all.data)[length(colnames(all.data))+1] <- "log10v22"
##########################

uni.factor.v56 <- unique(all.data[,57])

stat.v56 <- data.frame(1,1)

for(i in 1:length(uni.factor.v56)){
  stat.v56[i,1]<- as.numeric(uni.factor.v56)[i]
  stat.v56[i,2]<- length(all.data[which(all.data[,57]== stat.v56[i,1]),57]) 
}

v56<-stat.v56[order(-stat.v56[,2]),]
v56[,3]<-log10(v56[,2])
#hist(x18[,10],600)

for(i in 1:nrow(all.data)){
  all.data[i,length(colnames(all.data))+1] <- v56[which(all.data[i,57]==v56[,1]),3] 
}
colnames(all.data)[length(colnames(all.data))+1] <- "log10v56"





#####################################################################
#Get only important features
library(randomForest)
rf1 <- randomForest(train[,2] ~ ., train[,-c(1,24,127,57)] , ntree=50, norm.votes=FALSE, na.action = na.omit)
tt<-importance(rf1)
tt[ order(-tt[,1]) , ]

###########################################################################################
# сохранение результатов на жесткий диск

# сохранение результатов работы
stuff <- list(data=all.data)  # список ценных объектов
saveRDS(file = "beeline_v1.RDS",stuff) # сохраняем всё ценное в файл
mylist <- readRDS("beeline_v1.RDS") # читаем из файла что там есть 
#res_sh <- head(mylist$data[order(-mylist$data[,2]) ,],20) # просматриваем лучшие результаты

#res_sh

###########################################################################################





