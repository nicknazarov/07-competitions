#Скачиваем данные

rm(list=ls()) 
setwd("/home/nazarov/07-конкурсы/01-beeline/")
#setwd("J:/14 - конкурсы/01 -  beeline_bigdata/")
train <- read.csv(file="train.csv", header=TRUE, sep=",")
test <- read.csv(file="test.csv", header=TRUE, sep=",")
# 18 21
#train$ID <- 50001:100000

######################################################################
#общая визуализация и вывод в пдф

pdf(file="/home/nazarov/Рабочий стол/beeline.pdf")
#pdf(file="C:/Users/_/Desktop/Reality check/plots_33 (19.05.2014).pdf")

for (i in 24:62){
  boxplot(train[,i]~train[,63], data=train)
}
dev.off()   
#####################################################################
#as.numeric(train[,1])
#all.data <- data.frame(t(colnames(train)))
#all.data[-1,]
train$ID <-50001:100000
test$y <- -1
test[,65] <- test[,1]
test <- test[,-1]
colnames(test)[64]<- "ID"


all.data <- data.frame(1:100000)
#Объединяем датасеты
for (i in 1:64){
  all.data[,i]<- c(train[,i], test[,i])
}
colnames(all.data) <- colnames(train)
#####################################################################
#Для хэшированного фактора х18 строится таблица, где видно распределение значений для разных классов 
uni.factor.x18 <- unique(train[,19])
stat.x18 <- data.frame(1,1,1,1,1,1,1,1)

for(i in 1:length(uni.factor.x18)){
 # stat.x18[i,1]<- levels(uni.factor.x18)[i]
  stat.x18[i,1]<- uni.factor.x18[i]
  for(j in 0:6){
    stat.x18[i,j+2]<-length(which( uni.factor.x18[i]==train[train[,63]==j,19]))
  }
}
stat.x18[,9]<-stat.x18[,2]+stat.x18[,3]+stat.x18[,4]+stat.x18[,5]+stat.x18[,6]+stat.x18[,7]+stat.x18[,8]

x18<-stat.x18[order(-stat.x18[,9]),]

x18[,10]<-log10(x18[,9])
#hist(x18[,10],600)

for(i in 1:100000){
all.data[i,65] <- x18[which(all.data[i,19]==x18[,1]),10] 
}
colnames(all.data)[65] <- "log10x18"
#####################################################################
#Меняем хэшированные значения на логарифм от частоты встречаемости значения
uni.factor.x21 <- unique(all.data[,22])
stat.x21 <- data.frame(1,1)

for(i in 1:length(uni.factor.x21)){
  stat.x21[i,1]<- as.numeric(uni.factor.x21)[i]
  stat.x21[i,2]<- length(all.data[which(all.data[,22]==x21[i,1]),22]) 
}
x21<-stat.x21[order(-stat.x21[,2]),]

x21[,3]<-log10(x21[,2])
#hist(x18[,10],600)

for(i in 1:100000){
  all.data[i,66] <- x21[ which(all.data[i,22]==x21[,1]) ,3 ] 
}
colnames(all.data)[66] <- "log10x21"
#####################################################################
#Меняем хэшированные значения на логарифм от частоты встречаемости значения
uni.factor.x10 <- unique(all.data[,11])
stat.x10 <- data.frame(1,1)

for(i in 1:length(uni.factor.x10)){
  stat.x10[i,1]<- as.numeric(uni.factor.x10)[i]
  stat.x10[i,2]<- length(all.data[which(all.data[,11]==x10[i,1]),11]) 
}
x10<-stat.x10[order(-stat.x10[,2]),]
x10[,3]<-log10(x10[,2])
#hist(x18[,10],600)

for(i in 1:100000){
  all.data[i,67] <- x10[which(all.data[i,11]==x10[,1]),3] 
}
colnames(all.data)[67] <- "log10x10"

#####################################################################
#Проврека количеств уникальных значений для хэшированных значений в train и test

test.uni.factor.x18 <- unique(test[,20])
test.uni.factor.x21 <- unique(test[,23])
test.uni.factor.x10 <- unique(test[,12])


all.uni.factor.x18 <- unique(c(test.uni.factor.x18,uni.factor.x18))
all.uni.factor.x21 <- unique(c(test.uni.factor.x21,uni.factor.x21))
all.uni.factor.x10 <- unique(c(test.uni.factor.x10,uni.factor.x10))


###########################################################################################
# сохранение результатов на жесткий диск

# сохранение результатов работы
stuff <- list(data=all.data)  # список ценных объектов
saveRDS(file = "beeline_v1.RDS",stuff) # сохраняем всё ценное в файл
mylist <- readRDS("beeline_v1.RDS") # читаем из файла что там есть 
#res_sh <- head(mylist$data[order(-mylist$data[,2]) ,],20) # просматриваем лучшие результаты

#res_sh

###########################################################################################






