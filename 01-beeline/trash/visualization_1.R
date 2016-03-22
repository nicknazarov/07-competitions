

rm(list=ls()) 
setwd("/home/nazarov/07-конкурсы/01-beeline/")
train <- read.csv(file="train.csv", header=TRUE, sep=",")

# 18 21
train[,19]
tail(train[1,11])




require(grDevices)
boxplot(train[,19],train[,63], data=train)
plot.factor(train[,19])

require(ggplot2)
qplot(train[,19], train[,63])


train2 <- train[order(train[,63]),c(19,22,63)]


unique(train[,19])


boxplot(train[,27]~train[,63], data=train)


######################################################################
pdf(file="/home/nazarov/Рабочий стол/beeline.pdf")
#pdf(file="C:/Users/_/Desktop/Reality check/plots_33 (19.05.2014).pdf")


for (i in 24:62){
  boxplot(train[,i]~train[,63], data=train)

}

dev.off()   
#####################################################################
uni.factor.x18 <- unique(train[,19])
stat.x18 <- data.frame(1,1,1,1,1,1,1,1)

for(i in 1:length(uni.factor.x18)){
  stat.x18[i,1]<- levels(uni.factor.x18)[i]
  for(j in 0:6){
    stat.x18[i,j+2]<-length(which( uni.factor.x18[i]==train2[train2[,3]==j,1]))
  }
}
stat.x18[,9]<-stat.x18[,2]+stat.x18[,3]+stat.x18[,4]+stat.x18[,5]+stat.x18[,6]+stat.x18[,7]+stat.x18[,8]

dd<-stat.x18[order(-stat.x18[,9]),]

hist(stat.x18[,9],100)

#####################################################################

uni.factor.x21 <- unique(train[,22])
stat.x21 <- data.frame(1,1,1,1,1,1,1,1)

for(i in 1:length(uni.factor.x21)){
  stat.x21[i,1]<- levels(uni.factor.x21)[i]
  for(j in 0:6){
    stat.x21[i,j+2]<-length(which( uni.factor.x21[i]==train2[train2[,3]==j,2]))
  }
}
stat.x21[,9]<-stat.x21[,2]+stat.x21[,3]+stat.x21[,4]+stat.x21[,5]+stat.x21[,6]+stat.x21[,7]+stat.x21[,8]

dd<-stat.x21[order(-stat.x21[,9]),]

hist(stat.x21[,9],100)


#####################################################################

uni.factor.x10 <- unique(train[,11])
stat.x10 <- data.frame(1,1,1,1,1,1,1,1)

for(i in 1:length(uni.factor.x10)){
  stat.x10[i,1]<- levels(uni.factor.x10)[i]
  for(j in 0:6){
    stat.x10[i,j+2]<-length(which( uni.factor.x10[i]==train[train[,63]==j,11]))
  }
}
stat.x10[,9]<-stat.x10[,2]+stat.x10[,3]+stat.x10[,4]+stat.x10[,5]+stat.x10[,6]+stat.x10[,7]+stat.x10[,8]

dd<-stat.x10[order(-stat.x10[,9]),]

hist(stat.x10[,9],500)

