# place 1
#set working directory
setwd("F:/Credit scoring")

data <- read.csv("data.csv", header=T)
colnames (data)

##############
# create validation 
##############
#
train <- sample(nrow(data), floor(nrow(data) * 0.66)) 
training <- data[train, ] 
validation <- data[-train, ] 
remove (train)   # remove data to free up space 

#######################
# SMOTE
#######################
# Literature: http://cran.r-project.org/web/packages/DMwR/DMwR.pdf

require(DMwR)
# must set ydata as factor and has to be placed at the end!!!!!!!!
data$ydata <- as.factor(data$ydata)
data <- SMOTE(ydata ~ ., data
                ,k = 5
                ,perc.over = 700,perc.under=200)
table(data$ydata)


####################################################
#modelling procedure
####################################################

################
#GBM  
################
require(gbm)

model <- gbm(
  ydata ~ ., 
  distribution = "adaboost", 
  data = data, 
  #var.monotone = NULL,
  n.trees = 5000,
  interaction.depth = 4,
  n.minobsinnode = 30,
  shrinkage = 0.05,
  bag.fraction = 0.2,
  train.fraction = 0.8,
  #cv.folds=5,
  #keep.data = TRUE,
  verbose = TRUE)

predict <- predict.gbm (model, test, n.trees = 5000, type = "response")

################
# Random Forest
################
require(randomForest)
data$ydata <- as.factor(data$ydata)

model <- randomForest (ydata ~ ., data, ntree = 250, nodesize = 100)
predict <- predict (model, test, type = "prob")


################
#caret
################
#http://cran.r-project.org/web/packages/caret/caret.pdf
require(caret)
require(MASS)

data$ydata <- as.factor(data$ydata)
model <- train (ydata ~ ., data, method = "xxx")
# replace xxx with a function found in caret

predict <- predict (model, test)

#################
# write output
#################

write.csv(predict, file = "predict.csv", quote = FALSE, row.names = FALSE)


