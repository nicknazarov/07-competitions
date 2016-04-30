

rm(list=ls())
#setwd("/home/nazarov/07-competitions/02/scripts/")
setwd("/home/nick/01-projects/07-competitions/02/scripts/")

source("lib_func.R")
libraryBoot()

#PATH <- "/home/nazarov/07-competitions/02/data/"
#PATH_2 <- "/home/nazarov/Рабочий стол/santander.pdf"
PATH <- "/home/nick/01-projects/07-competitions/02/data/"
PATH_2 <- "/home/nick/Рабочий стол/santander.pdf"

N_CAT <- 15
FOR_SEED <- 1234
PCT <- 0.3
N_FACTORS <- 1:30

cat("Getting data\n")
print(system.time(x_raw <- getData(PATH)))

cat("Transforming data\n")
print(system.time({
  x <- xform_data(x_raw, N_CAT)
}))

##############################################################################################
#str(x$train, list.len =168 )

cat("Feature selection data\n")
print(system.time(list_of_features <- imp_features_rf(x, FOR_SEED)))
top_N_features <- list_of_features[N_FACTORS,2]

cat("Plot features importance\n")
plot(list_of_features[,1])

cat("Print hist for top factors\n")
print_to_file_top_fact (PATH_2, x$train, top_N_features)

#save(list_of_features,file='list_of_features.RData')
#load('list_of_features.RData')

#много сильно коррелированных фичей
#corr <- as.data.frame(cor( x_raw$train[,top_N_features[!(top_N_features %in% c("n0","flag_big_mort")) ]]))
#######################################################################################


load('list_of_features.RData')
load('importance_xgboost.RDA')

imp_rf <- list_of_features [1:15,2]
imp_xgboost <- as.data.frame(importance_matrix)[1:18, 1]  
uniq_features <- union(setdiff(imp_rf ,imp_xgboost ), setdiff(imp_xgboost ,imp_rf )) 
top_N_features <- union(uniq_features, intersect(imp_rf ,imp_xgboost ))
#corr <- as.data.frame(cor( x_raw$train[,all_features [!(all_features  %in% c("n0","flag_big_mort")) ]] ))

top_N_features <- top_N_features[!(top_N_features %in% c("imp_op_var41_efect_ult3",
                                                         "imp_op_var41_ult1","saldo_medio_var5_hace3",
                                                         "imp_op_var39_comer_ult1","saldo_medio_var5_ult3",
                                                         " num_venta_var44_ult1","imp_reemb_var17_ult1 ")) ]
                
 
#top_N_features               
data2learn <- subSample(x, PCT, top_N_features)

training <- feature_eng(data2learn$training)
testing <- feature_eng(data2learn$testing)

#top_N_features
summary(training)










cat("Building Model\n")
print(system.time(
  subm <- buildSubModels(d$training, d$testing)
))
m <- buildModels(subm, d$testing)

model.results <- list(d=d, m=m)
subm <- NULL
m <- NULL
d <- NULL
gc()
print(system.time(save(model.results, file='submission.rda')))
gc()

evalResults.GiveMeCredit(model.results$m, model.results$d$training,
                         model.results$d$testing)
gc()

cat("Making Submission\n")
print(system.time(makeSubmission.GiveMeCredit(model.results$m)))
cat("Saving\n")
gc()
model.results