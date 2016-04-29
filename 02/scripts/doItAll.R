

rm(list=ls())
setwd("/home/nazarov/07-competitions/02/scripts/")
source("lib_func.R")
libraryBoot()

PATH <- "/home/nazarov/07-competitions/02/data/"
PATH_2 <- "/home/nazarov/Рабочий стол/santander.pdf"
N_CAT <- 15
FOR_SEED <- 1234
PCT <- 0.3
N_FACTORS <- 1:30

cat("Getting data\n")
print(system.time(x <- getData(PATH)))
cat("Transforming data\n")
print(system.time({
  x <- xform_data(x, N_CAT)
}))



cat("Feature selection data\n")
print(system.time(list_of_features <- imp_features_rf(x, FOR_SEED)))
top_N_features <- list_of_features[N_FACTORS,2]

cat("Plot features importance\n")
plot(list_of_features[,1])

cat("Print hist for top factors\n")
print_to_file_top_fact (PATH_2, x$train, top_N_features)


data2learn <- subSample(x, PCT,top_N_features)

training <- feature_eng(data2learn$training)
testing <- feature_eng(data2learn$testing)

#top_N_features








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