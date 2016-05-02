library(caret)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)

# load in the training data
df_train = read_csv("04-GiveMeSomeCredit/Data/cs-training.csv") %>%
  na.omit() %>%                                                            
  # listwise deletion 
  select(-`[EMPTY]`) %>%
  mutate(SeriousDlqin2yrs = factor(SeriousDlqin2yrs,                   
                                   # factor variable for classification
                                   labels = c("Failure", "Success")))

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                              
  # binary classification
  eta = 0.01,                                                                 
  # learning rate
  max.depth = 3,                                                               
  # max tree depth
  eval_metric = "auc"                                                          
  # evaluation/loss metric
)

# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = as.matrix(df_train %>%
                                   select(-SeriousDlqin2yrs)),
                label = df_train$SeriousDlqin2yrs,
                params = xgb_params_1,
                nrounds = 100,                                                
                # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                          
                # stop if no improvement within 10 trees
)

# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = as.matrix(df_train %>%
                                     select(-SeriousDlqin2yrs)),
                  label = df_train$SeriousDlqin2yrs,
                  nrounds = 100, 
                  nfold = 5,                                                   
                  # number of folds in K-fold
                  prediction = TRUE,                                           
                  # return the prediction using the final model 
                  showsd = TRUE,                                               
                  # standard deviation of loss across folds
                  stratified = TRUE,                                           
                  # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()





modelLookup("logistic")
