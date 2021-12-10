# SETUP -------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)
library(randomForest)

load('../../data/model_data_no_impute.rda')
load('../../data/hyperparameters.rda')
source("imputation.R")

### Imputation Methods to Use
impute_methods <- c("trad", "mice", "complete")

sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")


### Models to Run
model1_covs <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model2_covs <- c("age_s1", "gender", "bmi_s1", "supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model3_covs <- c("waso", "timest1p", "timest2p", "times34p", "timeremp", "supinep",  "ai_all",   "rdi3p", "slpprdp", "slpeffp",  "gender",   "race",  "mstat",  "systbp", "diasbp",  "chol",  "hdl",  "trig",    "fev1",     "fvc",      "neck20",   "legcrp02", "pf_s1",    "bp_s1",    "gh_s1",    "mh_s1",    "pcs_s1", 
                 "mcs_s1",   "age_s1",   "ess_s1",   "bmi_s1",   "educat",   "waist",    "height",   "avgsat",   "minsat") 


rf_results <- function(covars, impute_type, mtry_param){
  # Impute the train and test datasets using imputation methods
  train_dat <- imputation_runner(model_dat_filt, impute_type)
  
  model_statement1 = paste0('factor(any_cvd) ~ ', paste0(covars, collapse = " + "))
  set.seed(71)
  rf <- randomForest::randomForest(as.formula(model_statement1),data=train_dat, mtry=mtry_param, importance=TRUE)
  return(rf)
  
}

## RANDOM FOREST

rf_1 <- list()
rf_2 <- list()
rf_3 <- list()
for(impute in c("complete", "trad", "mice")) {
  rf_1[[impute]] <- rf_results(model1_covs, impute, rf_model_base_model1$bestTune %>% as.numeric())
  rf_2[[impute]] <- rf_results(model2_covs, impute, rf_model_base_model2$bestTune %>% as.numeric())
  rf_3[[impute]] <- rf_results(model3_covs, impute, rf_model_base_model3$bestTune %>% as.numeric())
}

rf_importance <- data.frame(variable_importance = 1:10,
           rf_1_c = names(sort(importance(rf_1$complete)[,1], decreasing = TRUE)[1:10]),
           rf_1_t = names(sort(importance(rf_1$trad)[,1], decreasing = TRUE)[1:10]),
           rf_1_m = names(sort(importance(rf_1$mice)[,1], decreasing = TRUE)[1:10]),
           rf_2_c = names(sort(importance(rf_2$complete)[,1], decreasing = TRUE)[1:10]),
           rf_2_t = names(sort(importance(rf_2$trad)[,1], decreasing = TRUE)[1:10]),
           rf_2_m = names(sort(importance(rf_2$mice)[,1], decreasing = TRUE)[1:10]),
           rf_3_c = names(sort(importance(rf_3$complete)[,1], decreasing = TRUE)[1:10]),
           rf_3_t = names(sort(importance(rf_3$trad)[,1], decreasing = TRUE)[1:10]),
           rf_3_m = names(sort(importance(rf_3$mice)[,1], decreasing = TRUE)[1:10]))

table1()
