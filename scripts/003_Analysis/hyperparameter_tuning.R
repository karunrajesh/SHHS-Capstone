#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Hyperparameter tuning Random Forest and XGBoost models
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)
library(mice)

source('imputation.R')
# LOAD DATA ---------------------------------------------------------------

load('../../data/model_data_no_impute.rda')
#load('../../data/model_data_imputed.rda')
load('../../data/processed_data.rda')

#models = list("complete_cases" = model_dat_filt_cc, "TradImpute" = trad_impute, "MICEImpute" = mice_imputed)
impute_methods <- c("trad", "mice", "complete")

sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")



# HYPERPARMETER TUNING ----------------------------------------------------
# Hyperparameter tuning occurs on the complete cases version of the data
## A. RANDOM FOREST
# get the complete cases version of 
data_cc <- imputation_runner(model_dat_filt, "complete")

control <- trainControl(method='cv', number=10, search = 'grid') # does repeated train test splits
tunegrid <- expand.grid(.mtry = 1:length(sleep_preds))
# # Initializing and fitting the random forest
# rfRules allows for hyperparameter tuning on maxdepth as well as mtry
rf_model_base <- train(data_cc[1:nrow(data_cc), sleep_preds], factor(data_cc[1:nrow(data_cc), ][["any_cvd"]]), method = 'rf',
                       metric='Accuracy', tuneGrid=tunegrid, trControl = control)

rf_model_base$bestTune
#   mtry
# 1    1

## B. XGBOOST
## Train XGBoost
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  data_cc[1:nrow(data_cc), sleep_preds], 
  factor(data_cc[1:nrow(data_cc), ][["any_cvd"]]),
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

xgb_tune$bestTune
#   nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 1     200         2 0.025     0                1                1         1



# SAVE OUT HYPERPARAMETERS --------------------------------------------------

save(rf_model_base, xgb_tune, file ='../../data/hyperparameters.rda')

