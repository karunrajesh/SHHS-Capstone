#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Random Forest and XGBoost models
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

# Source cv function and model metrics
source('cv_functions.R')
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

# We tune mtry on the complete cases dataset, and get the best mtry = 1

model_statement = paste0('factor(any_cvd) ~ ', paste0(sleep_preds, collapse = " + "))

# 
# ### Base model
data_cc <- model_dat_filt_cc[, c(names(model_dat_filt_cc)[names(model_dat_filt_cc) %in% sleep_preds], 'any_cvd')]
# #Setting the default tuning parameters
# control <- trainControl(method='cv', number=10, search = 'grid') # does repeated train test splits
# tunegrid <- expand.grid(.mtry = 1:length(sleep_preds))
# # # Initializing and fitting the random forest
# # rfRules allows for hyperparameter tuning on maxdepth as well as mtry
# rf_model_base <- train(data_cc[1:nrow(data_cc), sleep_preds], factor(data_cc[1:nrow(data_cc), ][["any_cvd"]]), method = 'rf', 
#                        metric='Accuracy', tuneGrid=tunegrid, trControl = control)
# 
# # Get predictions
# preds <- predict(rf_model_base, data_cc, type = "raw") %>% as.numeric()-1
# 
# get_model_metrics(factor(data_cc$any_cvd), factor(preds), "randomForest", "complete_cases")
CV_MTRY = 1

#data_cc <- models[["complete_cases"]]
data_cc <- imputation_runner(model_dat_filt, "complete")
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
## CREATE A CV XGBOOST
xgb_model = xgboost(data=x_train,
                    label=train_outcome,
                    missing = NaN,
                    nrounds=200,
                    verbose=0,
                    eta=best_params$eta,
                    max_depth=best_params$Depth,
                    subsample=best_params$SubSampRate,
                    min_child_weight=1,
                    colsample_bytree=best_params$ColSampRate,
                    objective=obj,
                    eval_metric="rmse",
)
test$xgb <- predict(xgb_model, test_final, missing = NaN)

# CV TEST METRICS ---------------------------------------------------------

# Next we'll get the full cross-validated test metrics using the mtry above.
cv_results_rf(model_dat_filt,model_statement, "any_cvd", sleep_preds, mtry_param = CV_MTRY, "trad") 


