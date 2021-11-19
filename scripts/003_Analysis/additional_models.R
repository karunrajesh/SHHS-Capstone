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
# load('../../data/processed_data.rda')
# Load hyperparameters
load('../../data/hyperparameters.rda')

#models = list("complete_cases" = model_dat_filt_cc, "TradImpute" = trad_impute, "MICEImpute" = mice_imputed)
impute_methods <- c("trad", "mice", "complete")

sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")



# HYPERPARMETER TUNING ----------------------------------------------------

# Get best parameters from the random forest and XGBoost tuning
RF_MTRY <- rf_model_base$bestTune %>% as.numeric()

xgb_tune$bestTune$nrounds


# CV TEST METRICS ---------------------------------------------------------
model_statement = paste0('factor(any_cvd) ~ ', paste0(sleep_preds, collapse = " + "))
# Next we'll get the full cross-validated test metrics using the mtry above.
random_forests <- list()
for(impute in c("complete", "trad", "mice")) {
  random_forests[[impute]] <- cv_results_rf(model_dat_filt,model_statement, "any_cvd", sleep_preds, mtry_param = RF_MTRY, impute) 
}

random_forests <- map_df(random_forests, bind_rows)
random_forests$model_type <- 'randomForest'


xgboosts <- list()
for(impute in c("complete", "trad", "mice")) {
  xgboosts[[impute]] <- cv_results_xgb(model_dat_filt,model_statement, "any_cvd", sleep_preds, xgb_tune, impute) 
}

xgboosts <- map_df(xgboosts, bind_rows)
xgboosts$model_type <- 'xgBoost'



# SAVE MODEL RESULTS ------------------------------------------------------
save(random_forests, xgboosts, file = '../../data/tree_model_metrics.rda')

