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

model1_covs <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model2_covs <- c("age_s1", "gender", "bmi_s1", "supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")


# SET K FOR CV ------------------------------------------------------------

K = 3

# CV TEST METRICS ---------------------------------------------------------


## RANDOM FOREST
# MODEL 1
model_statement1 = paste0('factor(any_cvd) ~ ', paste0(model1_covs, collapse = " + "))
# Next we'll get the full cross-validated test metrics using the mtry above.
random_forests_model1 <- list()
for(impute in c("complete", "trad", "mice")) {
  random_forests_model1[[impute]] <- cv_results_rf(model_dat_filt,model_statement1, "any_cvd", sleep_preds, mtry_param = rf_model_base_model1$bestTune %>% as.numeric(), impute, K) 
}

random_forests_model1 <- map_df(random_forests_model1, bind_rows)
random_forests_model1$model_type <- 'randomForest_model1'

# MODEL 2
model_statement2 = paste0('factor(any_cvd) ~ ', paste0(model2_covs, collapse = " + "))
# Next we'll get the full cross-validated test metrics using the mtry above.
random_forests_model2 <- list()
for(impute in c("complete", "trad", "mice")) {
  random_forests_model2[[impute]] <- cv_results_rf(model_dat_filt,model_statement2, "any_cvd", sleep_preds, mtry_param = rf_model_base_model1$bestTune %>% as.numeric(), impute, K) 
}

random_forests_model2 <- map_df(random_forests_model2, bind_rows)
random_forests_model2$model_type <- 'randomForest_model2'


## XGBOOST
# MODEL 1
xgboosts_model1 <- list()
for(impute in c("complete", "trad", "mice")) {
  xgboosts_model1[[impute]] <- cv_results_xgb(model_dat_filt,model_statement1, "any_cvd", sleep_preds, xgb_tune_model1, impute, K) 
}

xgboosts_model1 <- map_df(xgboosts_model1, bind_rows)
xgboosts_model1$model_type <- 'xgBoost_model1'

# MODEL 2
xgboosts_model2 <- list()
for(impute in c("complete", "trad", "mice")) {
  xgboosts_model2[[impute]] <- cv_results_xgb(model_dat_filt,model_statement2, "any_cvd", sleep_preds, xgb_tune_model2, impute, K) 
}

xgboosts_model2 <- map_df(xgboosts_model2, bind_rows)
xgboosts_model2$model_type <- 'xgBoost_model2'


# SAVE MODEL RESULTS ------------------------------------------------------
save(random_forests_model1, random_forests_model2, xgboosts_model1, xgboosts_model2, file = '../../data/tree_model_metrics.rda')

