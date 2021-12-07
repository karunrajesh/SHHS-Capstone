#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Evaluate Best Models
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)  

# Source cv functions
source("cv_functions.R")

# LOAD DATA ---------------------------------------------------------------

# load('../../data/model_data_imputed.rda')
load('../../data/model_data_no_impute.rda')
load('../../data/processed_data.rda')

# 
# models = list("Complete Cases" = model_dat_filt_cc, "Mean Imputation" = trad_impute, "MICE Imputation" = mice_imputed)

model1_covs <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model2_covs <- c("age_s1", "gender", "bmi_s1", "supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")


# MODEL DEFINITIONS -------------------------------------------------------

# MODEL 1
model_form_any_cvd1 <- paste0("any_cvd ~ ", paste0(model1_covs, collapse = "+"))

# MODEL 2
model_form_any_cvd2 <- paste0("any_cvd ~ ", paste0(model2_covs, collapse = "+"))


# K-fold cross validation -------------------------------------------------
K = 3

# MODEL 1
cvd_results_model1 <- list()
for(impute in c("complete", "trad", "mice")) {
  cvd_results_model1[[impute]] <- cv_results(model_dat_filt,model_form_any_cvd1, "any_cvd", impute, K) 
}
# MODEL 2
cvd_results_model2 <- list()
for(impute in c("complete", "trad", "mice")) {
  cvd_results_model2[[impute]] <- cv_results(model_dat_filt,model_form_any_cvd2, "any_cvd", impute, K) 
}

cvd_results_model1 <- map_df(cvd_results_model1, bind_rows)
cvd_results_model2 <- map_df(cvd_results_model2, bind_rows)


# SAVE RESULTS ------------------------------------------------------------

save(cvd_results_model1, cvd_results_model2, file = '../../data/cvd_baseline_model_metrics.rda')

    