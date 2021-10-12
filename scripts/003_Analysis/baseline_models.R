#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Baseline Models
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)

# LOAD DATA ---------------------------------------------------------------

load('../../data/model_data_no_impute.rda')
load('../../data/processed_data.rda')

# PREPARE DATA ------------------------------------------------------------
baseline_preds <- names(shhs1)[names(shhs1) %in% preds]
# remove hosnr02
baseline_preds <- baseline_preds[baseline_preds != 'hosnr02']
outcome_vars <- c('any_cvd', 'any_chd', 'cvd_death', 'chd_death')

model_dat_filt <- model_data[, c('nsrrid', baseline_preds, outcome_vars) ] %>% 
  filter(!is.na(any_cvd), !is.na(any_chd), !is.na(cvd_death), !is.na(chd_death))
# What percentage of the data is considered complete case?
sum(complete.cases(model_dat_filt))/nrow(model_dat_filt)
#  0.5216184
# this is a pretty small percentage of the data. We should look into imputation methods

# What fields have the highest missingness
apply(model_dat_filt, 2, function(x) return(sum(is.na(x))))

# Some of the labs have high missingness (hosnr02 1138) - frequency of snoring is the most missing variable, may want to just drop it 

baseline_preds <- baseline_preds[baseline_preds != 'hosnr02']
model_dat_filt <- model_data[, c('nsrrid', baseline_preds, outcome_vars) ] %>% 
  filter(!is.na(any_cvd), !is.na(any_chd), !is.na(cvd_death), !is.na(chd_death))
# What percentage of the data is considered complete case?
sum(complete.cases(model_dat_filt))/nrow(model_dat_filt)
# 0.6580722

# complete cases data
model_dat_filt_cc <- model_dat_filt[complete.cases(model_dat_filt), ]

# TRAIN TEST SPLIT --------------------------------------------------------
# Complete case
train_ind <- sample(1:nrow(model_dat_filt_cc), size = floor(nrow(model_dat_filt_cc)*.8))
test_ind <- 1:nrow(model_dat_filt_cc)
test_ind <- test_ind[!test_ind %in% train_ind]
# BASELINE MODELS ---------------------------------------------------------
# Models that only have sleep architecture variables for predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat 
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

# Create model statements for four separate models (one with each outcome)
model_form <- paste(outcome_vars, "~", paste0(sleep_preds, collapse = "+"))

## A. Logistic regression (no regularization, complete cases, training set)

get_model_metrics <- function(test_y, preds) {
  precision <- posPredValue(preds, test_y)
  recall <- sensitivity(preds, test_y)
  F1 <- (2 * precision * recall) / (precision + recall)
  print(paste("Positive Predictive Value: ", precision))
  print(paste("Recall: ", recall))
  print(paste("F1 Score: ", F1))
  print(paste("Accuracy: ", mean(preds %>% as.numeric() == test_y %>% as.numeric())))
}

models_A <- list()
pred_probs_A <- list()
pred_class_A <- list()
for(i in seq_along(model_form)) {
  models_A[[i]] <- glm(model_form[i], family = 'binomial', data = model_dat_filt_cc[train_ind, ])
  pred_probs_A[[i]] <- predict(models_A[[i]], model_dat_filt_cc[test_ind, ], type = "response")
  pred_class_A[[i]] <- ifelse(pred_probs_A[[i]] > 0.5, 1, 0)
  print(paste("Modeling: ", outcome_vars[i]))
  get_model_metrics(as.factor(model_dat_filt_cc[test_ind, outcome_vars[i]] %>% unlist()), as.factor(pred_class_A[[i]]))
  print(summary(models_A[[i]]))
}

## B. Logistic regression (Lasso penalty)
# glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")
models_B <- list()
pred_probs_B <- list()
pred_class_B <- list()
for(i in seq_along(model_form)) {
  X_train <- data.matrix(model_dat_filt_cc[, sleep_preds][train_ind, ])
  y_train <- data.matrix(model_dat_filt_cc[, outcome_vars[i]][train_ind, ])
  X_test <- data.matrix(model_dat_filt_cc[, sleep_preds][test_ind, ])
  y_test <- data.matrix(model_dat_filt_cc[, outcome_vars[i]][test_ind, ]) 
  
  models_B[[i]] <- glmnet(X_train, y_train, family = 'binomial')
  pred_probs_B[[i]] <- predict(models_B[[i]], X_test, type = "response", s = min(models_B[[i]]$lambda))
  pred_class_B[[i]] <- ifelse(pred_probs_B[[i]] > 0.5, 1, 0)
  print(paste("Modeling: ", outcome_vars[i]))
  get_model_metrics(as.factor(y_test), as.factor(pred_class_B[[i]]))
  print(coef(models_B[[i]], s = min(models_B[[i]]$lambda)))
}



