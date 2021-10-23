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
library(mice)

# LOAD DATA ---------------------------------------------------------------

load('../../data/model_data_imputed.rda')
load('../../data/processed_data.rda')

models = list(model_dat_filt_cc, trad_impute, mice_imputed)


get_model_metrics <- function(test_y, preds) {
  precision <- posPredValue(preds, test_y)
  recall <- sensitivity(preds, test_y)
  F1 <- (2 * precision * recall) / (precision + recall)
  print(paste("Positive Predictive Value: ", precision))
  print(paste("Recall: ", recall))
  print(paste("F1 Score: ", F1))
  print(paste("Accuracy: ", mean(preds %>% as.numeric() == test_y %>% as.numeric())))
}

# Models that only have sleep architecture variables for predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat 
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

# Create model statements for four separate models (one with each outcome)
model_form <- paste(outcome_vars, "~", paste0(sleep_preds, collapse = "+"))

for(model in models){
  # TRAIN TEST SPLIT --------------------------------------------------------
  train_ind <- sample(1:nrow(model), size = floor(nrow(model)*.8))
  test_ind <- 1:nrow(model)
  test_ind <- test_ind[!test_ind %in% train_ind]
  # BASELINE MODELS ---------------------------------------------------------
  ## A. Logistic regression (no regularization, complete cases, training set)
  models_A <- list()
  pred_probs_A <- list()
  pred_class_A <- list()
  for(i in seq_along(model_form)) {
    models_A[[i]] <- glm(model_form[i], family = 'binomial', data = model[train_ind, ])
    pred_probs_A[[i]] <- predict(models_A[[i]], model[test_ind, ], type = "response")
    pred_class_A[[i]] <- ifelse(pred_probs_A[[i]] > 0.5, 1, 0)
    print(paste("Modeling: ", outcome_vars[i]))
    get_model_metrics(as.factor(model[test_ind, outcome_vars[i]] %>% unlist()), as.factor(pred_class_A[[i]]))
    print(summary(models_A[[i]]))
  }
  
  ## B. Logistic regression (Lasso penalty)
  # glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")
  models_B <- list()
  pred_probs_B <- list()
  pred_class_B <- list()
  for(i in seq_along(model_form)) {
    X_train <- data.matrix(model[train_ind, sleep_preds])
    y_train <- data.matrix(model[train_ind, outcome_vars[i]])
    X_test <- data.matrix(model[test_ind, sleep_preds])
    y_test <- data.matrix(model[test_ind, outcome_vars[i]]) 
    
    models_B[[i]] <- glmnet(X_train, y_train, family = 'binomial')
    pred_probs_B[[i]] <- predict(models_B[[i]], X_test, type = "response", s = min(models_B[[i]]$lambda))
    pred_class_B[[i]] <- ifelse(pred_probs_B[[i]] > 0.5, 1, 0)
    print(paste("Modeling: ", outcome_vars[i]))
    get_model_metrics(as.factor(y_test), as.factor(pred_class_B[[i]]))
    print(coef(models_B[[i]], s = min(models_B[[i]]$lambda)))
  }
  
  ## C.  Logistic regression (no regularization, complete cases, training set, one covariate at a time)
  model_form_single <- expand.grid(outcome_vars, sleep_preds) %>% 
    mutate(model_form = paste(Var1,'~' ,Var2)) 
  
  models_C <- list()
  pred_probs_C <- list()
  pred_class_C <- list()
  for(i in 1:nrow(model_form_single)) {
    print(paste("Modeling: ", model_form_single$model_form[i]))
    models_C[[i]] <- glm(model_form_single$model_form[i], family = 'binomial', data = model_dat_filt_cc)
    pred_probs_C[[i]] <- predict(models_C[[i]], model_dat_filt_cc, type = "response")
    pred_class_C[[i]] <- ifelse(pred_probs_C[[i]] > 0.5, 1, 0)
    
    get_model_metrics(as.factor(model_dat_filt_cc[, model_form_single$Var1[i]] %>% unlist()), as.factor(pred_class_C[[i]]))
    print(summary(models_C[[i]]))
  }
  
  
  
}
