#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Evaluate Best Models
#########################



# SETUP -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)  


# FUNCTIONS ---------------------------------------------------------------


get_model_metrics <- function(test_y, preds, model_type_input, model_form_input) {
  FN <- sum(test_y == 1 & preds == 0)
  FNR <- FN/sum(test_y == 0) # false negative rate
  precision <- posPredValue(preds, test_y)
  recall <- sensitivity(preds, test_y)
  F1 <- (2 * precision * recall) / (precision + recall)
  metrics_output <- data.frame(
    model_type = model_type_input,
    model_form = model_form_input,
    FNR = FNR,
    PPV = precision,
    recall = recall,
    F1 = F1
    # Accuracy = mean(preds %>% as.numeric() == test_y %>% as.numeric())
  )
  return(metrics_output)
}


# Specific CV for logistic regression
cv_results <- function(dataname, model_form, outcome_var, models) {
  dt <- models[[dataname]]
  # rows in data
  n = nrow(dt)
  # number of folds to use for cross-validation
  K = 10
  # random ordering of all the available data
  permutation = sample(1:n)  
  # vector to hold metrics for each fold
  PPV_fold = rep(0,K)
  recall_fold = rep(0,K)
  F1_fold = rep(0,K)
  
  # loop through the K data splits and estimate test MSE for each
  for (j in 1:K) {
    # 1. extract indices of units in the pseudo-test set for split j
    pseudotest = permutation[floor((j-1)*n/K+1) : floor(j*n/K)]  
    # 2. extract indices of units in the pseudo-training set for split j
    pseudotrain = setdiff(1:n, pseudotest)
    # 3. Fit model on pseudotrain
    model <- glm(model_form, data = dt[pseudotrain, ])
    # 4. compute metrics on pseudotest
    preds <- predict(model, dt[pseudotest, ], type = "response")
    preds <- ifelse(preds > 0.5, 1, 0)
    metrics <- get_model_metrics(as.factor(dt[pseudotest, "any_cvd"] %>% unlist()), as.factor(preds), "LogisticRegression", model_form)
    PPV_fold[j] = metrics$PPV[1]
    recall_fold[j] = metrics$recall[1]
    F1_fold[j] = metrics$F1[1]
  }
  PPV <- mean(PPV_fold, na.rm =T)
  recall <- mean(recall_fold, na.rm = T)
  F1 <- mean(F1_fold, na.rm = T)
  
  
  return(data.frame(
    data_type = dataname,
    PPV = PPV,
    recall = recall,
    F1 = F1
  ))
  
}


# Specific CV for random forest
cv_results_rf <- function(dataname, model_form, outcome_var, models, pred_names, mtry_param) {
  dt <- models[[dataname]]
  # rows in data
  n = nrow(dt)
  # number of folds to use for cross-validation
  K = 10
  # random ordering of all the available data
  permutation = sample(1:n)  
  # vector to hold metrics for each fold
  PPV_fold = rep(0,K)
  recall_fold = rep(0,K)
  F1_fold = rep(0,K)
  
  # loop through the K data splits and estimate test MSE for each
  for (j in 1:K) {
    # 1. extract indices of units in the pseudo-test set for split j
    pseudotest = permutation[floor((j-1)*n/K+1) : floor(j*n/K)]  
    # 2. extract indices of units in the pseudo-training set for split j
    pseudotrain = setdiff(1:n, pseudotest)
    model <- randomForest::randomForest(dt[pseudotrain, sleep_preds], factor(dt[pseudotrain, ][["any_cvd"]]), mtry = mtry_param)
    # 4. compute metrics on pseudotest
    preds <- predict(model, dt[pseudotest, ], type = "response") %>% as.numeric()-1
    metrics <- get_model_metrics(as.factor(dt[pseudotest, "any_cvd"] %>% unlist()), as.factor(preds), "RandomForest", model_form)
    PPV_fold[j] = metrics$PPV[1]
    recall_fold[j] = metrics$recall[1]
    F1_fold[j] = metrics$F1[1]
  }
  PPV <- mean(PPV_fold, na.rm =T)
  recall <- mean(recall_fold, na.rm = T)
  F1 <- mean(F1_fold, na.rm = T)
  
  
  return(data.frame(
    data_type = dataname,
    PPV = PPV,
    recall = recall,
    F1 = F1
  ))
  
}
