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

models = list("complete_cases" = model_dat_filt_cc, "TradImpute" = trad_impute, "MICEImpute" = mice_imputed)

metrics = list()

get_model_metrics <- function(test_y, preds, model_type_input, model_form_input) {
  FN <- sum(test_y == 1 & preds == 0)
  FNR <- FN/sum(test_y == 0) # false negative rate
  precision <- posPredValue(preds, test_y)
  recall <- sensitivity(preds, test_y)
  F1 <- (2 * precision * recall) / (precision + recall)
  print('output')
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

# Models that only have sleep architecture variables for predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat 
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

# Create model statements for four separate models (one with each outcome)
model_form <- paste(outcome_vars, "~", paste0(sleep_preds, collapse = "+"))
model_form_single <- expand.grid(outcome_vars, sleep_preds) %>% 
  mutate(model_form = paste(Var1,'~' ,Var2)) 

for(name in names(models)){
  model = models[[name]]
  # TRAIN TEST SPLIT --------------------------------------------------------
  train_ind <- sample(1:nrow(model), size = floor(nrow(model)*.7))
  test_ind <- 1:nrow(model)
  test_ind <- test_ind[!test_ind %in% train_ind]
  # BASELINE MODELS ---------------------------------------------------------
  ## A. Logistic regression (no regularization, complete cases, training set)
  models_A <- list()
  pred_probs_A <- list()
  pred_class_A <- list()
  pred_metrics_A <- list()
  for(i in seq_along(model_form)) {
    models_A[[i]] <- glm(model_form[i], family = 'binomial', data = model[train_ind, ])
    pred_probs_A[[i]] <- predict(models_A[[i]], model[test_ind, ], type = "response")
    pred_class_A[[i]] <- ifelse(pred_probs_A[[i]] > 0.5, 1, 0)
    print(paste("Modeling: ", outcome_vars[i]))
    pred_metrics_A[[i]] <- get_model_metrics(as.factor(model[test_ind, outcome_vars[i]] %>% unlist()), as.factor(pred_class_A[[i]]), "LogisticRegression", model_form[i])
    print(summary(models_A[[i]]))
  }
  
  ## B. Logistic regression (Lasso penalty)
  # glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")
  models_B <- list()
  pred_probs_B <- list()
  pred_class_B <- list()
  pred_metrics_B <- list()
  for(i in seq_along(model_form)) {
    X_train <- data.matrix(model[train_ind, sleep_preds])
    y_train <- data.matrix(model[train_ind, outcome_vars[i]])
    X_test <- data.matrix(model[test_ind, sleep_preds])
    y_test <- data.matrix(model[test_ind, outcome_vars[i]]) 
    
    models_B[[i]] <- glmnet(X_train, y_train, family = 'binomial')
    pred_probs_B[[i]] <- predict(models_B[[i]], X_test, type = "response", s = min(models_B[[i]]$lambda))
    pred_class_B[[i]] <- ifelse(pred_probs_B[[i]] > 0.5, 1, 0)
    print(paste("Modeling: ", outcome_vars[i]))
    pred_metrics_B[[i]] <- get_model_metrics(as.factor(y_test), as.factor(pred_class_B[[i]]), "LogisticRegressionLasso", model_form[i])
    print(coef(models_B[[i]], s = min(models_B[[i]]$lambda)))
  }
  
  ## C.  Logistic regression (no regularization, complete cases, training set, one covariate at a time)

  
  models_C <- list()
  pred_probs_C <- list()
  pred_class_C <- list()
  pred_metrics_C <- list()
  for(i in 1:nrow(model_form_single)) {
    print(paste("Modeling: ", model_form_single$model_form[i]))
    models_C[[i]] <- glm(model_form_single$model_form[i], family = 'binomial', data = model_dat_filt_cc)
    pred_probs_C[[i]] <- predict(models_C[[i]], model_dat_filt_cc, type = "response")
    pred_class_C[[i]] <- ifelse(pred_probs_C[[i]] > 0.5, 1, 0)
    pred_metrics_C[[i]] <- get_model_metrics(as.factor(model_dat_filt_cc[, model_form_single$Var1[i]] %>% unlist()), as.factor(pred_class_C[[i]]), "LogisticRegressionSingle", model_form_single$model_form[i])
    print(summary(models_C[[i]]))
  }
  
  
  model_metrics <- bind_rows(
    map_df(pred_metrics_A, bind_rows),
    map_df(pred_metrics_B, bind_rows),
    map_df(pred_metrics_C, bind_rows)
  ) %>% 
    mutate(
      imputation_type = name
    )
  
  metrics[[name]] <- model_metrics
}  


# OUTPUT MODEL METRICS ----------------------------------------------------
metrics <- map_df(metrics, bind_rows)

save(metrics, file = '../../data/baseline_model_metrics.rda')

