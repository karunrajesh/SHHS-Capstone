#########################
# Title: HDSC325: Capstone Final Metrics
# Purpose: Display the final metrics from models 
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD MODEL METRICS ------------------------------------------------------
load('../../data/tree_model_metrics.rda')
load('../../data/cvd_baseline_model_metrics.rda')


# COMBINE METRICS ---------------------------------------------------------

cvd_results_model1$model_type <- "logisticRegression"
cvd_results_model2$model_type <- "logisticRegression"

cvd_results_model1 <- cvd_results_model1 %>% 
  mutate(
    model_type = "logisticRegression",
    model_form = "Model 1"
  )

cvd_results_model2 <- cvd_results_model2 %>% 
  mutate(
    model_type = "logisticRegression",
    model_form = "Model 2"
  )

random_forests_model1 <- random_forests_model1 %>% 
  mutate(
    model_form = "Model 1"
  )

random_forests_model2 <- random_forests_model2 %>% 
  mutate(
    model_form = "Model 2"
  )

xgboosts_model1 <- xgboosts_model1 %>% 
  mutate(
    model_form = "Model 1"
  )

xgboosts_model2 <- xgboosts_model2 %>% 
  mutate(
    model_form = "Model 2"
  )


results_model1 <- bind_rows(
  cvd_results_model1,
  random_forests_model1,
  xgboosts_model1,
)

results_model2 <- bind_rows(
  cvd_results_model2,
  random_forests_model2,
  xgboosts_model2
)

knitr::kable(results_model1) %>% kableExtra::kable_styling()
knitr::kable(results_model2) %>% kableExtra::kable_styling()
# 
# |data_type |       PPV|    recall|        F1|model_type         |
# |:---------|---------:|---------:|---------:|:------------------|
# |complete  | 0.7496659| 0.9943310| 0.8523871|logisticRegression |
# |trad      | 0.7630695| 0.9954385| 0.8592487|logisticRegression |
# |mice      | 0.7484361| 0.9894588| 0.8509655|logisticRegression |
# |complete  | 0.7553791| 0.9908246| 0.8547052|randomForest       |
# |trad      | 0.7499111| 0.9913865| 0.8515276|randomForest       |
# |mice      | 0.7602551| 0.9913203| 0.8587785|randomForest       |
# |complete  | 0.7509975| 0.9781317| 0.8480107|xgBoost            |
# |trad      | 0.7402614| 0.9893328| 0.8457231|xgBoost            |
# |mice      | 0.7511928| 0.9896069| 0.8512475|xgBoost            |