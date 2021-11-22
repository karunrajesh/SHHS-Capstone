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

cvd_results$model_type <- "logisticRegression"

results <- bind_rows(
  cvd_results,
  random_forests,
  xgboosts
)

knitr::kable(results)
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