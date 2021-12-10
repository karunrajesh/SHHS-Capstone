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


# Put data type in row names
row.names(cvd_results_model1) <- cvd_results_model1$data_type
row.names(cvd_results_model2) <- cvd_results_model2$data_type
row.names(cvd_results_model3) <- cvd_results_model3$data_type

row.names(random_forests_model1) <- random_forests_model1$data_type
row.names(random_forests_model2) <- random_forests_model2$data_type
row.names(random_forests_model3) <- random_forests_model3$data_type

row.names(xgboosts_model1) <- xgboosts_model1$data_type
row.names(xgboosts_model2) <- xgboosts_model2$data_type
row.names(xgboosts_model3) <- xgboosts_model3$data_type

cvd_results_model1 <- cvd_results_model1 %>% select(-data_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

cvd_results_model2 <- cvd_results_model2 %>% select(-data_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

cvd_results_model3 <- cvd_results_model3 %>% select(-data_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

random_forests_model1 <- random_forests_model1 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

random_forests_model2 <- random_forests_model2 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

random_forests_model3 <- random_forests_model3 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

xgboosts_model1 <- xgboosts_model1 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

xgboosts_model2 <- xgboosts_model2 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

xgboosts_model3 <- xgboosts_model3 %>% select(-data_type, -model_type) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)



logisticReg <- cvd_results_model1 %>% 
  left_join(
    cvd_results_model2,
    by = "variable",
    suffix = c("", "_model2")
  ) %>% 
  left_join(
    cvd_results_model3,
    by = "variable",
    suffix = c("", "_model3")
  )

randomForest <- random_forests_model1 %>% 
  left_join(
    random_forests_model2,
    by = "variable",
    suffix = c("", "_model2")
  ) %>% 
  left_join(
    random_forests_model3,
    by = "variable",
    suffix = c("", "_model3")
  )

xgBoost <- xgboosts_model1 %>% 
  left_join(
    xgboosts_model2,
    by = "variable",
    suffix = c("", "_model2")
  ) %>% 
  left_join(
    xgboosts_model3,
    by = "variable",
    suffix = c("", "_model3")
  )


# OUTPUT ------------------------------------------------------------------

openxlsx::write.xlsx(list(logisticReg, randomForest, xgBoost), file = 'model_metrics.xlsx')
