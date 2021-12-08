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

cvd_results_model3 <- cvd_results_model3 %>% 
  mutate(
    model_type = "logisticRegression",
    model_form = "Model 3"
  )

random_forests_model1 <- random_forests_model1 %>% 
  mutate(
    model_form = "Model 1"
  )

random_forests_model2 <- random_forests_model2 %>% 
  mutate(
    model_form = "Model 2"
  )

random_forests_model3 <- random_forests_model3 %>% 
  mutate(
    model_form = "Model 3"
  )

xgboosts_model1 <- xgboosts_model1 %>% 
  mutate(
    model_form = "Model 1"
  )

xgboosts_model2 <- xgboosts_model2 %>% 
  mutate(
    model_form = "Model 2"
  )

xgboosts_model3 <- xgboosts_model3 %>% 
  mutate(
    model_form = "Model 3"
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


results_model3 <- bind_rows(
  cvd_results_model3,
  random_forests_model3,
  xgboosts_model3
)

knitr::kable(results_model1) %>% kableExtra::kable_styling()
knitr::kable(results_model2) %>% kableExtra::kable_styling()
knitr::kable(results_model3) %>% kableExtra::kable_styling()
