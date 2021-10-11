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


# BASELINE MODELS ---------------------------------------------------------
# Models that only have sleep architecture variables for predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat 
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

# Create model statements for four separate models (one with each outcome)
model_form <- paste(outcome_vars, "~", paste0(sleep_preds, collapse = "+"))

## A. Logistic regression (no regularization, complete cases)
models_A <- list()
for(i in seq_along(model_form)) {
  models_A[[i]] <- glm(model_form[i], family = 'binomial', data = model_dat_filt[complete.cases(model_dat_filt), ])
}

# Looking at the summaries of these models, predictors avgsat have strongest relationship with
# any_cvd

# Predictors slpprdp, waso, rdi3p, avgsat have strongest relationship with any_chd


## B. Logistic regression (Lasso penalty)
# glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")
models_B <- list()
for(i in seq_along(model_form)) {
  X <- data.matrix(model_dat_filt[, sleep_preds][complete.cases(model_dat_filt[, sleep_preds]), ])
  y <- data.matrix(model_dat_filt[, outcome_vars[i]][complete.cases(model_dat_filt[, sleep_preds]), ])
  models_B[[i]] <- glmnet(X, y, family = 'binomial')
}
coef(models_B[[4]], s = min(models_B[[4]]$lambda))

# any_cvd - removes times34p
# any_chd - removes timeremp
# cvd_death - removes times34p, rdi3p
# chd_death - removes timeremp



