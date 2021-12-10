# SETUP -------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)
library(randomForest)

load('../../data/model_data_no_impute.rda')
load('../../data/hyperparameters.rda')
source("imputation.R")

### Imputation Methods to Use
impute_methods <- c("trad", "mice", "complete")

sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")


### Models to Run
model1_covs <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model2_covs <- c("age_s1", "gender", "bmi_s1", "supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")

model3_covs <- c("waso", "timest1p", "timest2p", "times34p", "timeremp", "supinep",  "ai_all",   "rdi3p", "slpprdp", "slpeffp",  "gender",   "race",  "mstat",  "systbp", "diasbp",  "chol",  "hdl",  "trig",    "fev1",     "fvc",      "neck20",   "legcrp02", "pf_s1",    "bp_s1",    "gh_s1",    "mh_s1",    "pcs_s1", 
                 "mcs_s1",   "age_s1",   "ess_s1",   "bmi_s1",   "educat",   "waist",    "height",   "avgsat",   "minsat") 

# Impute the train and test datasets using imputation methods
train_dat <- imputation_runner(model_dat_filt, "complete")
# Train models
model_results <- function(covars, train_dat){

  
  model_statement1 = paste0('factor(any_cvd) ~ ', paste0(covars, collapse = " + "))
  set.seed(71)
  model <- glm(model_statement1, data = train_dat, family = "binomial")
  preds_probs <- predict(model, train_dat, type = "response")
  preds <- ifelse(preds_probs > 0.5, 1, 0)
  return(list(preds, preds_probs))
  
}

m1 <- model_results(model1_covs, train_dat)
m2 <- model_results(model2_covs, train_dat)
m3 <- model_results(model3_covs, train_dat)
train_dat$model1 <- factor(m1[[1]])
train_dat$model2 <- factor(m2[[1]])
train_dat$model3 <- factor(m3[[1]])

train_dat$any_cvd <- factor(train_dat$any_cvd)

# Model 1
list(
  "mod1_precision" =train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      precision=caret::precision(data=model1, reference=any_cvd)
    ) ,
  "mod1_recall" = train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      recall=caret::recall(data=model1, reference=any_cvd)
    ),
  "mod2_precision" = train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      precision=caret::precision(data=model2, reference=any_cvd)
    ),
  "mod2_recall" = train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      recall=caret::recall(data=model2, reference=any_cvd)
    ),
  "mod3_recall" = train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      precision=caret::precision(data=model3, reference=any_cvd)
    ),
  "mod3_precision" = train_dat %>% 
    mutate(
      age_cat = cut(age_s1, c(40,65, 80, 90), include.lowest = T)
    ) %>% 
    group_by(race, gender, age_cat) %>% 
    summarize(
      cnt = n(),
      recall=caret::recall(data=model3, reference=any_cvd)
    ) 
) %>% openxlsx::write.xlsx('metrics_race_age_gender.xlsx')

