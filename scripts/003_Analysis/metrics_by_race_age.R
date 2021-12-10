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

# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    precision=caret::precision(data=model1, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt| precision|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 0.8296703|
# |    1|      1|(64,80] | 251| 0.5755102|
# |    1|      1|(80,90] |  19| 0.5263158|
# |    1|      2|[40,64] | 246| 0.9512195|
# |    1|      2|(64,80] | 318| 0.7665615|
# |    1|      2|(80,90] |  46| 0.4222222|
# |    2|      1|(64,80] |  27| 0.4800000|
# |    2|      1|(80,90] |   5| 0.8000000|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 0.8717949|
# |    2|      2|(80,90] |   9| 0.2222222|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|

# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    recall=caret::recall(data=model1, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt|    recall|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 0.9869281|
# |    1|      1|(64,80] | 251| 0.9791667|
# |    1|      1|(80,90] |  19| 1.0000000|
# |    1|      2|[40,64] | 246| 1.0000000|
# |    1|      2|(64,80] | 318| 1.0000000|
# |    1|      2|(80,90] |  46| 1.0000000|
# |    2|      1|(64,80] |  27| 1.0000000|
# |    2|      1|(80,90] |   5| 1.0000000|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 0.9714286|
# |    2|      2|(80,90] |   9| 1.0000000|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|

# Model 2
# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    precision=caret::precision(data=model2, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt| precision|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 0.8270270|
# |    1|      1|(64,80] | 251| 0.6725146|
# |    1|      1|(80,90] |  19|        NA|
# |    1|      2|[40,64] | 246| 0.9512195|
# |    1|      2|(64,80] | 318| 0.7634069|
# |    1|      2|(80,90] |  46| 0.4666667|
# |    2|      1|(64,80] |  27| 0.5294118|
# |    2|      1|(80,90] |   5|        NA|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 0.8500000|
# |    2|      2|(80,90] |   9| 0.4000000|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|

# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    recall=caret::recall(data=model2, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt|    recall|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 1.0000000|
# |    1|      1|(64,80] | 251| 0.7986111|
# |    1|      1|(80,90] |  19| 0.0000000|
# |    1|      2|[40,64] | 246| 1.0000000|
# |    1|      2|(64,80] | 318| 0.9958848|
# |    1|      2|(80,90] |  46| 0.7368421|
# |    2|      1|(64,80] |  27| 0.7500000|
# |    2|      1|(80,90] |   5| 0.0000000|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 0.9714286|
# |    2|      2|(80,90] |   9| 1.0000000|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|

# Model 3

# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    precision=caret::precision(data=model3, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt| precision|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 0.8315217|
# |    1|      1|(64,80] | 251| 0.6855346|
# |    1|      1|(80,90] |  19| 1.0000000|
# |    1|      2|[40,64] | 246| 0.9512195|
# |    1|      2|(64,80] | 318| 0.7828947|
# |    1|      2|(80,90] |  46| 0.5600000|
# |    2|      1|(64,80] |  27| 0.5294118|
# |    2|      1|(80,90] |   5| 1.0000000|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 0.8750000|
# |    2|      2|(80,90] |   9| 0.0000000|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|

# Look at precision by race, age, and gender
train_dat %>% 
  mutate(
    age_cat = cut(age_s1, c(40,64, 80, 90), include.lowest = T)
  ) %>% 
  group_by(race, gender, age_cat) %>% 
  summarize(
    cnt = n(),
    recall=caret::recall(data=model3, reference=any_cvd)
  ) %>% knitr::kable()

# | race| gender|age_cat | cnt|    recall|
# |----:|------:|:-------|---:|---------:|
# |    1|      1|[40,64] | 185| 1.0000000|
# |    1|      1|(64,80] | 251| 0.7569444|
# |    1|      1|(80,90] |  19| 0.1000000|
# |    1|      2|[40,64] | 246| 1.0000000|
# |    1|      2|(64,80] | 318| 0.9794239|
# |    1|      2|(80,90] |  46| 0.7368421|
# |    2|      1|(64,80] |  27| 0.7500000|
# |    2|      1|(80,90] |   5| 0.7500000|
# |    2|      2|[40,64] |   1| 1.0000000|
# |    2|      2|(64,80] |  41| 1.0000000|
# |    2|      2|(80,90] |   9| 0.0000000|
# |    3|      1|[40,64] |   2| 1.0000000|
# |    3|      2|(64,80] |   3| 1.0000000|
