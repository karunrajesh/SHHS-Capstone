#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Evaluate Best Models
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)  

# Source cv functions
source("cv_functions.R")

# LOAD DATA ---------------------------------------------------------------

# load('../../data/model_data_imputed.rda')
load('../../data/model_data_no_impute.rda')
load('../../data/processed_data.rda')
load('../../data/baseline_model_metrics.rda')  
load('../../tree_model_metrics.rda')


models = list("Complete Cases" = model_dat_filt_cc, "Mean Imputation" = trad_impute, "MICE Imputation" = mice_imputed)

# BEST MODELS -------------------------------------------------------------
metrics %>% 
  filter(str_detect(model_form, "any_cvd")) %>% 
  arrange(-PPV, -F1, FNR) %>% slice(1)

# Logistic regression, Trad_Impute
model_form_any_cvd <- "any_cvd ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest1p+timest2p+waso+rdi3p+ai_all+avgsat+minsat"

metrics %>% 
  filter(str_detect(model_form, "any_chd")) %>% 
  arrange(-PPV, -F1, FNR) %>% slice(1)

# Logistic regression, Trad_Impute
model_form_any_chd <- "any_chd ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest1p+timest2p+waso+rdi3p+ai_all+avgsat+minsat"

metrics %>% 
  filter(str_detect(model_form, "chd_death")) %>% 
  arrange(-PPV, -F1, FNR) %>% slice(1)

# Logistic regression, MICEImpute
model_form_chd_death <- "chd_death ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest1p+timest2p+waso+rdi3p+ai_all+avgsat+minsat"

metrics %>% 
  filter(str_detect(model_form, "cvd_death")) %>% 
  arrange(-PPV, -F1, FNR) %>% slice(1)
## No PPV, MICE Impute logistic regression has lowest FNR 
model_form_cvd_death <- "cvd_death ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest1p+timest2p+waso+rdi3p+ai_all+avgsat+minsat"


# K-fold cross validation -------------------------------------------------

cvd_results <- list()
for(impute in c("complete", "trad", "mice")) {
  cvd_results[[impute]] <- cv_results(model_dat_filt,model_form_any_cvd, "any_cvd", impute) 
}
# 
# cvd_results = list()
# for(name in names(models)){
#   cvd_results[[name]] <- cv_results(models[[name]], model_form_any_cvd, 'any_cvd', models)
# }

# chd_results = list()
# for(name in names(models)){
#   chd_results[[name]] <- cv_results(models[[name]], model_form_any_chd, 'any_chd', models)
# }
# 
# 
# cvd_death_results = list()
# for(name in names(models)){
#   cvd_death_results[[name]] <- cv_results(name, model_form_cvd_death, 'cvd_death', models)
# }
# 
# chd_death_results = list()
# for(name in names(models)){
#   chd_death_results[[name]] <- cv_results(name, model_form_chd_death, 'chd_death', models)
# }



cvd_results <- map_df(cvd_results, bind_rows)
# chd_results <- map_df(chd_results, bind_rows)
# cvd_death_results <- map_df(cvd_death_results, bind_rows)
# chd_death_results <- map_df(chd_death_results, bind_rows)

save(cvd_results, file = '../../data/cvd_baseline_model_metrics.rda')
# 
# 
# # DOWNSAMPLE TO MODEL DEATH OUTCOMES --------------------------------------
# cc_chd <- model_dat_filt_cc %>% filter(chd_death == 1) %>% nrow() # 180
# cc_cvd <- model_dat_filt_cc %>% filter(cvd_death == 1) %>% nrow() # 180
# whole_cvd <- mice_imputed %>% filter(cvd_death == 1) %>% nrow() # 235
# whole_chd <- mice_imputed %>% filter(chd_death == 1) %>% nrow() # 359
# 
# models_ds_chd <- list(
#   "Complete Cases" = bind_rows(
#     model_dat_filt_cc %>% filter(chd_death == 0) %>% sample_n(cc_chd),
#     model_dat_filt_cc %>% filter(chd_death == 1)
#   ),
#   "Mean Imputation"  = bind_rows(
#     trad_impute %>% filter(chd_death == 0) %>% sample_n(whole_chd),
#     trad_impute %>% filter(chd_death == 1)
#   ),
#   "MICE Imputation"  = bind_rows(
#     mice_imputed %>% filter(chd_death == 0) %>% sample_n(whole_chd),
#     mice_imputed %>% filter(chd_death == 1)
#   )
# )
# 
# models_ds_cvd <- list(
#   "Complete Cases" = bind_rows(
#     model_dat_filt_cc %>% filter(cvd_death == 0) %>% sample_n(cc_cvd),
#     model_dat_filt_cc %>% filter(cvd_death == 1)
#   ),
#   "Mean Imputation"  = bind_rows(
#     trad_impute %>% filter(cvd_death == 0) %>% sample_n(whole_cvd),
#     trad_impute %>% filter(cvd_death == 1)
#   ),
#   "MICE Imputation"  = bind_rows(
#     mice_imputed %>% filter(cvd_death == 0) %>% sample_n(whole_cvd),
#     mice_imputed %>% filter(cvd_death == 1)
#   )
# )
# 
# 
# cvd_death_results = list()
# for(name in names(models_ds_cvd)){
#   cvd_death_results[[name]] <- cv_results(name, model_form_cvd_death, 'cvd_death', models_ds_cvd)
# }
# 
# chd_death_results = list()
# for(name in names(models_ds_chd)){
#   chd_death_results[[name]] <- cv_results(name, model_form_chd_death, 'chd_death', models_ds_chd)
# }
# 
# 
# cvd_death_results <- map_df(cvd_death_results, bind_rows)
# chd_death_results <- map_df(chd_death_results, bind_rows)
    