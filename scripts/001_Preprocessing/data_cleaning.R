#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Preprocess Data
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)

# LOAD DATA ---------------------------------------------------------------

shhs1 <- read_csv('../../../../../shhs/datasets/shhs1-dataset-0.16.0.csv')
shhs2 <- read_csv('../../../../../shhs/datasets/shhs2-dataset-0.16.0.csv')
outcomes <- read_csv('../../../../../shhs/datasets/shhs-cvd-summary-dataset-0.16.0.csv')  


# CLEAN DATA --------------------------------------------------------------
# Lower case the column names
names(shhs1) <- tolower(names(shhs1))
names(shhs2) <- tolower(names(shhs2))

# SUBSET PREDICTORS -------------------------------------------------------
# Predictors suggested by Dr. Wang
# Sleep predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat  ## ADD IN ahi index
# Questionnaire – ess_s1, saqli, fosq, hosnr02, legcrp02, sh308a-sh308f
# Other predictors/covariates to consider:
# Anthropometry – bmi_s1, height, waist, neck20
# Demography – gender, race, educat, age_s1, mstat
# General Health – bp_s1, gh_s1, mcs_s1, mh_s1, pcs_s1, pf_s1
# Clinical data – chol, hdl, trig, diasbp, systbp, fev1, fvc
preds <- c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso',
           'rdi3p', 'ai_all', 'avgsat', 'minsat',  'ess_s1', 'saqli', 'fosq', 'hosnr02', 'legcrp02', 
           'sh308a', 'sh308b', 'sh308c', 'sh308d', 'sh308e', 'sh308f', 'bmi_s1', 'height', 'waist', 
           'neck20', 'gender', 'race', 'educat', 'age_s1', 'mstat', 'bp_s1', 'gh_s1', 'mcs_s1', 'mh_s1', 
           'pcs_s1', 'pf_s1', 'chol', 'hdl', 'trig', 'diasbp', 'systbp', 'fev1', 'fvc')

# Are these predictors in both baseline and follow-up data? 
preds[!preds %in% names(shhs1)] # no not all 
# "saqli"  "fosq"   "sh308a" "sh308b" "sh308c" "sh308d" "sh308e" "sh308f" these are not in shhs1s
preds[!preds %in% names(shhs2)]# no not all 
# "ess_s1"   "hosnr02"  "legcrp02" "bmi_s1"   "height"   "waist"    "neck20"   "educat"   "mstat"    "bp_s1"    "gh_s1"   
# "mcs_s1"   "mh_s1"    "pcs_s1"   "pf_s1"    "chol"     "hdl"      "trig"     "diasbp"   "systbp"   "fev1"     "fvc" 

# Get subsets of predictors that are available in both sets
shhs1_preds <-  preds[preds %in% names(shhs1)]
shhs2_preds <-  preds[preds %in% names(shhs2)]
preds_both <- intersect(shhs1_preds, shhs2_preds)

# SUBSET OBSERVATIONS -----------------------------------------------------

# Subset observations to exclude people with previous reports
names(outcomes)[str_detect(names(outcomes), "prev")]
# "prev_mi"       "prev_mip"      "prev_stk"      "prev_chf"      "prev_revpro"   "prev_ang"      "afibprevalent"
no_prev_ids <- outcomes %>% 
  filter(prev_mi == 0, prev_mip == 0, prev_stk == 0, prev_chf == 0, prev_revpro == 0, afibprevalent == 0) %>% distinct(nsrrid) %>% pull(nsrrid)

# PREPARE MODELING DATA ---------------------------------------------------

## Subset data for our outcomes of interest
baseline_preds <- c('nsrrid', names(shhs1)[names(shhs1) %in% preds])
# remove hosnr02, due to high missingness
baseline_preds <- baseline_preds[baseline_preds != 'hosnr02']
outcome_vars <- c('any_cvd', 'any_chd', 'cvd_death', 'chd_death')


model_data <- shhs1[, baseline_preds] %>% 
  left_join(
    outcomes,
    by = c("nsrrid", "gender", "race", "age_s1")
  )

# Create a set of data that has baseline predictors and incident outcomes, no imputation
model_dat_filt_all <- model_data[, c(baseline_preds, outcome_vars) ] %>% 
  filter(!is.na(any_cvd), !is.na(any_chd), !is.na(cvd_death), !is.na(chd_death))

model_dat_filt <- model_data[model_data$nsrrid %in% no_prev_ids, c(baseline_preds, outcome_vars) ] %>% 
  filter(!is.na(any_cvd), !is.na(any_chd), !is.na(cvd_death), !is.na(chd_death))
# SAVE DATA ---------------------------------------------------------------
# Taking out preds_both
save(shhs1, shhs2, outcomes, preds, outcome_vars, file = "../../data/processed_data.rda")
save(model_dat_filt, model_dat_filt_all, file = '../../data/model_data_no_impute.rda')

