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
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat 
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

# remove patients from population who have existing cvd or chd (any_cvd, any_chd)

# Patients who report having hypertension - do we want to remove these?

# N
shhs1 %>% 
  group_by(srhype) %>% 
  summarize(cnt = n())

# # A tibble: 3 x 2
    # srhype   cnt
#       <dbl> <int>
#   1      0  3484
#   2      1  2069
#   3     NA   251

# Self reported angina (angina15)



# SAVE DATA ---------------------------------------------------------------

save(shhs1, shhs2, outcomes, preds, preds_both, file = "../../data/processed_data.rda")
  

