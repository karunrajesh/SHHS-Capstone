#########################
# Title: EDA of SHH1 datasets
# Purpose: Create data summaries of SHHS1 dataset
#########################

# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)

# LOAD DATA ---------------------------------------------------------------
shhs1 <- read_csv('../../../shhs/datasets/shhs1-dataset-0.16.0.csv')

# INITIAL VARIABLE SELECTION ----------------------------------------------
# Choose variables based on those given by Dr. Wang
# sleep predictors:
# Sleep architecture – supinep, slpeffp, slpprdp, timeremp, times34p, timest1p, timest2p, waso
# Polysomnography (osa) – rdi3p, ai_all, avgsat, minsat
# Questionnaire – ess_s1, saqli, fosq, hosnr02, legcrp02, sh308a-sh308f
# 
# Other predictors/covariates to consider:
# Anthropometry – bmi_s1, height, waist, neck20
# Demography – gender, race, educat, age_s1, mstat
# General Health – bp_s1, gh_s1, mcs_s1, mh_s1, pcs_s1, pf_s1
# Clinical data – chol, hdl, trig, diasbp, systbp, fev1, fvc

