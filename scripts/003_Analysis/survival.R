#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Survival Analysis
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)
library(gtsummary)

# LOAD DATA ---------------------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6701040/ this paper and supplementary material outlines how to do survival analysis
load('../../data/cvd_events_data.rda')
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

preds <- c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso',
           'rdi3p', 'ai_all', 'avgsat', 'minsat',  'ess_s1', 'saqli', 'fosq', 'hosnr02', 'legcrp02', 
           'sh308a', 'sh308b', 'sh308c', 'sh308d', 'sh308e', 'sh308f', 'bmi_s1', 'height', 'waist', 
           'neck20', 'gender', 'race', 'educat', 'age_s1', 'mstat', 'bp_s1', 'gh_s1', 'mcs_s1', 'mh_s1', 
           'pcs_s1', 'pf_s1', 'chol', 'hdl', 'trig', 'diasbp', 'systbp', 'fev1', 'fvc')

# KAPLAN MEIER ------------------------------------------------------------
KM <- survfit(Surv(event_dt, status) ~ 1, data = shhs_events_agg)

ggsurvplot(survfit(Surv(event_dt, status) ~ 1, data = shhs_events_agg), 
     xlab = "Days", 
     ylab = "Overall survival probability")


# COX PH MODEL ------------------------------------------------------------

# sleep predictors
coxph(Surv(event_dt, status) ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest2p+waso+rdi3p+ai_all+avgsat+minsat, data = shhs_events_agg_cc_pred) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE) 

# This demostrates that these variables don't really effect time-to-event since hazard ratios are all about 1

# all preds
coxph(Surv(event_dt, status) ~ supinep+slpeffp+slpprdp+timeremp+times34p+timest2p+waso+rdi3p+ai_all+avgsat+minsat+ess_s1+legcrp02+bmi_s1+height+waist+neck20+gender+race+educat+age_s1+mstat+bp_s1+gh_s1+mcs_s1+mh_s1+pcs_s1+pf_s1+chol+hdl+trig+diasbp+systbp+fev1+fvc, data = shhs_events_agg_cc_pred) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE) 


# Demographics alone
coxph(Surv(event_dt, status) ~ race+educat+age_s1, data = shhs_events_agg_cc_pred) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE) 

# clinical alone
coxph(Surv(event_dt, status) ~ bmi_s1+height+waist+neck20+mstat+bp_s1+gh_s1+mcs_s1+mh_s1+pcs_s1+pf_s1+chol+hdl+trig+diasbp+systbp+fev1+fvc, data = shhs_events_agg_cc_pred) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE) 
