#########################
# Title: 
# Purpose: Create Table 1s for Sleep project
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(table1)
library(gtsummary)  



# FUNCTIONS ---------------------------------------------------------------


# From https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#example-a-column-of-p-values 

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# LOAD DATA ---------------------------------------------------------------

load('../../data/model_data_no_impute.rda')


# PROCESS DATA ------------------------------------------------------------
sleep_arch <- c("waso", "timest1p", "timest2p", "times34p", "timeremp", "supinep",   "slpprdp", "slpeffp")
sleep_poly <- c("rdi3p",  "ai_all",  "avgsat",   "minsat")
demog <- c("gender",   "race",  "mstat",   "educat",  "age_s1")
anthro <- c("neck20",   "bmi_s1",    "waist",    "height" )
quest <- c("ess_s1",   "legcrp02")
clinical <- c("systbp", "diasbp",  "chol",  "hdl",  "trig",    "fev1",     "fvc"   )
health <- c("pf_s1",    "bp_s1",    "gh_s1",    "mh_s1",    "pcs_s1", "mcs_s1")

# sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")
# Sleep architecture
tbl1_data_sleep_arch <- model_dat_filt[, c(sleep_arch, "any_cvd")] %>% 
  rename(
    `Percent Time Supine` = supinep,
    `Sleep Efficiency` = slpeffp,
    `Total Sleep Duration` = slpprdp,
    `Percent of sleep time in REM` = timeremp,
    `Percent of sleep time in stage 3-4` = times34p,
    `Percent of sleep time in stage 1` = timest1p,
    `Percent of sleep time in stage 2` = timest2p,
    `Wake After Sleep Onset` = waso
  ) %>% 
  mutate(
    `CVD` = factor(ifelse(any_cvd == 1, "Had CVD Outcome", "No CVD Outcome"))
  )


tbl1_data_sleep_poly <- model_dat_filt[, c(sleep_poly, "any_cvd")] %>% 
  rename(
    `Overall Respiratory Disturbance Index (RDI) at >=3% oxygen desaturation` = rdi3p,
    `Arousal Index` = ai_all,
    `Average oxygen saturation (SaO2) in sleep` = avgsat,
    `Minimum oxygen saturation (SaO2) in sleep` = minsat
  ) %>% 
  mutate(
    `CVD` = factor(ifelse(any_cvd == 1, "Had CVD Outcome", "No CVD Outcome"))
  )

tbl1_data_demog <-  model_dat_filt[, c(demog, "any_cvd")] %>% 
  rename(
    `Age at Baseline` = age_s1
  ) %>% 
  mutate(
    `Gender` = factor(gender),
    `Race` = factor(race),
    `Marital Status` = factor(mstat),
    `Education Status` = factor(educat),
    `CVD` = factor(ifelse(any_cvd == 1, "Had CVD Outcome", "No CVD Outcome"))
  )



tbl1_sleep_arch <- table1(~ `Percent Time Supine` + `Sleep Efficiency` + `Total Sleep Duration` + 
         `Percent of sleep time in REM` + `Percent of sleep time in stage 3-4` +
         `Percent of sleep time in stage 1` + `Percent of sleep time in stage 2` +
         `Wake After Sleep Onset`| `CVD`, data =tbl1_data_sleep_arch, overall=F, extra.col=list(`P-value`=pvalue)) 

tbl1_sleep_poly <- table1(~ `Overall Respiratory Disturbance Index (RDI) at >=3% oxygen desaturation` + 
                            `Arousal Index` + `Average oxygen saturation (SaO2) in sleep` + 
                            `Average oxygen saturation (SaO2) in sleep` + `Minimum oxygen saturation (SaO2) in sleep`| `CVD`, data =tbl1_data_sleep_poly, overall=F, extra.col=list(`P-value`=pvalue)) 

tbl1_demog <- table1(~ Gender + Race + `Marital Status` + `Education Status` + `Age at Baseline` | `CVD`, data =tbl1_data_demog, overall=F, extra.col=list(`P-value`=pvalue))



