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
sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", "times34p", "timest1p", "timest2p", "waso", "rdi3p", "ai_all", "avgsat", "minsat")
# Sleep architecture
tbl1_data_sleep <- model_dat_filt[, c(sleep_preds, "any_cvd")] %>% 
  rename(
    `Percent Time Supine` = supinep,
    `Sleep Efficiency` = slpeffp,
    `Total Sleep Duration` = slpprdp,
    `Percent of sleep time in REM` = timeremp,
    `Percent of sleep time in stage 3-4` = times34p,
    `Percent of sleep time in stage 1` = timest1p,
    `Percent of sleep time in stage 2` = timest2p,
    `Wake After Sleep Onset` = waso,
    `Overall Respiratory Disturbance Index (RDI) at >=3% oxygen desaturation` = rdi3p,
    `Arousal Index` = ai_all,
    `Average oxygen saturation (SaO2) in sleep` = avgsat,
    `Minimum oxygen saturation (SaO2) in sleep` = minsat
  ) %>% 
  mutate(
    `CVD` = factor(ifelse(any_cvd == 1, "Had CVD Outcome", "No CVD Outcome"))
  )

tbl1_sleep <- table1(~ `Percent Time Supine` + `Sleep Efficiency` + `Total Sleep Duration` + 
         `Percent of sleep time in REM` + `Percent of sleep time in stage 3-4` +
         `Percent of sleep time in stage 1` + `Percent of sleep time in stage 2` +
         `Wake After Sleep Onset` +  `Overall Respiratory Disturbance Index (RDI) at >=3% oxygen desaturation` + 
         `Arousal Index` + `Average oxygen saturation (SaO2) in sleep` + 
         `Average oxygen saturation (SaO2) in sleep` + `Minimum oxygen saturation (SaO2) in sleep`| `CVD`, data =tbl1_data_sleep, overall=F, extra.col=list(`P-value`=pvalue)) 


tbl1_other <- model_dat_filt[, c("age_s1", "gender", "bmi_s1", "race", "any_cvd")] %>% 
  rename(
    `Age at Baseline` = age_s1,
    `BMI` = bmi_s1
  ) %>% 
  mutate(
    `Gender` = factor(gender),
    `Race` = factor(race),
    `CVD` = factor(ifelse(any_cvd == 1, "Had CVD Outcome", "No CVD Outcome"))
  )

tbl1_other <- table1(~  `Age at Baseline`  + `Gender` + `BMI` + `Race`| `CVD`, data =tbl1_other, overall=F, extra.col=list(`P-value`=pvalue) )


