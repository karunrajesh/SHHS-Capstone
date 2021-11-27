#########################
# Title: Capstone: LRT tests to determine if we should add in demographic effects
# Purpose: Evaluate necessity of adding in demographic factors to sleep preds model
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

load('../../data/model_data_imputed.rda')
load('../../data/processed_data.rda')

sleep_preds <- c("supinep", "slpeffp", "slpprdp", "timeremp", 
                 "times34p", "timest1p", "timest2p", "waso",
                 "rdi3p", "ai_all", "avgsat", "minsat")

all_preds <- names(model_dat_filt_cc)[!str_detect(names(model_dat_filt_cc), "any|death")]



model_form1 <- paste("any_cvd", "~", paste0(sleep_preds, collapse = "+"))
model_form2 <- paste("any_cvd", "~", paste0(all_preds, collapse = "+"))

model1 <- glm(model_form1, data = model_dat_filt_cc, family = "binomial")
model2 <- glm(model_form2, data = model_dat_filt_cc, family = 'binomial')
anova(model1, model2, test = "Chisq")
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1      1140     1262.2                          
# 2      1106     1041.3 34   220.94 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1