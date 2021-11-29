#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Preprocess Events Data
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)

# LOAD DATA ---------------------------------------------------------------
load('../../data/model_data_no_impute.rda')
load('../../data/processed_data.rda')


shhs_events <- read_csv('../../../../../shhs/datasets/shhs-cvd-events-dataset-0.16.0.csv')
shhs2 <- read_csv('../../../../../shhs/datasets/shhs2-dataset-0.16.0.csv')
# event_dt is number of days since baseline that the MI or stroke occurs

shhs_events_agg <- shhs_events %>% 
  group_by(nsrrid) %>% 
  summarize(cnt = n(), event_dt = min(event_dt))

# Limit it to those participants we're interested in. 
shhs_events_agg <- model_dat_filt %>% 
  select(nsrrid) %>% 
  left_join(
    shhs_events_agg,
    by = "nsrrid"
  )

# add on ecg date
shhs_events_agg <- shhs_events_agg %>% 
  left_join(
    shhs2 %>% select(nsrrid, ecgdate),
    by = "nsrrid"
  )

# censor individuals with no incidence on day of last follow-up (pull shhs2 ecgdate from shhs2, which is days since baseline PSG collected when ECG is collected) 
shhs_events_agg <- shhs_events_agg %>% 
  mutate(
    status = ifelse(is.na(cnt), 1, 2), # 1 if censored, 2 if event occurs
    event_dt = ifelse(is.na(event_dt), ecgdate, event_dt) #  if censored, take ecgdate as censored date
  )


# Create a complete cases version that has all predictors
shhs_events_agg_cc_pred <- shhs_events_agg %>% 
  inner_join(
    model_dat_filt[complete.cases(model_dat_filt), c("nsrrid", preds[preds %in% names(model_dat_filt)], outcome_vars)],
    by = "nsrrid"
  )


#https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html


# SAVE DATA ---------------------------------------------------------------
save(shhs_events_agg, shhs_events_agg_cc_pred, file = "../../data/cvd_events_data.rda")

