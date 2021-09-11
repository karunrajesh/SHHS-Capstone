#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: 
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)

# LOAD DATA ---------------------------------------------------------------

shhs1 <- read_csv('../../../../shhs/datasets/shhs1-dataset-0.16.0.csv')
shhs2 <- read_csv('../../../../shhs/datasets/shhs2-dataset-0.16.0.csv')
outcomes <- read_csv('../../../../shhs/datasets/shhs-cvd-summary-dataset-0.16.0.csv')

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

# Lower case the column names
names(shhs1) <- tolower(names(shhs1))
names(shhs2) <- tolower(names(shhs2))

# Are these predictors in both baseline and follow-up data? 
preds[!preds %in% names(shhs1)] # no not all 
# "saqli"  "fosq"   "sh308a" "sh308b" "sh308c" "sh308d" "sh308e" "sh308f" these are not in shhs1s
preds[!preds %in% names(shhs2)]# no not all 
# "ess_s1"   "hosnr02"  "legcrp02" "bmi_s1"   "height"   "waist"    "neck20"   "educat"   "mstat"    "bp_s1"    "gh_s1"   
# "mcs_s1"   "mh_s1"    "pcs_s1"   "pf_s1"    "chol"     "hdl"      "trig"     "diasbp"   "systbp"   "fev1"     "fvc" 

shhs1_preds <-  preds[preds %in% names(shhs1)]
shhs2_preds <-  preds[preds %in% names(shhs2)]

# these predictors are not in the follow-up data
corrplot(cor(shhs1[, shhs1_preds], use="pairwise.complete.obs"))

# Create a plot of the missing values in the outcomes
outcomes_sum <- data.frame(colSums(is.na(outcomes)))
outcomes_sum$Outcome <- row.names(outcomes_sum)
row.names(outcomes_sum) <- NULL
names(outcomes_sum) <- c('Percent of Values Missing', 'Outcome')
outcomes_sum$`Percent of Values Missing` <- outcomes_sum$`Percent of Values Missing`/nrow(outcomes)
outcomes_sum <- outcomes_sum %>% filter(!Outcome %in% c('nsrrid', 'pptid', 'age_s1', 'censdate', 'gender', 'race', 'visitnumber')) %>% arrange(`Percent of Values Missing`)

outcomes_sum$color <- ifelse(outcomes_sum$`Percent of Values Missing` > 0.25, 1, 0)

ggplot(outcomes_sum, aes(x = reorder(Outcome, -`Percent of Values Missing`), y = `Percent of Values Missing`, fill = factor(color))) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = c('dodgerblue4', 'firebrick4')) +
  scale_y_continuous(labels = scales::percent) + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x=element_text(angle= 45, vjust = 0.5)) +
  xlab('Potential Cardiovascular Outcomes') +
  ggtitle('Missing values in potential cardiovascular outcomes')
  


# Target population: participants with baseline, follow-up, and complete cardiovascular outcomes 
outcomes_complete <- outcomes_sum %>% filter(`Percent of Values Missing`< 0.25) %>% pull(Outcome)

# get ids that are in shhs1 and shhs2
both_ids <- intersect(shhs1$nsrrid, shhs2$nsrrid)


outcomes_filtered <- outcomes %>% 
  # Captures rows where all completed variables are not missing
  filter_at(vars(outcomes_complete), all_vars(!is.na(.))) %>% 
  # captures IDs from above
  filter(nsrrid %in% both_ids)

# how many participants in the filtered population?
length(unique(outcomes_filtered$nsrrid)) # 4080

# Create distributions of filtered data on demographics
# race, age_s1

outcomes_filtered %>% 
  distinct(nsrrid, race) %>% 
  group_by(race) %>% 
  summarize(cnt = n(), .groups = 'drop') %>% 
  ggplot(aes(x = race, y = cnt)) +
  geom_bar(stat = 'identity') + 
  theme_bw() 

