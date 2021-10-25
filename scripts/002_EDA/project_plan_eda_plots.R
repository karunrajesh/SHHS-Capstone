#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Exploratory Data Analysis
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)

# LOAD DATA ---------------------------------------------------------------


load('../../data/processed_data.rda')
load('../../data/model_data_no_impute.rda')
shhs1_preds <-  preds[preds %in% names(shhs1)]
shhs2_preds <-  preds[preds %in% names(shhs2)]


# EDA ---------------------------------------------------------------------

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


# COHORT IDENTIFICATION ---------------------------------------------------

# SAQLI - questionnaire which determines sleep apnea quality of life
# ess_1 - sleepiness questionaire

# who experiences sleepiness?
shhs1 %>% 
  filter(nsrrid %in% both_ids) %>% 
  ggplot(aes(x = ess_s1)) +
  geom_histogram(fill = 'darkorchid2', color = 'darkorchid3') + 
  theme_bw() +
  xlab('Epworth Sleepiness Scale Score at Baseline') +
  ylab('Count') +
  ggtitle('Distribution of Sleepliness Score at Baseline')


# # who experiences sleep apnea?
# shhs1 %>% 
#   filter(nsrrid %in% both_ids) %>% 
#   ggplot(aes(x = saqli)) +
#   geom_histogram(fill = 'darkorchid2', color = 'darkorchid3') + 
#   theme_bw() +
#   xlab('Epworth Sleepiness Scale Score at Baseline') +
#   ylab('Count') +
#   ggtitle('Distribution of Sleepliness Score at Baseline')
#   

# EDA on final pop --------------------------------------------------------

# distribution of age by gender
model_dat_filt %>% 
  mutate(gender = cut(gender, 2, labels=c('Male', 'Female')) ) %>% 
  ggplot(aes(x = gender, y = age_s1, fill = gender)) +
  scale_fill_viridis_d(alpha = 0.3) +
  theme_bw() +
  xlab('Gender') +
  ylab('Age at Baseline') +
  ggtitle('Distribution of Age across Gender') +
  geom_violin()

# Distribution of counts by race and gender
model_dat_filt %>% 
  mutate(Race = cut(race, 3, labels=c('White', 'Black', 'Other')),
         Gender = cut(gender, 2, labels=c('Male', 'Female'))
         ) %>% 
  select(Race, Gender, age_s1) %>% 
  group_by(Race, Gender) %>% 
  summarize(pct_pop = n()/nrow(model_dat_filt)) %>% 
  ggplot(aes(x = Race, y = pct_pop, fill = Gender)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  scale_fill_viridis_d(alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  xlab('Race') +
  ylab('% of Population') +
  ggtitle('Distribution of Race and Gender')

# AHI index distribution
shhs1 %>% 
  filter(nsrrid %in% model_dat_filt$nsrrid) %>% 
  mutate(sleep_apnea_cat = case_when(
    ahi_a0h3 < 5 ~ 'None/Minimal',
    ahi_a0h3 >= 5 & ahi_a0h3 < 15 ~ 'Mild',
    ahi_a0h3 >= 15 & ahi_a0h3 < 30 ~ 'Moderate',
    ahi_a0h3 >= 30 ~ 'Severe'
  )) %>% 
  group_by(sleep_apnea_cat) %>% 
  summarize(pct_pop = n()/nrow(model_dat_filt)) %>% 
  ggplot(aes(x = factor(sleep_apnea_cat, levels = c('None/Minimal', 'Mild', 'Moderate', 'Severe')), y = pct_pop)) +
  geom_bar(stat = 'identity', fill = '#481567FF', color = "#440154FF", alpha = 0.3) +
  # scale_x_continuous(breaks = seq(0, 150, by = 20)) +
  scale_y_continuous(labels = scales::percent) +
  xlab('Sleep Apnea Severity') +
  ylab('% of Population') +
  ggtitle('Distribution of Sleep Apnea at Baseline', subtitle = 'Using AHI >= 3%') +
  theme_bw()
  
# Frequency of our outcomes of interest 
outcomes_freq <- data.frame(apply(model_dat_filt[, outcome_vars],2, sum))
outcomes_freq$outcome <- row.names(outcomes_freq)
names(outcomes_freq) <- c('Count', 'Outcome')

outcomes_freq %>% 
  ggplot(aes(x = factor(Outcome, levels = c('any_cvd', 'any_chd', 'cvd_death', 'chd_death')), y = Count)) +
  geom_bar(stat = 'identity', fill = '#481567FF', color = "#440154FF", alpha = 0.3) +
  # scale_x_continuous(breaks = seq(0, 150, by = 20)) +
  xlab('Outcome of Interest') +
  ylab('Count') +
  ggtitle('Distribution of Outcomes of Interest') +
  theme_bw()

# Look at distribution of "prev" outcomes 
prev_outcomes <- names(outcomes)[str_detect(names(outcomes), 'prev')]
outcomes[, c('nsrrid', prev_outcomes)] %>% 
  filter(nsrrid %in% model_dat_filt$nsrrid) %>% 
  select(-nsrrid) %>% 
  gather() %>% 
  filter(!is.na(value)) %>% 
  group_by(key) %>% 
  summarize(cnt = sum(value))



# # BASLINE LOGISITIC MODELS ------------------------------------------------
# 
# # join datasets 
# shhs1 <- shhs1 %>% 
#   left_join(outcomes_filtered, by = c('nsrrid', 'pptid'))
# 
# model_statement = paste0('any_cvd ~ ', paste(preds[preds %in% names(shhs1)], collapse = " + "))
# logit_cvd <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
# summary(logit_cvd)
# 
# # Just sleep architecture
# model_statement = paste0('any_cvd ~ ', paste(c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso'), collapse = ' + '))
# logit_cvd_sleeparch <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
# summary(logit_cvd_sleeparch)
# 
# 
# # Just sleep architecture, with revasc proc
# model_statement = paste0('cvd_death ~ ', paste(c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso'), collapse = ' + '))
# logit_cvd_sleeparch <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
# summary(logit_cvd_sleeparch)
