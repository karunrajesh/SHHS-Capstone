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


# who experiences sleep apnea?
shhs1 %>% 
  filter(nsrrid %in% both_ids) %>% 
  ggplot(aes(x = saqli)) +
  geom_histogram(fill = 'darkorchid2', color = 'darkorchid3') + 
  theme_bw() +
  xlab('Epworth Sleepiness Scale Score at Baseline') +
  ylab('Count') +
  ggtitle('Distribution of Sleepliness Score at Baseline')
  


# BASLINE LOGISITIC MODELS ------------------------------------------------

# join datasets 
shhs1 <- shhs1 %>% 
  left_join(outcomes_filtered, by = c('nsrrid', 'pptid'))

model_statement = paste0('any_cvd ~ ', paste(preds[preds %in% names(shhs1)], collapse = " + "))
logit_cvd <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
summary(logit_cvd)

# Just sleep architecture
model_statement = paste0('any_cvd ~ ', paste(c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso'), collapse = ' + '))
logit_cvd_sleeparch <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
summary(logit_cvd_sleeparch)


# Just sleep architecture, with revasc proc
model_statement = paste0('cvd_death ~ ', paste(c('supinep', 'slpeffp', 'slpprdp', 'timeremp', 'times34p', 'timest1p', 'timest2p', 'waso'), collapse = ' + '))
logit_cvd_sleeparch <- glm(model_statement, family = binomial(), data = shhs1[complete.cases(shhs1[, preds[preds %in% names(shhs1)]]), ])
summary(logit_cvd_sleeparch)
