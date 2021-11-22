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


# SUMMARIZE ---------------------------------------------------------------

# Unique participants
model_dat_filt %>% distinct(nsrrid) %>% nrow() # 1397

# Participants by gender
model_dat_filt %>% 
  group_by(gender) %>% 
  summarize(cnt = n(), .groups = "drop")
# 1 - Male; 2 - Female

# A tibble: 2 x 2
# gender   cnt
# <dbl> <int>
# 1      1   589
# 2      2   808

# Outcome any_cvd
model_dat_filt %>% 
  group_by(any_cvd) %>% 
  summarize(cnt = n(), .groups = "drop")
# A tibble: 2 x 2
# any_cvd   cnt
# <dbl> <int>
# 1       0  1041
# 2       1   356

# Average age by race and gender
model_dat_filt %>% 
  group_by(race, gender) %>% 
  summarize(avg_age = mean(age_s1), .groups = "drop")

# A tibble: 6 x 3
# race gender avg_age
# <dbl>  <dbl>   <dbl>
# 1     1      1    67.0
# 2     1      2    67.3
# 3     2      1    74.8
# 4     2      2    74.8
# 5     3      1    64  
# 6     3      2    73 
