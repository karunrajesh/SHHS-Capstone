#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Baseline Models
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)

# LOAD DATA ---------------------------------------------------------------


load('../../data/processed_data.rda')