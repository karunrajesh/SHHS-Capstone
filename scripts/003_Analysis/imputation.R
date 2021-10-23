#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Imputation Methods
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)
library(mice)

# LOAD DATA ---------------------------------------------------------------
load('../../data/model_data_no_impute.rda')
load('../../data/processed_data.rda')
############## IMPUTATION METHODS ##################

# First convert categorical variables to factors
categorical_vars <- c("gender", "race", "mstat", "legcrp02", "educat")
model_dat_filt <- model_dat_filt %>%
  mutate_at(categorical_vars, factor)

### Mode Imputation on Categorical Variables
impute_mode <- function(cat_column){ 
  no_na_column <- cat_column[!is.na(cat_column)]
  unique_x <- unique(no_na_column)
  mode <- unique_x[which.max(tabulate(match(no_na_column,unique_x)))]
  cat_column[is.na(cat_column)] <- mode
  return(cat_column)
}

imputed_categorical <- apply(model_dat_filt[,categorical_vars], 2, impute_mode)

### Mean Imputation on Numeric Variables
numeric_vars <- colnames(model_dat_filt[,!names(model_dat_filt) %in% c(categorical_vars,outcome_vars)])

impute_mean <- function(num_column){
  num_mean <- mean(num_column, na.rm = TRUE)
  num_column[is.na(num_column)] <- num_mean
  return(num_column)
}

imputed_numeric <- apply(model_dat_filt[,numeric_vars], 2, impute_mean)

trad_impute <- cbind(imputed_numeric, imputed_categorical, model_dat_filt[,outcome_vars])
apply(trad_impute, 2, function(x) return(sum(is.na(x))))
### Linear Imputation on Numeric Variables (since we are not dealing with time-series, probably shouldn't use) TBD


### MICE method
init <- mice(model_dat_filt, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix
predM[, outcome_vars] <- 0 # removes outcome variables from being used as predictors for the imputation
set.seed(103)
mice_imputed = mice(model_dat_filt, method=meth, predictorMatrix=predM, m=5) # uses pmm method for imputation
mice_imputed <- complete(mice_imputed)
apply(mice_imputed, 2, function(x) return(sum(is.na(x))))
###########################

# complete cases data
model_dat_filt_cc <- model_dat_filt[complete.cases(model_dat_filt), ]

# SAVE Imputed data models -------------------------------------------------------------

save(model_dat_filt_cc, trad_impute, mice_imputed, file = '../../data/model_data_imputed.rda')
