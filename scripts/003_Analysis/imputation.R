#########################
# Title: Capstone - Sleep Heart Health Study Analysis
# Purpose: Imputation Methods
#########################



# SETUP -------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(corrplot)
library(glmnet)
library(caret)
library(mice)

# LOAD DATA ---------------------------------------------------------------
#load('../../data/model_data_no_impute.rda')
#load('../../data/processed_data.rda')

############## IMPUTATION METHODS ##################

############ Traditional Imputation ###########

### Mode Imputation on Categorical Variables
impute_mode <- function(cat_column){ 
  no_na_column <- cat_column[!is.na(cat_column)]
  unique_x <- unique(no_na_column)
  mode <- unique_x[which.max(tabulate(match(no_na_column,unique_x)))]
  cat_column[is.na(cat_column)] <- mode
  return(cat_column)
}

### Mean Imputation on Numeric Variables

impute_mean <- function(num_column){
  num_mean <- mean(num_column, na.rm = TRUE)
  num_column[is.na(num_column)] <- num_mean
  return(num_column)
}

# Function: Traditional imputation using mean and mode imputation methods
# Input: Dataset to be imputed
# Output: Imputed dataset
# Purpose: Get full imputed dataset
trad_impute <- function(dat){
  # First convert categorical variables to factors
  categorical_vars <- c("gender", "race", "mstat", "legcrp02", "educat")
  model_dat_filt <- dat %>%
    mutate_at(categorical_vars, factor)
  # Impute categorical variables
  dat_impute_categorical <- apply(model_dat_filt[,categorical_vars], 2, impute_mode)
  # Hard coding outcome variables
  outcome_vars <- c("any_cvd", "any_chd", "chd_death", "cvd_death")
  # Get numeric vars
  numeric_vars <- colnames(dat[,!names(dat) %in% c(categorical_vars,outcome_vars)])
  # Impute numeric variables
  dat_imputed_numeric <- apply(dat[,numeric_vars], 2, impute_mean)
  # Combine datasets back together
  trad_impute_df <- cbind(dat_imputed_numeric, dat_impute_categorical, dat[,outcome_vars])
  print(apply(trad_impute_df, 2, function(x) return(sum(is.na(x)))))
  return(trad_impute_df)
}

#################################################

# apply(train_trad_impute, 2, function(x) return(sum(is.na(x))))
### Linear Imputation on Numeric Variables (since we are not dealing with time-series, probably shouldn't use) TBD


########## MICE method ##########

# Function: MICE imputation 
# Input: Dataset to be imputed
# Output: Imputed dataset
# Purpose: Get full MICE imputed dataset
mice_impute <- function(dat){
  outcome_vars <- c("any_cvd", "any_chd", "chd_death", "cvd_death")
  init <- mice(dat, maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  predM[, outcome_vars] <- 0 # removes outcome variables from being used as predictors for the imputation
  set.seed(103)
  mice_imputed = mice(dat, method=meth, predictorMatrix=predM, m=5) # uses pmm method for imputation
  mice_imputed <- complete(mice_imputed)
  print(apply(mice_imputed, 2, function(x) return(sum(is.na(x)))))
  return(mice_imputed)
}

##################################

########## Complete-Case #########

# Function: Complete-case imputation
# Input: Dataset to be imputed
# Output: Imputed dataset
# Purpose: Get complete-case imputed dataset
complete_impute <- function(dat){
  complete_df <- dat[complete.cases(dat), ]
  print(apply(complete_df, 2, function(x) return(sum(is.na(x)))))
  return(complete_df)
}

# Caller function
imputation_runner <- function(dat, impute_method){
  if(impute_method == "trad"){
    dat_imputed <- trad_impute(dat)
    return(dat_imputed)
  }
  else if(impute_method == "mice"){
    dat_imputed <- mice_impute(dat)
    return(dat_imputed)
  }
  else{
    dat_imputed <- complete_impute(dat)
    return(dat_imputed)
  }
  
}

##################################
# Testing -------------------------------------------------------------
#trad_model <- trad_impute(model_dat_filt)
#mice_model <- mice_impute(model_dat_filt)
#complete_model <- complete_impute(model_dat_filt)

#trad_model <- imputation_runner(model_dat_filt, "trad")
#mice_model <- imputation_runner(model_dat_filt, "mice")
#complete_model <- imputation_runner(model_dat_filt, "complete")


#save(train_model_dat_filt_cc, train_trad_impute, train_mice_imputed, file = '../../data/train_model_data_imputed.rda')
#save(test_model_dat_filt_cc, test_trad_impute, test_mice_imputed, file = '../../data/test_model_data_imputed.rda')
