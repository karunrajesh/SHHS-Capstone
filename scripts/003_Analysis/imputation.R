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

# First convert categorical variables to factors
categorical_vars <- c("gender", "race", "mstat", "legcrp02", "educat")
model_dat_filt <- model_dat_filt %>%
  mutate_at(categorical_vars, factor)

train_ind <- sample(1:nrow(model_dat_filt), size = floor(nrow(model_dat_filt)*.7))
test_ind <- 1:nrow(model_dat_filt)
test_ind <- test_ind[!test_ind %in% train_ind]

train_model_dat_filt <- model_dat_filt[train_ind,]
test_model_dat_filt <- model_dat_filt[test_ind,]

############## IMPUTATION METHODS ##################


### Mode Imputation on Categorical Variables
impute_mode <- function(cat_column){ 
  no_na_column <- cat_column[!is.na(cat_column)]
  unique_x <- unique(no_na_column)
  mode <- unique_x[which.max(tabulate(match(no_na_column,unique_x)))]
  cat_column[is.na(cat_column)] <- mode
  return(cat_column)
}

train_imputed_categorical <- apply(train_model_dat_filt[,categorical_vars], 2, impute_mode)
test_imputed_categorical <- apply(test_model_dat_filt[,categorical_vars], 2, impute_mode)


### Mean Imputation on Numeric Variables
numeric_vars <- colnames(train_model_dat_filt[,!names(train_model_dat_filt) %in% c(categorical_vars,outcome_vars)])

impute_mean <- function(num_column){
  num_mean <- mean(num_column, na.rm = TRUE)
  num_column[is.na(num_column)] <- num_mean
  return(num_column)
}

train_imputed_numeric <- apply(train_model_dat_filt[,numeric_vars], 2, impute_mean)
test_imputed_numeric <- apply(test_model_dat_filt[,numeric_vars], 2, impute_mean)

train_trad_impute <- cbind(train_imputed_numeric, train_imputed_categorical, train_model_dat_filt[,outcome_vars])
apply(train_trad_impute, 2, function(x) return(sum(is.na(x))))

test_trad_impute <- cbind(test_imputed_numeric, test_imputed_categorical, test_model_dat_filt[,outcome_vars])
apply(train_trad_impute, 2, function(x) return(sum(is.na(x))))
### Linear Imputation on Numeric Variables (since we are not dealing with time-series, probably shouldn't use) TBD


### MICE method
init <- mice(train_model_dat_filt, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix
predM[, outcome_vars] <- 0 # removes outcome variables from being used as predictors for the imputation
set.seed(103)
train_mice_imputed = mice(train_model_dat_filt, method=meth, predictorMatrix=predM, m=5) # uses pmm method for imputation
train_mice_imputed <- complete(train_mice_imputed)
apply(train_mice_imputed, 2, function(x) return(sum(is.na(x))))

init <- mice(test_model_dat_filt, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix
predM[, outcome_vars] <- 0 # removes outcome variables from being used as predictors for the imputation
set.seed(103)
test_mice_imputed = mice(test_model_dat_filt, method=meth, predictorMatrix=predM, m=5) # uses pmm method for imputation
test_mice_imputed <- complete(test_mice_imputed)
apply(test_mice_imputed, 2, function(x) return(sum(is.na(x))))
###########################

# complete cases data
train_model_dat_filt_cc <- train_model_dat_filt[complete.cases(train_model_dat_filt), ]
test_model_dat_filt_cc <- test_model_dat_filt[complete.cases(test_model_dat_filt), ]
# SAVE Imputed data models -------------------------------------------------------------


save(train_model_dat_filt_cc, train_trad_impute, train_mice_imputed, file = '../../data/train_model_data_imputed.rda')
save(test_model_dat_filt_cc, test_trad_impute, test_mice_imputed, file = '../../data/test_model_data_imputed.rda')
