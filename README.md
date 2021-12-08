# SHHS-Capstone 
*Karun Rajesh and Nellie Ponarul*  
Predicting the risk of cardiovascular disease from sleep architecture variables using the Sleep Heart Health Study. 

Script flow:  

*Data Processing*
1. _data_cleaning.R_: Cleans data and subsets data to those with reported CVD outcomes who have reported not having a history of MI, MIP, STK, CHF, REVPRO or AFIB

*Modeling*
1. _cv_functions.R_: Has a function for the logistic regression model, random forest model, and XGboost model which returns K-Fold cross-validated precision, recall and F1 score for each model. Three imputation methods are performed for each split. 
2. _baseline_models_cv.R_: Runs Cross validated test metrics for logistic regression models 
3. _hyperparameter_tuning.R_: Hyperparameter tunes the random forest and XGBoost models for each given model
4. _tree_models_cv.R_: Runs Cross validated test metrics for random forest and XGBoost models using best tuned parameters from _hyperparameter_tuning.R_
5. _final_metrics.R_: Aggregated prediction metrics from different models into one table
