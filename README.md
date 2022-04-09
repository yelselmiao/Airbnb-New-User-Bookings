# Airbnb New User Bookings Project

## Overview 
Since 2008, an increasing number of travelers have chosen to book their accommodation through Airbnb instead of traditional hotels. Through analyzing new users’ data, Airbnb can personalize and prioritize recommendations and advertisements for each user to provide better services.

After cleaning, transformation, and joining the `user` and `session` datatsets, we acquired a dataset of 34074 rows and 20 columns. We trained and made predictions with three models:   
* _Naive Bayes_  
* _Random Forest_  
* _Multinomial Logistic Regression_ 

## Description of Repository  
The structure of the repository are arranged into the following way:  
* **Code**: this folder stores the R codes of analysis    
  * [Data_Cleaning_and_EDA](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/Data_Cleaning_and_EDA.Rmd): this notebook contains the code of the data cleaning and transforming of the `user` and `session` datatsets, as well as some exploratory analysis    
  * [multinomial_logit.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/multinom_logit.Rmd): this notebook stores the codes of training and predicting with multinomial logistic regression  
  * [Naive_Bayes.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/Naive_Bayes.Rmd): this notebook stores the codes of training and predicting with naive Bayes classifiers    
  * [RandomForest.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/RandomForest.Rmd): this notebook stores the codes of training and predicting with random forest models  
  * [helper_function.R](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/helper_function.R): this R script contains the helper/wrapper functions of our analysis   
  * [modeling_function.R](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/modeling_function.R): this R script contains the some helper functions specifically for the modeling process  

* **Data**: this folder stores the data and modeling outcomes:  
  * `airbnb_cleaned.Rdata`: the final cleaned dataset  
  * `rf_results.RData`: the results of the random forest models  
  * `rfe_1.Rdata`, `rfe_2.Rdata`, `rfe_3.Rdata`, `rfe_4.Rdata`, and `rfe_5.Rdata`: the random forest recursive feature elimination object for each fold  
  * `train_users_2.csv`: the original `user` dataset  

* **Figures**: some plots of visualizations  
* **Presentation**: slides of the presentation  
