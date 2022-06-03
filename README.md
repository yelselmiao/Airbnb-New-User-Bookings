# Airbnb New User Bookings Project

## Overview 
Since 2008, an increasing number of travelers have chosen to book their accommodation through Airbnb instead of traditional hotels. Through analyzing new users’ data, Airbnb can personalize and prioritize recommendations and advertisements for each user to provide better services. The Kaggle competition can be found [here](https://www.kaggle.com/competitions/airbnb-recruiting-new-user-bookings/overview). 

After cleaning, transformation, and joining the `user` and `session` datatsets, we acquired a [cleaned dataset](\Data\airbnb_cleaned.Rdata) of 34074 rows and 20 columns. We trained and made predictions with three kinds of models:   
* _Naive Bayes_  
* _Random Forest_  
* _Multinomial Logistic Regression_ 

## Description of Repository  
The structure of the repository is arranged into the following way:  
* **Code**: this folder stores the R codes of analysis    
  * [1_Data_Cleaning_and_EDA](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/Data_Cleaning_and_EDA.Rmd): this notebook contains the codex of the data cleaning and transformation for the `user` and `session` datatsets, as well as some exploratory analysis    
  * [2_Naive_Bayes.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/Naive_Bayes.Rmd): this notebook stores the codes of training and predicting with naive Bayes classifiers     
  * [3_RandomForest.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/RandomForest.Rmd): this notebook stores the codes of training and predicting with random forest models    
  * [4_multinomial_logit.Rmd](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/multinom_logit.Rmd): this notebook stores the codes of training and predicting with multinomial logistic regression models    
  * [helper_function.R](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/helper_function.R): this R script contains the helper/wrapper functions of our analysis   
  * [modeling_function.R](https://github.com/yelselmiao/Airbnb-New-User-Bookings/blob/master/Code/modeling_function.R): this R script contains the helper functions specifically for the model fitting process  

* **Data**: this folder stores the data and modeling outcomes:  
  * [airbnb_cleaned.Rdata](\Data\airbnb_cleaned.Rdata): the final cleaned dataset]  
  * [rf_results.RData](\Data\rf_results.RData): the results of the random forest models, including the misclassification rates of 50% and 80% prediction interval, rate misclassification of the point prediction, AUC, and the interval score for each random forest model  
  * [rf_model_objects.RData](https://drive.google.com/file/d/1ujC5eLZP8lob5y57CtPvHZrTrkW7FHor/view?usp=sharing): these are four fitted random forest model objects. Since it's too big for Github, we uploaded to Google drive. You may click on the link and download it to take a closer look at the model 
  * [rfe_1.Rdata](\Data\rfe_1.Rdata), [rfe_2.Rdata](\Data\rfe_2.Rdata), [rfe_3.Rdata](\Data\rfe_3.Rdata), [rfe_4.Rdata](\Data\rfe_4.Rdata), and [rfe_5.Rdata](\Data\rfe_5.Rdata): the random forest recursive feature elimination object for each fold  
  * [train_users_2.csv](\Data\train_users_2.csv): the original `user` dataset    
  * [session.csv](https://drive.google.com/file/d/1l24LMftgIZfgZjAno8dELdJEsGuK0mPJ/view): the original `session` dataset  

* **Figures**: some plots of visualizations  
* **Presentation**: slides of the presentation  

## How to Run the Code
To reproduce our result, please run the notebooks in the `Code` fold in the following order:   
- **1_Data_Cleaning_and_EDA.Rmd**: you may need to download the `sesson` dataset from [Kaggle](https://www.kaggle.com/competitions/airbnb-recruiting-new-user-bookings/overview) and save it into the [Data](https://github.com/yelselmiao/Airbnb-New-User-Bookings/tree/master/Data) folder before running this notebook because it's too big for Github
- **2_Naive_Bayes.Rmd**: You can directly run this notebook without the need to download any files.   
- **3_RandomForest.Rmd**: Sinc running random forest models is extremely time-consuming, the objects of fitted random forest model are in  [rf_model_objects.RData](https://drive.google.com/file/d/1ujC5eLZP8lob5y57CtPvHZrTrkW7FHor/view?usp=sharing); and the model summary statistics are in [rf_results.RData](\Data\rf_results.RData)
- **4_multinomial_logit.Rmd**: You can directly run this notebook without the need to download any files. 


 
