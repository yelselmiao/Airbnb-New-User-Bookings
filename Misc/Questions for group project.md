1. Should we sub -unknown- with NA? lining to just keep it that way because this could be people who choose to hide their information.  --> Yes, replace with NA (missing at random ish)
2. Date of first booking prior to data of account creation? Missing Values? Sub in mean for missing values?
3. What should we do with NDF, should we use it as a blocking? Two models for each cross validation?

________________________________________________________________________

1. Check each variable NA proportion, sensitivity analysis for cut off, use mice package to substitute NA
  -- sa, Plot
2. Choose a base line model, feature engineering (variable selection)
________________________________________________________________________
##  Rubric:

15 points for Content and Scientific Merit; divided into
(i) 3 points for introduction (background, importance, objective),
(ii) 9 points for body (methodology of wrangling or prediction/classification, inclusion of graphs/plots/figures, logical reasoning).
(iii) 3 points for conclusion (summarize major points, summarize weaknesses, take-home messages)

________________________________________________________________________

## Slides: <= 15  (1-2)  aiming :12

3-4 Introduction 
  - Project Overview (background, Objective, Importance)
      - Propaganda, advertising target population, renter selection, fee
  - Dataset Overview
    - nrow(test/train), n(varibale), reponse, predictors, continuous, categorical(level)
6-8 EDA
  - Missing Value
  - Inbalance Class (Predictor & Response)
  - Transforming
    - Outlier Process
    - Skewness
    - Correlation
1-2 methodology (Briefly Explain)
  - Set Baseline Model
  - Variable Selection
  - Ultimate Model; Model for Comparision
    - Cross Validation (K-fold)
1-2 Conclusion
  - ...
    

    

