# Airbnb New User Bookings Project
Note: Although the seed is set to 2023, we ran into a technique issue for which set seeds works differently on one of the team members computers. Therefore, for both the session cleaning and random forest file, to reproduce the exact outcome, please use the RData files in the <data> folder. Otherwise the code may produce a sligthly different result as the observations in cross-validation folds will be different.

## Required R Library
- tidyverse
- ggplot2
- lubridate
- mice
- caret
- purrr
- randomForest
- pROC

## Order in which Files are Ran
1. (Optional, sourced in R markdowns) helper_functions.R
2. (Optional, sourced in R markdowns) modeling_functions.R
3. Feature Engineering.Rmd (**原Session & EDA, to be renamed**)
4. Naive_Bayes.Rmd
6. RandomForest.Rmd (**Need to be combined with RFE for random forest**)
7. Multinomial.Rmd
