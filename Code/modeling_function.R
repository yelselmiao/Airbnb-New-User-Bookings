
#'
#' Function of fitting Naive Bayes model with five-fold cross validation
#' Output prints the misclassification rates of 50% and 80% prediction interval for each fold as well the average over five folds
#' @param data the input dataset 
#' @return strings that report misclassification rates 
#'
NB_fitting <- function(data) {
  oos_50_misclass_vec <- rep(NA, 5)
  oos_80_misclass_vec <- rep(NA, 5)
  
  labels_vec <- c('other', 'US', 'NDF')
  for (i in 1:5) {
    holdout <- extract_folds(data, i, folds)
    train <- data[-folds[[i]], ]
    
    classifier <- naiveBayes(country_destination ~ ., data = train)
    oos_pred <- predict(classifier, newdata = holdout, type = "raw")
    oos_pred_label <- CategoryPredInterval(oos_pred, labels_vec)
    
    oos_50_misclass <-
      get_misclass_rate(holdout$country_destination, oos_pred_label$pred50)
    oos_80_misclass <-
      get_misclass_rate(holdout$country_destination, oos_pred_label$pred80)
    oos_50_misclass_vec[i] <- oos_50_misclass
    oos_80_misclass_vec[i] <- oos_80_misclass
    
    print(paste0('Fold ', i))
    print(paste0(
      "Misclassification rate of 50% PI: ",
      round(oos_50_misclass, 2)
    ))
    print(paste0(
      "Misclassification rate of 80% PI: ",
      round(oos_80_misclass, 2)
    ))
  }
  print(paste0(
    'The average 5-fold misclassification of 50% PI is: ',
    round(mean(oos_50_misclass_vec),2)
  ))
  print(paste0(
    'The average 5-fold misclassification of 80% PI is: ',
    round(mean(oos_80_misclass_vec), 2)
  ))
  
}