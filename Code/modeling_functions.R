
#' Random Forest Undersampling Model Function
#' Undersample to the other response
#' The sampling number is then multiplied by 2/3 
#' as the Random Forest algorithm take 2/3 of  the data for the forest-making, and the leftover for calculating OOB error
#'
#' @param training the training data set
#' @param holdout the holdout data set
#'
#' @return a named list consists of the random forest model, the probability prediction for the testing set, the 50% and 80% prediction interval and their misclassification rate
randomforest_model <- function(training, holdout) {
  # Tunning mtry
  mtry <- tuneRF(training %>% select(-country_destination) , training$country_destination, ntreeTry=500,
                 stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  # Fit Model & Make Prediction
  # rf <- randomForest(country_destination ~., data = training, importance=TRUE, ntree = 500, mtry = best.m)
  samplesize <- (((training %>% group_by(country_destination) %>% tally())$n)/3 * 2) %>% round()
  samplesize <-  rep(samplesize[1],3) # 1 for undersapling, 3 for over sampling
  names(samplesize) <- c("other", "US", "NDF")
  rf <- randomForest(country_destination ~., data = training, importance=TRUE, ntree = 500, mtry = best.m,
                     strata =train$country_destination, samplesize = samplesize)
  rf_pred <- predict(rf,type = "prob", newdata = holdout)
  # Extract Prediction Interval, misclassification rate, AUC
  pred_interval <- CategoryPredInterval(rf_pred, holdout$country_destination %>% levels())
  # table(holdout$country_destination, pred_interval$pred50)
  # table(holdout$country_destination, pred_interval$pred80)
  mis_class <- lapply(pred_interval, get_misclass_rate, actual = holdout$country_destination)
  
  return(list(model = rf, pred = rf_pred, pred_interval = pred_interval, mis_class = mis_class))
}
