
#' Random Forest Undersampling Model Function
#' Undersample to the other response
#' The sampling number is then multiplied by 2/3 
#' as the Random Forest algorithm take 2/3 of  the data for the forest-making, and the leftover for calculating OOB error
#'
#' @param training the training data set
#' @param holdout the holdout data set
#' @param type type of random forest model to run, one of "full", "sub", "under", "over"
#'
#' @return a named list consists of the random forest model, the probability prediction for the testing set, the 50% and 80% prediction interval and their misclassification rate
randomforest_model <- function(training, holdout, type = "full") {
  
  # Tunning mtry
  mtry <- tuneRF(training %>% select(-country_destination) , training$country_destination, ntreeTry=500,
                 stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  # Fit Model based on model type
  if (type %in% c("full", "sub")){
    rf <- randomForest(country_destination ~., data = training, importance=TRUE, ntree = 500, mtry = best.m)
  } else {
    samplesize <- (((training %>% group_by(country_destination) %>% tally())$n)/3 * 2) %>% round()
    if(type == "under"){ ndx = 1} else {ndx = 3} # 1 for undersampling, 3 for over sampling
    samplesize <-  rep(samplesize[ndx],3) 
    names(samplesize) <- c("other", "US", "NDF")
    rf <- randomForest(country_destination ~., data = training, importance=TRUE, ntree = 500, mtry = best.m,
                       strata =train$country_destination, samplesize = samplesize)
  }
  
  # Make prediction
  rf_pred <- predict(rf,type = "prob", newdata = holdout)
  # Extract Prediction Interval, misclassification rate
  pred_interval <- CategoryPredInterval(rf_pred, holdout$country_destination %>% levels())
  # table(holdout$country_destination, pred_interval$pred50)
  # table(holdout$country_destination, pred_interval$pred80)
  mis_class <- lapply(pred_interval, get_misclass_rate, actual = holdout$country_destination)
  mis_class_point <- lapply(pred_interval, get_point_misclass_rate, actual = holdout$country_destination)
  
  # get interval score and auc
  IS <- lapply(as.list(0.5, 0.2), function(alpha){ComputeIntervalScore(rf_pred, holdout$country_destination, alpha)})
  auc <- multiclass.roc(holdout$country_destination, rf_pred[,2])$auc
  
  return(list(model = rf, pred = rf_pred, pred_interval = pred_interval, 
              mis_class = mis_class, mis_class_point = mis_class_point,
              interval_score = IS, auc = auc))
}


#' Run 5 fold cross validation and produce a list of random forest model with different types
#'
#' @param df datafram to be split and run
#' @param foldidx the row index for each split
#' @param type type of random forest model to run, one of "full", "sub", "under", "over"
#'
#' @return a list fo models and model stats for each fold.
rf_5fold_cross <- function(df, foldidx, type = "full") {
    
  model_lists <- lapply(as.list(1:5), function(t){
    subst_list <- extract_folds(df, t, folds, test = TRUE)
    if (type == "sub") {
      if (t == 5) { # subset based on rfe results
      subst_list <- extract_folds(df %>% select(country_destination, signup_method, log_unique_actionTrp, date_first_active,
                                                date_account_created, first_browser, AppleCate, log_total_action, log_age),
                                  t, foldidx, test = TRUE)} else {
                                    subst_list <- extract_folds(df %>% select(-c(signup_app, signup_flow, affiliate_channel, first_affiliate_tracked)), t, folds, test = TRUE)}}
    train <- subst_list$train
    test <- subst_list$test
    return(randomforest_model(train, test, type))
  })
  return(model_lists)
}
