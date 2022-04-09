########################################  Naive Bayes  
#'
#' Function of fitting Naive Bayes model with five-fold cross validation
#' Output prints the misclassification rates based on 50% and 80% prediction interval 
#' @param data the input dataset 
#' @return a dataframe of two columns and five rows. The first five rows is the misclassification rate of each
#' fold, and the last row is the mean misclassification rate over five folds. The first column is generated from
#' 50% prediction interval; and the second column is generated from 80% misclassification rate 
#' @example NB_fitting(airbnb_data)
#' 
NB_fitting <- function(data) {
  
  mis_res_mat <- matrix(NA, ncol = 2, nrow = 6)
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
    mis_res_mat[i,1] <- round(oos_50_misclass, 3)
    mis_res_mat[i,2] <- round(oos_80_misclass,3) 
  }
  
  mis_res_mat[6, 1] <- round(mean(mis_res_mat[1:5, 1]),3)
  mis_res_mat[6, 2] <- round(mean(mis_res_mat[1:5, 2]),3)
  
  mis_res_df <- as.data.frame(mis_res_mat)
  rownames(mis_res_df) <- c('Fold 1', 'Fold 2', 'Fold 3','Fold 4','Fold 5','Avg')
  colnames(mis_res_df) <- c('50% PI', '80% PI')
  
  return(mis_res_df)
}   
 

#' Function of fitting naive bayes with resampling and oversampling method 
#' @param data the input dataset 
#' @return a dataframe with the misclassification rate of oversampled model and undersmapled model based
#' on 50% and 80% prediction interval
#' @example NB_fitting_sampling(airbnb_sub)

NB_fitting_sampling <- function(data) {
  
  mis_res_mat <- matrix(NA, ncol = 4, nrow = 6)
  labels_vec <- c('other', 'US', 'NDF')
  
  for (i in 1:5) {
    holdout <- extract_folds(data, i, folds)
    train <- data[-folds[[i]], ]
    
    # up sampling 
    train_upsample <- upSample(x = train[, -16],
                               y = train[, 16]) 
    classifier_upsample <- naiveBayes(Class ~ ., data = train_upsample)
    oos_pred_upsample <- predict(classifier_upsample, newdata = holdout, type = "raw")
    oos_pred_label_upsample <- CategoryPredInterval(oos_pred_upsample, labels_vec)    
    
    oos_50_misclass_upsample <-
      get_misclass_rate(holdout$country_destination, oos_pred_label_upsample$pred50)
    oos_80_misclass_upsample <-
      get_misclass_rate(holdout$country_destination, oos_pred_label_upsample$pred80)
    
    mis_res_mat[i,1] <- round(oos_50_misclass_upsample, 3)
    mis_res_mat[i,2] <- round(oos_80_misclass_upsample,3)     
    
    # down sampling 
    train_downsample <- downSample(x = train[, -16],
                                   y = train[, 16])
    classifier_downsample <- naiveBayes(Class ~ ., data = train_downsample)
    oos_pred_downsample <- predict(classifier_downsample, newdata = holdout, type = "raw")
    oos_pred_label_downsample <- CategoryPredInterval(oos_pred_downsample, labels_vec)  
    
    
    oos_50_misclass_downsample <-
      get_misclass_rate(holdout$country_destination, oos_pred_label_downsample$pred50)
    oos_80_misclass_downsample <-
      get_misclass_rate(holdout$country_destination, oos_pred_label_downsample$pred80)
    
    
    mis_res_mat[i,3] <- round(oos_50_misclass_downsample, 3)
    mis_res_mat[i,4] <- round(oos_80_misclass_downsample,3) 
  }
  
  # calculate the avg miclass-rate
  for (j in 1:4){
    mis_res_mat[6, j] <- round(mean(mis_res_mat[1:5, j]),3)
  }
  mis_res_df <- as.data.frame(mis_res_mat)
  rownames(mis_res_df) <- c('Fold 1', 'Fold 2', 'Fold 3','Fold 4','Fold 5','Avg')
  colnames(mis_res_df) <- c('up_50', 'up_80', 'down_50', 'down_80')
  
  return(mis_res_df)
}   


########################################  Random Forest
#' Function of perfom recursive feature elimination for ran forest
#' @param data the whole dataframe  
#' @return a rfe object 

rfe_rf_procedure <- function(data){
  X_train <- data[, -ncol(data)]
  y_train <- data[, ncol(data)]
  control_rfe = rfeControl(functions = rfFuncs, # random forest
                           method = "repeatedcv", # repeated cv
                           repeats = 1,  
                           number = 3) 
  result_rfe = rfe(x = X_train, 
                   y = y_train, 
                   sizes = c(5:(ncol(data) -1)),
                   rfeControl = control_rfe)
  return(result_rfe)
}

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

  # get interval score and auc
  IS50 <- ComputeIntervalScore(rf_pred, holdout$country_destination, 0.5)
  IS80 <- ComputeIntervalScore(rf_pred, holdout$country_destination, 0.2)
  auc <- multiclass.roc(holdout$country_destination, rf_pred[,2])$auc
  
  return(list(model = rf, pred = rf_pred, pred_interval = pred_interval, 
              mis_class = mis_class,
              interval_score = list(is50 = IS50, is80 = IS80), 
              auc = auc))
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

################################################## Function o fitting multinomial logistic regression
#' Function o fitting a multinomial logistic regression
#' @param data a dataframe
#  @stepwise logical whethe to perform stepwise selection base on AIC

multinom_logit_fitting <- function(data, stepwise = FALSE) {
  mis_res_mat <- matrix(NA, ncol = 2, nrow = 6)
  
  # AUC value
  AUC_value <- matrix(NA, ncol = 1, nrow = 6)
  # misclassification rate of point prediction
  mis_res_single_mat <- matrix(NA, ncol = 1, nrow = 6)
  # interval score
  IS_mat <- matrix(NA, ncol = 2, nrow = 6)
  
  
  for (i in 1:5) {
    holdout <- extract_folds(data, i, folds)
    train <- data[-folds[[i]],]
    
    multi <-
      multinom(country_destination ~ ., trace = FALSE, data = train)
    if (stepwise == TRUE) {
      multi = MASS::stepAIC(multi, trace = FALSE)
    }
    multi_pred <- predict(multi, newdata = holdout, type = "probs")
    multi_label <- predict(multi, newdata = holdout)
    pred_interval <-
      CategoryPredInterval(multi_pred, holdout$country_destination %>% levels())
    
    # point prediction
    mis_res_single_mat[i, 1] <-
      round(sum(multi_label != holdout$country_destination) / nrow(holdout),
            3)
    
    # interval score
    IS_mat[i, 1] <-
      round(ComputeIntervalScore(multi_pred, holdout$country_destination, 0.5),
            3)
    IS_mat[i, 2] <-
      round(ComputeIntervalScore(multi_pred, holdout$country_destination, 0.2),
            3)
    
    
    # Prediction interval
    oos_50_misclass <-
      get_misclass_rate(holdout$country_destination, pred_interval$pred50)
    oos_80_misclass <-
      get_misclass_rate(holdout$country_destination, pred_interval$pred80)
    
    mis_res_mat[i, 1] <- round(oos_50_misclass, 3)
    mis_res_mat[i, 2] <- round(oos_80_misclass, 3)
    
    # AUC
    AUC_value[i, 1] <-
      round(multiclass.roc(holdout$country_destination, multi_pred)$auc,
            3)
    
  }
  
  mis_res_mat[6, 1] <- round(mean(mis_res_mat[1:5, 1]), 3)
  mis_res_mat[6, 2] <- round(mean(mis_res_mat[1:5, 2]), 3)
  
  mis_res_single_mat[6, 1] <-
    round(mean(mis_res_single_mat[1:5, 1]), 3)
  
  IS_mat[6, 1] <- round(mean(IS_mat[1:5, 1]), 3)
  IS_mat[6, 2] <- round(mean(IS_mat[1:5, 2]), 3)
  
  # AUC value
  AUC_value[6, 1] <- round(mean(AUC_value[1:5, 1]), 3)
  rownames(AUC_value) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  
  rownames(mis_res_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  colnames(mis_res_mat) <- c('50% PI', '80% PI')
  
  
  
  
  rownames(IS_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  colnames(IS_mat) <- c('50% PI', '80% PI')
  
  rownames(mis_res_single_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  
  return(
    list(
      mis_res_pred_int = mis_res_mat,
      mis_res_point_pred = mis_res_single_mat,
      IS = IS_mat,
      AUC = AUC_value
    )
  )
  
}         
    

#' Function o fitting a multinomial logistic regression, conditional on resampling
#' @param data a dataframe
#  @stepwise resample whethe to perform oversampling or undersampling

multinom_logit_resampling <- function(data, resample = 'over') {
  mis_res_mat <- matrix(NA, ncol = 2, nrow = 6)
  # AUC value
  AUC_value <- matrix(NA, ncol = 1, nrow = 6)
  # misclassification rate of point prediction
  mis_res_single_mat <- matrix(NA, ncol = 1, nrow = 6)
  # interval score
  IS_mat <- matrix(NA, ncol = 2, nrow = 6)
  
  
  for (i in 1:5) {
    holdout <- extract_folds(data, i, folds)
    train <- data[-folds[[i]],]
    
    if (resample == 'over') {
      train <- upSample(x = train[, -ncol(data)],
                        y = train[, ncol(data)])
    }
    if (resample == 'under') {
      train <- downSample(x = train[, -ncol(data)],
                          y = train[, ncol(data)])
    }

    
    multi <-
      multinom(Class ~ ., trace = FALSE, data = train)

    multi_pred <- predict(multi, newdata = holdout, type = "probs")
    multi_label <- predict(multi, newdata = holdout)
    pred_interval <-
      CategoryPredInterval(multi_pred, holdout$country_destination %>% levels())
    
    # point prediction
    mis_res_single_mat[i, 1] <-
      round(sum(multi_label != holdout$country_destination) / nrow(holdout),
            3)
    
    # interval score
    IS_mat[i, 1] <-
      round(ComputeIntervalScore(multi_pred, holdout$country_destination, 0.5),
            3)
    IS_mat[i, 2] <-
      round(ComputeIntervalScore(multi_pred, holdout$country_destination, 0.2),
            3)
    
    
    # Prediction interval
    oos_50_misclass <-
      get_misclass_rate(holdout$country_destination, pred_interval$pred50)
    oos_80_misclass <-
      get_misclass_rate(holdout$country_destination, pred_interval$pred80)
    
    mis_res_mat[i, 1] <- round(oos_50_misclass, 3)
    mis_res_mat[i, 2] <- round(oos_80_misclass, 3)
    
    # AUC
    AUC_value[i, 1] <-
      round(multiclass.roc(holdout$country_destination, multi_pred)$auc,
            3)
    
  }
  
  mis_res_mat[6, 1] <- round(mean(mis_res_mat[1:5, 1]), 3)
  mis_res_mat[6, 2] <- round(mean(mis_res_mat[1:5, 2]), 3)
  
  mis_res_single_mat[6, 1] <-
    round(mean(mis_res_single_mat[1:5, 1]), 3)
  
  IS_mat[6, 1] <- round(mean(IS_mat[1:5, 1]), 3)
  IS_mat[6, 2] <- round(mean(IS_mat[1:5, 2]), 3)
  
  # AUC value
  AUC_value[6, 1] <- round(mean(AUC_value[1:5, 1]), 3)
  rownames(AUC_value) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  
  rownames(mis_res_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  colnames(mis_res_mat) <- c('50% PI', '80% PI')
  
  
  
  
  rownames(IS_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  colnames(IS_mat) <- c('50% PI', '80% PI')
  
  rownames(mis_res_single_mat) <-
    c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Avg')
  
  return(
    list(
      mis_res_pred_int = mis_res_mat,
      mis_res_point_pred = mis_res_single_mat,
      IS = IS_mat,
      AUC = AUC_value
    )
  )
  
}         


