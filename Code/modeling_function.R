
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
    mis_res_mat[i,1] <- round(oos_50_misclass, 2)
    mis_res_mat[i,2] <- round(oos_80_misclass,2) 
  }
  
  mis_res_mat[6, 1] <- round(mean(oos_50_misclass_vec[1:5]),2)
  mis_res_mat[6, 2] <- round(mean(oos_80_misclass_vec[1:5]),2)
  
  mis_res_df <- as.data.frame(mis_res_mat)
  rownames(mis_res_df) <- c('Fold 1', 'Fold 2', 'Fold 3','Fold 4','Fold 5','Avg')
  colnames(mis_res_df) <- c('50% PI', '80% PI')
  
  return(mis_res_df)
}   
 