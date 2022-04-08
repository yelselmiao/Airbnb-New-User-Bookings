destination_prop_plot <- function(data){
  data_des <- data %>%
    count(country_destination) %>%
    mutate(
      perc = round(proportions(n) * 100, 1),
      res = str_c(n, "(", perc, ")%"),
      country_destination = as.factor(country_destination)
    )
  
  p <- ggplot(data_des, aes(country_destination, n, fill = country_destination)) +
    geom_col() +
    geom_text(aes(label = res), vjust = -0.5)
  return(p)
  
}

# function that's used to combine categories with extremly small counts  (count < 100)
category_comb <- function(df) {
  cleaned_df <-  df %>%
    mutate(
      signup_flow = ifelse(
        signup_flow %in% c('0', '12', '23', '24', '25'),
        signup_flow,
        'other'
      ),
      language = ifelse(
        language %in% c('de', 'en', 'es', "fr", 'ko', 'zh'),
        language,
        'other'
      ),
      affiliate_provider = ifelse(
        affiliate_provider %in% c(
          'baidu',
          "craigslist",
          'email-marketing',
          "facebook-open-graph",
          'gsp',
          'meetup',
          'naver',
          "padmapper",
          'wayn',
          "vast",
          "yahoo",
          'yandex'
        ),
        'other',
        affiliate_provider
      ),
      first_affiliate_tracked =  replace(
        first_affiliate_tracked,
        first_affiliate_tracked %in% c('local ops', 'marketing', 'product'),
        'tracked-other'
      ),
      first_device_type = replace(
        first_device_type,
        first_device_type == 'SmartPhone (Other)',
        'Other/Unknown'
      ),
      first_browser = ifelse(
        first_browser %in% c('Chrome', 'Safari', 'Firefox', "Chrome Mobile", 'IE', 'Mobile Safari'),
        first_browser,
        'other'
      )
    )
  return(cleaned_df)
  
}


# function for further merging categories 
category_comb_II <- function(df){
  df_cleaned <- df %>% 
    mutate(signup_method = ifelse(signup_method == 'facebook', 'facebook', 'basic_google'), 
           signup_flow = ifelse(signup_flow == '0', 'page_0', 'other_page'), 
           language = ifelse(language == 'en', 'en', 'other'), 
           affiliate_channel = ifelse(affiliate_channel == 'direct','direct', 'nondirect_sem'), 
           affiliate_provider = ifelse(affiliate_provider == 'direct', 'direct', 'nondirect_google'), 
           first_affiliate_tracked = ifelse(first_affiliate_tracked %in% c('untracked', NA), 'untracked', 'tracked'), 
           signup_app = ifelse(signup_app %in% c('Web', 'Moweb'), 'Web', signup_app), 
           first_device_type = case_when(first_device_type %in% c('Android Phone', 'Android Tablet')~'Android', 
                                         first_device_type %in% c('iPad', 'iPhone', 'Mac Desktop')~'Apple', 
                                         first_device_type == 'Windows Desktop'~'Windows', 
                                         first_device_type %in% c('Desktop (Other)', 'Other/Unknown')~'Other/Unknown'), 
           first_browser = case_when(first_browser %in% c('Firefox', 'IE', 'other')~'other', 
                                     first_browser %in% c('Mobile Safari', 'Safari')~'Safari', 
                                     first_browser  %in% c("Chrome", "Chrome Mobile")~'Chrome')
    )
  return(df_cleaned)
}

# function that's used to check category count
category_count <- function(df){
  df %>%
    select(signup_method:country_destination) %>%
    map(function(x)
      table(x))
}


# sort out the destination column
des_sorter <- function(df){
  df_des_sorted <- df %>% 
    mutate(country_destination = ifelse(country_destination =="NDF", "NDF", ifelse(country_destination == "US", "US", "other")),
           country_destination = as_factor(country_destination))
  return(df_des_sorted)
}


# re-clean the loaded data prior to model fitting
data_set_up <- function(loaded_data){
  cleaned_data <- loaded_data %>%
    mutate(
      date_first_active = as.Date(date_first_active,'%Y-%m-%d'),
      date_account_created = as.Date(date_account_created, '%Y-%m-%d')
    ) %>% 
    dplyr::select(-c(X, id, date_first_booking)) %>% 
    mutate_if(is.character, as.factor)
  
  return(cleaned_data)
}

# operator 
`%notin%` <- Negate(`%in%`)


# ________________________ Folds ________________________

#' Fold Operation Function
#' Either extract the data frame for the kth fold (test = FALSE)
#' or extract a named list with both the training set and the testing set (test = TRUE), 
#' with the testing being the target fold
#'
#' @param df airbnb_train, the cleaned data
#' @param target_fold integer, the fold we want to extract
#' @param fold_idx list, a list of index for each fold, an example should be the outcome of createFolds form caret Package
#' @param k the number of folds we have, DEFAULT = 5
#' @param test wheter to only extract a dataframe or to extract the full testing and training data set as a named list
#'
#' @return if(test) -> list, else (dataframe)
#'
#' @examples extract_folds(airbnb_train, 1, folds)
extract_folds <- function(df, target_fold, fold_idx, k = 5, test = FALSE) {
  if (length(fold_idx) < k){stop("The length of fold index list does not match the number of folds. \n Default k is 5")}
  test_set <- df[fold_idx[[target_fold]], ]
  if(!test){return(test_set)}
  train_set <- df[unlist(fold_idx[-target_fold]), ]
  return(list(train = train_set, test = test_set))
}


#’ # refer to the function from https://uglab.stat.ubc.ca/~hjoe/stat447/Notes/stat447-classification-predintervals.pdf
#’ @description   Prediction intervals for a categorical response
#’ @param ProbMatrix of dimension nxJ, J = # categories,
#’             each row is a probability mass function
#’ @param labels vector of length J, with short names for categories
#’ @details
#’ A more general function can be written so the levels of prediction intervals
#’ can be other than 0.50 and 0.80.
#’ @example
#’ labels=c("A","B","C","D")
#’ p1=c(0.3,0.2,0.1,0.4); p2=c(0.6,0.1,0.1,0.2); ProbMatrix=rbind(p1,p2)
#’ CategoryPredInterval(ProbMatrix,labels)
#’
#’ @return list with two string vectors of length n:
#’  pred50 has 50% prediction intervals; pred80 has 80% prediction intervals
#’
CategoryPredInterval = function(ProbMatrix, labels)
{
  ncases = nrow(ProbMatrix)
  pred50 = rep(NA, ncases)
  pred80 = rep(NA, ncases)
  for (i in 1:ncases)
  {
    p = ProbMatrix[i, ]
    ip = order(p, decreasing = T)
    pOrdered = p[ip] # decreasing order
    labelsOrdered = labels[ip] # decreasing order
    G = cumsum(pOrdered) # cumulative sum from largest
    k1 = min(which(G >= 0.5))  # level1= 0.5
    k2 = min(which(G >= 0.8))  # level2= 0.8
    pred1 = labelsOrdered[1:k1]
    pred2 = labelsOrdered[1:k2]
    pred50[i] = paste(pred1, collapse = "")
    pred80[i] = paste(pred2, collapse = "")
  }
  list(pred50 = pred50, pred80 = pred80)
}

#' Calculate prediction interval misclassification rate 
#'
#' @param actual the actual value
#' @param pred the prediction value
#'
#' @return double, calculated missclassication rate
get_misclass_rate <- function(actual, pred) {
  acc_tbl <- table(actual, pred) %>% as.data.frame.table()
  all_obs <- acc_tbl$Freq %>% sum()
  crt_class <- (acc_tbl %>% rowwise() %>% filter(grepl(actual, pred)))$Freq %>% sum()
  return((all_obs - crt_class)/all_obs)
}


#' Calculate point prediction misclassification rate
#'
#' @param actual the actual value
#' @param pred the prediction value
#'
#' @return double, calculated missclassication rate
get_point_misclass_rate <- function(actual, pred) {
  acc_tbl <- table(actual, pred) %>% as.data.frame.table()
  all_obs <- acc_tbl$Freq %>% sum()
  crt_class <- (acc_tbl %>% rowwise() %>% filter(pred == actual))$Freq %>% sum()
  return((all_obs - crt_class)/all_obs)
}


#' check the proportion of each category in the response variable 
#' @param date the dataset that you would like to check 
#' @return a dataframe demonstrating the count and proportion of each category in the response variable 
#' @example destination_prop_tb(airbnb_data)
destination_prop_tb <- function(data) {
  data %>%
    group_by(country_destination) %>%
    tally() %>%
    mutate(prop = scales::percent(n / sum(n)))
}

loadRData <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Cite: LetterRecog-multLogit.pdf
#' @description
#' Prediction intervals for a categorical response
#' @param ProbMatrix of dimension nxJ, J = # categories,
#' each row is a probability mass function
#' @param labels vector of length J, with short names for categories
#'
#' @details
#' A more general function can be written so the levels of prediction intervals
#' can be other than 0.50 and 0.80.
#'
#' @return list with two string vectors of length n:
#' pred50 has 50% prediction intervals
#' pred80 has 80% prediction intervals
#'
CategoryPredInterval = function(ProbMatrix,labels)
{ ncases=nrow(ProbMatrix)
pred50=rep(NA,ncases); pred80=rep(NA,ncases)
for(i in 1:ncases)
{ p=ProbMatrix[i,]
ip=order(p,decreasing=T)
pOrdered=p[ip] # decreasing order
labelsOrdered=labels[ip] # decreasing order
G=cumsum(pOrdered) # cumulative sum from largest
k1=min(which(G>=0.5)) # level1= 0.5
k2=min(which(G>=0.8)) # level2= 0.8
pred1=labelsOrdered[1:k1]; pred2=labelsOrdered[1:k2]
pred50[i]=paste(pred1,collapse="")
pred80[i]=paste(pred2,collapse="")
}
list(pred50=pred50, pred80=pred80)
}


#' Calculate Interval Score for a single fold
#'
#' @param predProb  a matrix of prediction probabilities with row names (other, US, NDF),
#' @param actual a vector of the actual class
#' @param alpha scalar number, e.g. at 80% level, alpha = 0.2
#'
#' @return a scalar number of interval score
ComputeIntervalScore <- function(predProb, actual, alpha) {
  
  # Calculate interval score for a single observation
  calculateIS <- function(other, US, NDF, actual){ 
    other = as.double(other)
    US = as.double(US)
    NDF = as.double(NDF)
    score <- switch (actual,
                     "other" = {2/alpha * (US + NDF) + ifelse(other < (1-alpha), 2/alpha * (1-alpha - other), 0)},
                     "US" = {2/alpha * (other + NDF) + ifelse(US < (1-alpha), 2/alpha * (1-alpha - US), 0)},
                     "NDF" = {2/alpha * (US + other) + ifelse(NDF < (1-alpha), 2/alpha * (1-alpha - NDF), 0)}
    )
    return(score)
  }
  IS <- predProb %>% as.data.frame %>% mutate(target = actual) %>%
    apply(1, function(r){calculateIS(r[1], r[2], r[3], r[4])}) %>% 
    mean()
  
  return(IS)
}


