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

#function that's used to combine categories with extremly small counts 
category_comb <- function(df) {
  cleaned_df <-  df %>%
    mutate(
      signup_flow = ifelse(
        signup_flow %in% c('0', '1', '2', '3', '12', '23', '24', '25'),
        signup_flow,
        'other'
      ),
      language = ifelse(
        language %in% c('de', 'en', 'es', 'it', 'ko', 'ru', 'zh'),
        language,
        'other'
      ),
      affiliate_provider = ifelse(
        affiliate_provider %in% c(
          'baidu',
          'email-marketing',
          'gsp',
          'meetup',
          'naver',
          'wayn',
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
        first_browser %in% c('Chrome', 'Safari', 'Firefox', NA, 'IE', 'Mobile Safari'),
        first_browser,
        'other'
      )
    )
  return(cleaned_df)
  
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
    mutate(NDF = ifelse(country_destination == 'NDF', 1, 0),
           continent_des = case_when(country_destination %in% c('DE', 'FR', 'GB', 'IT', 'NL', 'PT', 'ES')~'Europe',
                                     country_destination == 'AU'~'Australia',
                                     country_destination %in% c('CA', 'US')~'Americas',
                                     country_destination == 'other' ~ 'other',
                                     country_destination == 'NDF' ~ 'NDF'))
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

# operatpr 
`%notin%` <- Negate(`%in%`)





