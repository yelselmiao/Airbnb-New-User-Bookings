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