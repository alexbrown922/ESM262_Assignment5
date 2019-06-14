#' rainfall_mean
#' Find the daily mean rainfall for every year and location. Output location, year, and rainfall
#' @param rainfall_data df w/ columns Rainfall, Date, Locaiton
#' @param location shows you desired location, Albury default
#' @author Alex Brown



rainfall_mean = function(rainfall_data, location = "Albury", plot_out = FALSE){
  
  rain_df <- rainfall_data %>%
    dplyr::mutate(year = lubridate::year(Date),
                  month = lubridate::month(Date),
                  day = lubridate::day(Date)) %>%
    filter(Location == location) %>%
    group_by(year, Location) %>%
    summarise(rainfall_mean = mean(Rainfall, na.rm = TRUE)) %>% 
    ungroup
  
#.rda
  
  if(plot_out){
    plot <- ggplot(rain_df, aes(x = as.factor(year), y = rainfall_mean)) + geom_col() + labs(title = sprintf("Annual Average Rainfall Percentage", location), x = "Year", y = "Average Rainfall") + theme_classic() +
      #      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_text(angle = -45, hjust = 1))
    
  }
  
  return(
    list(
      rain_table = rain_df,
      plot = plot
    ))
}
