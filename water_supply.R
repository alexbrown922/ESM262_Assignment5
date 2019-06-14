#' water_supply
#' Finds an output for water supply quality based on location and year
#' @param rainfall_data df w/ columns Location, Date, Rainfall
#' @param location shows you desired location, Albury default
#' @param plot_out allows you to produce a plot
#' @author Alex Brown

water_supply = function(rainfall_data, location = "Albury", plot_out = FALSE){
  
  rain_df <- rainfall_data %>%
    dplyr::mutate(year = lubridate::year(Date),
                  month = lubridate::month(Date),
                  day = lubridate::day(Date)) %>%
    filter(Location == location) %>%
    group_by(year, Location) %>%
    summarise(annual_rainfall = sum(Rainfall, na.rm = TRUE)) %>%
    mutate(water_quality =
             case_when(
               annual_rainfall > 450 ~ "superior",
               annual_rainfall < 450 & annual_rainfall > 300 ~ "intermediate",
               annual_rainfall < 300 ~ "poor"
             )) %>% 
    ungroup()
  
  if(plot_out){
    plot <- ggplot(rain_df, aes(x = as.factor(year), y = annual_rainfall)) + geom_col(aes(fill = water_quality)) + labs(title = sprintf("Annual Rainfall Percentage", location), x = "Year", y = "Annual Rainfall") + theme_classic() +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_discrete(name = "Water Quality") +
      theme(axis.text.x = element_text(angle = -45, hjust = 1))
    
  }
  
  return(
    list(
      table = rain_df,
      plot = plot
    ))
  
  
  
}