#' rainfall_maximum
#'Find the max rainfall for each location and year/month. Output the location, year, month, and rainfall
#' @param rainfall_data df w/ columns Rainfall, Date, Location
#' @param location shows you desired location, Albury default
#' @param year shows you desired years, default 2009 - 2017
#' @param which_month shows you desired month, default May
#' @author Alex Brown

rainfall_maximum = function(rainfall_data, location = "Albury", year = 2009:2017, which_month = 5){
  
  rain_df <- rainfall_data %>%
    dplyr::mutate(year = lubridate::year(Date),
                  month = lubridate::month(Date),
                  day = lubridate::day(Date)) %>%
    dplyr::filter(Location == location, year %in% year, month == which_month) %>%
    dplyr::select(year, month, day, Rainfall)
  
  rain_array <- array(data = rain_df$Rainfall,
                      dim = c(30, 9),
                      dimnames = list(
                        1:30, # days of the month
                        2009:2017)) 
  rain_month_max <- apply(rain_array, MARGIN = 2, FUN = max, na.rm = TRUE)
  
  return(
    list(Location = location,
         Month = which_month,
         rainfall_maximum = rain_month_max)
  )
  
}
