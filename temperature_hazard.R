#' temperature_hazard
#' Find the number of days at each location where there is risk of health effects from freezing weather and extreme heat at 9am
#' @param data df w/ columns Location, Date, Temp9am
#' @author Alex Brown

temp_hazard = function(data){
  
  clim_df <- data %>%
    dplyr::mutate(year = lubridate::year(Date),
                  month = lubridate::month(Date),
                  day = lubridate::day(Date)) %>%
    mutate(hazard = case_when(
      Temp9am >35 ~ "extreme heat",
      Temp9am < 35 & Temp9am >= 0 ~ "comfortable",
      Temp9am < 0 ~ "freezing"
    ) )
  hazard_df <- clim_df %>%
    group_by(Location) %>%
    summarise(extreme_heat_n = sum(hazard == "extreme heat", na.rm = TRUE),
              comfortable_n = sum(hazard == "comfortable", na.rm = TRUE),
              freezing_n = sum(hazard == "freezing", na.rm = TRUE)) %>% 
    ungroup()
  
  return(list(table = hazard_df)
  )
  
  
  
}

#.rda