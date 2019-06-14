#' temperature_AC
#' Find the potential cost of extreme heat to people's air conditioning bill. #numbers way too large
#' @param data df w/ columns Date, MaxTemp
#' @author Alex Brown

temperature_AC <- function(data){
  
  AC_data <- data %>%
    dplyr::mutate(year = lubridate::year(Date),
                  month = lubridate::month(Date),
                  day = lubridate::day(Date)) %>%
    mutate(AC_bill = case_when(MaxTemp >= 35 ~ MaxTemp*15,
                                     MaxTemp < 35 ~ 0)) %>%
    group_by(year) %>%
    summarise(total_AC_bill = sum(AC_bill, na.rm = TRUE)) %>% 
    ungroup()
  
  return(list(table = AC_data))
}

#.rda