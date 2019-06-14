library(testthat)
library(tidyverse)



test_that("temp_hazard_works" ,
          {rainfall_data = as.data.frame(cbind(Date = c("2011-02-17", "2013-06-12", "2014-10-11", "2015-11-17"), Location = c("Albury", "Albury", "Albury", "Albury"),
                                           Rainfall=rep(1, times=4), Temp9am = rep(41, times = 4)))
          rainfall_data$Temp9am <- as.numeric(as.character(rainfall_data$Temp9am))
          
          
          
          
          expect_that(temp_hazard(rainfall_data)$table$extreme_heat_n, equals(4))
          })

#.rda