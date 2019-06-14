library(testthat)
library(tidyverse)
          
          
          test_that("water_supply_works" ,
                    {rain_df = as.data.frame(cbind(Date = c("2011-02-17", "2013-06-12", "2014-10-11", "2015-11-17"), Location = c("Albury", "Albury", "Albury", "Albury"),
                                                     Rainfall=rep(1, times=4)))
                    rain_df$Rainfall <- as.numeric(as.character(rain_df$Rainfall))
                    
                    
                    
                    
                    expect_that(water_supply(rain_df)$table$annual_rainfall, equals(c(1,1,1,1)))
                    })

          #.rda