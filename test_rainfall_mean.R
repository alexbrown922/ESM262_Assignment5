library(testthat)
library(tidyverse)



test_that("rainfall_mean_works" ,
          {rainfall_data = as.data.frame(cbind(Date = c("2011-02-17", "2013-06-12", "2014-10-11", "2015-11-17"), Location = c("Albury", "Albury", "Albury", "Albury"),
                                           Rainfall=rep(1, times=4)))
          rainfall_data$Rainfall <- as.numeric(rainfall_data$Rainfall)
          
          
          
          
          expect_that(rainfall_mean(rainfall_data, plot_out=TRUE)$rain_table$rainfall_mean, equals(c(1,1,1,1)))
          })