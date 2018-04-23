library(tidyverse)

df <-
  foreign::read.dta('helpers/master.dta') %>%
  tbl_df()

nsim <- 500
