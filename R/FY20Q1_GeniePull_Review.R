## PROJECT:  COP20
## AUTHOR:   Aaron Chafetz | USAID
## PURPOSE:  R Script for reviewing FY20Q1 Genie in-process data  
## LICENSE:  MIT
## DATE:     2020-02-07
## UPDATED:  2020-02-10

#dependencies
  library(tidyverse)
  library(ICPIutilities) #devtools::install_github("ICPI/ICPIutilities")

#genie filepath
  path <- "../Downloads/PEPFAR-Data-Genie-OUByIMs-2020-02-10-group2.zip"

#import
  df <- read_msd(path, save_rds = FALSE, remove_txt = FALSE)

#table
  df %>%
    distinct(indicator, disaggregate) %>%
    arrange(indicator, disaggregate)

#ind list
  unique(df$indicator) %>% sort() %>% paste0(collapse = ", ")

#disagg list
  unique(df$disaggregate) %>% sort() %>% paste0(collapse = ", ")