## PROJECT:  COP20 TZA
## AUTHOR:   A.CHAFETZ | USAID
## LICENSE:  MIT
## PURPOSE:  treatment target planning, data needed
## DATE:     2020-02-11
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(ICPIutilities)


# IMPORT ------------------------------------------------------------------

  #import tza TX data
    df_tza <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
      read_rds() %>% 
      filter(operatingunit == "Tanzania")
    
  #import Naomi model for FY20 PLHIV and TX_CURR_SUBNAT estimates
    df_naomi <- read_excel("data/Data Pack_Tanzania_Mag Updates-3.2.20_SpectrumAsText.xlsx",
                         sheet = "Spectrum")

# MUNGE -------------------------------------------------------------------

  ## MER
  
  #TX_CURR FY19
    df_tx <- df_tza %>% 
      filter(indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == "2019") %>% 
      group_by(snu1, snu1uid, psnu, psnuuid, snuprioritization) %>% 
      summarise(tx_curr_fy19 = sum(cumulative, na.rm = TRUE)) %>% 
      ungroup()

  #NET NEW
    df_nn <- df_tza %>% 
      filter(indicator == "TX_NET_NEW",
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(snu1,snu1uid, psnu, psnuuid, snuprioritization, fiscal_year) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%  
      ungroup() %>% 
      reshape_msd(clean = TRUE)
    
  #identify last 6 quarters
    last6qtrs <- df_nn %>% 
      distinct(period) %>% 
      arrange(period) %>% 
      tail(6) %>% 
      pull()
    
  #NET NEW average from last 6 Q
    df_nn <- df_nn %>% 
      filter(period %in% last6qtrs) %>% 
      group_by(snu1, psnu, psnuuid) %>% 
      summarise(net_new_6q_avg = mean(val, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(net_new_6q_avg = round(net_new_6q_avg, 0))


  ##Spectrum
    
  #clean up DP import
    df_naomi <- df_naomi %>% 
      filter(dataelement %in% c("IMPATT.PLHIV (SUBNAT, Age/Sex)", 
                                "TX_CURR_SUBNAT (N, SUBNAT, Age Aggregated/Sex): Receiving ART")) %>% 
      group_by(indicator = dataelement, psnu) %>% 
      summarise_at(vars(value), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(indicator = case_when(indicator == "IMPATT.PLHIV (SUBNAT, Age/Sex)" ~ "plhiv_fy20",
                                   TRUE ~ "tx_curr_fy20"),
             value = round(value, 0)) %>% 
      spread(indicator, value)
  
  ##Merged dataset
    df_output <- df_tx %>% 
      left_join(df_nn) %>% 
      left_join(df_naomi)


# EXPORT ------------------------------------------------------------------

  write_csv(df_output, "out/data/TZA_Tx_targeting_info.csv", na = "")
