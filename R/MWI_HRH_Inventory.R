## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare HRH inventory
## DATE:     2020-02-06
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(janitor)


# IMPORT ------------------------------------------------------------------

  #import PSNUxIM for Malawi

    df <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
      read_rds()

  #import HRH inventory
    df_natl_hrh <- list.files("data", "HRH Data", full.names = TRUE) %>% 
      read_excel(sheet = "Clean Data", col_types = c(.default = "text")) %>% 
      clean_names()
    

# MUNGE -------------------------------------------------------------------

  #MSD - filter for HRH & aggregate at psnu level
    df_mer <- df %>% 
      filter(operatingunit == "Malawi",
             indicator == "HRH_CURR",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2019) %>% 
      count(psnu, wt = cumulative) %>% 
      mutate(psnu = str_remove(psnu, " District")) %>% 
      rename(district = psnu, 
             mer_hrh = n)

  #HRH Inventory - select key fields for cadres and aggregate
    df_natl_hrh_c <- df_natl_hrh %>% 
      select(district, starts_with("number_of_workers")) %>%
      gather(cadre_type, natl_inventory, -district) %>% 
      mutate(natl_inventory = as.numeric(natl_inventory),
             district = str_to_sentence(district),
             district = str_replace(district, "_b", " B")) %>% 
      count(district, wt = natl_inventory, name = "natl_inventory")
  
  #merge
    full_join(df_natl_hrh_c, df_mer) %>% 
      arrange(desc(natl_inventory))
    
