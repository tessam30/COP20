## PROJECT:  COP20 TZA
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  Reviewing EGPAF Data
## DATE:     2020-02-03
## UPDATED: 


# GENIE ARTIFACT ----------------------------------------------------------

# Site By IM Extract
# DATIM data as of: 02/03/2020 03:38:56 UTC
# Genie report updated: 02/03/2020 20:50:49 UTC
# 
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1 
# Operating Unit: Tanzania
# Daily/Frozen: Daily
# Mechanisms: 18060
# Indicator: HTS_TST, HTS_TST_POS

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(ICPIutilities)


# IMPORT ------------------------------------------------------------------

  path <- list.files("~/Data/", "GENIE", full.names = TRUE)
  
  #df_tza <- read_msd(path)
  
  df_tza <- read_rds(path)

# MUNGE -------------------------------------------------------------------

  df_export <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result"),
           fiscal_year %in% c(2018, 2019)) %>%
    group_by(operatingunit, snu1, psnu, psnuuid, primepartner, sitename, orgunituid, sitetype, 
             indicator, trendscoarse, sex, modality, fiscal_year) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup() 
  
  df_export <- df_export %>% 
    reshape_msd(clean = TRUE) %>% 
    mutate(trendscoarse = str_remove(trendscoarse, " (Age|Sex)"))
  
  df_site_target <- df_export %>% 
    filter(indicator == "HTS_TST",
           period == "FY19",
           period_type == "targets") %>% 
    group_by(orgunituid) %>% 
    summarise(fy19_hts_full_site_target = sum(val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(fy19_hts_full_site_target) %>% 
    mutate(fy19_hts_full_site_target_cum_share = cume_dist(fy19_hts_full_site_target))
  
  df_export <- left_join(df_export, df_site_target)

# EXPORT ------------------------------------------------------------------

  write_csv(df_export, "out/data/TZA_EGPAF_HTS_v2.csv", na = "")
