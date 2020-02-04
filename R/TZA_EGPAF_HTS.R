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

  path <- list.files("~/Data/", "Genie-Site", full.names = TRUE)
  
  df_tza <- read_msd(path)


# MUNGE -------------------------------------------------------------------

  df_export <- df_tza %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result")) %>%
    group_by(operatingunit, snu1, psnu, psnuuid, primepartner, sitename, orgunituid, sitetype, 
             indicator, trendscoarse, sex, modality, fiscal_year) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup()

# EXPORT ------------------------------------------------------------------

  write_csv(df_export, "out/data/TZA_EGPAF_HTS.csv", na = "")
