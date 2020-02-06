## PROJECT:  COP20 TZA
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  Reviewing EGPAF Data
## DATE:     2020-02-03
## UPDATED: 


# GENIE ARTIFACT ----------------------------------------------------------

# Site By IM Extract
# DATIM data as of: 02/04/2020 03:43:59 UTC
# Genie report updated: 02/04/2020 21:01:47 UTC
# 
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1 
# Operating Unit: Tanzania
# Daily/Frozen: Daily
# Indicator: PrEP_NEW, PrEP_CURR
# Disaggregate: Total Numerator


# DEPENDENCIES ------------------------------------------------------------

  library(ICPIutilities)
  library(tidyverse)

# IMPORT ------------------------------------------------------------------

df <- read_msd("C:/Users/achafetz/Downloads/PEPFAR-Data-Genie-SiteByIMs-2020-02-05.zip", save_rds = FALSE)

df_prep <- df %>% 
  filter(indicator %in% c("PrEP_NEW", "PrEP_CURR"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year > 2018) %>% 
  group_by(fundingagency, snu1, psnu, sitename, sitetype, orgunituid, 
          mech_code, mech_name, primepartner, fiscal_year,  indicator) %>% 
  summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  dplyr::filter_if(is.double, dplyr::any_vars(.!= 0)) %>% 
  rename_official()


# EXPORT ------------------------------------------------------------------

  write_csv(df_prep, "out/data/TZA_PrEP_IPs_Sites.csv", na = "")
