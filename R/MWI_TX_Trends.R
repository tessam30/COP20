## PROJECT: COP20 MWI
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: MER achievement for TX for data pack prep work
## DATE:    2020-01-29

library(tidyverse)

df_mwi <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Malawi")

df_export <- df_mwi %>% 
  filter(indicator %in% c("TX_NEW", "TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year %in% c(2019, 2020)) %>% 
  mutate(cumulative = ifelse(indicator == "TX_NEW", qtr4, cumulative)) %>% 
  group_by(psnu, psnuuid, fiscal_year, indicator, ageasentered, sex) %>% 
  summarise_at(vars(targets, cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(type, val, targets, cumulative) %>% 
  filter(!(fiscal_year == 2019 & type == "targets")) %>% 
  mutate(type = ifelse(indicator == "TX_NEW" & type == "cumulative", "qtr4", type)) %>% 
  unite(ind, c(fiscal_year, indicator, type), sep = "_") %>%
  mutate(ind = paste0("FY", ind)) %>% 
  filter(val > 0) %>% 
  spread(ind, val)


write_csv(df_export, "~/COP20/MWI_MSD_TX_v2.csv", na = "")
