## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Trends in Peds Tx by partner
## DATE:    2020-02-17
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)
library(readxl)
library(ICPIutilities)
library(nationalparkcolors)
library(patchwork)

# IMPORT ------------------------------------------------------------------

path <-"data/PEPFAR-Data-Genie-OUByIMs-2020-02-17.zip"

df_genie <- path %>% 
  read_msd(remove_txt = FALSE) %>% 
  filter(fiscal_year == 2020)

df_msd <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

df_tza <- bind_rows(df_msd, df_genie)


df_tx <- df_tza %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         ((fiscal_year == 2017 & standardizeddisaggregate == "MostCompleteAgeDisagg") |
            (fiscal_year >= 2018 & standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"))),
         ageasentered %in% c("<01", "01-09", "01-04", "05-09", "10-14", "<15"))

df_tx <- df_tx %>% 
  mutate(primepartner = case_when(str_detect(primepartner, "Amref|AMREF") ~ "Amref",
                                  primepartner == "ARIEL GLASER PEDIATRIC AIDS H EALTHCARE INITIATIVE" ~ "APAHI",
                                  str_detect(primepartner, "Baylor|BAYLOR") ~ "Baylor",
                                  primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                  str_detect(primepartner, "Elizabeth") ~ "EGPAF",
                                  str_detect(primepartner, "Henry") ~ "Henry Jackson",
                                  str_detect(primepartner, "JSI") ~ "JSI",
                                  str_detect(primepartner, "MANAGEMENT") ~ "MDH",
                                  str_detect(primepartner, "THPS") ~ "THPS",
                                  str_detect(primepartner, "Pharmaccess") ~ "Pharmaccess")) %>% 
  group_by(primepartner, fiscal_year, indicator) %>%
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  arrange(primepartner, indicator, period)


order <- df_tx %>% 
  filter(indicator == "TX_CURR", 
         period == "FY20Q1") %>% 
  arrange(desc(val)) %>% 
  pull(primepartner) %>% 
  c(., "Pharmaccess", "THPS")

df_tx <- df_tx %>% 
  mutate(primepartner = factor(primepartner, order))
  


v_txcurr <- df_tx %>%
  filter(indicator == "TX_CURR") %>% 
  ggplot(aes(period, val)) + 
  geom_col(fill =  "#26456a") +
  facet_grid(primepartner ~ indicator, switch = "y") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "",
                              "FY20Q1")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT"),
        strip.placement = "outside")

v_txnew <- df_tx %>%
  filter(indicator == "TX_NEW") %>% 
  ggplot(aes(period, val)) + 
  geom_col(fill =  "#739bcc") +
  facet_grid(primepartner ~ indicator, switch = "y") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "",
                              "FY20Q1")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT"),
        #strip.placement = "outside"
        strip.text.y = element_blank())

(v_txcurr | v_txnew)  
