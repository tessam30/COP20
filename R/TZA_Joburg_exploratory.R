## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: exploratory viz for Joburg deck
## DATE:    2020-02-17
## UPDATED: 


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(patchwork)


# IMPORT ------------------------------------------------------------------
  
  path <-"data/PEPFAR-Data-Genie-OUByIMs-2020-02-17.zip"
  
  df_genie <- path %>% 
    read_msd(remove_txt = FALSE) %>% 
    filter(fiscal_year == 2020)
  
  df_msd <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

  df_tza <- bind_rows(df_msd, df_genie)
  
  df_tx <- df_tza %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year > 2017) %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    arrange(indicator, period) %>% 
    mutate(lab = case_when(indicator == "TX_CURR" ~ paste0(round(val/1000000, 2), "m"),
                           TRUE ~ paste0(round(val/1000, 1), "k")))
  
  
  df_curr_snu1 <- df_tza %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           snu1 %in% c("Mwanza", "Kagara"),
           fiscal_year > 2017) %>% 
    group_by(indicator, snu1, fiscal_year) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    arrange(indicator, period) %>% 
    mutate(lab = case_when(indicator == "TX_CURR" ~ paste0(round(val/1000000, 2), "m"),
                           TRUE ~ paste0(round(val/1000, 1), "k")))

  
  
  df_men <- df_tza %>% 
    filter(indicator %in% c("TX_CURR", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age Aggregated/Sex/Result",
                                           "Modality/Age/Sex/Result"),
           sex == "Male",
           trendscoarse == "15+",
           fiscal_year > 2017) %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    arrange(indicator, period) %>% 
    mutate(lab = paste0(round(val/1000, 1), "k"))
  
  
  
  df_cxca <- df_msd %>% 
    filter(indicator %in% c("CXCA_SCRN", "CXCA_SCRN_POS"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(indicator, fiscal_year) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    arrange(indicator, period) %>% 
    mutate(lab = paste0(round(val/1000, 1), "k"))
  
  df_tza %>% 
    filter(indicator %in% c("TX_CURR", "HTS_TST_POS"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age Aggregated/Sex/Result",
                                           "Modality/Age/Sex/Result"),
           fiscal_year > 2017) %>% 
    count(ageasentered, trendscoarse, fiscal_year, wt = cumulative) %>% 
    spread(fiscal_year, n)
  
  
  
  
# PLOT --------------------------------------------------------------------

  
  theme_set(theme_minimal(base_family = "Gill Sans MT")) 
  
  pal <- add_color("woods")

#Tx_NEW
  df_tx %>% 
    filter(indicator == "TX_NEW") %>% 
    ggplot(aes(period, val)) +
    geom_col(fill = pal[1]) +
    geom_text(aes(label = lab),
              family = "Calibri Light", color = "gray30", size = 4,
              vjust = -1) +
    geom_hline(yintercept = 0) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                "FY19Q1", "", "FY19Q3", "",
                                "FY20Q1")) +
    facet_wrap(indicator ~ ., scales = "free_y") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "none")  
  
  
  ggsave("out/plots/TZA_TX_NEW_trends.png",
         height = 5.63, width = 10, dpi = 300)
  
  
#TX_CURR
  df_tx %>% 
    filter(indicator == "TX_CURR") %>% 
    ggplot(aes(period, val)) +
    geom_col(fill = pal[1]) +
    geom_text(aes(label = lab),
              family = "Calibri Light", color = "gray30", size = 4,
              vjust = -1) +
    geom_hline(yintercept = 0) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                "FY19Q1", "", "FY19Q3", "",
                                "FY20Q1")) +
    facet_wrap(indicator ~ ., scales = "free_y") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "none")  
  
  
  ggsave("out/plots/TZA_TX_CURR_trends.png",
         height = 5.63, width = 10, dpi = 300)
  
#TX_CURR + NETNEW
  
  df_tx %>% 
    filter(indicator %in% c("TX_NET_NEW", "TX_CURR")) %>% 
    ggplot(aes(period, val)) +
    geom_blank(aes(y = val * 1.2)) +
    geom_col(fill = pal[1]) +
    geom_text(aes(label = lab, vjust = ifelse(val <0, 1, -1)),
              family = "Calibri Light", color = "gray30", size = 4) +
    geom_hline(yintercept = 0) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                "FY19Q1", "", "FY19Q3", "",
                                "FY20Q1")) +
    facet_grid(indicator ~ .,  switch = "y") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "none") 
  
  ggsave("out/plots/TZA_TX_NET_NEW_trends.png",
         height = 5.63, width = 10, dpi = 300)

  
#case identification + TX_CURR men
  df_men %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_CURR")) %>% 
    ggplot(aes(period, val)) +
    geom_blank(aes(y = val * 1.2)) +
    geom_col(fill = pal[1]) +
    geom_text(aes(label = lab, vjust = ifelse(val <0, 1, -1)),
              family = "Calibri Light", color = "gray30", size = 4) +
    geom_hline(yintercept = 0) +
    labs(x = NULL, y = NULL, subtitle = "Males 15+") +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                "FY19Q1", "", "FY19Q3", "",
                                "FY20Q1")) +
    facet_grid(indicator ~ .,  #scales = "free_y", 
               switch = "y") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "none") 
  
  ggsave("out/plots/TZA_caseid_trends.png",
         height = 5.63, width = 10, dpi = 300)
  
  
  
#cervical cancer screen
  
  df_cxca %>% 
    ggplot(aes(period, val)) +
    geom_blank(aes(y = val * 1.2)) +
    geom_col(fill = pal[1]) +
    geom_text(aes(label = lab, vjust = ifelse(val <0, 1, -1)),
              family = "Calibri Light", color = "gray30", size = 4) +
    geom_hline(yintercept = 0) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = comma) +
    facet_grid(indicator ~ .,  #scales = "free_y", 
               switch = "y") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "none")
  
  ggsave("out/plots/TZA_CXCA_SCRN.png",
         height = 5.63, width = 10, dpi = 300)
  