## PROJECT:  COP20 TZA
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  evaluate targeting trajectory
## NOTE:     adapted from work in Malawi (MWI_TargetTrajectory)
## DATE:     2020-02-10
## UPDATED:  

#dependencies
  library(tidyverse)
  library(scales)
  library(extrafont)
  library(ICPIutilities)

#definte color palette
  pal <- add_color(palette = "woods")

#import tza TX data
  df_tza <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")

#TX agg
  df_tza_tx <- df_tza %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(indicator, fiscal_year) %>% 
    summarise_at(vars(targets, cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(type, value, targets, cumulative) %>% 
    mutate(value = na_if(value, 0))

#extract value for plotting projections from FY19 result
  df_tx_19 <- df_tza_tx %>% 
    filter(type == "cumulative",
           fiscal_year == 2019) %>% 
    mutate(type = "projected")

#projection of trajectory to meet FY20 target
  df_tx_20targ <- df_tza_tx %>% 
    filter((type == "targets" & fiscal_year == 2020) | 
           (type == "cumulative" & fiscal_year == 2019)) %>% 
    rename(target_gap = value) %>% 
    mutate(type = "projected")

#import Naomi model for FY20 PLHIV and TX_CURR_SUBNAT estimates
  df_naomi <- read_csv("data/naomi export_September 2020_fromDP.csv")

  df_naomi <- df_naomi %>% 
    filter(dataelement %in% c("IMPATT.PLHIV (SUBNAT, Age/Sex)", 
                              "TX_CURR_SUBNAT (N, SUBNAT, Age Aggregated/Sex): Receiving ART")) %>% 
    group_by(dataelement) %>% 
    summarise_at(vars(value), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(indicator = dataelement) %>% 
    mutate(indicator = case_when(indicator == "IMPATT.PLHIV (SUBNAT, Age/Sex)" ~ "PLHIV",
                                   TRUE ~ "TX_CURR_SUBNAT"),
           type = "projected") %>% 
    mutate(fiscal_year = 2020) 

#extract TX_CURR_SUBNAT for projection
  df_naomi_subnat <- df_naomi %>% 
    filter(indicator == "TX_CURR_SUBNAT") %>% 
    bind_rows(df_tx_19) %>% 
    rename(TX_CURR_SUBNAT = value)

#extract PLHIV for reference lines
  df_naomi_plhiv <- df_naomi %>% 
    spread(indicator, value) %>% 
    select(-TX_CURR_SUBNAT) %>% 
    mutate(indicator = "PLHIV",
           PLHIV_90pct = PLHIV * .95*.95,
           PLHIV_81pct = PLHIV * .9*.9)
  
  
  PLHIV_81pct_lab <- paste0(round(df_naomi_plhiv$PLHIV_81pct /1000000,2), "m (81% PLHIV on ART)")
  PLHIV_90pct_lab <- paste0(round(df_naomi_plhiv$PLHIV_90pct /1000000,2), "m (90% PLHIV on ART)")

#extract PLHIV gap for trajectory for reaching 2nd 95
  df_plhiv_gap <- df_naomi %>% 
    filter(indicator == "PLHIV") %>% 
    mutate(value = .95*.95 * value) %>% 
    bind_rows(df_tx_19) %>% 
    rename(PLHIV_gap = value)
  
#add FY21 to dataset (placeholder)
  df_fy21 <- tibble::tribble(
     ~fiscal_year, ~ type,
      2021, "projected"
    )

#bind data extracts together
  df_viz <- bind_rows(df_tza_tx, df_naomi_subnat, df_naomi_plhiv, df_plhiv_gap, df_tx_20targ, df_fy21)

#viz labeling
  df_viz <- df_viz %>% 
    mutate(lab = case_when(indicator == "TX_CURR" ~ paste0(round(value/1000000,2), "m"),
                           TX_CURR_SUBNAT > 0 & fiscal_year == 2020 ~ paste0(round(TX_CURR_SUBNAT /1000000,2), "m"),
                           PLHIV_gap > 0 & fiscal_year == 2020 ~ paste0(round(PLHIV_gap /1000000,2), "m")),
           lab = na_if(lab, "NAm"),
           lab2 = case_when(indicator == "TX_CURR_SUBNAT" ~ "Naomi projection",
                            indicator == "PLHIV" & PLHIV_gap > 0 ~ "90% coverage",
                            type == "targets" & fiscal_year == 2020 ~ "COP19 target"),
           lab2_x = 2020,
           lab2_y = case_when(indicator == "TX_CURR_SUBNAT" ~ TX_CURR_SUBNAT,
                              indicator == "PLHIV" & PLHIV_gap > 0 ~ PLHIV_gap,
                              type == "targets" & fiscal_year == 2020 ~ value))
#plot
  df_viz %>% 
    ggplot(aes(fiscal_year, value, group = type, color = type)) +
    geom_hline(aes(yintercept = PLHIV_90pct), linetype = "dashed", na.rm = TRUE, color = "gray50") +
    geom_hline(aes(yintercept = PLHIV_81pct), linetype = "dashed", na.rm = TRUE, color = "gray50") +
    geom_vline(aes(xintercept = 2021), na.rm = TRUE, color = "#cc5234", size = 2) +
    geom_path(aes(y = target_gap), size = .75, na.rm = TRUE, linetype = "dashed") +
    geom_path(aes(y = PLHIV_gap), size = .75, na.rm = TRUE, linetype = "dashed") +
    geom_point(aes(y = PLHIV_gap), size = 6, na.rm = TRUE) +
    geom_path(aes(y = TX_CURR_SUBNAT), size = .75, na.rm = TRUE, linetype = "dashed") +
    geom_point(aes(y = TX_CURR_SUBNAT), size = 6, na.rm = TRUE) +
    geom_path(size = 1.5, na.rm = TRUE) +
    geom_point(size = 6, na.rm = TRUE) +
    geom_text(aes(label = lab), na.rm = TRUE, vjust = -1.2, color = "gray30", family = "Calibri Light") +
    geom_text(aes(y = TX_CURR_SUBNAT, label = lab), na.rm = TRUE, vjust = -1.2, color = "gray30", family = "Calibri Light") +
    geom_text(aes(y = PLHIV_gap, label = lab), na.rm = TRUE, vjust = -1.2, color = "gray30", family = "Calibri Light") +
    geom_text(aes(x = 2017.4, y = PLHIV_90pct, label = PLHIV_90pct_lab), 
              na.rm = TRUE, vjust = -1, color = "gray50", family = "Calibri Light") +
    geom_text(aes(x = 2017.4, y = PLHIV_81pct, label = PLHIV_81pct_lab), 
              na.rm = TRUE, vjust = -1, color = "gray50", family = "Calibri Light") +
    geom_text(aes(x = lab2_x, y =lab2_y, label = lab2),
              na.rm = TRUE, hjust = -.1, vjust = 1.2, size = 4,
              color = "gray30", family = "Calibri Light") +
    geom_text(aes(x = 2017.5, y = 1142500, label = "COP targets"),
              na.rm = TRUE, hjust = 1, vjust = -1, size = 4,
              color = "#cc5234", family = "Calibri Light") +
    geom_text(aes(x = 2017.5, y = 1003500, label = "MER results"),
              na.rm = TRUE, hjust = 0, vjust = 1.5, size = 4,
              color = "#335b8e", family = "Calibri Light") +
    scale_y_continuous(labels = comma) + 
    scale_color_manual(name = NULL,
                       breaks = fct_rev(df_viz$type),
                       values = c("#335b8e", "#6ca18f", "#cc5234")) +
    labs(x = NULL, y = NULL, title = "COP20 TANZNIA | TREATMENT TARGETING",
         caption = "Sources: FY19Q4c MSD; Naomi 2020 Estimates 2020-02-07") +
    theme_minimal()+
    theme(text = element_text(family = "Calibri Light", size = 16),
          plot.title = element_text(family = "Calibri", face = "bold", size = 24, color = "#6ca18f"),
          plot.caption = element_text(color = "gray30"),
          legend.position = "none")
  
#export
  ggsave("out/plots/TZA_TX_targeting_v2.png", dpi = 300,
         height = 7.5, width = 13.33)
