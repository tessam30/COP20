## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Trends in Peds Cascade
## DATE:    2020-02-03
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(readxl)
  library(ICPIutilities)
  library(nationalparkcolors)

# IMPORT ------------------------------------------------------------------

  df_tza <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------
  cascade <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")
  
  df_viz <- df_tza %>% 
    filter(indicator %in% cascade,
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
           trendscoarse == "<15") %>% 
    group_by(indicator, trendscoarse, fiscal_year) %>%
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    arrange(indicator, trendscoarse, period) %>% 
    mutate(indicator = factor(indicator, cascade),
           fy = str_sub(period, 3,4)) %>% 
    group_by(indicator) %>% 
    mutate(extra_space = max(val, na.rm = TRUE)) %>% 
    ungroup()
           

  pal <- park_palette("SmokyMountains")

  df_viz %>% 
    ggplot(aes(period, val, group = fy, color = indicator)) +
    geom_blank(aes(y = extra_space * 1.2)) +
    geom_hline(yintercept = 0, color = "gray30") +
    geom_point(size = 6) +
    geom_path(size = 1) +
    scale_y_continuous(labels = comma, expand = c(0.005, 0.005), position = "right") +
    scale_color_manual(values = pal) +
    facet_grid(indicator ~ ., scales = "free_y", switch = "y") +
    labs(x = NULL, y = NULL,
         title = "PEDIATRIC CLINICAL CASCADE TRENDS",
         subtitle = "FY17-19 | Tanzania",
         caption = "Source: FY19Q4c MSD") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Calibri Light", size = 12),
          strip.text = element_text(family = "Calibri", size = 12, face = "bold"),
          plot.title = element_text(family = "Calibri", size = 18, face = "bold"),
          plot.subtitle = element_text(family = "Calibri", size = 14)
          )
  
  ggsave("out/plots/TZA_cascade_peds.png",
         dpi = 300, width = 10, height = 5.66)
  