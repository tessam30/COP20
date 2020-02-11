## PROJECT:  COP20 TZA
## AUTHOR:   A.CHAFETZ | USAID
## LICENSE:  MIT
## PURPOSE:  mapping PLHIV burden/unmet need
## NOTE:     relies on output from TZA_TxTargeting
## DATE:     2020-02-11
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(sf)
  library(RColorBrewer)
  library(extrafont)
  library(patchwork)


# IMPORT ------------------------------------------------------------------

  shp_tza_snu <- st_read("shp/Tanzania_PROD_4_Region_RegionLsib_2019_May.shp") %>% 
    mutate(snu1 = as.character(orgunit_na),
           snu1uid = as.character(orgunit_in)) %>% 
    select(snu1uid, geometry)

  shp_tza_psnu <- st_read("shp/Tanzania_PROD_5_District_DistrictLsib_2019_Mar.shp") %>% 
    mutate(psnu = as.character(name),
           psnuuid = as.character(uid)) %>% 
    select(psnuuid, geometry)

  df_tza_plhiv <- read_csv("out/data/TZA_Tx_targeting_info.csv")
  
# Create map theme --------------------------------------------------------
  
  map_theme <- function() {
    theme(text = element_text(family = "Calibri Light", size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          #panel.border = element_blank(),
          # panel.border = element_blank(),
          panel.grid.major = element_line(colour="white"),
          panel.grid.minor = element_line(colour="white"),
          panel.background = element_blank(),
          #legend.position = "bottom", 
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(family = "Calibri", face = "bold")
          
    )
  }
  

# Munge -------------------------------------------------------------------
  
  #remove mil
    df_tza_plhiv <- filter(df_tza_plhiv, snu1!= "_Military Tanzania")
  
  #create unmet need & coverage
    df_tza_plhiv <- df_tza_plhiv %>% 
      mutate(unmetneed = (.95*.95*plhiv_fy20) - tx_curr_fy19,
             coverage = tx_curr_fy19/plhiv_fy20)
    
  #join shapefile to data & create quantiles for mappping
    sf_tza_plhiv <- left_join(df_tza_plhiv, shp_tza_psnu, by = "psnuuid") %>% 
      mutate(plhiv_quantile = ntile(.$plhiv_fy20, 5),
             unmet_quantile = ntile(.$unmetneed, 5),
             cov_quantile = ntile(.$coverage,5))
    
# Function for creating the legend ----------------------------------------
    
    ntile_legend <- function(col, n = 5){
      col %>% 
        quantile(., seq(0, 1, 1/n), na.rm = TRUE) %>% 
        round(digits = 0) %>% 
        tibble(breaks = .) %>% 
        mutate(breaks_formatted = scales::comma(breaks),
               quantile = 1:n(),
               legend_lab = paste(breaks_formatted, "-", lead(breaks_formatted))) %>%
        filter(breaks != max(breaks)) %>% 
        pull(legend_lab)
    }
  
    ntile_legend_pct <- function(col, n = 5){
      col %>% 
        quantile(., seq(0, 1, 1/n), na.rm = TRUE) %>% 
        round(digits = 3) %>% 
        tibble(breaks = .) %>% 
        mutate(breaks_formatted = scales::percent(breaks, 1),
               quantile = 1:n(),
               legend_lab = paste(breaks_formatted, "-", lead(breaks_formatted))) %>%
        filter(breaks != max(breaks)) %>% 
        pull(legend_lab)
    }
    

# MAP ---------------------------------------------------------------------

    
    lgnd_plhiv <- ntile_legend(sf_tza_plhiv$plhiv_fy20)
    lgnd_unmet <- ntile_legend(sf_tza_plhiv$unmetneed)
    lgnd_cov <- ntile_legend_pct(sf_tza_plhiv$coverage)
    
    palette <- brewer.pal("GnBu", n = 5)
    palette_priorizations <- brewer.pal("Accent", n = 5)
    
    v_plhiv <- sf_tza_plhiv %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = factor(plhiv_quantile)), color = "gray50", size = .3, na.rm = TRUE) +
      geom_sf(data = shp_tza_snu, aes(geometry = geometry), color = "gray30", size = .5, fill = NA, na.rm = TRUE) +
      scale_fill_manual(values = palette, 
                        labels = lgnd_plhiv,
                        na.value = "gray60",
                        name = "PLHIV") +
      map_theme() +
      labs(title = "2020 PLHIV CONCENTRATED: NORTH AND CENTRAL",
           caption = "Source: Spectrum [COP20 Data Pack]\nFY19Q4c MSD") +
      guides(colour = guide_legend(nrow = 1))
    
    ggsave("out/plots/TZA_TxPlanning_PLHIV.png", dpi = 300,
           width = 8, height = 5.66)
    
    v_unmet <- sf_tza_plhiv %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = factor(unmet_quantile)), color = "gray50", size = .3, na.rm = TRUE) +
      geom_sf(data = shp_tza_snu, aes(geometry = geometry), color = "gray30", size = .5, fill = NA, na.rm = TRUE) +
      scale_fill_manual(values = palette, 
                        labels = lgnd_unmet,
                        na.value = "gray60",
                        name = "Unmet need") +
      map_theme() +
      labs(title = "UNMET NEED TO REACH 95-95", #TREATMENT COVERAGE TARGETS
           caption = "Source: Spectrum [COP20 Data Pack]\nFY19Q4c MSD") +
      guides(colour = guide_legend(nrow = 1))
    
    ggsave("out/plots/TZA_TxPlanning_UnmetNeed.png", dpi = 300,
           width = 8, height = 5.66)
    
    
    v_cov <- sf_tza_plhiv %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = factor(cov_quantile)), color = "gray50", size = .3, na.rm = TRUE) +
      geom_sf(data = shp_tza_snu, aes(geometry = geometry), color = "gray30", size = .5, fill = NA, na.rm = TRUE) +
      scale_fill_manual(values = palette, 
                        labels = lgnd_cov,
                        na.value = "gray60",
                        name = "Tx Coverage of PLHIV") +
      map_theme() +
      labs(title = "TREATMENT COVERAGE OF PLHIV",
           caption = "Source: Spectrum [COP20 Data Pack]\nFY19Q4c MSD") +
      guides(colour = guide_legend(nrow = 1))
    
    ggsave("out/plots/TZA_TxPlanning_PLHIVCoverage.png", dpi = 300,
           width = 8, height = 5.66)
    
    v_prior <- sf_tza_plhiv %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = snuprioritization), color = "gray50",size = .3,  na.rm = TRUE) +
      geom_sf(data = shp_tza_snu, aes(geometry = geometry), color = "gray30", size = .5, fill = NA, na.rm = TRUE) +
      scale_fill_manual(values = c(palette[4], palette[2], palette[1]),
                        name = "FY19 Prioritizations") +
      map_theme() +
      labs(title = "COUNCIL PRIORITIZATIONS",
           caption = "Source: FY19Q4c MSD") +
      guides(colour = guide_legend(nrow = 1))
    
    ggsave("out/plots/TZA_TxPlanning_Prioritizations.png", dpi = 300,
           width = 8, height = 5.66)
  
    v_all <- (v_prior + v_plhiv) / (v_unmet + v_cov)
    
    ggsave("out/plots/TZA_TxPlanning_Maps.png", plot = v_all, dpi = 300,
           width = 8, height = 5.66)
    
    