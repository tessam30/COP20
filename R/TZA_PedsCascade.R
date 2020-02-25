## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Trends in Peds Cascade
## DATE:    2020-02-03
## UPDATED: 2020-02-17

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(readxl)
  library(ICPIutilities)
  library(nationalparkcolors)


# GLOBAL VARIABLES --------------------------------------------------------

  cascade <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")
  pal <- park_palette("SmokyMountains")


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
  
  df_viz <- df_tza %>% 
    filter(indicator %in% cascade,
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
           trendscoarse == "<15") %>% 
    mutate(partner = case_when(str_detect(primepartner, "DELOITTE") ~ "Deloitte",
                               str_detect(primepartner, "Elizabeth Glaser") ~ "EGPAF"))
  
  

# PLOT FUNCTION -----------------------------------------------------------

    plot_ind <- function(df, partner_sel = NULL, snu_sel = NULL){
        
      subt <- "FY17-20 | Tanzania"
      save <- paste0("TZA_Peds_CascadeInd", partner_sel, snu_sel, ".png")
      
      if(!is.null(partner_sel)) {
        df <- filter(df, partner == partner_sel)
        subt <- paste(subt, "|", partner_sel)
      }
      
      if(!is.null(snu_sel)){
        df <- filter(df, snu1 == snu_sel)
        subt <- str_replace(subt, "Tanzania", snu_sel)
      }
      
      
      
      df <- df %>% 
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
      
      
      v <- df %>% 
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
             subtitle = subt,
             caption = "Source: DATIM Genie Pull [2020-02-17]") +
        theme_minimal() +
        theme(legend.position = "none",
              text = element_text(family = "Calibri Light", size = 12),
              strip.text = element_text(family = "Calibri", size = 12, face = "bold"),
              plot.title = element_text(family = "Calibri", size = 18, face = "bold"),
              plot.subtitle = element_text(family = "Calibri", size = 14)
        )
      
      ggsave(file.path("out", "plots", save),
             dpi = 300, width = 10, height = 5.66)
      
      return(v)
    }  

  #peds nationally  
    plot_ind(df_viz)
    
  #peds for Deloitte & EGPAF
    walk(.x = c("Deloitte", "EGPAF"), 
         .f = ~ plot_ind(df_viz, partner_sel = .x))
    
  #key regions
    walk(.x = c("Arusha", "Dodoma", "Singida", "Iringa", "Morogoro", "Njombe"),
         .f = ~ plot_ind(df_viz, snu_sel = .x))

  