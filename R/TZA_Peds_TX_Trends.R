## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: visualize trends in TX for peds
## NOTE:    adapted from FY19AgencySelfAssessments/FY19Q4_TZA_TX_Comparison.R
## DATE:    2020-02-09
## UPDATED: 


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(patchwork)


# IMPORT ------------------------------------------------------------------


df_tza <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")


# MUNGE -------------------------------------------------------------------

  #filter for partners and peds
    df_tx <- df_tza %>% 
      mutate(partner = case_when(str_detect(primepartner, "DELOITTE") ~ "Deloitte",
                                 str_detect(primepartner, "Elizabeth Glaser") ~ "EGPAF")) %>% 
      filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
             fundingagency == "USAID",
             !is.na(partner),
             fiscal_year > 2017,
             trendscoarse == "<15")
    
  #aggregate and reshape
    df_tx <- df_tx %>% 
      group_by(indicator, partner, fiscal_year) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      arrange(partner, indicator, period) 

  #add elements for plotting
    df_viz <- df_tx %>% 
      mutate(pd_fill = case_when(period == "FY19Q4" ~ "z_now",
                                 str_detect(period, "19") ~ "y_rest of fy19",
                                 TRUE ~ "x_earlier"),
             indicator = paste(indicator, "<15"))



# PLOT FUNCTION -----------------------------------------------------------


    plot_tx <- function(partner_sel){
      
      
      theme_set(theme_minimal(base_family = "Gill Sans MT"))
      
      v_curr <- df_viz %>% 
        filter(indicator == "TX_CURR <15",
               partner == partner_sel) %>% 
        ggplot(aes(period, val, fill = pd_fill)) +
        geom_col() +
        geom_text(aes(label = comma(val,1)),
                  family = "Calibri Light", color = "gray30", size = 3,
                  vjust = -1) +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values = c("#739bcc", "#335B8E", "#26456a")) +
        scale_y_continuous(labels = comma) +
        scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                                    "FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "")) +
        facet_wrap(indicator ~ ., scales = "free_y") +
        theme_light() +
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.text = element_text(size = 16),
              legend.position = "none")
      
      v_new <- df_viz %>% 
        filter(indicator %in% c("TX_NEW <15", "TX_NET_NEW <15"),
               partner == partner_sel) %>% 
        ggplot(aes(period, val, fill = pd_fill)) +
        geom_col() +
        geom_blank(aes(y = 1.2 * val)) +
        geom_text(aes(label = comma(val, 1),
                      vjust = ifelse(val <0, 1, -.8)), size = 3,
                  family = "Calibri Light", color = "gray30") +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values =  c("#739bcc", "#335B8E", "#26456a")) +
        scale_y_continuous(labels = comma)+
        scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                                    "FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "")) +
        facet_wrap(. ~ indicator, ncol = 1) +
        theme_light() +
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.text = element_text(size = 16),
              legend.position = "none")
      
      
      v_out <- (v_curr + v_new) + 
        plot_annotation(title = paste(partner_sel, 'Peds Treatment Trends'),
                        caption = "Source: FY19Q4c MSD",
                        theme = theme(title = element_text(size = 18, face = "bold"),
                                      plot.caption = element_text(color = "gray30", size = 10)))
      
      ggsave(paste0("out/plots/TZA_Peds_TxTrends_", partner_sel,".png"), plot = v_out,
             height = 5.63, width = 10,dpi = 300)
      
      return(v_out)
    }


# PLOT AND EXPLORT --------------------------------------------------------


  plot_tx("Deloitte")
  
  plot_tx("EGPAF")

