## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Trends in Comm Index and Index Modalities
## DATE:    2020-02-07
## UPDATED: 2020-02-24

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(readxl)
  library(ICPIutilities)
  library(nationalparkcolors)
  library(patchwork)

# IMPORT ------------------------------------------------------------------

df_msd <- list.files("~/Data", "OU_IM", full.names = TRUE)[2] %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

  #filter to HTS modalities disagg
    df_hts <- df_msd %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result", 
                                             "Modality/Age/Sex/Result"))
  #aggregate, grouping by modality type
    df_hts <- df_hts %>% 
      mutate(mod_type = case_when(str_detect(modality, "Mod") ~ "Community",
                             TRUE ~ "Facility")) %>% 
      group_by(indicator, mod_type, fiscal_year) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      arrange(mod_type, indicator, period)
  
  #add positivity
    df_hts <- df_hts %>% 
      spread(indicator, val) %>% 
      mutate(Positivity = HTS_TST_POS/HTS_TST) %>% 
      group_by(period) %>% 
      mutate(`Share of All Positives` = HTS_TST_POS/sum(HTS_TST_POS, na.rm = TRUE)) %>% 
      ungroup() 

  #viz modifications
    df_viz <- df_hts %>% 
      mutate(fy = str_extract(period, "FY[:digit:]{2}")) %>% 
      gather(ind, val, HTS_TST:`Share of All Positives`) %>% 
      mutate(q1_lab = case_when(str_detect(period, "Q1") & ind == "HTS_TST_POS" ~ comma(val, 1),
                                str_detect(period, "Q1") & ind == "Positivity" ~ percent(val, .1),
                                str_detect(period, "Q1") & ind == "Share of All Positives" ~ percent(val, 1)),
             point = case_when(period %in% c("FY17Q1", "FY20Q1") ~ val))
      


# BASIC PLOT FUNCTION -----------------------------------------------------

    plot_elements <- function(df){
      df %>% 
        ggplot(aes(period, val, group = mod_type, color = mod_type, fill = mod_type)) +
        geom_hline(yintercept = 0) +
        geom_blank(aes(y = val *1.2)) +
        geom_text(aes(label = q1_lab), na.rm = TRUE, vjust = -1,
                  family = "Calibri Light", size = 3, color = "gray30") +
        facet_grid(ind ~ mod_type, switch = "y") +
        labs(x = NULL, y = NULL) +
        scale_color_manual(values = c(pal[4], pal[6], pal[3])) +
        scale_fill_manual(values = c(pal[4], pal[6], pal[3])) +
        scale_x_discrete(labels = c("FY18", "", "", "",
                                    "FY19", "", "", "",
                                    "FY20")) +
        theme(axis.text.y = element_blank(),
              legend.position = "none")
    }    
    
# PLOTS -------------------------------------------------------------------

    pal <- park_palette("SmokyMountains")
    
    theme_set(theme_minimal(base_size = 11, base_family = "Calibri"))
    
    #hts_pos
      v_pos <- df_viz %>%
        filter(ind == "HTS_TST_POS") %>% 
        plot_elements() +
        geom_col() +
        theme(axis.text.x = element_blank())
        
   #positivity
    v_yield <- df_viz %>%
      filter(ind == "Positivity") %>% 
      plot_elements() +
      geom_path(size = 1) +
      geom_point(aes(y = point), size = 3, na.rm = TRUE) +
      theme(axis.text.x = element_blank(),
            strip.text.x = element_blank())
      
    #share of tests
      v_share <- df_viz %>%
        filter(ind == "Share of All Positives") %>% 
        plot_elements() +
        geom_area() +
        theme( strip.text.x = element_blank())

# COMBINE PLOTS -----------------------------------------------------------

 p <- (v_pos / v_yield / v_share) + 
        plot_annotation(title = 'Nearly 1 Community Test for Every 3 in the Facility in FY20Q1',
                        subtitle = "FY18-20 Tanzania",
                        caption = "Source: FY19Q4c MSD + FY20Q1 Genie Pull [2020-02-17]",
                        theme = theme(title = element_text(family = "Calibri", size = 13),
                                      plot.subtitle = element_text(family = "Calibri", size = 11),
                                      plot.caption = element_text(family = "Calibri Light", color = "gray30",
                                                                  size = 9)))

# EXPORT ------------------------------------------------------------------

   ggsave("out/plots/TZA_CommTestingTrends.png", plot = p, dpi = 300,
          width = 10, height = 5.66)      
 