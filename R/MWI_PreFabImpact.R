## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  Compare growth rates of sites pre/post construction
## DATE:     2020-01-28

#dependencies
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(ICPIutilities)
  library(scales)
  library(extrafont)

#import
  path <- "~/COP20/Malawi Pre-fab clinic and pharmacy inventory_updated 4_17_2019.xlsx"
  df_prefab <- read_excel(path, range = "A3:J58")

#clean up names
  df_prefab <- janitor::clean_names(df_prefab)
  
  df_prefab <- df_prefab %>% 
    mutate(health_facility = case_when(health_facility == "Chileka Health Centre Lilongwe Static Art Static Art"	~ "Chileka Health Centre Lilongwe Static Art",
                                       health_facility == "Daeyang Luke Hospital Public" ~ "Daeyang Luke Hospital",
                                       health_facility == "Zomba City Clinic" ~ "City Clinic Zomba",
                                       health_facility == "Chilipa Health Centre"	~ "Chilipa Health Centre Zomba",
                                       health_facility == "Ngwelelo Health Centre" ~ 	"Ngwelero Health Centre",
                                       TRUE ~ health_facility))

#identify FY quarter
  df_prefab <- df_prefab %>% 
    select(psnu = district, sitename = health_facility, completion_date) %>% 
    mutate(qtr_prior_2 = completion_date %m-% months(6) %>% quarter(with_year = TRUE, fiscal_start = 10),
           qtr_prior_1 = completion_date %m-% months(3) %>% quarter(with_year = TRUE, fiscal_start = 10),
           qtr_post_1 = completion_date %m+% months(3) %>% quarter(with_year = TRUE, fiscal_start = 10),
           qtr_post_2 = completion_date %m+% months(6) %>% quarter(with_year = TRUE, fiscal_start = 10)) %>% 
    gather(period, quarter, starts_with("qtr")) %>% 
    separate(period, c(NA, "type", "qtr_dist"), sep = "_")


#import GENIE (TX_CURR Site x IM )
  path_site <- "~/COP20/PEPFAR-Data-Genie-SiteByIMs-2020-01-28.zip"
  df_msd_site <- read_msd(path_site, save_rds = FALSE, remove_txt = FALSE)   
  
  
  df_msd_site <- df_msd_site %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>% 
    #mutate(psnu = str_remove(psnu, " District")) %>% 
    group_by(fiscal_year, operatingunit, orgunituid, sitename) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd("long") %>% 
    filter(str_detect(period, "targets|cumulative", negate = TRUE)) %>% 
    mutate(period = str_remove(period, "fy") %>% str_replace("q", ".") %>% as.double) %>% 
    rename(quarter = period,
           tx_curr = val)
  
#join
  df_join <- df_prefab %>% 
    left_join(df_msd_site) %>% 
    arrange(psnu, sitename, type, qtr_dist)
  
  df_join <- df_join %>% 
    group_by(sitename, type) %>% 
    mutate(growth = (tx_curr - lag(tx_curr))/lag(tx_curr)) %>% 
    ungroup() %>% 
    mutate(type = fct_rev(type))

  df_join %>% 
    ggplot(aes(type, growth)) +
    geom_hline(yintercept = 0, color = "gray30") +
    geom_boxplot(na.rm = TRUE, color = 'gray50', outlier.size = 0) + 
    geom_jitter(aes(color = type, alpha = .2), size = 2.5, width = 0.25, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(position = "top", labels = c("PRIOR", "POST")) +
    scale_color_manual(values = c("#95a5a6", "#2980b9")) +
    labs(x = NULL, y = "TX_CURR growth rate",
         title = "Positive impact of Pre-Fabs | FY19",
         subtitle = "higher avg TX_CURR growth in the 2 qtrs post pre-fab",
         caption = "DATIM Genie 2020-01-28
         Malawi Pre-fab clinic inventory 2019-04-17") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT"),
          axis.text.x = element_text(face = "bold"),
          plot.caption = element_text(size = 8, color = "gray30"))

 ggsave("~/COP20/MWI_PreFabImpact.png", dpi = 330,
        width = 4, height = 5.625)
 
 write_csv(df_join, "~/COP20/MWI_PreFabImpact_data.csv", na = "")
 