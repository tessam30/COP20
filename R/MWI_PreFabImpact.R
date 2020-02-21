## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  Compare growth rates of sites pre/post construction
## DATE:     2020-01-28
## UPDATED:  2020-02-21


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(ICPIutilities)
  library(scales)
  library(extrafont)


# IMPORT DATA -------------------------------------------------------------

  ##data sets provided by USAID/Malawi
  
  #filepaths
    path_prefab <- "data/Malawi Pre-fab clinic and pharmacy inventory_updated 4_17_2019.xlsx"
    path_dha <- "data/MoH_TX_CURR_FY18Q1 to FY20Q1.xls"

  #import info on the site that receieved prefabs and when
    df_prefab <- read_excel(path_prefab, range = "A3:J58")

  #import DHA TX data (issues with DATIM reporting for MWI in FY19)
    df_dha <- read_excel(path_dha)  
    
  rm(path_prefab, path_dha)
  
# MUNGE -------------------------------------------------------------------

  ##prefab clean up   
  
  #clean up names
    df_prefab <- clean_names(df_prefab)
    
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


   ##DHA clean up
  
   #fix var names
    df_dha <- df_dha %>% 
      rename_all(tolower) %>% 
      rename(orgunituid = orgunit,
             psnu = district) %>% 
      mutate(psnu = str_remove(psnu, " District"))
    
   #reshape
    df_dha <- df_dha %>% 
      gather(quarter, tx_curr, starts_with("fy")) %>% 
      mutate(quarter = str_replace(quarter, "fy", "20") %>% str_replace("q", "\\.") %>% as.numeric)

# JOIN --------------------------------------------------------------------

  ##growth 2Q's before/after
    
  #join DHA data + prefab info
    df_join_onlyprefab <- df_prefab %>% 
      left_join(df_dha) %>% 
      arrange(psnu, sitename, type, qtr_dist)
  
  #calc growth for pre/post prefab
    df_join_onlyprefab <- df_join_onlyprefab %>% 
      group_by(sitename, type) %>% 
      mutate(growth = (tx_curr - lag(tx_curr))/lag(tx_curr)) %>% 
      ungroup() %>% 
      mutate(type = fct_rev(type))

  #growth 1 year before and after
  #identify FY19Q1 prefab sites
    lst_prefab_19q1 <- df_prefab %>% 
      distinct(psnu, sitename, completion_date) %>% 
      mutate(qtr_complete = quarter(completion_date, with_year = TRUE, fiscal_start = 10)) %>% 
      filter(qtr_complete == "2019.1") %>% 
      pull(sitename)
    
    df_dha_yoy <- df_dha %>% 
      filter(quarter %in% c(2018.1, 2019.1, 2020.1)) %>% 
      mutate(prefab_19q1 = ifelse(sitename %in% lst_prefab_19q1, "FY19Q1 Pre-Fab Sites", "Normal Sites")) %>% 
      arrange(psnu, sitename, quarter)
    
  #calc growth for pre/post prefab
    df_dha_yoy <- df_dha_yoy %>% 
      group_by(sitename) %>% 
      mutate(growth = (tx_curr - lag(tx_curr))/lag(tx_curr),
             growth = na_if(growth, Inf)) %>% 
      ungroup() %>% 
      filter(quarter != 2018.1) %>% 
      mutate(type = case_when(quarter == 2019.1 ~ "FY18-19",
                              quarter == 2020.1 ~ "FY19-20"))
# PLOT --------------------------------------------------------------------

    
  #look at only pre fab sites
  set.seed(42)
  
  df_join_onlyprefab %>% 
    ggplot(aes(type, growth)) +
    geom_hline(yintercept = 0, color = "gray30") +
    geom_boxplot(na.rm = TRUE, color = 'gray50', outlier.alpha = 0) + 
    geom_jitter(aes(color = type, alpha = .2), size = 2.5, width = 0.25, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_x_discrete(position = "top", labels = c("PRIOR", "POST")) +
    scale_color_manual(values = c("#95a5a6", "#2980b9")) +
    labs(x = NULL, y = "TX_CURR growth rate",
         title = "Positive impact of FY19 Pre-Fabs",
         subtitle = "higher avg TX_CURR growth in sites \n2 qtrs post pre-fab install",
         caption = "MoH TX_CURR FY18Q1-FY20Q1 Malawi + Pre-fab clinic inventory 2019-04-17") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT"),
          axis.text.x = element_text(face = "bold"),
          plot.caption = element_text(size = 8, color = "gray30"))

 ggsave("out/plots/MWI_PreFabImpact_v2.png", dpi = 330,
        width = 4, height = 5.625)
 
 write_csv(df_join_onlyprefab, "out/data/MWI_PreFabImpact_data.csv", na = "")
 
 
 
 
 #look at yoy change
 set.seed(42)
 
 df_dha_yoy %>% 
   ggplot(aes(type, growth)) +
   geom_hline(yintercept = 0, color = "gray30") +
   geom_boxplot(na.rm = TRUE, color = 'gray50', outlier.alpha = 0) + 
   geom_jitter(aes(color = type, alpha = .1), size = 2.5, width = 0.25, na.rm = TRUE) +
   facet_grid(. ~ prefab_19q1) +
   scale_y_continuous(labels = percent_format(1),limits = c(-1, 1.5)
   ) +
   scale_x_discrete(position = "top") +
   scale_color_manual(values = c("#95a5a6", "#2980b9")) +
   labs(x = NULL, y = "TX_CURR growth rate",
        title = "Limited impact of FY19Q1 Pre-Fabs",
        subtitle = "lower avg TX_CURR growth in sites post pre-fab install",
        caption = "MoH TX_CURR FY18Q1-FY20Q1 \nMalawi Pre-fab clinic inventory 2019-04-17") +
   theme_minimal() +
   theme(legend.position = "none",
         text = element_text(family = "Gill Sans MT"),
         axis.text.x = element_text(face = "bold"),
         plot.caption = element_text(size = 8, color = "gray30"),
         strip.placement = "outside",
         strip.text = element_text(face = "bold"))
 
 ggsave("out/plots/MWI_PreFabImpact_yoy.png", dpi = 330,
        width = 6, height = 5.625)

# Analysis II -------------------------------------------------------------

  
  
  
   