## PROJECT:  COP20 MWI
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  Visualize changes to Spectrum PLHIV values between COP19 and COP20
## DATE:     2020-01-27
## UPDATED:  2020-02-02

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(scales)
  library(extrafont)



# FUNCTION TO IMPORT AND MUNGE PLHIV FROM DP ------------------------------


  import_impatt <- function(filepath, estimate_year){
    
    df <- read_excel(filepath, sheet = "Spectrum")
    
    if("psnu_id" %in% names(df))
      df <- rename(df, psnuuid = psnu_id)
    
    df <- df %>% 
      filter(dataelement == "IMPATT.PLHIV (SUBNAT, Age/Sex)") %>% 
      select(psnu, psnuuid, age = categoryOption_name_1, sex = categoryOption_name_2, value) %>% 
      mutate(psnuuid = str_remove_all(psnuuid, "\\[|]"),
             age = str_remove_all(age, '=|"'),
             psnu = str_remove(psnu, " District"),
             year = estimate_year,
             age = case_when(age == "<1" ~ "<01",
                             age == "1-4" ~ "01-04",
                             age == "5-9" ~ "05-09",
                             TRUE ~ age),
             age = fct_inorder(age) %>% fct_rev()) %>% 
      select(year, everything())
    
    return(df)
  
  }
  

# IMPORT DP DATA ----------------------------------------------------------

  #identify files
    files <- c("../Downloads/COP19 DataPack_Malawi_03182019 _Final.xlsx", 
               "../Downloads/Data Pack_Malawi_20200120225029 25012020.xlsx")
    
  #import adding the year
    df_plhiv <- map2_dfr(.x = files, 
                         .y = c("2018", "2019"),
                         .f = ~ import_impatt(.x, .y))
  

# MUNGING FOR GRAPH -------------------------------------------------------

  #add growth for labeling
    df_plhiv <- df_plhiv %>% 
      group_by(psnu, sex, age) %>% 
      mutate(growth = (value - lag(value))/lag(value)) %>% 
      ungroup()
    
  #add grouping variable for coloring
    df_plhiv <- mutate(df_plhiv, color_sexyear = paste(sex, year))
    
  #list colors & fill
    lst_col <- c("Male 2018" = "gray50", "Female 2018" = "gray50", "Male 2019" = "#7E6BC9", "Female 2019" = "#6585CF")
    lst_fil <- c("Male 2018" = "white", "Female 2018" = "white", "Male 2019" = "#7E6BC9", "Female 2019" = "#6585CF")

 

# FUNCTION FOR VISUALZING AGE/SEX PLHIV CHANGE BY PSNU --------------------

  plot_plhiv <- function(district){
    plhiv_plot <- df_plhiv %>% 
      filter(psnu == district) %>%
      ggplot(aes(age, value)) +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_path(size = 1, color = "gray50") +
      geom_point(aes(color = color_sexyear, fill = color_sexyear), size = 6, shape = 21, stroke = 2) +
      geom_text(aes(label = percent(growth, 1)), na.rm = TRUE,
                size = 2.5, color = "gray30", family = "Calibri Light", vjust = 3) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 15000), label = comma) +
      scale_color_manual(values = lst_col) +
      scale_fill_manual(values = lst_fil) +
      facet_wrap(. ~ sex) +
      theme_minimal() +
      labs(x = NULL, y = NULL,
           title = toupper(district),
           subtitle ="2018-2019 Change in PLHIV Estimates",
           caption = "Source: COP19 and COP20 Data Packs IMPATT values") +
      theme(legend.position = "none",
            text = element_text(family = "Calibri Light", size = 13),
            plot.title = element_text(size = 18, family = "Calibri", face = "bold", color = "#6BB1C9"),
            plot.subtitle = element_text(size = 14, family = "Calibri Light"),
            strip.text = element_text(size = 14, family = "Calibri", face = "bold"),
            plot.caption = element_text(color = "gray30", size = 11))
    
    time <- Sys.time() %>% str_remove_all("-|:")
    
    ggsave(paste0("PLHIV_", time, district, ".png"), plot = plhiv_plot, path = "out/plots",
           width = 10, height = 5.625, dpi = 300)
  }

    

# EXPORT PSNU PLOTS -------------------------------------------------------

  #order from largest PLHIV to least  
   psnu_ordered <- df_plhiv %>% 
      filter(year == "2019") %>% 
      count(psnu, wt = value, sort = TRUE) %>% 
      pull(psnu)
   
 #plot and explort
  walk(psnu_ordered, plot_plhiv)
  
  

# PLOT FOR OU LEVEL -------------------------------------------------------

  #aggregate to OU level  
    df_plhiv_ou <- df_plhiv %>% 
      group_by(year, age, sex, color_sexyear) %>% 
      summarise_at(vars(value), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      group_by(sex, age) %>% 
      mutate(growth = (value - lag(value))/lag(value)) %>% 
      ungroup()


  #plot
    df_plhiv_ou %>% 
      ggplot(aes(age, value)) +
      geom_hline(yintercept = 0, color = "gray30") +
      geom_path(size = 1, color = "gray50") +
      geom_point(aes(color = color_sexyear, fill = color_sexyear), size = 6, shape = 21, stroke = 2) +
      geom_text(aes(label = percent(growth, 1)), na.rm = TRUE,
                size = 2.5, color = "gray30", family = "Calibri Light", vjust = 3) +
      coord_flip() +
      scale_y_continuous(label = comma) +
      scale_color_manual(values = lst_col) +
      scale_fill_manual(values = lst_fil) +
      facet_wrap(. ~ sex) +
      theme_minimal() +
      labs(x = NULL, y = NULL,
           title = toupper("Malawi"),
           subtitle ="2018-2019 Change in PLHIV Estimates",
           caption = "Source: COP19 and COP20 Data Packs IMPATT values") +
      theme(legend.position = "none",
            text = element_text(family = "Calibri Light", size = 13),
            plot.title = element_text(size = 18, family = "Calibri", face = "bold", color = "#6BB1C9"),
            plot.subtitle = element_text(size = 14, family = "Calibri Light"),
            strip.text = element_text(size = 14, family = "Calibri", face = "bold"),
            plot.caption = element_text(color = "gray30", size = 11))
 
  
    ggsave("PLHIV_Malawi.png", path = "out/plots/PLHIV",
       width = 10, height = 5.625, dpi = 300)

