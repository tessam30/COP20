## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare HRH inventory
## DATE:     2020-02-11
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(ICPIutilities)
  library(extrafont)
  library(scales)



# GLOBALS -----------------------------------------------------------------


  theme_set(theme_minimal(base_size = 12, base_family = "Gill Sans MT"))

  pal <- add_color("woods")

# IMPORT ------------------------------------------------------------------

  #import site DHA data
    df_dha <- read_excel("data/MoH_TX_CURR_FY18Q1 to FY20Q1.xls") 

  #import HRH inventory
    df_hrh <- read_excel("data/HRH Data with UIDs.xlsx", col_types = c(.default = "text")) %>% 
      clean_names()
    
    
  #import FY18 HRH inventory
    df_hrh_fy18 <- read_excel("data/2018 HRH Inventory with UIDs.xlsx", col_types = c(.default = "text")) %>% 
      clean_names()
    

# MUNGE -------------------------------------------------------------------

  #fix var names
  df_dha <- df_dha %>% 
    rename_all(tolower) %>% 
    rename(orgunituid = orgunit,
           psnu = district) %>% 
    mutate(psnu = str_remove(psnu, " District"))
  
  #reshape
  df_dha <- df_dha %>% 
    gather(period, tx_curr, starts_with("fy")) %>% 
    mutate(period = toupper(period))
    
  #net new
  df_dha <- df_dha %>% 
    filter(period %in% c("FY18Q4","FY19Q4")) %>% 
    arrange(orgunituid, period) %>% 
    group_by(orgunituid) %>% 
    mutate(tx_net_new = tx_curr - lag(tx_curr)) %>% 
    ungroup() 
  
  #psnu level
  df_dha_psnu <- df_dha %>% 
    group_by(psnu, indicator, period) %>% 
    summarise_at(vars(tx_curr), sum, na.rm = TRUE) %>% 
    ungroup()
  
  #filter for just FY19Q4
  df_dha <- df_dha %>% 
    filter(period == "FY19Q4")

  #HRH Inventory - select key fields and rename
    df_hrh <- df_hrh %>% 
     select(orgunituid = facility_uid,
            sitename = facility,
            psnu = district,
            cadre_category,
            cadre_category_minor = minor_cadre_category,
            cadre_name,
            staff.fundedby_gov = of_government_supported_staff,
            staff.fundedby_globalfund = of_global_fund_supported_staff,
            staff.fundedby_pepfar = of_pepfar_supported_staff,
            support_type_pepfar = typeof_pepfar_support,
            workers.comp_salary = of_workersunder_salaryper_ca,
            workers.comp_stipend = of_workersunder_stipendper_c,
            workers.comp_nonmonetary = of_workersunder_non_monetary,
            amt.comp_salary = semi_annual_salary_amount_dolla,
            amt.comp_stipend = semi_annual_stipend_amount_doll,
            amt.comp_nonmonetary = semi_annual_non_monetary_benefit) %>% 
      mutate(psnu = str_remove(psnu, " District") %>% 
               str_replace("_", " ") %>% 
               str_to_sentence %>% 
               str_replace("bay", "Bay"))
            
  #HRH Inventory - convert to numeric & multiple semi annual payments by 2
    df_hrh <- df_hrh %>% 
      mutate_at(vars(starts_with("workers"), starts_with("staff")), as.integer) %>% 
      mutate_at(vars(starts_with("amt")), as.double) %>% 
      mutate_at(vars(starts_with("amt")), ~ . * 2)
  
  #HRH - clean up support type
    df_hrh <- df_hrh %>% 
      mutate(support_type_pepfar = str_to_sentence(support_type_pepfar),
             support_type_pepfar = na_if(support_type_pepfar, "0"))
  
  #HRH FY18
    df_hrh_fy18 <- df_hrh_fy18 %>% 
      select(psnu = district, 
             sitename = facility, 
             orgunituid = organisationunitid,
             starts_with("hda"),
             starts_with("expert")) %>% 
      gather(type, workers, starts_with("hda"), starts_with("expert")) %>% 
      mutate(psnu = ifelse(psnu == "Nkhatabay", "Nkhata Bay", psnu),
             workers = as.numeric(workers),
             period = "FY18Q4") %>% 
      group_by(psnu, period) %>% 
      summarise_at(vars(workers), sum, na.rm = TRUE) %>% 
      ungroup()

# EXPLORE -----------------------------------------------------------------


    df_hrh %>% 
      count(cadre_category, cadre_category_minor, cadre_name, wt = staff.fundedby_pepfar) %>% 
      arrange(cadre_category, cadre_category_minor) %>% 
      print(n = Inf)
    


# ANALYSIS ----------------------------------------------------------------

##RETENTION via HCW and Export clients    
    
  #aggregate # of HCW/Expert Clients per site
    df_hrh_hcw <- df_hrh %>%
      filter(staff.fundedby_pepfar > 0,
             cadre_name %in% c("Community Health Worker", "Expert Client")) %>% 
      select(-starts_with("staff")) %>% 
      gather(type, val, starts_with("workers"), starts_with("amt"), na.rm = TRUE) %>% 
      filter(val > 0) %>% 
      separate(type, c("type", "source"), sep = "\\.") %>%
      mutate(source = str_remove(source, "comp_") %>% str_to_sentence) %>%
      filter(support_type_pepfar == source) %>% 
      group_by(orgunituid, sitename, psnu, cadre_category, support_type_pepfar, type) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(type, val)
      
  #all source cost per lay worker
    df_hrh_hcw <- df_hrh_hcw %>% 
      group_by(orgunituid, sitename, psnu, cadre_category) %>% 
      summarise_at(vars(amt, workers), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(cost_per_hcw = amt/workers)
  
  #join with fy18 data 
    df_hrh_hcw_psnu <- df_hrh_hcw %>%
      mutate(period = "FY19Q4") %>% 
      group_by(psnu, period) %>% 
      summarise_at(vars(workers), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      bind_rows(df_hrh_fy18) %>% 
      arrange(psnu, period) %>% 
      full_join(df_dha_psnu, .)
    
  #merge
    df_hrh_hcw <- df_hrh_hcw %>% 
      full_join(df_dha, .)



    
    
  #viz cleanup
    df_viz_hcw <- df_hrh_hcw %>% 
      mutate(ratio_tx_to_hcw = tx_curr/workers) %>% 
      filter(!is.na(indicator)) %>%  #filter out hcw w/o site TX info
      group_by(psnu) %>%
      mutate(psnu_cost_per_hcw = mean(cost_per_hcw, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(psnu = fct_reorder(psnu, -psnu_cost_per_hcw)) %>%
      arrange(psnu)
      
#plot
    
    
    df_viz_hcw %>% 
      filter(!is.na(cadre_category)) %>% 
      ggplot(aes(workers, tx_net_new, size = tx_curr, color = indicator)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_point(alpha = .2, na.rm = TRUE) +
      #geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
      scale_color_manual(values = pal[1]) +
      scale_size(name = "TX_CURR (FY19)",
                 labels = comma) +
      labs(y = "TX_NET_NEW (FY18-19)", 
           x = "# of HDAs/Expert Clients (FY19)",
           caption = "Source: Malawi DHA FY18-20, FY19 Malawi HRH Inventory",
           title = "POSITIVE RELATIONSHIP BETWEEN LAY HCWS + INCREASED NET NEW",
           subtitle = "Malawi | 2019") +
      theme(plot.caption = element_text(color = "gray30", size = 10)) +
      guides(color = FALSE)
    
    
    set.seed(42)
    
    df_viz_hcw %>% 
      ggplot(aes(psnu, cost_per_hcw, color = indicator)) +
      geom_boxplot(color = "gray60", outlier.alpha = 0, na.rm = TRUE) +
      geom_jitter(size = 2, alpha = 0.25, width = 0.2, na.rm = TRUE) +
      stat_summary(fun.y = mean, geom = "point", size = 5, na.rm = TRUE) +
      coord_flip() +
      scale_color_manual(values = pal[1]) +
      scale_y_continuous(labels = comma) +
      labs(y = "site cost per CHW", x = NULL,
           caption = "Source: Malawi DHA FY18-20, FY19 Malawi HRH Inventory",
           title = "AVERAGE SITE COST PER EXPERT CLIENT/HDA",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "none",
            plot.caption = element_text(size = 10))
    
    
    
    set.seed(42)
    
    df_viz_hcw %>% 
      mutate(psnu = as.character(psnu)) %>%
      group_by(psnu) %>%
      mutate(psnu_ratio_tx_to_hcw = mean(ratio_tx_to_hcw, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(psnu = fct_reorder(psnu, -psnu_ratio_tx_to_hcw)) %>%
      ggplot(aes(psnu, ratio_tx_to_hcw, color = indicator)) +
      geom_boxplot(color = "gray60", outlier.alpha = 0, na.rm = TRUE) +
      geom_jitter(size = 2, alpha = 0.25, width = 0.2, na.rm = TRUE) +
      stat_summary(fun.y = mean, geom = "point", size = 5, na.rm = TRUE) +
      coord_flip() +
      scale_y_log10(expand = c(0.005, 0.005), labels = comma) +
      scale_color_manual(values = pal[1]) +
      labs(y = "ratio of current on Tx to CHW (log scale)", x = NULL,
           caption = "Source: Malawi DHA FY18-20, FY19 Malawi HRH Inventory",
           title = "RATIO OF PEPFAR TREATMENT PATENTS TO HCWS",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "none",
            plot.caption = element_text(color = "gray30", size = 10))
    
    psnu_order <- df_hrh_hcw_psnu %>% 
      filter(period == "FY19Q4") %>% 
      arrange(tx_curr) %>% 
      pull(psnu)
    
    df_hrh_hcw_psnu <- df_hrh_hcw_psnu %>% 
      mutate(psnu = factor(psnu, psnu_order))
      
    
    df_hrh_hcw_psnu %>% 
      ggplot(aes(psnu, tx_curr, color = period, group = psnu)) +
      geom_path(size = 1, color = "gray60") +
      geom_point(size = 5) +
      coord_flip() +
      scale_color_manual(values = c("gray60", pal[1])) +
      scale_y_continuous(label = comma) +
      labs(y = "TX_CURR Change", x = NULL,
           title = "UNIFORM INCREASE IN CURRENT ON TX FY18 to FY19",
           caption = "Source: Malawi DHA FY18-20, FY19 Malawi HRH Inventory") +
      theme(plot.caption = element_text(color = "gray30", size = 10))
    
    df_hrh_hcw_psnu %>% 
      ggplot(aes(psnu, workers, color = period, group = psnu)) +
      geom_path(size = 1, color = "gray60") +
      geom_point(size = 5) +
      coord_flip() +
      scale_color_manual(values = c("gray60", pal[1])) +
      scale_y_continuous(label = comma) +
      labs(y = "Expert Client/HDA Change", x = NULL,
           title = "ACROSS THE BOARD LAYWORKER DECLINED FROM FY18 to FY19",
           caption = "Source: Malawi DHA FY18-20, FY19 Malawi HRH Inventory") +
      theme(plot.caption = element_text(color = "gray30", size = 10))
   
    