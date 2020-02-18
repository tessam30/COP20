## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare HRH inventory
## DATE:     2020-02-06
## UPDATED:  2020-02-11



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

  #import SitexIM for Malawi Genie
    df_genie <- read_msd("data/PEPFAR-Data-Genie-SiteByIMs-2020-02-11_MWI_TX.zip", save_rds = FALSE, remove_txt = FALSE)

  #import HRH inventory
    df_hrh <- list.files("data", "HRH Data with UIDs", full.names = TRUE) %>%
      read_excel(col_types = c(.default = "text")) %>% 
      clean_names()
    

# MUNGE -------------------------------------------------------------------

  #MSD - filter for HRH & aggregate at psnu level
    df_genie_tx19 <- df_genie %>% 
      filter(operatingunit == "Malawi",
             #indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2019) %>% 
      group_by(psnu, sitename, orgunituid, indicator, fiscal_year) %>% 
      summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, cumulative)
    
    df_genie_proxyret19 <- df_genie %>% 
      filter(operatingunit == "Malawi",
             indicator %in% c("TX_CURR", "TX_NEW"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year %in% c(2018, 2019)) %>% 
      group_by(psnu, sitename, orgunituid, indicator, fiscal_year) %>% 
      summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, cumulative) %>% 
      arrange(orgunituid, fiscal_year) %>% 
      group_by(orgunituid) %>% 
      mutate(proxy_ret = TX_CURR / (lag(TX_CURR) + TX_NEW)) %>% 
      ungroup() %>% 
      filter(fiscal_year == 2019)

  #HRH Inventory - select key fields and rename
    df_hrh <- df_hrh %>% 
     select(orgunituid = facility_uid,
            sitename = facility,
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
            amt.comp_nonmonetary = semi_annual_non_monetary_benefit)
            
  #HRH Inventory - convert to numeric & multiple semi annual payments by 2
    df_hrh <- df_hrh %>% 
      mutate_at(vars(starts_with("workers"), starts_with("staff")), as.integer) %>% 
      mutate_at(vars(starts_with("amt")), as.double) %>% 
      mutate_at(vars(starts_with("amt")), ~ . * 2)
  
  #HRH - clean up support type
    df_hrh <- df_hrh %>% 
      mutate(support_type_pepfar = str_to_sentence(support_type_pepfar),
             support_type_pepfar = na_if(support_type_pepfar, "0"))
  

# EXPLORE -----------------------------------------------------------------


    df_hrh %>% 
      count(cadre_category, cadre_category_minor, cadre_name, wt = staff.fundedby_pepfar) %>% 
      arrange(cadre_category, cadre_category_minor) %>% 
      print(n = Inf)
    


# ANALYSIS ----------------------------------------------------------------

##RETENTION via HCW and Export clients    
    
  #aggregate # of HCW/Expert Clients per site
    df_hrh_chw <- df_hrh %>%
      filter(cadre_name %in% c("Community Health Worker", "Expert Client")) %>% 
      group_by(orgunituid, sitename, cadre_category) %>% 
      summarise_at(vars(staff.fundedby_pepfar), sum, na.rm = TRUE) %>% 
      ungroup()
     
  #merge
    df_hrh_chw <- full_join(df_genie_tx19, df_hrh_chw)

    df_hrh_chw %>% 
      count(is.na(staff.fundedby_pepfar))
  
  #viz cleanup
    df_viz_chw <- df_hrh_chw %>% 
      mutate(ratio_tx_to_chw = TX_CURR/staff.fundedby_pepfar) %>% 
      filter(psnu != "_Military Malawi",
             staff.fundedby_pepfar > 0 ) %>% 
      mutate(psnu = str_remove(psnu, " District")) %>% 
      group_by(psnu) %>% 
      mutate(psnu_ratio_tx_to_chw = median(ratio_tx_to_chw, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(psnu = fct_reorder(psnu, -psnu_ratio_tx_to_chw)) %>% 
      arrange(psnu)
      
#plot
    
    set.seed(42)
    
    
    df_viz_chw %>% 
      ggplot(aes(psnu, ratio_tx_to_chw, color = as.character(fiscal_year))) +
      geom_boxplot(color = "gray60", outlier.alpha = 0, na.rm = TRUE) +
      geom_jitter(size = 2, alpha = 0.25, width = 0.2, na.rm = TRUE) +
      stat_summary(fun.y = mean, geom = "point", size = 5, na.rm = TRUE) +
      coord_flip() +
      scale_y_log10(expand = c(0.005, 0.005), labels = comma) +
      scale_color_manual(values = pal[1]) +
      labs(y = "ratio of current on Tx to CHW (log scale)", x = NULL,
           caption = "Source: FY19Q4c MSD, FY19 Malawi HRH Inventory",
           title = "RATIO OF PEPFAR TREATMENT PATENTS TO HCWS",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "none",
            plot.caption = element_text(size = 10))
    
    #export
    ggsave("out/plots/MWI_HRH_TXtoHCWratio.png", dpi = 300,
           height = 7.5, width = 13.33)
    

    
##Retention
    df_hrh_chw_ret <- full_join(df_genie_proxyret19, df_hrh_chw)
    
    
    #viz cleanup
    df_viz_chw_ret <- df_hrh_chw_ret %>% 
      mutate(ratio_tx_to_chw = TX_CURR/staff.fundedby_pepfar) %>% 
      filter(psnu != "_Military Malawi",
             staff.fundedby_pepfar > 0) %>% 
      mutate(psnu = str_remove(psnu, " District")) %>% 
      group_by(psnu) %>% 
      mutate(psnu_ratio_tx_to_chw = median(ratio_tx_to_chw, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(psnu = fct_reorder(psnu, -psnu_ratio_tx_to_chw)) %>% 
      arrange(psnu)
    
    set.seed(42)
    
    
    df_viz_chw_ret %>%
      ggplot(aes(ratio_tx_to_chw, proxy_ret)) +
      geom_hline(yintercept = 1, color = 'gray30', linetype = "dashed") +
      geom_point(alpha = .15, size = 3) +
      scale_x_log10(expand = c(0.005, 0.005), labels = comma) +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "ratio of current on Tx to CHW (log scale)", y = "Proxy Retention",
           caption = "Source: FY19Q4c MSD, FY19 Malawi HRH Inventory",
           title = "COMPARISON OF RATIO OF PEPFAR TREATMENT PATENTS TO HCWs TO PROXY RETENTION",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "none",
            plot.caption = element_text(size = 10))
    
    df_viz_chw_ret %>%
      ggplot(aes(staff.fundedby_pepfar, proxy_ret)) +
      geom_hline(yintercept = 1, color = 'gray30', linetype = "dashed") +
      geom_jitter(size = 3, alpha = 0.25, width = .02, na.rm = TRUE) +
      scale_x_log10(expand = c(0.005, 0.005), labels = comma) +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = "PEPFAR Supported CHW (log scale)", y = "Proxy Retention",
           caption = "Source: FY19Q4c MSD, FY19 Malawi HRH Inventory",
           title = "COMPARISON OF PEPFAR HCWs TO PROXY RETENTION",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "none",
            plot.caption = element_text(size = 10))
     
    
     
      
    df_viz_ret_comp <- df_hrh_chw_ret %>% 
      mutate(hcw = ifelse(is.na(staff.fundedby_pepfar), "No HWCs", "1+ HWCs")) %>% 
      group_by(psnu, hcw) %>% 
      summarise_at(vars(proxy_ret), mean, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(psnu != "_Military Malawi") %>% 
      mutate(psnu = str_remove(psnu, " District")) %>% 
      spread(hcw, proxy_ret) %>% 
      mutate(psnu = fct_reorder(psnu, `1+ HWCs`)) %>% 
      gather(hcw, proxy_ret, -psnu)
    
    #export
    ggsave("out/plots/MWI_HRH_HCWtoRet.png", dpi = 300,
           height = 7.5, width = 13.33)
    
    
    
    df_viz_ret_comp %>% 
      ggplot(aes(proxy_ret, psnu, color = hcw, fill = hcw, group = psnu)) +
      geom_path(color = "gray60", na.rm = TRUE) +
      geom_point(size = 6, shape = 21,stroke = 2, na.rm = TRUE) +
      scale_x_continuous(expand = c(0.005, 0.005), labels = percent_format(1)) +
      scale_color_manual(values = c(pal[1], "gray50")) +
      scale_fill_manual(values = c(pal[1], "white")) +
      labs(x = "Proxy Retention", y = NULL, color = NULL, fill = NULL,
           caption = "Source: FY19Q4c MSD, FY19 Malawi HRH Inventory",
           title = "AVERGE SITE PROXY RETENTION WITH/WITHOUT HWC",
           subtitle = "Malawi | 2019") +
      theme(legend.position = "top",
            plot.caption = element_text(size = 10))
    
    #export
    ggsave("out/plots/MWI_HRH_PSNU_RetAvg_HWC.png", dpi = 300,
           height = 7.5, width = 13.33)
    
    
    
                         