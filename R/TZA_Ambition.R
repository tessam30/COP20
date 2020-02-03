## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Provide targets for Ambition funding
## DATE:    2020-02-01
## UPDATED: 2020-02-02

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(readxl)


# GLOBAL VARIABLES --------------------------------------------------------

  #msd path
    folderpath_msd <- "~/Data"
    path_msd <- list.files(folderpath_msd, "PSNU_IM", full.names = TRUE)
    
  #data pack path
    dp_path <- "data/TZ COP 19 Data Pack 20190502_sitetool_May28_100pm_forUploadv5.xlsx"
    
  #output
    folderpath_out <- "out"
    
  #Deloitte regions
    dlt_regions <- c("Ruvuma", "Mtwara", "Njombe", "Iringa", "Morogoro", "Lindi")

  #egpath regions
    # df_tza %>% 
    #   filter(indicator == "TX_CURR",
    #          fundingagency == "USAID",
    #          primepartner == "Elizabeth Glaser Pediatric AIDS Foundation",
    #          fiscal_year == 2019) %>% 
    #   count(snu1, wt = targets) %>% 
    #   pull(snu1)
    
    egpaf_regions <- c("Arusha", "Dodoma", "Kilimanjaro", "Manyara", "Singida", "Tabora")

  #clinical cascade
    cascade <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")

  #colors
    lightblue <- "#A7C6ED"
    usaidblue <- "#002F6C"
    medblue <- "#0067B9"
    usaidred <- "#BA0C2F"
    
    
# IMPORT ------------------------------------------------------------------

  #import MSD
    df_tza <- read_rds(path_msd) %>% 
      filter(operatingunit == "Tanzania")
    
    
# IMPORT AND MUNGE PLHIV FROM DP ------------------------------
    
  #import COP20 DP
    df_plhiv <- read_excel(dp_path, sheet = "Spectrum")
    
  #clean columns
    df_plhiv <-  df_plhiv %>% 
      rename_all(tolower) %>% 
      filter(str_detect(dataelement, "PLHIV")) %>% 
      select(psnuuid, age = categoryoption_name_1, sex = categoryoption_name_2, value)
    
  #add age trendscourse
    df_plhiv <- df_plhiv %>% 
      mutate(trendscoarse = ifelse(age %in% c("<01", "01-04", "05-09", "10-14"),  "<15", "15+")) 
  
  #aggregate to SNU1 level for Males
    df_plhiv_m <- df_tza %>% 
      distinct(snu1, psnu, psnuuid) %>% 
      right_join(df_plhiv) %>% 
      filter(sex == "Male") %>% 
      group_by(snu1, trendscoarse) %>% 
      summarize(plhiv = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(indicator = "TX_CURR")
      
  

# FINDING MEN -------------------------------------------------------------
    
  #key clinical cascade indicators for 15+ men with target achievement by each Deloitte region
    df_dlt_m_u15 <- df_tza %>% 
      filter(fundingagency == "USAID",
             snu1 %in% dlt_regions,
             fiscal_year == 2019,
             indicator %in% cascade,
             standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
             sex == "Male",
             trendscoarse == "15+") %>% 
      group_by(indicator, snu1, trendscoarse) %>% 
      summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(targets > 0) %>% 
      mutate(indicator = factor(indicator, cascade),
             achv = cumulative/targets)
    
  #add plhiv
    df_dlt_m_u15 <- left_join(df_dlt_m_u15, df_plhiv_m) %>% 
      mutate(indicator = factor(indicator, cascade))
    
  #oder by TX_CURR size
    snu_order <- df_dlt_m_u15 %>% 
      filter(indicator == "TX_CURR") %>% 
      count(snu1, wt = targets, sort = TRUE) %>% 
      pull(snu1)
    
  #plot
    df_dlt_m_u15 %>% 
      ggplot(aes(factor(snu1, rev(snu_order)), cumulative)) +
      geom_blank(aes(y = targets * 1.2)) + 
      geom_col(aes(factor(snu1, rev(snu_order)), plhiv), color = lightblue, size = 1, fill = NA, na.rm = TRUE) +
      geom_col(fill = lightblue, na.rm = TRUE) +
      geom_hline(yintercept = 0) +
      geom_errorbar(aes(ymin = targets, ymax = targets), size = 1, width = .5, color = usaidblue) + 
      geom_text(aes(y = targets, label = percent(achv, 1)), vjust = -2.8,
                family = "Gill Sans MT", size = 3) +
      facet_grid(. ~ indicator, scales = "free") +
      coord_flip() + 
      theme_minimal() +
      scale_y_continuous(label = comma) +
      labs(x = NULL, y = NULL, title = "CLINICAL CASCADE TARGET ACHIEVEMENT FY19",
           subtitle = "Deloitte Regions | Males 15+",
           caption = "FY19Q4i MSD\nCOP19 TZA Data Pack") + 
      theme(text = element_text(family = "Gill Sans MT"),
            plot.caption = element_text(color = "gray30"))
  #export
    write_csv(df_dlt_m_u15, file.path(folderpath_out, "TZA_FY19_Deloitte_Cascade_Males_o15v2.csv", na = ""))
    ggsave(file.path(folderpath_out, "plots","TZA_FY19_Deloitte_Cascade_Males_o15_updated.png"),
           dpi = 300, width = 10, height = 5.66)
    
  #key clinical cascade indicators for all men with target achievement by each Deloitte region
    df_dlt_m <- df_tza %>% 
      filter(fundingagency == "USAID",
             primepartner == "DELOITTE CONSULTING LIMITED",
             fiscal_year == 2019,
             indicator %in% cascade,
             standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
             sex == "Male") %>% 
      group_by(indicator, snu1) %>% 
      summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(targets > 0) %>% 
      mutate(indicator = factor(indicator, cascade),
             achv = cumulative/targets,
             trendscoarse = "All")
    
  #plot
    df_dlt_m %>% 
      ggplot(aes(factor(snu1, rev(snu_order)), cumulative)) +
      geom_blank(aes(y = targets * 1.2)) + 
      geom_col(fill = lightblue) +
      geom_hline(yintercept = 0) +
      geom_errorbar(aes(ymin = targets, ymax = targets), size = 1, width = .5, color = usaidblue) + 
      geom_text(aes(y = targets, label = percent(achv, 1)), vjust = -2.8,
                family = "Gill Sans MT", size = 3) +
      facet_grid(. ~ indicator, scales = "free") +
      coord_flip() + 
      theme_minimal() +
      scale_y_continuous(label = comma) +
      labs(x = NULL, y = NULL, title = "CLINICAL CASCADE TARGET ACHIEVEMENT FY19",
           subtitle = "Deloitte | Males All Ages",
           caption = "FY19Q4i MSD") + 
      theme(text = element_text(family = "Gill Sans MT"),
            plot.caption = element_text(color = "gray30"))
    
    write_csv(df_dlt_m, file.path(folderpath_out, "TZA_FY19_Deloitte_Cascade_Males_allagesv2.csv", na = ""))
    ggsave(file.path(folderpath_out, "TZA_FY19_Deloitte_Cascade_Males_allages.png"),
           dpi = 300, width = 10, height = 5.66)
    

# FINDING/TREATING PEDS/OVC -----------------------------------------------

  #key clincial cascade indicator for children <15 with target achievement by EGPAF region
    df_egpaf_u15 <- df_tza %>% 
      filter(fundingagency == "USAID",
             primepartner == "Elizabeth Glaser Pediatric AIDS Foundation",
             fiscal_year == 2019,
             indicator %in% cascade,
             standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
             trendscoarse == "<15") %>% 
      group_by(indicator, snu1, trendscoarse) %>% 
      summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      filter(targets > 0) %>% 
      mutate(indicator = factor(indicator, cascade),
             achv = cumulative/targets)
  
  #oder by TX_CURR size
    snu_order_egpaf <- df_egpaf_u15 %>% 
      filter(indicator == "TX_CURR") %>% 
      count(snu1, wt = targets, sort = TRUE) %>% 
      pull(snu1)
    
  #plot
    df_egpaf_u15 %>% 
      ggplot(aes(factor(snu1, rev(snu_order_egpaf)), cumulative)) +
      geom_blank(aes(y = targets * 1.2)) + 
      geom_col(fill = lightblue) +
      geom_hline(yintercept = 0) +
      geom_errorbar(aes(ymin = targets, ymax = targets), size = 1, width = .5, color = usaidblue) + 
      geom_text(aes(y = targets, label = percent(achv, 1)), vjust = -2.8,
                family = "Gill Sans MT", size = 3) +
      facet_grid(. ~ indicator, scales = "free") +
      coord_flip() + 
      theme_minimal() +
      scale_y_continuous(label = comma) +
      labs(x = NULL, y = NULL, title = "CLINICAL CASCADE TARGET ACHIEVEMENT FY19",
           subtitle = "EGPAF | Males <15",
           caption = "FY19Q4i MSD") + 
      theme(text = element_text(family = "Gill Sans MT"),
            plot.caption = element_text(color = "gray30"))
    
  #export
    write_csv(df_egpaf_u15, file.path(folderpath_out, "TZA_FY19_EGPAF_Cascade_Males_u15v2.csv", na = ""))
    ggsave(file.path(folderpath_out, "TZA_FY19_EGPAF_Cascade_Males_u15.png"),
           dpi = 300, width = 10, height = 5.66)
  
    
  #TX_CURR <15, OVC_SERV, and OVC_HIVSTAT_POS target achievemnet by EGPAF region
    df_egpaf_regions_u15 <- df_tza %>% 
      filter(fundingagency == "USAID",
             snu1 %in% egpaf_regions,
             fiscal_year == 2019,
             (indicator == "TX_CURR" & trendscoarse == "<15" & standardizeddisaggregate == "Age/Sex/HIVStatus") |
             (indicator %in% c("OVC_SERV",  "OVC_HIVSTAT_POS") & standardizeddisaggregate == "Total Numerator")) %>% 
      group_by(indicator, snu1, trendscoarse) %>% 
      summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      #filter(targets > 0) %>% 
      mutate(indicator = ifelse(indicator == "TX_CURR", "TX_CURR <15", indicator),
             indicator = factor(indicator, c("TX_CURR <15", "OVC_SERV", "OVC_HIVSTAT_POS")),
             achv = cumulative/targets,
             achv = na_if(achv, Inf))

  #plot
    df_egpaf_regions_u15 %>% 
      ggplot(aes(factor(snu1, rev(snu_order_egpaf)), cumulative)) +
      geom_blank(aes(y = targets * 1.2)) + 
      geom_col(fill = lightblue) +
      geom_hline(yintercept = 0) +
      geom_errorbar(aes(ymin = targets, ymax = targets), size = 1, width = .5, color = usaidblue, na.rm = TRUE) + 
      geom_text(aes(y = targets, label = percent(achv, 1)), vjust = -2.8,
                family = "Gill Sans MT", size = 3, na.rm = TRUE) +
      facet_grid(. ~ indicator, scales = "free") +
      coord_flip() + 
      theme_minimal() +
      scale_y_continuous(label = comma) +
      labs(x = NULL, y = NULL, title = "TREATMENT & OVC TARGET ACHIEVEMENT FY19",
           subtitle = "EGPAF Regions",
           caption = "FY19Q4i MSD") + 
      theme(text = element_text(family = "Gill Sans MT"),
            plot.caption = element_text(color = "gray30"))
    
  #export
    write_csv(df_egpaf_regions_u15, file.path(folderpath_out, "TZA_FY19_EGPAF_Regions_TX_OVCv2.csv", na = ""))
    ggsave(file.path(folderpath_out, "TZA_FY19_EGPAF_Regions_TX_OVC.csv.png"),
           dpi = 300, width = 10, height = 5.66)
    