## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: recalculating NET NEW
## NOTE:    building off work from Fy19AgencySelfAssessments/Fy19Q4i_TZA_NET_NEW_Adjustment.R
## DATE:    2020-02-23
## UPDATED: 



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(ICPIutilities)
library(scales)
library(extrafont)

# API ---------------------------------------------------------------------

  #DATIM Information
    myuser <- ""
    baseurl <- "https://www.datim.org/"
  
  #API url 
  url <- 
    paste0(baseurl,
      "api/29/analytics.json?",
      "dimension=ou:LEVEL-7;mdXu6iCbn2G&", 
      "dimension=bw8KHXzxd9i&", #Funding Agency
      "dimension=SH885jaRe0o&", #Funding Mechanism
      "dimension=pe:2017Q3;2017Q4;2018Q1;2018Q2;2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4&",
      "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area: TX_CURR
      "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results: Results
      "dimension=RUkVjD3BsS1:PE5QVF0w4xj&",  #Top Level Numerator
      "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
    )
  
  
  #pull site TX_CURR data
    df_site_tx <- get_datim_targets(url, myuser, mypwd(myuser))


# MUNGE -------------------------------------------------------------------

  #keep select vars
    df_site_tx_clean <- df_site_tx %>% 
      select(operatingunit = orglvl_3,
             snu1 = orglvl_4,
             # psnu = orglvl_5,
             sitename = `Organisation unit`,
             orgunituid,
             fundingagency = `Funding Agency`,
             mech_code = `Funding Mechanism`,
             # indicator = `Technical Area`,
             period = `Period`,
             tx_curr = Value) 

  #clean up mech_code and convert period to FY period
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(mech_code = str_extract(mech_code, "[:digit:]+"),
             period = str_replace(period, "to [:alpha:]{3}", "1,") %>% 
               mdy() %>%
               quarter(with_year = TRUE, fiscal_start = 10) %>% 
               as.character %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q"))
    
  #clean agencies
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/"),
             fundingagency = factor(fundingagency, c("CDC","USAID", "DOD")))
    
    
  #add in partner and mech names
    df_site_tx_clean <- rename_official(df_site_tx_clean)
    
  #rename partners
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(primepartner = case_when(primepartner == "AMREF HEALTH AFRICA HQ" ~ "AMREF",
                                      primepartner == "ARIEL GLASER PEDIATRIC AIDS H EALTHCARE INITIATIVE" ~ "APAHI",
                                      primepartner == "MANAGEMENT AND DEVELOPMENT FO R HEALTH" ~ "MDH",
                                      primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS",
                                      primepartner %in% c("Baylor College of Medicine", "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA") ~ "Baylor",
                                      primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                      primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                      primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                      primepartner == "Henry M. Jackson Foundation For The Advancement Of Military Medicine, Inc., The" ~ "HJF",
      ))
    
    ip_order <- df_site_tx_clean %>% 
      filter(period == "FY20Q1") %>% 
      agg_by(primepartner) %>% 
      arrange(desc(tx_curr)) %>% 
      pull(primepartner) %>%
      c(., "THPS")
    
    df_site_tx_clean <- mutate(df_site_tx_clean, primepartner = factor(primepartner, ip_order))
    
  # #create net new
  #   df_site_tx_clean <- df_site_tx_clean %>% 
  #     arrange(snu1, orgunituid, mech_code, period) %>% 
  #     group_by(mech_code, orgunituid) %>% 
  #     mutate(tx_net_new = tx_curr - lag(tx_curr)) %>% 
  #     ungroup() %>% 
  #     filter(period != "FY17Q4")
  #   


# FUNCTION - PLOT TX ------------------------------------------------------

  plot_tx <- function(df, var, title){
    
    if("fundingagency_adj" %in% names(df))
      df <- mutate(df, fundingagency = fundingagency_adj)
    
    df %>% 
      mutate(placeholder = {{var}} * 1.2) %>% 
      ggplot(aes(period, {{var}})) +
      geom_hline(yintercept = 0) +
      geom_col() +
      geom_blank(aes(y = placeholder)) +
      geom_text(aes(label = comma({{var}}), 
                    vjust = ifelse({{var}} < 0, 1, -1)),
                family = "Calibri Light") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                  "FY19Q1", "", "FY19Q3", "",
                                  "FY20Q1")) +
      facet_grid(fundingagency ~ ., switch = "y") +
      labs(x = NULL, y = NULL, 
           title = title,
           caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
      theme_minimal() +
      theme(text = element_text(family = "Calibri Light"),
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(family = "Calibri"),
            plot.caption = element_text(color = "gray30"))
  }
    

    plot_tx_ip <- function(df, var, title){
      
      if("primepartner_adj" %in% names(df))
        df <- mutate(df, primepartner = primepartner_adj)
      
      df %>% 
        mutate(placeholder = {{var}} * 1.4) %>% 
        ggplot(aes(period, {{var}})) +
        geom_hline(yintercept = 0) +
        geom_col() +
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = comma({{var}}, accuracy = 1), 
                      vjust = ifelse({{var}} < 0, 1, -1)),
                  family = "Calibri Light") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "",
                                    "FY20Q1")) +
        facet_grid(primepartner ~ ., switch = "y") +
        labs(x = NULL, y = NULL, 
             title = title,
             caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Calibri Light"),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(family = "Calibri"),
              plot.caption = element_text(color = "gray30"))
    }
    
    

# FUNCTION - AGGREGATE BY -------------------------------------------------

    agg_by <- function(df, ...){
      df_agg <- df %>% 
        group_by_at(c(vars(..., period))) %>% 
        summarise_if(is.double, sum, na.rm = TRUE) %>%
        ungroup() 
      
      df_agg <- arrange_at(df_agg, vars(..., period))
      
      
      df_agg_nn <- df_agg %>%
        group_by_at(vars(...)) %>%
        mutate(tx_net_new = tx_curr - lag(tx_curr)) %>%
        ungroup() %>%
        filter(period != "FY17Q4")
      
      return(df_agg_nn)
    }
    
       
# PLOT UNADJUSTED TX TRENDS -----------------------------------------------

  
  df_tx_agency <- agg_by(df_site_tx_clean, operatingunit, fundingagency) 
  df_tx_partner <- agg_by(df_site_tx_clean, operatingunit, fundingagency, primepartner) 
  
  plot_tx(df_tx_agency, tx_curr, "TX_CURR, unadjusted")
  plot_tx(df_tx_agency, tx_net_new, "TX_NET_NEW, unadjusted")
  
  df_tx_partner %>% 
    filter(primepartner != "THPS") %>% 
    plot_tx_ip(tx_net_new, "TX_NET_NEW, unadjusted")
    

# ADJUST SITE TX VIA INHERITING -------------------------------------------

  
  #are there any sites with more than one mechanism working there in FY20Q1?
    multi_partner_sites <- df_site_tx_clean %>%
      filter(period == "FY20Q1") %>%
      distinct(orgunituid, mech_code) %>%
      count(orgunituid) %>%
      filter(n > 1) %>%
      pull(orgunituid)
    
    # df_site_tx_clean %>%
    #   filter(orgunituid %in% multi_partner_sites,
    #          period == "FY20Q1") %>%
    #   arrange(orgunituid, mech_code) %>%
    #   View()
    
  #drop dup mech in two sites
    df_site_tx_clean_adj <- df_site_tx_clean %>% 
      filter(!(period == "FY20Q1" & mech_code == "12217"))
    
  #identify which sites "belong" to which mechs in FY20Q1
    df_site_assignments <- df_site_tx_clean_adj %>%
      filter(period == "FY20Q1") %>%
      distinct(orgunituid, fundingagency, mech_code, mech_name, primepartner) %>% 
      rename_at(vars(fundingagency, mech_code, mech_name, primepartner), ~paste0(., "_adj"))
    
  #apply assignments historically
    df_site_tx_reassigned <- df_site_tx_clean_adj %>% 
      # select(-c(fundingagency, mech_code, mech_name, primepartner)) %>% 
      left_join(df_site_assignments, by = "orgunituid")
    
    
  #remove NA parnters
    # df_site_tx_reassigned %>% 
    #   count(period, is.na(primepartner_adj)) %>% 
    #   spread(`is.na(primepartner_adj)`, n)
    df_site_tx_reassigned <- df_site_tx_reassigned %>% 
      filter(!is.na(primepartner_adj))
    
# PLOT ADJUSTED TX TRENDS -------------------------------------------------
  
  #aggregate up to agency level
    df_tx_snu_adj <- agg_by(df_site_tx_reassigned, operatingunit, fundingagency_adj, snu1)
    df_tx_agency_adj <- agg_by(df_site_tx_reassigned, operatingunit, fundingagency_adj) 
    df_tx_partner_adj <- agg_by(df_site_tx_reassigned, operatingunit, primepartner_adj) 
    

    plot_tx(df_tx_agency_adj, tx_curr, "TX_CURR, reassigned")
    plot_tx(df_tx_agency_adj, tx_net_new, "TX_NET_NEW, reassigned")
    
    df_tx_partner_adj %>% 
      filter(primepartner_adj != "THPS") %>% 
      plot_tx_ip(tx_net_new, "TX_NET_NEW, reassigned")

    