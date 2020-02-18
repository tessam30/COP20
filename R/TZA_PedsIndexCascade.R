## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: Trends in Peds Index testing cascade
## DATE:    2020-02-17
## UPDATED: 

  path <-"data/PEPFAR-Data-Genie-OUByIMs-2020-02-17.zip"
  
  df_tza <- path %>% 
    read_msd(remove_txt = FALSE) %>% 
    filter(operatingunit == "Tanzania")

  
  df_tza %>% 
    filter(indicator == "HTS_INDEX",
           fiscal_year == 2020) %>% 
    count(standardizeddisaggregate, modality, wt = cumulative)
  
  
 df_acc <- df_tza %>% 
    filter(indicator == "HTS_INDEX",
           standardizeddisaggregate %in% c("1:Age/Sex/IndexCasesOffered", "2:Age/Sex/IndexCasesAccepted"),
           sex == "Female",
           fiscal_year == 2020) %>% 
    group_by(primepartner, standardizeddisaggregate, modality) %>% 
    summarize_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(`Adult Females Accepting Index Testing` = `2:Age/Sex/IndexCasesAccepted`/`1:Age/Sex/IndexCasesOffered`) %>% 
   select(-`2:Age/Sex/IndexCasesAccepted`, -`1:Age/Sex/IndexCasesOffered`)
    
  
df_pedscontacts <- df_tza %>% 
    filter(indicator == "HTS_INDEX",
           ((standardizeddisaggregate == "2:Age/Sex/IndexCasesAccepted" & sex == "Female" & trendscoarse == "15+") |
            (standardizeddisaggregate == "3:Age Aggregated/Sex/Contacts" & ageasentered == "<15")),
           fiscal_year == 2020) %>% 
    group_by(primepartner, standardizeddisaggregate, modality) %>% 
    summarize_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(`Child Contacts Listed by Adults Females` = `3:Age Aggregated/Sex/Contacts` / `2:Age/Sex/IndexCasesAccepted`) %>% 
    select(-`3:Age Aggregated/Sex/Contacts`,  -`2:Age/Sex/IndexCasesAccepted`)
  
  
  
 df_pedsaccepting <- df_tza %>% 
    filter(indicator == "HTS_INDEX",
           ((standardizeddisaggregate == "3:Age Aggregated/Sex/Contacts" & ageasentered == "<15") |
              (standardizeddisaggregate == "4:Age/Sex/Result" & trendscoarse == "<15")), # otherdisaggregate != "Known at Entry" 
           fiscal_year == 2020) %>% 
    group_by(primepartner, standardizeddisaggregate, modality) %>% 
    summarize_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(`Child Contacts Accepting Testing` = `4:Age/Sex/Result`/`3:Age Aggregated/Sex/Contacts`) %>% 
    select(-`4:Age/Sex/Result`, -`3:Age Aggregated/Sex/Contacts`)
    
  
  
  
 df_pedspositivity <- df_tza %>% 
    filter(indicator == "HTS_INDEX",
           standardizeddisaggregate == "4:Age/Sex/Result", 
           trendscoarse == "<15",
           otherdisaggregate != "Known at Entry",
           fiscal_year == 2020) %>% 
    group_by(primepartner, standardizeddisaggregate, modality, statushiv) %>% 
    summarize_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(statushiv, cumulative) %>% 
    mutate(`Child Index Positvity` = Positive/(Positive + Negative)) %>% 
   select(-Negative, - Positive, -standardizeddisaggregate)

 df_index <- bind_rows(df_acc, df_pedscontacts, df_pedsaccepting, df_pedspositivity)  

 df_viz <- df_index %>% 
   gather(type, value, -primepartner, -modality, na.rm = TRUE) %>% 
   mutate(modality = case_when(modality == "Index" ~ "Facility",
                               modality == "IndexMod" ~ "Community"),
          primepartner = case_when(primepartner == "AMREF HEALTH AFRICA HQ" ~ "AMREF",
                                   primepartner == "ARIEL GLASER PEDIATRIC AIDS H EALTHCARE INITIATIVE" ~ "APAHI",
                                   primepartner == "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" ~ "Baylor",
                                   primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                   primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                   primepartner == "Henry M. Jackson Foundation For The Advancement Of Military Medicine, Inc., The" ~ "Hnery Jackson",
                                   primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
                                   primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                   primepartner == "MANAGEMENT AND DEVELOPMENT FO R HEALTH" ~ "MDH",
                                   primepartner == "Trustees Of Columbia University In The City Of New York" ~ "Columbia University",
                                   primepartner == "TBD" ~ "TBD"
                                   ),
          value  = case_when(type == "Child Contacts Lists by Adults Females" ~ as.character(round(value, 1)),
                             TRUE ~ percent(value, 1)),
          type = factor(type, c("Adult Females Accepting Index Testing", "Child Contacts Listed by Adults Females",
                                "Child Contacts Accepting Testing", "Child Index Positvity")))

 df_viz %>% 
   ggplot(aes(primepartner, fct_rev(type))) +
   geom_tile(color = "gray30", fill = "white") +
   geom_text(aes(label = value), family = "Gill Sans MT", size = 4) +
   labs(x = NULL, y = NULL,
        title = "FY20 Peds Index Testing in Tanzania",
        caption = "Source: FY20Q1 DATIM Genie [2020-02-17]") +
   facet_wrap(. ~ modality, scales = "free_x") +
   theme_minimal() +
   theme(panel.grid = element_blank(),
         text = element_text(family = "Gill Sans MT"),
         plot.caption = element_text(color = "gray30"))
 
 ggsave("out/plots/TZA_PedsIndexTiles.png", dpi = 300,
        width = 10, height = 5.66)    
  