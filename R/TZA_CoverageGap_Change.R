## PROJECT:  COP20 TZA
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  Visualize change in coverage gap
## DATE:     2020-02-10
## UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(nationalparkcolors)


# FUNCTION TO IMPORT AND MUNGE PLHIV FROM DP ------------------------------


import_impatt <- function(filepath, estimate_year){
  
  df <- read_excel(filepath, sheet = "Spectrum")
  
  if("psnu_id" %in% names(df))
    df <- rename(df, psnuuid = psnu_id)
  
  df <- df %>% 
    rename_all(tolower) %>% 
    filter(dataelement %in% c("IMPATT.PLHIV (SUBNAT, Age/Sex)", 
                              "TX_CURR_SUBNAT (N, SUBNAT, Age Aggregated/Sex): Receiving ART")) %>%
    mutate(psnu = str_remove(psnu, " \\(.*"),
           psnuuid = str_remove_all(psnuuid, "\\[|\\]")) %>% 
    group_by(psnu, psnuuid, dataelement) %>% 
    summarise_at(vars(value), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(indicator = dataelement) %>% 
    mutate(indicator = case_when(indicator == "IMPATT.PLHIV (SUBNAT, Age/Sex)" ~ "PLHIV",
                                 TRUE ~ "TX_CURR_SUBNAT"),
           year = estimate_year) #%>%
    # unite(ind, c("indicator", "year")) %>% 
    # spread(ind, value)
  
  return(df)
  
}

# IMPORT ------------------------------------------------------------------


#org hierarhcy - https://www.datim.org/api/sqlViews/qyp2aXRK52d/data.csv?var=OU:TZ

df_orgs <- read_csv("data/TZA OU sites.csv", col_types = c(.default = "c"))


#COP19 DP
df_cop19 <- import_impatt("data/TZ COP 19 Data Pack 20190502_sitetool_May28_100pm_forUploadv5.xlsx", 2019)


#COP20 DP
df_cop20 <- import_impatt("data/Data Pack_Tanzania_Mag Updates-3.2.20_SpectrumAsText.xlsx", 2020)



# MUNGE -------------------------------------------------------------------


df_orgs <- df_orgs %>% 
  select(psnuuid = orgunit_internal_id, snu1 = orgunit_parent)

df_impatt <- bind_rows(df_cop20, df_cop19) %>% 
  left_join(df_orgs) %>%
  spread(indicator, value) %>% 
  group_by(snu1, year) %>% 
  summarise_at(vars(PLHIV, TX_CURR_SUBNAT), sum, na.rm = TRUE) %>% 
  ungroup()
  
order <- df_impatt %>% 
  filter(year == 2020) %>% 
  count(snu1, wt = PLHIV) %>% 
  arrange(n) %>% 
  pull(snu1)


df_impatt <- df_impatt %>% 
  mutate(snu1 = factor(snu1, order),
         year = as.character(year),
         coverage = TX_CURR_SUBNAT/PLHIV ,
         coverage_lab = percent(coverage, 1),
         unmetneed = PLHIV - TX_CURR_SUBNAT,
         unmetneed_lab = comma(unmetneed, 1)) %>% 
  filter(!is.na(snu1))

# PLOT --------------------------------------------------------------------

pal <- park_palette("SmokyMountains")

df_impatt %>% 
  ggplot(aes(snu1, PLHIV, color = year, fill = year)) +
  geom_blank(aes(y = PLHIV * 1.05)) +
  geom_col(fill = NA) +
  geom_col(aes(y = TX_CURR_SUBNAT)) +
  geom_text(aes(label = unmetneed_lab), hjust = -.2,
            family = "Calibri Light") +
  coord_flip() +
  facet_wrap(. ~ year) +
  labs(x = NULL, y = NULL, 
       title = "SHRINKING UNMET NEED",
       caption = "Source: COP19 + COP20 Spectrum Estimates [Data Pack]") +
  scale_y_continuous(label = comma) +
  scale_color_manual(aesthetics = c("colour", "fill"), values = c(pal[3], pal[2])) + #c("blue", "black")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Calibri Light"),
        plot.caption = element_text(family = "Calibri Light", size = 9, color = "gray30"),
        title = element_text(family = "Calibri", face = "bold", size = 18),
        strip.text = element_text(family = "Calibri", face = "bold", size = 16))

#export
ggsave("out/plots/TZA_UnmetNeed.png", dpi = 300,
       height = 7.5, width = 13.33)
