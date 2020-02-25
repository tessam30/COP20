## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: VL by age band
## DATE:    2020-02-17
## UPDATED: 2020-02-19

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)
library(ICPIutilities)
library(nationalparkcolors)

# IMPORT ------------------------------------------------------------------

path <-"data/PEPFAR-Data-Genie-PSNUByIMs-2020-02-17_VL.zip"

df_genie <- path %>% 
  read_msd(remove_txt = FALSE) %>% 
  filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

#filter to VL & aggregate
df_genie_vl <- df_genie %>%
  mutate(age = case_when(ageasentered %in% c("25-29", "30-34", "40-44", "44-49") ~ "25-49",
                         TRUE ~ ageasentered)) %>% 
    filter(indicator %in% c("TX_CURR","TX_PVLS"),
           standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus","Age Aggregated/Sex/HIVStatus", 
                                           "Age/Sex/HIVStatus"),
           age != "Unknown Age") %>%
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
    group_by(snu1, indicator, fiscal_year, sex, age) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() 

#create VL
df_genie_vl <- df_genie_vl %>% 
  reshape_msd(clean = TRUE) %>% 
  arrange(period, age) %>% 
  spread(indicator, val) %>% 
  group_by(snu1, sex, age) %>% 
  mutate(`VL Coverage` = TX_PVLS_D/lag(TX_CURR, 2)) %>% 
  ungroup() %>% 
  mutate(`VL Supression` = TX_PVLS/TX_PVLS_D) %>%
  select(-starts_with("TX"), -period_type) %>% 
  gather(ind, val, starts_with("VL")) %>% 
  filter(period == "FY20Q1")


snu_order <- df_genie %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == 2020) %>% 
  count(snu1, wt = cumulative) %>% 
  arrange(n) %>% 
  pull(snu1)

#viz adjustments
df_viz <- df_genie_vl %>% 
  mutate(lab = percent(val, 1),
         type = case_when(is.na(val) ~ as.character(NA),
                          val <.75 ~ "<75%",
                          val < .85 ~ "75-85%",
                          TRUE ~ ">85%"),
         type = factor(type, c("<75%", "75-85%", ">85%")),
         snu1 = factor(snu1, snu_order))


# PLOT --------------------------------------------------------------------

#pal <- c("#26456a", "#335B8E", "#739bcc") #blue mono
pal <- c("#F5796D", "#FBF583", "#64C195") #red yellow green

#VL coverage
df_viz %>% 
  filter(ind == "VL Coverage") %>% 
  ggplot(aes(age, snu1, fill = type), na.rm = TRUE) +
  geom_tile(color = "white") +
  geom_text(aes(label = lab), na.rm = TRUE, size = 2.5,
            family = "Gill Sans MT", color = "gray30") +
  facet_wrap(. ~ sex) +
  labs(x = NULL, y = NULL, 
       #title = "FY20Q1 Tanzania VL Coverage",
       caption = "Source: FY20Q1 Genie Pull [2020-02-17]") +
  theme_minimal() +
  scale_x_discrete(expand = c(0.005, 0.005),  position = "top") +
  scale_fill_manual(values =  pal, name = NULL, na.translate = FALSE) +
  theme(text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        panel.grid = element_blank(),
        strip.placement = "outside")

ggsave("out/plots/TZA_VLCoverage_FY20Q1.png", dpi = 300,
       width = 10, height = 5.66) 

df_viz %>% 
  filter(ind == "VL Supression") %>% 
  ggplot(aes(age, snu1, fill = type), na.rm = TRUE) +
  geom_tile(color = "white") +
  geom_text(aes(label = lab), na.rm = TRUE, size = 2.5,
            family = "Gill Sans MT", color = "gray30") +
  facet_wrap(. ~ sex) +
  labs(x = NULL, y = NULL, 
       #title = "FY20Q1 Tanzania VL Suppression",
       caption = "Source: FY20Q1 Genie Pull [2020-02-17]") +
  theme_minimal() +
  scale_x_discrete(expand = c(0.005, 0.005),  position = "top")+
  scale_fill_manual(values =  pal, name = NULL, na.translate = FALSE) +
  theme(text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30"),
        panel.grid = element_blank(),
        strip.placement = "outside")

ggsave("out/plots/TZA_VLSuppression_FY20Q1.png", dpi = 300,
       width = 10, height = 5.66) 
