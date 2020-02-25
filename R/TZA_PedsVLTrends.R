## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: VL Trends in Peds
## DATE:    2020-02-07
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(extrafont)
  library(ICPIutilities)
  library(nationalparkcolors)

# IMPORT ------------------------------------------------------------------

df_tza <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")


# MUNGE -------------------------------------------------------------------

  #filter to VL & aggregate
df_vl <- df_tza %>%
  filter(indicator %in% c("TX_CURR","TX_PVLS"),
         standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus","Age Aggregated/Sex/HIVStatus", 
                                         "Age/Sex/HIVStatus"),
         trendscoarse == "<15",
         fiscal_year < 2020) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
  group_by(indicator, fiscal_year) %>% 
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
  ungroup() 

#create VL
df_vl <- df_vl %>% 
  reshape_msd(clean = TRUE) %>% 
  arrange(period) %>% 
  spread(indicator, val) %>% 
  mutate(`VL Coverage` = TX_PVLS_D/lag(TX_CURR, 2),
         `VL Supression` = TX_PVLS/TX_PVLS_D) %>% 
  select(-starts_with("TX"), -period_type) %>% 
  gather(ind, val, starts_with("VL")) %>% 
  filter(!str_detect(period, "FY17"),
         !is.na(val))
  
#viz adjustments
df_viz <- df_vl %>% 
  mutate(point = case_when(period %in% c("FY18Q4", "FY19Q4") ~ val),
         lab = case_when(!is.na(point) ~ percent(val, 1)))

# PLOT --------------------------------------------------------------------

pal <- park_palette("SmokyMountains")

theme_set(theme_minimal(base_size = 11, base_family = "Calibri"))


df_viz %>% 
  ggplot(aes(period, val, group = ind, color = ind)) +
  geom_hline(yintercept = 0) +
  geom_blank(aes(y = 1.1 * val)) +
  geom_path(size = 1) +
  geom_point(aes(y = point), size = 4, na.rm = TRUE) +
  geom_point() +
  geom_text(aes(label = lab, vjust = ifelse(period == "FY18Q4", 1.8, -1)), family = "Gill Sans MT", size = 4, 
            color = "gray30", na.rm = TRUE) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(labels = c("FY18Q4", "", "FY19Q2", "", "FY19Q4")) +
  scale_color_manual(values = c(pal[2], pal[3])) +
  facet_wrap(. ~ ind) +
  labs(x = NULL, y = NULL,
       title = "VL supression among peds is growing, but not as fast as coverage",
       subtitle = "FY18Q4 - FY19Q4 Tanzania <15",
       caption = "Source: FY19Q4c MSD") +
  theme(legend.position = "none",
        plot.title = element_text(family = "Calibri", size = 14, face = "bold"),
        strip.text = element_text(family = "Calibri", size  = 12, face = "bold"),
        plot.caption = element_text(color = "gray30", size = 9))

# EXPORT ------------------------------------------------------------------

  ggsave("out/plots/TZA_PedsVL.png", dpi = 300,
         width = 10, height = 5.66) 
  