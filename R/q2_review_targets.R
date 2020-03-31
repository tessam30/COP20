##  PROJECT: Q2 review target analysis
##  AUTHOR:  jdavis | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-03-30
##  UPDATE:

#Dependancies----------------------------------------------------------
library(tidyverse)
library(ICPIutilities)
library(tameDP)
#install.packages("janitor")
library(janitor)
library(ggplot2)
library(skimr)
library(scales)
library(extrafont)
library(patchwork)

indc <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "OVC_SERV", "KP_PREV")

#folders---------------------------------------------------------------
data_in <- "C:/Users/Josh/Documents/data/fy20_q1_v1/targets"
data_out <- "C:/Users/Josh/Documents/data/fy20_q1_v1/data_out"
dps <- "C:/Users/Josh/Documents/data/fy20_q1_v1/cop20_dps"

#create cop20 targets--------------------------------------------------

files <- list.files(dps, full.names = TRUE)

#read in all DPs and combine into one data frame
df_all <- map_dfr(.x = files,
                  .f = ~ tame_dp(.x))


#munge historic targets------------------------------------------------

df_15_16 <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY15-17_20200320_v2_1.rds"))

df_15_16 <- df_15_16 %>% 
  filter(standardizeddisaggregate == "Total Numerator") %>% 
  group_by(operatingunit, fiscal_year, indicator, fundingagency) %>% 
  summarise(targets = sum(targets)) %>%
  ungroup() %>% 
  filter(targets != 0)

#read_msd("C:/Users/Josh/Documents/data/fy20_q1_v1/MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.zip")

df_curr <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.rds")) %>% 
  filter(standardizeddisaggregate == "Total Numerator") %>% 
  group_by(operatingunit, fiscal_year, indicator, fundingagency) %>% 
  summarise(targets = sum(targets, na.rm = TRUE)) %>%
  ungroup()

df_mer <- bind_rows(df_curr, df_15_16) %>%
  mutate(agency_other = case_when(fundingagency == "USAID" ~ "USAID",
                                  fundingagency == "HHS/CDC" ~ "CDC",
                                  TRUE ~ "Other"),
         agency_other = factor(agency_other, c("USAID", "CDC", "Other")))

df_mer %>% filter(indicator == "TX_CURR",
                   fundingagency %in% c("USAID", "HHS/CDC"),
                   fiscal_year %in% c(2018, 2019)) %>% distinct(agency_other, fiscal_year, targets)

df_mer %>% group_by(fiscal_year, indicator, agency_other) %>% summarise(targets = sum(targets))

#Viz munge-----------------------------------------------------------
# create proportion

df_viz <- df_mer %>%
  filter(indicator %in% indc) %>% 
  group_by(indicator, fiscal_year, operatingunit) %>%
  mutate(share_ou = targets / sum(targets, na.rm = TRUE),
         share_ou = round(share_ou, 2)) %>% 
  ungroup() %>%
  group_by(indicator, fiscal_year) %>%
  mutate(fy_indic_total = sum(targets, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(indicator, fiscal_year, fundingagency) %>%
  mutate(agency_indic_share = targets / fy_indic_total,
         agency_indic_share = round(agency_indic_share, 2))


#viz-----------------------------------------------------------------

# Compute the cumulative percentages (top of each rectangle)
df_viz_gl$ymax <- cumsum(df_viz_gl$share_gl)

# Compute the bottom of each rectangle
df_viz_gl$ymin <- c(0, head(df_viz_gl$ymax, n=-1))

# Compute label position
df_viz_gl$labelPosition <- (df_viz_gl$ymax + df_viz_gl$ymin) / 2

# Compute a good label
df_viz_gl$label <- paste0(df_viz_gl$indicator, "\n value: ", df_viz_gl$share_gl)


df_viz_gl %>% filter(indicator == "HTS_TST") %>% 
ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=share_gl)) +
  geom_rect() +
  facet_grid(. ~ fiscal_year) +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

## viz part two
df_viz %>% filter(indicator == "HTS_TST") %>% 
ggplot(aes(x=fiscal_year, y=agency_indic_share, fill=agency_other)) + 
  geom_area(alpha=0.6 , size=0.5, colour="black")










